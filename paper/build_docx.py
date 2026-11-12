#!/usr/bin/env python3
"""Fill the official SSAC 2027 Word template with the manuscript.

Inputs : paper/manuscript.txt (text with light markup), results/paper/ (numbers, tables and
         figures written by section 9 of ranking.R), paper/references.json (Crossref metadata).
Output : paper/SSAC27_268_Beyond_the_Rank_Order.docx (then convert to PDF with LibreOffice).
Usage  : python3 paper/build_docx.py [template.docx]   (default: paper/SSAC27_paper_template.docx,
         the official SSAC 2027 template, not redistributed in the repository)

Markup : # / ## headings, blank-line paragraphs, *italic*, **bold**, ^sup^, ~sub~,
         [@key; @key] citations, {{number_key}}, \\eqref{label}, [[placeholder]] (highlighted),
         EQ|label|latex, FIG|file|width_in|caption, TAB|csv|caption|col widths, REFS.
"""
import copy, csv, json, re, subprocess, sys, tempfile, zipfile
from pathlib import Path
from docx import Document
from docx.shared import Inches, Pt
from docx.oxml.ns import qn
from docx.oxml import OxmlElement
from lxml import etree

ROOT = Path(__file__).resolve().parent.parent
TEMPLATE = Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT / "paper" / "SSAC27_paper_template.docx"
OUT = ROOT / "paper" / "SSAC27_268_Beyond_the_Rank_Order.docx"
ASSETS = ROOT / "results" / "paper"
PANDOC = "pandoc"

TITLE = "Beyond the Rank Order: Quantifying Uncertainty in Physical-Test Rankings for Football Player Selection"
TRACK = "Paper Track: Soccer"
PAPER_ID = "Paper ID: 268"
GITHUB = "github.com/praveenmaths89/athlete-ranking-uncertainty"

numbers = {r["key"]: r["value"] for r in csv.DictReader(open(ASSETS / "numbers.csv", encoding="utf-8"))}
refs = json.load(open(ROOT / "paper" / "references.json", encoding="utf-8"))
cite_order = []

doc = Document(TEMPLATE)
body = doc.element.body
P = doc.paragraphs
proto = {k: copy.deepcopy(P[i]._p) for k, i in
         dict(section=5, blank_after_section=6, body=7, subsection=12, refs_head=29, refs_blank=30,
              ref=31, appendix_head=38).items()}
proto["eq"] = copy.deepcopy(doc.tables[0]._tbl)

# ---- title block -----------------------------------------------------------------------------
def set_text(p, text, size=None):
    runs = p.runs
    rpr = copy.deepcopy(runs[0]._r.find(qn("w:rPr"))) if runs else None
    for r in runs:
        r._r.getparent().remove(r._r)
    r = p.add_run(text)
    if rpr is not None:
        r._r.insert(0, rpr)
    if size:
        r.font.size = Pt(size)
    return p

set_text(P[0], TITLE)
set_text(P[2], TRACK)
set_text(P[3], PAPER_ID)
gh = copy.deepcopy(P[3]._p)
P[3]._p.addnext(gh)
from docx.text.paragraph import Paragraph
set_text(Paragraph(gh, P[3]._parent), GITHUB, size=12)

# ---- remove the template body after the title block ----------------------------------------
start = P[5]._p
sectPr = body.find(qn("w:sectPr"))
el = start
while el is not None and el is not sectPr:
    nxt = el.getnext()
    body.remove(el)
    el = nxt

def append(el):
    sectPr.addprevious(el)
    return el

# ---- inline text ---------------------------------------------------------------------------
TOKEN = re.compile(r"(\*\*.+?\*\*|\*.+?\*|\^.+?\^|~.+?~|\[@[^\]]+\]|\[\[.+?\]\]|\\eqref\{[^}]+\})")
eq_numbers = {}

def fill_numbers(text):
    def rep(m):
        k = m.group(1)
        if k not in numbers:
            raise KeyError(f"number '{k}' missing from results/paper/numbers.csv")
        return numbers[k]
    return re.sub(r"\{\{(\w+)\}\}", rep, text)

def cite(keys):
    nums = []
    for k in keys:
        if k not in refs:
            raise KeyError(f"reference '{k}' missing from references.json")
        if k not in cite_order:
            cite_order.append(k)
        nums.append(cite_order.index(k) + 1)
    nums = sorted(set(nums))
    parts, i = [], 0
    while i < len(nums):
        j = i
        while j + 1 < len(nums) and nums[j + 1] == nums[j] + 1:
            j += 1
        parts.append(f"{nums[i]}\u2013{nums[j]}" if j - i >= 2 else ", ".join(map(str, nums[i:j + 1])))
        i = j + 1
    return "[" + ", ".join(parts) + "]"

def make_run(text, size=11, bold=False, italic=False, vert=None, highlight=False):
    r = OxmlElement("w:r")
    rpr = OxmlElement("w:rPr")
    f = OxmlElement("w:rFonts"); f.set(qn("w:ascii"), "Cambria"); f.set(qn("w:hAnsi"), "Cambria"); rpr.append(f)
    if bold: rpr.append(OxmlElement("w:b"))
    if italic: rpr.append(OxmlElement("w:i"))
    if highlight:
        h = OxmlElement("w:highlight"); h.set(qn("w:val"), "yellow"); rpr.append(h)
    for tag in ("w:sz", "w:szCs"):
        s = OxmlElement(tag); s.set(qn("w:val"), str(int(size * 2))); rpr.append(s)
    if vert:
        v = OxmlElement("w:vertAlign"); v.set(qn("w:val"), vert); rpr.append(v)
    r.append(rpr)
    t = OxmlElement("w:t"); t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve"); t.text = text
    r.append(t)
    return r

def field_runs(instr, shown, size=11):
    out = []
    for kind in ("begin", None, "separate", "text", "end"):
        if kind == "text":
            out.append(make_run(shown, size)); continue
        r = make_run("", size); r.remove(r.find(qn("w:t")))
        if kind is None:
            it = OxmlElement("w:instrText"); it.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
            it.text = f" {instr} "; r.append(it)
        else:
            fc = OxmlElement("w:fldChar"); fc.set(qn("w:fldCharType"), kind); r.append(fc)
        out.append(r)
    return out

def add_inline(p, text, size=11, bold=False):
    text = fill_numbers(text)
    for tok in TOKEN.split(text):
        if not tok:
            continue
        if tok.startswith("**"):
            p.append(make_run(tok[2:-2], size, bold=True))
        elif tok.startswith("[@"):
            p.append(make_run(cite([k.strip().lstrip("@") for k in tok[1:-1].split(";")]), size, bold))
        elif tok.startswith("[["):
            p.append(make_run("[" + tok[2:-2] + "]", size, bold, highlight=True))
        elif tok.startswith("\\eqref"):
            lab = tok[7:-1]
            p.append(make_run("(", size)); [p.append(r) for r in field_runs(f"REF {lab} \\h", str(eq_numbers[lab]), size)]
            p.append(make_run(")", size))
        elif tok.startswith("*"):
            p.append(make_run(tok[1:-1], size, bold, italic=True))
        elif tok.startswith("^"):
            p.append(make_run(tok[1:-1], size, bold, vert="superscript"))
        elif tok.startswith("~"):
            p.append(make_run(tok[1:-1], size, bold, vert="subscript"))
        else:
            p.append(make_run(tok, size, bold))

def clear_runs(p):
    for child in list(p):
        if child.tag != qn("w:pPr"):
            p.remove(child)
    return p

def paragraph(text, kind="body", size=11, bold=False, align=None, keep_next=False, after=None):
    p = clear_runs(copy.deepcopy(proto[kind]))
    ppr = p.find(qn("w:pPr"))
    if align:
        jc = ppr.find(qn("w:jc"))
        if jc is None:
            jc = OxmlElement("w:jc"); ppr.append(jc)
        jc.set(qn("w:val"), align)
    if keep_next:
        ppr.insert(0, OxmlElement("w:keepNext"))
    if after is not None:
        sp = ppr.find(qn("w:spacing"))
        if sp is None:
            sp = OxmlElement("w:spacing"); ppr.append(sp)
        sp.set(qn("w:after"), str(after))
    add_inline(p, text, size, bold)
    return append(p)

# ---- equations: LaTeX -> OMML via pandoc, placed in the template's equation table --------------
def omml(latex):
    with tempfile.TemporaryDirectory() as d:
        md, dx = Path(d) / "e.md", Path(d) / "e.docx"
        md.write_text(f"$${latex}$$\n", encoding="utf-8")
        subprocess.run([PANDOC, str(md), "-o", str(dx)], check=True)
        xml = zipfile.ZipFile(dx).read("word/document.xml")
    root = etree.fromstring(xml)
    m = root.find(".//{http://schemas.openxmlformats.org/officeDocument/2006/math}oMathPara")
    if m is None:
        m = root.find(".//{http://schemas.openxmlformats.org/officeDocument/2006/math}oMath")
    return m

bookmark_id = [100]
def equation(label, latex):
    n = len(eq_numbers) + 1
    eq_numbers[label] = n
    t = copy.deepcopy(proto["eq"])
    cells = t.findall(".//" + qn("w:tc"))
    mid = cells[1].find(qn("w:p"))
    for old in mid.findall("{http://schemas.openxmlformats.org/officeDocument/2006/math}oMathPara"):
        mid.remove(old)
    mid.append(omml(latex))
    right = cells[2].find(qn("w:p"))
    for bs in right.iter(qn("w:bookmarkStart")):
        bs.set(qn("w:name"), label); bs.set(qn("w:id"), str(bookmark_id[0]))
    for be in right.iter(qn("w:bookmarkEnd")):
        be.set(qn("w:id"), str(bookmark_id[0]))
    bookmark_id[0] += 1
    for tt in right.iter(qn("w:t")):
        if tt.text and tt.text.strip().isdigit():
            tt.text = str(n)
    append(t)

# ---- figures and tables ----------------------------------------------------------------------
counters = {"Figure": 0, "Table": 0}

def caption(text, size=10):
    m = re.match(r"((?:Figure|Table) [A-Z]?\d+):\s*(.*)", text)
    p = clear_runs(copy.deepcopy(proto["body"]))
    add_inline(p, "**" + m.group(1) + ".** " + m.group(2), size)
    return append(p)

def figure(file, width, cap):
    p = doc.add_paragraph()
    p.alignment = 1
    p.paragraph_format.keep_with_next = True
    p.paragraph_format.space_after = Pt(4)
    p.add_run().add_picture(str(ASSETS / file), width=Inches(float(width)))
    caption(cap)

def set_cell_border(cell, **edges):
    tcPr = cell._tc.get_or_add_tcPr()
    b = OxmlElement("w:tcBorders")
    for edge, val in edges.items():
        e = OxmlElement(f"w:{edge}"); e.set(qn("w:val"), "single"); e.set(qn("w:sz"), str(val))
        e.set(qn("w:space"), "0"); e.set(qn("w:color"), "000000"); b.append(e)
    tcPr.append(b)

def table(file, cap, widths, row_filter=None):
    rows = list(csv.reader(open(ASSETS / file, encoding="utf-8")))
    if row_filter:   # presentation only, e.g. "Age-adjusted rank:26-48" keeps that rank window
        col, rng = row_filter.rsplit(":", 1)
        lo, hi = (float(x) for x in rng.split("-"))
        j = rows[0].index(col)
        rows = [rows[0]] + [r for r in rows[1:] if lo <= float(r[j]) <= hi]
    cp = caption(cap)
    cp.find(qn("w:pPr")).insert(0, OxmlElement("w:keepNext"))
    sp = cp.find(qn("w:pPr")).find(qn("w:spacing")); sp.set(qn("w:after"), "60")
    widths = [float(w) for w in widths.split(",")]
    t = doc.add_table(rows=len(rows), cols=len(rows[0]))
    t.autofit = False
    for j, gc in enumerate(t._tbl.tblGrid.findall(qn("w:gridCol"))):
        gc.set(qn("w:w"), str(int(widths[j] * 1440)))
    t.alignment = 1
    for i, row in enumerate(rows):
        for j, val in enumerate(row):
            c = t.cell(i, j)
            c.width = Inches(widths[j])
            p = c.paragraphs[0]
            p.paragraph_format.space_after = Pt(0)
            # short tables (<= 15 rows) are never split across pages; long ones keep caption + first rows together
            p.paragraph_format.keep_with_next = i < (len(rows) - 1 if len(rows) <= 15 else min(3, len(rows) - 1))
            p.alignment = 0 if j == 0 or (j == 1 and "KPI" in rows[0][1]) else 1
            add_inline(p._p, val, size=9, bold=(i == 0))
            edges = {}
            if i == 0: edges.update(top=8, bottom=4)
            if i == len(rows) - 1: edges.update(bottom=8)
            if edges: set_cell_border(c, **edges)
    trPr = t.rows[0]._tr.get_or_add_trPr(); trPr.append(OxmlElement("w:tblHeader"))   # repeat header row
    for r in t.rows:
        cs = OxmlElement("w:cantSplit"); r._tr.get_or_add_trPr().append(cs)
    spacer = doc.add_paragraph(); spacer.paragraph_format.space_after = Pt(6)

# ---- references (APA 7, numbered in order of first citation) -----------------------------------
def apa_authors(a):
    names = [f"{fam}, {' '.join(g[0] + '.' for g in re.split(r'[ -]', giv) if g)}".strip().rstrip(",") for fam, giv in a]
    if len(names) > 20:
        names = names[:19] + ["\u2026 " + names[-1]]
    if len(names) == 1:
        return names[0]
    return ", ".join(names[:-1]) + ", & " + names[-1]

def reference_text(r):
    title = r["title"].strip().rstrip(".")
    if r.get("subtitle"):
        title += ": " + r["subtitle"].strip()
    title = re.sub(r"<[^>]+>", "", title)
    s = f"{apa_authors(r['authors'])} ({r['year']}). "
    if r["type"] == "book":
        s += f"*{title}*. {r['publisher']}."
    elif r["type"] == "proceedings-article":
        s += f"{title}. In *{r['container']}* (pp. {r['page'].replace('-', chr(8211))}). {r['publisher']}."
    else:
        s += f"{title}. *{re.sub(r'<[^>]+>', '', r['container']).replace('&amp;', '&')}*"
        if r.get("volume"): s += f", *{r['volume']}*"
        if r.get("issue"): s += f"({r['issue']})"
        if r.get("page"): s += f", {r['page'].replace('-', chr(8211))}"
        s += "."
    return s + f" https://doi.org/{r['doi']}"

def references():
    append(clear_runs(copy.deepcopy(proto["refs_head"]))).append(make_run("References", 16, bold=True))
    append(copy.deepcopy(proto["refs_blank"]))
    for i, k in enumerate(cite_order, 1):
        p = clear_runs(copy.deepcopy(proto["ref"]))
        ppr = p.find(qn("w:pPr"))
        ind = OxmlElement("w:ind"); ind.set(qn("w:left"), "426"); ind.set(qn("w:hanging"), "426"); ppr.append(ind)
        sp = OxmlElement("w:spacing"); sp.set(qn("w:after"), "80"); ppr.append(sp)
        jc = ppr.find(qn("w:jc"))
        if jc is not None: jc.set(qn("w:val"), "left")
        add_inline(p, f"[{i}]\t" + reference_text(refs[k]).replace("*", "\x00").replace("\x00", "*"))
        append(p)

# ---- walk the manuscript ---------------------------------------------------------------------------
import os
text = Path(os.environ.get("MANUSCRIPT", ROOT / "paper" / "manuscript.txt")).read_text(encoding="utf-8")
blocks = [b.strip() for b in re.split(r"\n\s*\n", text) if b.strip()]
in_appendix = False
for b in blocks:
    if b.startswith("# "):
        title = b[2:].strip()
        if title == "Appendix":
            in_appendix = True
            h = append(clear_runs(copy.deepcopy(proto["appendix_head"]))); h.append(make_run("Appendix", 16, bold=True))
            h.find(qn("w:pPr")).insert(0, OxmlElement("w:keepNext"))
            bl = append(copy.deepcopy(proto["refs_blank"])); bl.find(qn("w:pPr")).insert(0, OxmlElement("w:keepNext"))
            continue
        p = clear_runs(copy.deepcopy(proto["section"])); p.append(make_run(title, 16, bold=True)); append(p)
        p.find(qn("w:pPr")).insert(0, OxmlElement("w:keepNext"))
        bl = append(copy.deepcopy(proto["blank_after_section"])); bl.find(qn("w:pPr")).insert(0, OxmlElement("w:keepNext"))
    elif b.startswith("## "):
        p = clear_runs(copy.deepcopy(proto["subsection"])); p.append(make_run(b[3:].strip(), 12, bold=True)); append(p)
        p.find(qn("w:pPr")).insert(0, OxmlElement("w:keepNext"))
    elif b.startswith("EQ|"):
        _, lab, latex = b.split("|", 2); equation(lab, latex)
    elif b.startswith("FIG|"):
        _, f, w, cap = b.split("|", 3); figure(f, w, cap)
    elif b.startswith("TAB|"):
        parts = b.split("|"); table(parts[1], parts[2], parts[3], fill_numbers(parts[4]) if len(parts) > 4 else None)
    elif b == "REFS":
        references()
    elif re.match(r"\d+\. ", b):   # numbered list: one paragraph per item, hanging indent
        for item in re.split(r"\n(?=\d+\. )", b):
            p = paragraph(" ".join(item.split("\n")), after=80)
            ind = OxmlElement("w:ind"); ind.set(qn("w:left"), "360"); ind.set(qn("w:hanging"), "360")
            p.find(qn("w:pPr")).append(ind)
        p.find(qn("w:pPr")).find(qn("w:spacing")).set(qn("w:after"), "220")
    else:
        paragraph(" ".join(b.split("\n")))

# Drop the template's sample footnote (its reference was in the removed sample text)
for rel in doc.part.rels.values():
    if rel.reltype.endswith("/footnotes"):
        fpart = rel.target_part
        froot = etree.fromstring(fpart.blob)
        for fn in froot.findall(qn("w:footnote")):
            if fn.get(qn("w:id")) not in ("-1", "0"):
                froot.remove(fn)
        fpart._blob = etree.tostring(froot, xml_declaration=True, encoding="UTF-8", standalone=True)
        if hasattr(fpart, "_element"):
            fpart._element = froot
doc.core_properties.author = ""
doc.core_properties.last_modified_by = ""
doc.core_properties.title = TITLE
doc.save(OUT)
print(f"wrote {OUT}; {len(cite_order)} references cited; {len(eq_numbers)} equations")
unused = sorted(set(refs) - set(cite_order))
print("not cited (omitted from list):", ", ".join(unused))
