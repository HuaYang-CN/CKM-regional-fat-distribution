"""Reproduce current main-figure presentation from the original assembled PDFs.
No models, statistical tests, or participant-level calculations are performed.
"""
from pathlib import Path
import argparse, shutil, math, os
import pypdfium2 as pdfium
import pymupdf as fitz
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.colors import HexColor
parser=argparse.ArgumentParser()
parser.add_argument('--source-dir',required=True)
parser.add_argument('--output-dir',required=True)
args=parser.parse_args()
SRC=Path(args.source_dir).resolve();FIG=Path(args.output_dir).resolve();FIG.mkdir(parents=True,exist_ok=True)
if any(FIG.iterdir()): raise ValueError('Use an empty output directory.')
if SRC==FIG:raise ValueError('Use a different output directory to preserve the source figures.')
fonts=Path(os.environ.get('CKM_FONT_ROOT', Path(__file__).resolve().parent/'fonts'))
for n,f in [('Sans','LiberationSans-Regular.ttf'),('SansBold','LiberationSans-Bold.ttf')]:pdfmetrics.registerFont(TTFont(n,str(fonts/f)))
def generate_flow():
    path=FIG/'Figure_1.pdf';w,h=496.063,460;c=canvas.Canvas(str(path),pagesize=(w,h))
    c.setTitle('Figure 1. Participant selection and analytical framework');c.setAuthor('Hua Yang; Meng Zhou; Feng Xu; Jiajun Zhao')
    def tx(s,x,y,size=8,b=False,align='center'):
        c.setFont('SansBold' if b else 'Sans',size);c.setFillColor(HexColor('#222222'))
        getattr(c,{'center':'drawCentredString','left':'drawString','right':'drawRightString'}[align])(x,h-y,s)
    def box(x,y,bw,bh,lines,tint='#f3f5f6',bold=False):
        c.setFillColor(HexColor(tint));c.setStrokeColor(HexColor('#71808b'));c.setLineWidth(.7);c.roundRect(x,h-y-bh,bw,bh,2,fill=1,stroke=1)
        for i,t in enumerate(lines):tx(t,x+bw/2,y+bh/2-(len(lines)-1)*5+3+i*10,7.5,bold)
    def ar(x,y,x2,y2):
        c.setStrokeColor(HexColor('#71808b'));c.setLineWidth(.8);c.line(x,h-y,x2,h-y2)
        a=math.atan2(y2-y,x2-x)
        for d in [-.5,.5]:c.line(x2,h-y2,x2-3.5*math.cos(a+d),h-(y2-3.5*math.sin(a+d)))
    tx('A  Chinese cohorts',10,14,9,True,'left');tx('B  NHANES 2011–2018',258,14,9,True,'left')
    box(10,26,226,32,['1676 adults screened','January 2022–August 2025'],bold=True)
    ar(123,58,123,75)
    box(10,75,226,38,['475 clinical exclusions','146 with insufficient staging information'])
    ar(123,113,123,130)
    box(10,130,226,32,['1055 eligible adults aged 30–79 years'],tint='#f7efec',bold=True)
    ar(90,162,65,182);ar(156,162,181,182)
    box(10,182,110,48,['Derivation cohort','n = 955','451 in stages 3–4'],'#f7efec',True)
    box(131,182,105,48,['Repeated measures','n = 100','37 progressors + 63'],'#edf3f8',True)
    items=[(26,['39,156 demographic records'],['27,974 outside age range']),
           (75,['11,182 aged 30–59 years'],['6518 without positive fasting weights']),
           (124,['4664 with positive fasting weights'],['1141 with incomplete CKM staging']),
           (173,['3523 with complete CKM staging'],['835 missing ≥1 adiposity input'])]
    for y,ls,ex in items:
        box(258,y,228,29,ls);ar(372,y+29,372,y+49)
        tx(ex[0],262,y+41,6.7,False,'left')
    box(258,222,228,34,['Final analytic sample: n = 2688','179 in stages 3–4'],'#edf3f8',True)
    tx('C  Cross-sectional replication and longitudinal support',10,278,9,True,'left')
    box(10,292,142,43,['Chinese BIA inputs','Within-cohort standardization','Derive PC1 and PC2'],'#f7efec',True)
    box(177,292,142,43,['NHANES DXA inputs','Within-cohort standardization','Apply Chinese loading vectors'],'#edf3f8')
    box(344,292,142,43,['NHANES DXA inputs','Independent PCA','Compare loadings and scores'],'#edf3f8')
    ar(152,313,177,313)
    ar(81,335,81,394)
    box(10,394,142,51,['Longitudinal PC2 changes','Fix Chinese means, SDs','and loading vectors'])
    ar(248,335,248,368);ar(415,335,415,368)
    box(177,368,309,38,['Concurrent CKM stages 3–4 versus stages 0–2','Survey-weighted association models in NHANES'],bold=True)
    tx('Structural PCA is unweighted; association models use the survey design.',177,428,6.8,False,'left')
    c.save()
    shutil.copy2(SRC/'Figure_1.pdf',FIG/'Figure_2.pdf')
    shutil.copy2(SRC/'Figure_3.pdf',FIG/'Figure_3.pdf')
    original=fitz.open(SRC/'IJO_Supplementary_Information.pdf')
    src=fitz.open();src.insert_pdf(original,from_page=20,to_page=20);page=src[0]
    cap_top=min(r.y0 for r in page.search_for('Supplementary Fig. 17.'))
    clip=fitz.Rect(49,78.5,546.5,cap_top-9)
    page.add_redact_annot(fitz.Rect(0,0,page.rect.width,clip.y0),fill=False)
    page.add_redact_annot(fitz.Rect(0,clip.y1,page.rect.width,page.rect.height),fill=False)
    page.apply_redactions(images=0,graphics=0,text=0)
    promoted=fitz.open();dst=promoted.new_page(width=clip.width,height=clip.height)
    dst.show_pdf_page(dst.rect,src,0,clip=clip)
    promoted.set_metadata({'title':'Figure 4. Longitudinal PC2 changes accompanying CKM stage progression','author':'Hua Yang; Meng Zhou; Feng Xu; Jiajun Zhao'})
    promoted.save(FIG/'Figure_4.pdf',garbage=4,deflate=True)
    for n in range(1,5):
        d=pdfium.PdfDocument(str(FIG/f'Figure_{n}.pdf'))
        im=d[0].render(scale=600/72).to_pil().convert('RGB');im.save(FIG/f'Figure_{n}.png',dpi=(600,600))
        im=d[0].render(scale=300/72).to_pil().convert('RGB');im.save(FIG/f'Figure_{n}_300dpi.png',dpi=(300,300))
    print('Main figures: 4 vector PDFs and 4 RGB PNGs at 600 dpi')

generate_flow()
