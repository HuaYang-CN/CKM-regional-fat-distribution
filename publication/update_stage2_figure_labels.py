"""Update publication figure labels only; no statistical calculations."""
from pathlib import Path
from collections import Counter
from PIL import Image
import argparse, shutil, os
import pymupdf as fitz
p=argparse.ArgumentParser();p.add_argument("--source-dir",required=True);p.add_argument("--output-dir",required=True);a=p.parse_args()
SRC=Path(a.source_dir).resolve();OUT=Path(a.output_dir).resolve();assert SRC!=OUT
if OUT.exists() and any(OUT.iterdir()): raise ValueError('Use a new or empty output directory.')
OUT.mkdir(parents=True,exist_ok=True);(OUT/"Figures").mkdir(exist_ok=True)
FONT_DIR=Path(os.environ.get('CKM_FONT_ROOT', Path(__file__).resolve().parent/'fonts'))
regular=FONT_DIR/'LiberationSans-Regular.ttf';bold=FONT_DIR/'LiberationSans-Bold.ttf'
assert regular.is_file() and bold.is_file()
def spans(page):
    return [s for b in page.get_text('dict')['blocks'] for line in b.get('lines',[]) for s in line['spans']]
def background(page,rect):
    pix=page.get_pixmap(matrix=fitz.Matrix(2,2),clip=rect,alpha=False)
    im=Image.frombytes('RGB',(pix.width,pix.height),pix.samples)
    rgb=Counter(im.getdata()).most_common(1)[0][0]
    return tuple(c/255 for c in rgb)
def edit_label(page,old,new,center=False):
    ss=[s for s in spans(page) if s['text']==old]
    assert len(ss)==1,(old,ss)
    s=ss[0];r=fitz.Rect(s['bbox']);r.x0-=.3;r.x1+=.3;r.y0-=.3;r.y1+=.3
    bg=background(page,r)
    page.add_redact_annot(r,fill=bg);page.apply_redactions(images=0,graphics=0,text=0)
    fn='Stage2Regular';page.insert_font(fontname=fn,fontfile=str(regular))
    fs=s['size'];font=fitz.Font(fontfile=str(regular));x=s['origin'][0]
    if center:x=(s['bbox'][0]+s['bbox'][2]-font.text_length(new,fontsize=fs))/2
    rgb=tuple(((s['color']>>sh)&255)/255 for sh in [16,8,0])
    page.insert_text((x,s['origin'][1]),new,fontname=fn,fontsize=fs,color=rgb)
def replace_box_text(page,rect,lines):
    ss=[s for s in spans(page) if rect.contains(fitz.Rect(s['bbox']))]
    assert len(ss)==3,len(ss)
    bg=background(page,fitz.Rect(ss[0]['bbox']))
    for s in ss:
        r=fitz.Rect(s['bbox']);r.x0-=.3;r.x1+=.3;r.y0-=.3;r.y1+=.3
        page.add_redact_annot(r,fill=bg)
    page.apply_redactions(images=0,graphics=0,text=0)
    fn='Stage2Bold';page.insert_font(fontname=fn,fontfile=str(bold));font=fitz.Font(fontfile=str(bold))
    for i,line in enumerate(lines):
        fs=min(7.5,(rect.width-6)/font.text_length(line,fontsize=1))
        x=rect.x0+(rect.width-font.text_length(line,fontsize=fs))/2
        page.insert_text((x,rect.y0+11+i*10),line,fontname=fn,fontsize=fs,color=(.133,.133,.133))
embedded={}
for n in [1,3,4]:
    pdf=fitz.open(SRC/f'Figure_{n}.pdf');pg=pdf[0]
    if n==1:
        replace_box_text(pg,fitz.Rect(10,182,120,230),['Derivation cohort','n = 955','Stage 2: n = 504','Stages 3–4: n = 451'])
        replace_box_text(pg,fitz.Rect(131,182,236,230),['Repeated measures','n = 100','37: stage 2 to stages 3–4','63: stages 3–4 at both visits'])
    elif n==3:
        edit_label(pg,'Stage 2 vs 3–4 (n = 1 712)','Stages 3–4 vs stage 2 (n = 1712)')
        edit_label(pg,'+ Alcohol, Stage 2 vs 3–4 (n = 1 445)','+ Alcohol, stages 3–4 vs stage 2 (n = 1445)')
    elif n==4:edit_label(pg,'Stages 0–2 to Stages 3–4','Stage 2 to Stages 3–4',center=True)
    pdf.save(OUT/f'Figures/Figure_{n}.pdf',garbage=4,deflate=True)
    pix=pg.get_pixmap(dpi=600,alpha=False);pix.save(OUT/f'Figures/Figure_{n}.png')
    pix=pg.get_pixmap(dpi=300,alpha=False);embedded[f'word/media/image{n}.png']=pix.tobytes('png')
    pdf.close()
