"""Recompose publication panels from saved estimates and original vector PDFs.
No statistical model is fitted. All forest estimates are read from the source
Word supplementary tables; all other panels retain their original vectors.
"""
from pathlib import Path
import sys, re, json, math, hashlib, shutil, argparse
from zipfile import ZipFile
from lxml import etree as E
ROOT=Path(__file__).resolve().parent.parent
import pymupdf as fitz
NS={'w':'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
BLUE=(.19,.36,.53);RED=(.72,.29,.27);INK=(.13,.17,.20);GREY=(.51,.56,.60)

PANEL_MAP={
 '1':['1A','1B','1C'],'2':['2A','2C','2B'],'3':['3A','3B','3C'],'4':['4A','4B','4C','4D'],
 'S1':['S1A','S1B','S1C'],'S2':['S2A','S2B'],
 'S3':['S3A','S5B'],'S4':['S4A','S5A'],'S5':['S3B','S4B'],
 'S6':['S6A','S6B'],'S7':['S7A','S7B','S7C','S7D'],
 'S8':['S8A','S8B'],'S9':['S9A','S9B','S9C'],
 'S10':['S8C','S9D','S8D'],'S11':['S10A','S10B','S11A','S11B'],
 'S12':['S12A','S12B','S12C']}

def text(el):return ''.join(el.xpath('.//w:t/text()',namespaces=NS))
def tables_from_docx(path):
    with ZipFile(path) as z:tree=E.fromstring(z.read('word/document.xml'))
    result={};current=None
    for el in tree.find('w:body',NS):
        if (m:=re.match(r'^Table (S\d+)\.',text(el))):current=m[1]
        if el.tag.endswith('}tbl') and current:
            result.setdefault(current,[]).append([[text(c) for c in r.findall('w:tc',NS)] for r in el.findall('w:tr',NS)])
    return result
def estimate(s):
    vals=[float(x) for x in re.findall(r'-?\d+(?:\.\d+)?',s.replace('−','-'))]
    assert len(vals)==3,(s,vals)
    assert 0<vals[1]<vals[0]<vals[2],(s,vals)
    return vals
def put(page,x,y,s,size=7,font='helv',color=INK):
    page.insert_text((x,y),s.replace('–','-').replace('−','-'),fontname=font,fontsize=size,color=color)
def rule(page,p1,p2,color=INK,width=.6,dashes=None):
    page.draw_line(p1,p2,color=color,width=width,dashes=dashes)
def heading(page,letter,title,y=14):
    put(page,8,y,letter,8,'hebo');put(page,27,y,title,8,'hebo')
def forest(page,y,letter,title,labels,values,pvalues=None,colors=None,xrange=(.3,1.08),ticks=(.3,.5,.7,1),xlabel='Odds ratio (95% CI)',step=22):
    heading(page,letter,title,y+11)
    x0,x1=191,366
    tx=lambda v:x0+(math.log(v)-math.log(xrange[0]))/(math.log(xrange[1])-math.log(xrange[0]))*(x1-x0)
    first=y+37;last=first+(len(labels)-1)*step;axis=last+19
    rule(page,(tx(1),y+23),(tx(1),axis),GREY,.7,'[3 3]')
    put(page,386,y+25,'OR (95% CI)',7,'hebo')
    if pvalues is not None:put(page,466,y+25,'p',7,'hebo')
    for i,(label,vs) in enumerate(zip(labels,values)):
        yy=first+i*step;col=(colors or [BLUE]*len(labels))[i]
        put(page,16,yy+2.5,label,7)
        rule(page,(tx(vs[1]),yy),(tx(vs[2]),yy),col,1.05)
        for v in vs[1:]:rule(page,(tx(v),yy-3),(tx(v),yy+3),col,.9)
        page.draw_circle((tx(vs[0]),yy),2.3,color=col,fill=col)
        put(page,386,yy+2.5,f'{vs[0]:.2f} ({vs[1]:.2f}–{vs[2]:.2f})',6.7)
        if pvalues is not None:put(page,460,yy+2.5,pvalues[i],6.5)
    rule(page,(x0,axis),(x1,axis))
    for v in ticks:
        x=tx(v);rule(page,(x,axis),(x,axis+3));s=f'{v:g}';put(page,x-fitz.get_text_length(s,fontsize=7)/2,axis+12,s,7)
    put(page,205,axis+27,xlabel,7)
    return axis+36

def compose(src,out,qa):
    src, out, qa = (path.resolve() for path in (src, out, qa))
    if out == qa or src in (out, qa) or out in src.parents or qa in src.parents:
        raise ValueError("Choose distinct source and output directories.")
    for path in (out, qa):
        if path.exists() and any(path.iterdir()):
            raise ValueError(f"Use a new or empty output directory: {path}")
        path.mkdir(parents=True, exist_ok=True)
    (out/'Supplementary').mkdir(exist_ok=True)
    word_source=src/'Supplementary_Materials_S1.docx'
    tables=tables_from_docx(word_source) if word_source.exists() else json.loads((src/'saved_table_inputs.json').read_text(encoding='utf8'))
    pdfs={}
    def source(no):
        if no not in pdfs:
            p=src/'Figures'/('Supplementary' if no.startswith('S') else '')/f'Figure_{no}.pdf'
            pdfs[no]=fitz.open(p)
        return pdfs[no]
    def tile(dest,no,clip,x,y,w=None,h=None,newletter=None):
        clip=fitz.Rect(clip);w=w or clip.width;h=h or clip.height
        tmp=fitz.open();tp=tmp.new_page(width=clip.width,height=clip.height)
        tp.show_pdf_page(tp.rect,source(no),0,clip=clip)
        if newletter:
            # The top-left panel letter is the only vector content removed.
            spans=[s for b in tp.get_text('dict')['blocks'] if b['type']==0 for l in b['lines'] for s in l['spans'] if s['text'] in ['A','B','C','D'] and s['bbox'][1]<20]
            assert len(spans)==1,(no,clip,spans)
            b=fitz.Rect(spans[0]['bbox']);tp.add_redact_annot(b+(-.5,-.5,.5,.5),fill=(1,1,1))
            tp.apply_redactions(images=0,graphics=0)
            put(tp,b.x0,max(b.y1-1,7.2),newletter,7,'hebo')
        dest.show_pdf_page(fitz.Rect(x,y,x+w,y+h),tmp,0)
        tmp.close()
    def save(no,doc):
        rel=('Supplementary/' if no.startswith('S') else '')+f'Figure_{no}'
        doc.save(out/(rel+'.pdf'),garbage=4,deflate=True)
        pix=doc[0].get_pixmap(dpi=600,alpha=False);pix.save(out/(rel+'.png'))
        doc[0].get_pixmap(dpi=144,alpha=False).save(qa/f'Figure_{no}.png')
        doc.close()
    # Unchanged figures retain their original bytes.
    changed={'2','S3','S4','S5','S8','S9','S10','S11'}
    for no in PANEL_MAP:
        if no in changed:continue
        rel=('Supplementary/' if no.startswith('S') else '')+f'Figure_{no}'
        shutil.copy2(src/'Figures'/(rel+'.pdf'),out/(rel+'.pdf'))
        png_source=src/'Figures'/(rel+'.png')
        if png_source.exists():shutil.copy2(png_source,out/(rel+'.png'))
        else:
            with fitz.open(src/'Figures'/(rel+'.pdf')) as original:
                original[0].get_pixmap(dpi=600,alpha=False).save(out/(rel+'.png'))
    # Main Figure 2 puts PC2 before PC1, matching the narrative.
    d=fitz.open();p=d.new_page(width=496.06299,height=373)
    tile(p,'2',(0,0,496.06299,138),0,0)
    tile(p,'2',(0,256,496.06299,373),0,138,newletter='B')
    tile(p,'2',(0,138,496.06299,256),0,255,newletter='C')
    save('2',d)
    # S3: local association estimates and the original stage-specific means.
    vals=[estimate(r[2]) for r in tables['S9'][0][1:3]]
    ps=[r[3] for r in tables['S9'][0][1:3]]
    d=fitz.open();p=d.new_page(width=497.5,height=244)
    heading(p,'A','NHANES-derived associations',16)
    x0,x1=60,277;xx=lambda v:x0+(math.log(v)-math.log(.35))/(math.log(2.5)-math.log(.35))*(x1-x0)
    for i,vs in enumerate(vals):
        yy=76+i*68;put(p,17,yy+2.5,f'PC{i+1}',7)
        rule(p,(xx(vs[1]),yy),(xx(vs[2]),yy),BLUE,1.2)
        for v in vs[1:]:rule(p,(xx(v),yy-3),(xx(v),yy+3),BLUE,1)
        p.draw_circle((xx(vs[0]),yy),2.5,color=BLUE,fill=BLUE)
        put(p,60,yy-17,f'OR {vs[0]:.2f} ({vs[1]:.2f}–{vs[2]:.2f}); p {"=" if not ps[i].startswith("<") else ""} {ps[i]}',7)
    rule(p,(xx(1),43),(xx(1),185),GREY,.7,'[3 3]');rule(p,(x0,185),(x1,185))
    for v in [.4,.6,1,2]:rule(p,(xx(v),185),(xx(v),188));put(p,xx(v)-4,200,f'{v:g}',7)
    put(p,107,218,'Odds ratio per weighted SD',7)
    tile(p,'S5',(310,0,497.5,226.00024),310,10)
    save('S3',d)
    # S4 and S5 are drawn from the saved table values, never from refitted models.
    rows=tables['S10'][0]
    age=[rows[i] for i in [2,4,6]]
    labels=[];vs=[];cols=[]
    for label,row in zip(['China','NHANES-derived','Fixed Chinese loadings'],age):
        for spec,col_idx,col in [('primary',3,BLUE),('age-adjusted',4,RED)]:
            labels.append(f'{label}: {spec}');vs.append(estimate(row[col_idx]));cols.append(col)
    alcohol=tables['S11'][0][1:]
    alc_labels=['Fixed score: no alcohol adjustment','Fixed score: alcohol-adjusted','NHANES-derived: no alcohol adjustment','NHANES-derived: alcohol-adjusted','Fixed score: alcohol-adjusted, stages 2–4']
    d=fitz.open();p=d.new_page(width=497.5,height=444)
    forest(p,4,'A','Age adjustment of PC2 associations',labels,vs,colors=cols,step=21)
    forest(p,233,'B','Alcohol sensitivity of PC2 associations',alc_labels,[estimate(r[1]) for r in alcohol],pvalues=[r[2] for r in alcohol],step=22,xlabel='Odds ratio per weighted SD')
    save('S4',d)
    loco=tables['S13'][0][1:];interaction=tables['S14'][0][1:]
    d=fitz.open();p=d.new_page(width=497.5,height=368)
    forest(p,4,'A','Leave-one-cycle-out estimates: fixed-loading PC2',[r[0].replace('2011-2012','2011–2012').replace('2013-2014','2013–2014').replace('2015-2016','2015–2016').replace('2017-2018','2017–2018') for r in loco],[estimate(r[3]) for r in loco],pvalues=[r[4] for r in loco],xlabel='Odds ratio per weighted SD')
    forest(p,205,'B','Exploratory PC2-by-age interactions',['China','NHANES-derived','Fixed Chinese loadings'],[estimate(r[2]) for r in interaction],pvalues=[r[3] for r in interaction],xrange=(.7,1.65),ticks=(.8,1,1.2,1.5),xlabel='Interaction OR per 10-year age increment',step=22)
    save('S5',d)
    # Remaining rearrangements retain the original vector curves/points exactly.
    d=fitz.open();p=d.new_page(width=497.5,height=191)
    tile(p,'S8',(0,0,497.5,181),0,5);save('S8',d)
    d=fitz.open();p=d.new_page(width=497.5,height=373)
    tile(p,'S9',(0,0,497.5,172),0,5)
    tile(p,'S9',(0,180,248,363.30023),124.75,184)
    save('S9',d)
    d=fitz.open();p=d.new_page(width=497.5,height=510)
    tile(p,'S8',(0,181,263,443.70026),0,5,newletter='A')
    tile(p,'S9',(263,180,497.5,363.30023),263,5,newletter='B')
    tile(p,'S8',(263,181,497.5,443.70026),131.5,275,newletter='C',h=230,w=205.3)
    save('S10',d)
    d=fitz.open();p=d.new_page(width=497.5,height=485)
    tile(p,'S10',(0,0,497.5,239.95026),0,7)
    tile(p,'S11',(0,0,253,229.35016),0,253,newletter='C')
    tile(p,'S11',(253,0,497.5,229.35016),253,253,newletter='D')
    save('S11',d)
    provenance={'panel_map':PANEL_MAP,'saved_table_inputs':{k:tables[k] for k in ['S9','S10','S11','S13','S14']},'models_refitted':False,'redrawn_forests':['S3A','S4A','S4B','S5A','S5B'],'other_panels':'Original vector geometry retained; panel letters and placement updated.'}
    (qa/'panel_provenance.json').write_text(json.dumps(provenance,ensure_ascii=False,indent=2),encoding='utf8')
    for doc in pdfs.values():doc.close()
    return provenance

if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('--source-dir',type=Path,required=True);parser.add_argument('--output-figures',type=Path,required=True);parser.add_argument('--qa-dir',type=Path,required=True)
    a=parser.parse_args();compose(a.source_dir,a.output_figures,a.qa_dir)
    print('Recomposed eight figures; original statistical estimates retained.')
