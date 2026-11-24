"""Publication-only layout of locked CKM results. No model fitting.

All effects/loadings are read from the audited R3 workbooks. The 2,688 scatter
glyphs are copied by an affine coordinate transform from the audited R3 vector
PDF. Their original control points and transformed centres are audited to JSON.

Required inputs are the Stage 02 workbooks and analytical PDFs under
CKM_OUTPUT_ROOT (default: <code package>/results). The original submission used
audited R3 workbooks with repaired dimension metadata but unchanged cell values.
The portable copy resets read-only worksheet dimensions before reading records.
The n=2688 scatter panel is read from Figure_4_Cross_platform_replication.pdf.

Output mapping (three panels per main figure):
  Figure 1: China loading heatmap; crude/adjusted PC1 ORs; PC2 ORs.
  Figure 2: China/NHANES PC1 loadings; PC2 loadings; Tucker congruence CIs.
  Figure 3: PC2 score concordance; fixed-loading ORs; fixed-PC2 sensitivities.
Formal figures are vector PDFs; RGB PNG/TIFF are author backup exports.
No PCA, score estimation, regression, bootstrap or inferential test is run here.
Environment-specific paths/font discovery may be adapted in a distributed copy.
"""
from pathlib import Path
import math, json, hashlib, csv, re, os
import numpy as np
import openpyxl, fitz
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.colors import Color, HexColor
from PIL import Image

ROOT=Path(os.environ.get('CKM_CODE_ROOT', Path(__file__).resolve().parents[1])).resolve()
RESULTS=Path(os.environ.get('CKM_OUTPUT_ROOT', ROOT/'results')).resolve()
PUBLICATION=Path(os.environ.get('CKM_PUBLICATION_ROOT', RESULTS/'IJO_Publication')).resolve()
WORK=PUBLICATION/'work'
MAIN=PUBLICATION/'Main_Figures'
BACKUP=PUBLICATION/'Author_Backups'/'Main_Figures'
SUPP=PUBLICATION/'Supplementary_Figures'
for p in (MAIN,BACKUP,SUPP,WORK/'qa'): p.mkdir(parents=True,exist_ok=True)
FONTROOT=Path(os.environ.get('CKM_FONT_ROOT', ROOT/'publication'/'fonts')).resolve()
for n,f in [('ArialCompatible','LiberationSans-Regular.ttf'),('ArialCompatible-Bold','LiberationSans-Bold.ttf'),('ArialCompatible-Italic','LiberationSans-Italic.ttf')]:
    pdfmetrics.registerFont(TTFont(n,str(FONTROOT/f)))
F='ArialCompatible'; FB=F+'-Bold'; FI=F+'-Italic'
W=175/25.4*72
BLUE=HexColor('#315f89'); RED=HexColor('#b75049'); INK=HexColor('#202a32')
GREY=HexColor('#89939c'); LIGHT=HexColor('#e5e8eb'); WHITE=Color(1,1,1)
WB=openpyxl.load_workbook(RESULTS/'04_publication_tables/CKM_publication_tables.xlsx',read_only=True,data_only=True)
SB=openpyxl.load_workbook(RESULTS/'04_publication_tables/Supplementary_figure_source_data.xlsx',read_only=True,data_only=True)
def records(wb,name):
    ws=wb[name];ws.reset_dimensions();a=list(ws.values);return [dict(zip(a[0],r)) for r in a[1:] if any(v is not None for v in r)]
def tab(name): return records(WB,name)
def stab(name):return records(SB,name)
def pval(v):return '<0.001' if v<.001 else f'{v:.3f}'
def ci(r,d=2):return f"{r['OR']:.{d}f} ({r['Lower']:.{d}f}–{r['Upper']:.{d}f})"
def txt(c,x,y,s,bold=False,color=INK,align='left',size=7):
    c.setFont(FB if bold else F,size);c.setFillColor(color)
    f={'left':c.drawString,'center':c.drawCentredString,'right':c.drawRightString}[align]
    s=re.sub(r'(?<=\d),(?=\d{3}(?:\D|$))',' ',str(s))
    f(x,c._pagesize[1]-y,s)
def line(c,x1,y1,x2,y2,color=INK,width=1,dash=None):
    c.setStrokeColor(color);c.setLineWidth(width);c.setDash(dash or []);H=c._pagesize[1]
    c.line(x1,H-y1,x2,H-y2);c.setDash([])
def rect(c,x,y,w,h,fill,stroke=None):
    c.setFillColor(fill);c.setStrokeColor(stroke or fill);c.setLineWidth(1)
    c.rect(x,c._pagesize[1]-y-h,w,h,fill=1,stroke=int(stroke is not None))
def dot(c,x,y,color=BLUE,r=2.6):
    c.setFillColor(color);c.circle(x,c._pagesize[1]-y,r,fill=1,stroke=0)
def title(c,letter,label,x,y):
    txt(c,x,y,letter,True);txt(c,x+13,y,label,True)
def new(path,H):
    c=canvas.Canvas(str(path),pagesize=(W,H),pageCompression=1)
    c.setTitle(path.stem);c.setAuthor('CKM study authors');c.setSubject('Locked audited results; publication layout only')
    return c
def maplog(v,x0,x1,lo,hi):return x0+(math.log(v)-math.log(lo))/(math.log(hi)-math.log(lo))*(x1-x0)
def forest(c,rows,ys,x0,x1,lo,hi,ticks,labelx=8,cix=375,px=W-10,axis_y=None,color=BLUE,labels=None,head_y=None,xlabel='Odds ratio (95% CI)'):
    if head_y:
        txt(c,cix,head_y,'OR (95% CI)',bold=True);txt(c,px,head_y,'P',bold=True,align='right')
    y0=min(ys)-10;y1=(axis_y or max(ys)+14)
    if lo<=1<=hi:line(c,maplog(1,x0,x1,lo,hi),y0,maplog(1,x0,x1,lo,hi),y1,GREY,1,[3,3])
    for i,(r,y) in enumerate(zip(rows,ys)):
        col=color[i] if isinstance(color,list) else color
        lab=labels[i] if labels else r.get('label',r.get('Analysis',r.get('Model','')))
        if lab: txt(c,labelx,y+2.5,lab)
        l=maplog(r['Lower'],x0,x1,lo,hi);u=maplog(r['Upper'],x0,x1,lo,hi);x=maplog(r['OR'],x0,x1,lo,hi)
        assert x0-.01<=l<=u<=x1+.01,(lab,l,u,x0,x1)
        line(c,l,y,u,y,col);line(c,l,y-3,l,y+3,col);line(c,u,y-3,u,y+3,col);dot(c,x,y,col)
        txt(c,cix,y+2.5,ci(r));txt(c,px,y+2.5,pval(r['P']),align='right')
    if axis_y:
        line(c,x0,axis_y,x1,axis_y)
        for v in ticks:
            x=maplog(v,x0,x1,lo,hi);line(c,x,axis_y,x,axis_y+3);txt(c,x,axis_y+13,f'{v:g}',align='center')
        txt(c,(x0+x1)/2,axis_y+26,xlabel,align='center')
def save(c):c.showPage();c.save()
def export(pdf,backup=True):
    d=fitz.open(pdf);p=d[0]
    out=BACKUP if backup else SUPP
    for dpi,ext in [(300,'png'),(600,'tif')]:
        pix=p.get_pixmap(matrix=fitz.Matrix(dpi/72,dpi/72),alpha=False)
        im=Image.frombytes('RGB',[pix.width,pix.height],pix.samples)
        target=out/(pdf.stem+('.png' if ext=='png' else '.tif'))
        im.save(target,dpi=(dpi,dpi),**({'compression':'tiff_lzw'} if ext=='tif' else {}))
    pix=p.get_pixmap(matrix=fitz.Matrix(2,2),alpha=False);pix.save(WORK/'qa'/(pdf.stem+'.png'))

def fig1():
    path=MAIN/'Figure_1.pdf';c=new(path,373)
    title(c,'A','Chinese derivation loadings (n = 955)',8,16)
    load={r['Variable']:r for r in tab('China_loadings')}
    cols=[('BMI','BMI'),('VFA','VFA'),('Trunk','Trunk%'),('Left arm','LA%'),('Right arm','RA%'),('Left leg','LL%'),('Right leg','RL%')]
    x0=48;cw=53
    for j,(lab,key) in enumerate(cols):
        txt(c,x0+(j+.5)*cw,37,lab,align='center')
        for i,pc in enumerate(['PC1','PC2']):
            v=load[key][pc];scale=min(abs(v)/.8,1)
            base=RED if v>0 else BLUE
            fill=Color(1-(1-base.red)*scale,1-(1-base.green)*scale,1-(1-base.blue)*scale)
            rect(c,x0+j*cw,44+i*25,cw-2,23,fill)
            txt(c,x0+(j+.5)*cw-1,59+i*25,f'{v:.3f}',align='center',color=WHITE if scale>.65 else INK)
    for i,pc in enumerate(['PC1','PC2']):txt(c,8,59+i*25,pc,True)
    txt(c,456,29,'Variance',align='center');txt(c,456,38,'explained',align='center')
    for y,s in [(59,'90.6%'),(84,'5.3%')]:txt(c,456,y,s,align='center',bold=True)
    cbx=168;cbw=178
    for j in range(200):
        v=-.8+1.2*j/199;sc=abs(v)/.8;b=RED if v>0 else BLUE
        rect(c,cbx+j*cbw/200,105,cbw/200+.1,6,Color(1-(1-b.red)*sc,1-(1-b.green)*sc,1-(1-b.blue)*sc))
    for v in [-.8,-.4,0,.4]:txt(c,cbx+(v+.8)/1.2*cbw,123,f'{v:g}',align='center')
    txt(c,125,111,'Loading',align='right')
    data=tab('China_primary_OR')
    for pc,letter,topy,col,lo,hi,ticks in [('PC1','B',150,RED,.95,1.25,[1,1.1,1.2]),('PC2','C',264,BLUE,.35,1.08,[.4,.6,.8,1])]:
        title(c,letter,f'{pc} and concurrent CKM Stages 3–4',8,topy)
        rows=[r for r in data if r['Exposure']==pc]
        forest(c,rows,[topy+37,topy+61],139,326,lo,hi,ticks,labelx=21,cix=355,px=482,axis_y=topy+77,color=col,labels=['Crude','Mutually adjusted'],head_y=topy+21,xlabel='Odds ratio per 1 PC-score unit')
    save(c);export(path)

def fig2():
    path=MAIN/'Figure_2.pdf';c=new(path,373)
    rows=tab('Loading_comparison')
    for pc,letter,left,lo,hi,ticks in [('PC1','A',8,0,.48,[0,.1,.2,.3,.4]),('PC2','B',257,-.82,.48,[-.8,-.4,0,.4])]:
        title(c,letter,f'{pc} loading comparison',left,16)
        dot(c,left+17,33,RED);txt(c,left+24,36,'China')
        dot(c,left+73,33,BLUE);txt(c,left+80,36,'NHANES')
        x0=left+64;x1=left+223;ys=[59+23*i for i in range(7)]
        xm=lambda v:x0+(v-lo)/(hi-lo)*(x1-x0)
        if lo<0:line(c,xm(0),48,xm(0),209,LIGHT)
        for r,y in zip(rows,ys):
            lab='VFA / VAT' if r['Variable']=='VAT' else r['Variable']
            txt(c,left,y+2.5,lab)
            a=xm(r['China_'+pc]);b=xm(r['NHANES_'+pc])
            line(c,min(a,b),y,max(a,b),y,GREY);dot(c,a,y,RED);dot(c,b,y,BLUE)
        ay=212;line(c,x0,ay,x1,ay)
        for v in ticks:line(c,xm(v),ay,xm(v),ay+3);txt(c,xm(v),ay+14,f'{v:g}',align='center')
        txt(c,(x0+x1)/2,ay+28,'Standardized loading',align='center')
    title(c,'C','Loading-vector congruence',8,271)
    txt(c,348,286,'Tucker φ (95% CI)',bold=True)
    x0=118;x1=321;lo=0;hi=1.0;xm=lambda v:x0+(v-lo)/(hi-lo)*(x1-x0)
    for r,y,col in zip(tab('Loading_bootstrap'),[302,325],[RED,BLUE]):
        txt(c,21,y+2.5,r['Component'],True)
        l=xm(r['Lower']);u=xm(r['Upper']);line(c,l,y,u,y,col);line(c,l,y-3,l,y+3,col);line(c,u,y-3,u,y+3,col);dot(c,xm(r['Point']),y,col,1.7)
        txt(c,348,y+2.5,f"{r['Point']:.3f} ({r['Lower']:.3f}–{r['Upper']:.3f})")
    ay=339;line(c,x0,ay,x1,ay)
    for v in [0,.25,.5,.75,1.0]:line(c,xm(v),ay,xm(v),ay+3);txt(c,xm(v),ay+14,f'{v:g}',align='center')
    txt(c,(x0+x1)/2,366,'Tucker congruence coefficient',align='center')
    save(c);export(path)

def scatter_original(c,x,y,w):
    source=RESULTS/'03_publication_figures/Main_Figures_v19_Reconciled/Figure_4_Cross_platform_replication.pdf'
    page=fitz.open(source)[0]
    points=[v for v in page.get_drawings() if v['rect'].width<5 and v['rect'].height<5 and v['rect'].x0>560 and v['rect'].y0>310 and len(v['items'])==2]
    assert len(points)==2688
    # Exact original visible panel geometry; scale is isotropic to preserve shape.
    sx0=630.6480102539062;sy0=353.56201171875;sx1=850.551025390625;sy1=560.7030029296875
    scale=w/(sx1-sx0);h=(sy1-sy0)*scale;H=c._pagesize[1]
    trans=lambda q:(x+(q.x-sx0)*scale,y+(q.y-sy0)*scale)
    xx0=766.0159912109375;yy0=435.13299560546875
    unitx=(839.1290283203125-692.9019775390625)/5
    unity=(552.1129760742188-376.6409912109375)/6
    px=lambda v:x+(xx0+v*unitx-sx0)*scale
    py=lambda v:y+(yy0-v*unity-sy0)*scale
    # Identity line, clipped to the same data window; no regression or ellipse.
    lo=max((sx0-xx0)/unitx,(yy0-sy1)/unity)
    hi=min((sx1-xx0)/unitx,(yy0-sy0)/unity)
    line(c,px(lo),py(lo),px(hi),py(hi),GREY,1,[3,3])
    original=[];newpoints=[];controls=[]
    c.saveState();c.setFillColor(BLUE);c.setFillAlpha(.23)
    for d in points:
        p=c.beginPath();start=d['items'][0][1];a,b=trans(start);p.moveTo(a,H-b)
        for item in d['items']:
            assert item[0]=='c'
            vals=[]
            for q in item[2:]:
                a,b=trans(q);vals.extend([a,H-b]);controls.append([q.x,q.y,a,b])
            p.curveTo(*vals)
        p.close();c.drawPath(p,stroke=0,fill=1)
        centre=fitz.Point((d['items'][0][1].x+d['items'][0][-1].x)/2,d['items'][0][1].y)
        original.append([centre.x,centre.y]);newpoints.append(list(trans(centre)))
    c.restoreState()
    line(c,x,y,x,y+h);line(c,x,y+h,x+w,y+h)
    for v in [-4,-2,0,2]:line(c,x-3,py(v),x,py(v));txt(c,x-6,py(v)+2.5,str(v),align='right')
    for v in [-2.5,0,2.5]:line(c,px(v),y+h,px(v),y+h+3);txt(c,px(v),y+h+13,f'{v:g}',align='center')
    txt(c,x+4,y+11,'n = 2,688; Pearson r = 0.927')
    txt(c,x+w/2,y+h+27,'NHANES-derived PC2 (weighted SD)',align='center')
    c.saveState();c.translate(x-24,H-(y+h/2));c.rotate(90);c.setFont(F,7);c.setFillColor(INK);c.drawCentredString(0,0,'Fixed China-loading PC2 (weighted SD)');c.restoreState()
    inverse=np.array(newpoints);inverse[:,0]=(inverse[:,0]-x)/scale+sx0;inverse[:,1]=(inverse[:,1]-y)/scale+sy0
    error=float(np.max(np.abs(inverse-np.array(original))))
    audit={'source_pdf':str(source),'source_sha256':hashlib.sha256(source.read_bytes()).hexdigest(),'source_panel':'D','source_glyph_count':len(points),'output_glyph_count':len(newpoints),'transform':{'type':'isotropic affine','scale':scale,'source_origin':[sx0,sy0],'target_origin':[x,y]},'max_inverse_transform_error_pt':error,'all_glyph_control_points_preserved_under_affine_transform':True,'data_coordinates_or_statistical_scores_reestimated':False,'single_uniform_colour':True,'removed_non_point_layers':['ordinary least squares fitted line','ellipse'],'reference_line':'identity y=x','source_centres_pdf_pt':original,'output_centres_pdf_pt_top_down':newpoints}
    (WORK/'scatter_2688_vector_preservation.json').write_text(json.dumps(audit,indent=2))
    assert error<1e-10

def fig3():
    path=MAIN/'Figure_3.pdf';c=new(path,421)
    title(c,'A','PC2 score concordance in NHANES',8,16)
    scatter_original(c,38,44,194)
    title(c,'B','Fixed China-loading associations',253,16)
    rows=[r for r in tab('External_OR') if r['Analysis'] in ['Fixed China-loading PC1','Fixed China-loading PC2']]
    forest(c,rows,[75,111],292,375,.35,2.6,[.5,1,2],labelx=264,cix=386,px=487,axis_y=140,color=[RED,BLUE],labels=['PC1','PC2'],head_y=44,xlabel='Odds ratio per weighted SD')
    txt(c,266,185,'n = 2,688; 179 in CKM Stages 3–4')
    txt(c,266,199,'Adjusted for both scores, sex and smoking')
    title(c,'C','Fixed China-loading PC2: sensitivity analyses',8,279)
    primary=rows[1]
    age=next(r for r in tab('External_age') if r['Analysis']=='Fixed China-loading PC2 + age')
    alc=next(r for r in stab('S11_alcohol_sensitivity') if r['Analysis']=='Fixed China loading: alcohol-adjusted')
    stage=next(r for r in tab('External_OR') if r['Analysis']=='Fixed China-loading PC2, Stage 2 vs 3-4')
    both=next(r for r in stab('S11_alcohol_sensitivity') if r['Analysis']=='Fixed China loading: alcohol-adjusted Stage 2 vs Stage 3-4')
    labs=['Primary (n = 2,688)','+ Age (n = 2,688)','+ Alcohol (n = 2,262)','Stage 2 vs 3–4 (n = 1,712)','+ Alcohol, Stage 2 vs 3–4 (n = 1,445)']
    forest(c,[primary,age,alc,stage,both],[306,323,340,357,374],181,338,.35,1.06,[.4,.6,.8,1],labelx=21,cix=356,px=482,axis_y=385,labels=labs,head_y=294,xlabel='Odds ratio per weighted SD')
    save(c);export(path)

def qa_main():
    result=[]
    for p in sorted(MAIN.glob('Figure_*.pdf')):
        d=fitz.open(p);pg=d[0];spans=[s for b in pg.get_text('dict')['blocks'] for l in b.get('lines',[]) for s in l['spans']]
        bad=[s['text'] for s in spans if not(pg.rect.x0-.05<=s['bbox'][0] and s['bbox'][2]<=pg.rect.x1+.05 and pg.rect.y0-.05<=s['bbox'][1] and s['bbox'][3]<=pg.rect.y1+.05)]
        a={'file':p.name,'width_mm':pg.rect.width/72*25.4,'height_mm':pg.rect.height/72*25.4,'min_font_pt':min(s['size'] for s in spans),'max_font_pt':max(s['size'] for s in spans),'outside_page_text':bad,'embedded_fonts':pg.get_fonts(),'raster_image_count':len(pg.get_images()),'sha256':hashlib.sha256(p.read_bytes()).hexdigest()}
        assert abs(a['width_mm']-175)<.001 and not bad,a
        result.append(a)
    (WORK/'main_figures_technical_qa.json').write_text(json.dumps(result,indent=2))

if __name__=='__main__':
    if any(MAIN.iterdir()) or any(BACKUP.iterdir()): raise ValueError('Use a fresh CKM_PUBLICATION_ROOT.')
    fig1();fig2();fig3();qa_main()
    print('Created three 175-mm vector PDFs, 300-dpi PNG and 600-dpi TIFF author backups; no models fitted.')
