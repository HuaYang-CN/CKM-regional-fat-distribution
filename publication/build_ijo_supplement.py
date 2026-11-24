"""Presentation-only supplementary figures; all scientific arrays are locked.

S2,3,5-12 retain audited R3 vector paths, with text re-laid at 5-7 pt and
stroke-only graph rules at least 1 pt at a 175 mm output width. S4 uses the
complete reporting table's locked bootstrap intervals. S15 and S17 use saved
Chinese objects; none of the figures fits a statistical model.

Inputs beyond build_ijo_figures.py:
  Audited R3 original supplementary PDFs S2, S3 and S5-S12.
  Supplementary_Table_S8.csv from the complete reporting table pipeline: its
    Estimate/CI_Lower/CI_Upper/Bootstrap_P form one locked bootstrap result.
  Four China-only plotting CSVs exported by export_locked_china_plot_objects.R
    from the existing master (saved curves and paired scores; no model fitting).
The paired Chinese scores are restricted participant-level input and must not
be included in a public supplementary-code archive without authorization.

Output mapping:
  S1 China flow; S2 algorithms; S3 comparator performance; S4 NRI/IDI/calibration;
  S5 DCA differences; S6 SHAP summary; S7 SHAP dependencies; S8 longitudinal
  deltas; S9 NHANES loading stability; S10 NHANES same-sample apparent
  performance; S11 alcohol/stage sensitivities; S12 age sensitivities;
  S13 corrected sequential NHANES flow; S14 cohorts and projection framework;
  S15 locked China RCS; S16 NHANES derived/LOCO ORs; S17 paired China evidence.
S13/S14 counts are sequential (39156,11182,4664,3523,2688); the age-domain
marginal staging count 3986 is not a sequential eligible sample and is omitted.
"""
from build_ijo_figures import *
from pypdf import PdfReader, PdfWriter, Transformation
from pypdf.generic import ContentStream, FloatObject
import io

SOURCE=RESULTS/'03_publication_figures/Supplementary_Figures_v4_Optimized'
DATA=Path(os.environ.get('CKM_CHINA_PLOT_DATA_DIR', WORK/'locked_china_plot_data')).resolve()
def readcsv(p):return list(csv.DictReader(Path(p).open()))
def outpath(n):return SUPP/f'Supplementary_Figure_S{n}.pdf'
def finish(c,n):save(c);export(outpath(n),False)

def normalize_source(n):
    source=SOURCE/f'Supplementary_Figure_S{n}.pdf';r=PdfReader(source);src=r.pages[0]
    oldw=float(src.mediabox.width);oldh=float(src.mediabox.height);scale=W/oldw
    extra=16 if n==10 else 0
    pg=fitz.open(source)[0]
    spans=[(s,l['dir']) for b in pg.get_text('dict')['blocks'] for l in b.get('lines',[]) for s in l['spans']]
    ops=ContentStream(src.get_contents(),r);newops=[];inside=False;oldlw=1
    for vals,op in ops.operations:
        if op==b'BT':inside=True;continue
        if op==b'ET':inside=False;continue
        if inside:continue
        if op==b'w':oldlw=float(vals[0])
        if op in (b'S',b's') and oldlw*scale<1:
            newops.append(([FloatObject(1/scale)],b'w'));newops.append((vals,op));newops.append(([FloatObject(oldlw)],b'w'))
        else:newops.append((vals,op))
    ops.operations=newops;src.replace_contents(ops)
    writer=PdfWriter();p=writer.add_blank_page(width=W,height=oldh*scale+extra)
    p.merge_transformed_page(src,Transformation().scale(scale).translate(0,0))
    overlay=io.BytesIO();c=canvas.Canvas(overlay,pagesize=(W,oldh*scale+extra));H=c._pagesize[1]
    if extra:txt(c,8,11,'NHANES apparent performance (n = 2,688)',True)
    audit=[]
    for s,direction in spans:
        text=s['text'];font=FB if 'Bold' in s['font'] else F
        if n==8:
            text=text.replace('Stable severe','Stable Stage 3–4').replace('Mild-to-severe progressor','Progressor')
        size=max(5,min(7,s['size']*scale));dx,dy=direction
        x,y=s['origin'];x*=scale;y=y*scale+extra
        oldextent=(s['bbox'][2]-s['bbox'][0]) if abs(dx)>.8 else(s['bbox'][3]-s['bbox'][1])
        delta=(oldextent*scale-pdfmetrics.stringWidth(text,font,size))/2
        x+=dx*delta;y+=dy*delta
        if n==10 and text in ['0.608','0.692','0.717','0.721','0.0531','0.0515','0.0521','0.0517']:
            x+=4
        c.saveState();c.translate(x,H-y);c.rotate(math.degrees(math.atan2(-dy,dx)));c.setFont(font,size)
        colour=s['color'];c.setFillColor(Color(((colour>>16)&255)/255,((colour>>8)&255)/255,(colour&255)/255));c.drawString(0,0,text);c.restoreState()
        audit.append({'text':text,'old_final_size':s['size']*scale,'final_size':size})
    save(c);overlay.seek(0);p.merge_page(PdfReader(overlay).pages[0]);writer.write(outpath(n));export(outpath(n),False)
    return {'figure':n,'source':str(source),'source_sha256':hashlib.sha256(source.read_bytes()).hexdigest(),'vector_geometry':'unchanged apart from uniform page scale','source_text_spans':len(spans),'text_changes':audit,'rule_width_change':'stroke-only graph rules increased to >=1 pt; filled point paths retained'}

def arrow(c,x1,y1,x2,y2,color=GREY):
    line(c,x1,y1,x2,y2,color)
    a=math.atan2(y2-y1,x2-x1);sz=4
    for d in [-.55,.55]:line(c,x2,y2,x2-sz*math.cos(a+d),y2-sz*math.sin(a+d),color)
def box(c,x,y,w,h,lines,bold=False,tint='grey'):
    colour={'grey':HexColor('#f1f4f6'),'red':HexColor('#f8eeec'),'blue':HexColor('#edf3f8')}[tint]
    rect(c,x,y,w,h,colour,GREY)
    if isinstance(lines,str):lines=[lines]
    by=y+h/2-(len(lines)-1)*5+2.3
    for i,s in enumerate(lines):txt(c,x+w/2,by+10*i,s,bold,align='center')

def s1():
    n=1;c=new(outpath(n),249)
    box(c,146,8,206,34,['Adults screened for multifrequency BIA','n = 1,676'],True)
    arrow(c,249,42,249,70)
    line(c,249,55,395,55);arrow(c,395,55,395,67)
    box(c,313,67,174,46,['Excluded','Clinical criteria: n = 475','Insufficient staging data: n = 146'])
    box(c,146,122,206,34,['Eligible participants','n = 1,055'],True)
    arrow(c,249,70,249,122)
    arrow(c,218,156,133,190);arrow(c,280,156,370,190)
    box(c,15,190,224,47,['Chinese derivation cohort','n = 955'],True,'red')
    box(c,263,190,224,47,['Distinct repeated-measures cohort','n = 100'],True,'blue')
    finish(c,n)

def s4():
    n=4;c=new(outpath(n),367)
    data=readcsv(RESULTS/'04_publication_tables/Supplementary_Tables_v4/individual_csv/Supplementary_Table_S8.csv')
    def rget(comp,metric):return next(r for r in data if r['Comparison']=='FDP vs '+comp and r['Metric']==metric)
    comps=['Baseline','BMI','VFA']
    for letter,label,metric,left,lo,hi,ticks in [('A','Continuous NRI','Continuous_NRI',8,0,.55,[0,.1,.2,.3,.4,.5]),('B','Integrated discrimination improvement','IDI',257,0,.105,[0,.025,.05,.075,.1])]:
        title(c,letter,label,left,15);x0=left+50;x1=left+219;xm=lambda v:x0+(v-lo)/(hi-lo)*(x1-x0)
        for i,co in enumerate(comps):
            y=48+i*36;r=rget(co,metric);l=xm(float(r['CI_Lower']));u=xm(float(r['CI_Upper']));x=xm(float(r['Estimate']))
            txt(c,left+4,y+2.5,'Baseline' if co=='Baseline' else'+'+co)
            line(c,l,y,u,y,BLUE);line(c,l,y-3,l,y+3,BLUE);line(c,u,y-3,u,y+3,BLUE);dot(c,x,y)
        ay=140;line(c,x0,ay,x1,ay)
        for v in ticks:line(c,xm(v),ay,xm(v),ay+3);txt(c,xm(v),ay+14,f'{v:g}',align='center')
        txt(c,(x0+x1)/2,167,'Estimate (bootstrap 95% CI)',align='center')
    title(c,'C','NRI components',8,195);dot(c,27,210,RED);txt(c,34,212.5,'Event NRI');dot(c,108,210,BLUE);txt(c,115,212.5,'Non-event NRI')
    x0=58;x1=227;xm=lambda v:x0+v/.33*(x1-x0)
    for i,co in enumerate(comps):
        y=239+i*35;txt(c,12,y+2.5,'Baseline' if co=='Baseline' else'+'+co)
        for metric,d,col in [('NRI_Event',-5,RED),('NRI_Nonevent',5,BLUE)]:
            r=rget(co,metric);l=xm(float(r['CI_Lower']));u=xm(float(r['CI_Upper']));x=xm(float(r['Estimate']))
            line(c,l,y+d,u,y+d,col);line(c,l,y+d-2,l,y+d+2,col);line(c,u,y+d-2,u,y+d+2,col);dot(c,x,y+d,col,2.2)
    ay=329;line(c,x0,ay,x1,ay)
    for v in [0,.1,.2,.3]:line(c,xm(v),ay,xm(v),ay+3);txt(c,xm(v),ay+14,f'{v:g}',align='center')
    txt(c,(x0+x1)/2,357,'Estimate (bootstrap 95% CI)',align='center')
    title(c,'D','Calibration curves',257,195)
    d=readcsv(DATA/'calibration_curves.csv');x0=292;x1=480;y0=223;y1=329
    xm=lambda v:x0+v*(x1-x0);ym=lambda v:y1-v*(y1-y0)
    line(c,x0,y1,x1,y1);line(c,x0,y0,x0,y1)
    for v in [0,.25,.5,.75,1]:line(c,xm(v),y1,xm(v),y1+3);txt(c,xm(v),y1+14,f'{v:g}',align='center')
    for v in [0,.5,1]:line(c,x0-3,ym(v),x0,ym(v));txt(c,x0-6,ym(v)+2.5,f'{v:g}',align='right')
    line(c,xm(0),ym(0),xm(1),ym(1),GREY,1,[3,3])
    colours={'Baseline':GREY,'BMI':HexColor('#bd8922'),'FDP':RED,'VFA':BLUE}
    for i,(model,col) in enumerate(colours.items()):
        pts=[(xm(float(r['Predicted'])),ym(float(r['Observed']))) for r in d if r['Model']==model]
        if not pts:pts=[(xm(float(r['Predicted'])),ym(float(r['Observed']))) for r in d if r['Model']=='+'+model]
        for a,b in zip(pts,pts[1:]):line(c,*a,*b,col)
        x=279+i*50;line(c,x,210,x+8,210,col);txt(c,x+11,212.5,model if model=='Baseline' else'+'+model,size=6)
    txt(c,(x0+x1)/2,357,'Predicted probability',align='center')
    c.saveState();c.translate(269,c._pagesize[1]-(y0+y1)/2);c.rotate(90);c.setFont(F,7);c.setFillColor(INK);c.drawCentredString(0,0,'Observed probability');c.restoreState()
    finish(c,n)
    (WORK/'S4_locked_interval_source.json').write_text(json.dumps({'source':'Supplementary_Table_S8.csv','data':data,'point_estimates_refitted':False,'bootstrap_rerun':False},indent=2))

def s13():
    n=13;c=new(outpath(n),326)
    steps=[(8,['NHANES 2011–2018 demographic records','n = 39,156']),
           (72,['Aged 30–59 years','n = 11,182']),
           (136,['Positive fasting-subsample weight','n = 4,664']),
           (200,['Complete revised CKM staging','n = 3,523'])]
    for y,ls in steps:box(c,31,y,249,32,ls,True)
    reasons=[['Outside the eligible age range','n = 27,974 excluded'],
             ['No positive fasting-subsample weight','n = 6,518 excluded'],
             ['Incomplete revised CKM staging','n = 1,141 excluded'],
             ['Missing ≥1 of seven adiposity inputs','n = 835 excluded']]
    for y,ls in zip([56,120,184,251],reasons):
        arrow(c,155.5,y-16 if y<250 else 232,155.5,y+16 if y<250 else 272)
        arrow(c,155.5,y,306,y);box(c,306,y-14,181,28,ls)
    box(c,31,272,249,42,['Final analytic sample: n = 2,688','179 participants in CKM Stages 3–4'],True,'blue')
    finish(c,n)

def s14():
    n=14;c=new(outpath(n),512)
    title(c,'A','Chinese cohorts',8,16);title(c,'B','NHANES cohort',257,16)
    box(c,27,31,201,30,['1,676 screened adults'],True);arrow(c,128,61,128,78)
    box(c,14,78,227,37,['475 clinical exclusions','146 insufficient staging records']);arrow(c,128,115,128,132)
    box(c,27,132,201,35,['1,055 eligible','Aged 30–79 years'],True,'red')
    arrow(c,97,167,66,187);arrow(c,158,167,189,187)
    box(c,8,187,116,51,['Derivation n = 955','451 in Stages 3–4'],False,'red')
    box(c,137,187,108,51,['Separate paired n = 100','37 + 63 participants'],False,'blue')
    box(c,266,31,221,32,['39,156 participants, 2011–2018'],True)
    box(c,266,79,221,32,['11,182 aged 30–59 years'],True)
    box(c,266,127,221,32,['Positive fasting-subsample weight','n = 4,664'],False,'blue')
    box(c,266,175,221,32,['Complete revised CKM staging','n = 3,523'],False,'red')
    for y,k in [(63,'27,974'),(111,'6,518'),(159,'1,141'),(207,'835')]:
        arrow(c,377,y,377,y+16 if y<207 else 224)
        txt(c,269,y+11,f'Excluded: {k}',size=6.5)
    box(c,266,224,221,29,['Final n = 2,688; 179 in Stages 3–4'],True,'blue')
    title(c,'C','Projection and replication framework',8,279)
    box(c,8,300,143,44,['Chinese derivation BIA cohort','Standardize seven inputs','Define PCA'],True,'red')
    box(c,177,300,143,44,['NHANES DXA','Within-cohort standardization','of seven inputs'],False,'blue')
    box(c,345,300,143,44,['NHANES DXA','Independent PCA'],False,'blue')
    arrow(c,80,344,80,439)
    box(c,8,439,143,53,['Repeated-measures projection','Use locked Chinese means,','SDs and loading vectors'],False,'grey')
    arrow(c,248,344,248,380)
    line(c,80,357,248,357);arrow(c,80,344,80,357)
    txt(c,166,369,'China loading vectors only',align='center',size=6.5)
    box(c,177,380,143,42,['Fixed China-loading scores','PC1 and PC2'],True,'blue')
    arrow(c,416,344,416,380)
    box(c,345,380,143,42,['Independent PC1 and PC2','Compare loadings and scores'],False,'blue')
    arrow(c,248,422,248,448);arrow(c,416,422,416,448)
    box(c,177,448,311,44,['Replication of concurrent CKM-stage associations','Survey-weighted analyses in NHANES'],True)
    finish(c,n)

def poly(c,points,fill=None,stroke=BLUE,alpha=1):
    if not points:return
    c.saveState();p=c.beginPath();H=c._pagesize[1];p.moveTo(points[0][0],H-points[0][1])
    for x,y in points[1:]:p.lineTo(x,H-y)
    if fill is not None:p.close();c.setFillColor(fill)
    c.setStrokeColor(stroke);c.setFillAlpha(alpha);c.setStrokeAlpha(alpha);c.setLineWidth(1);c.drawPath(p,stroke=int(fill is None),fill=int(fill is not None));c.restoreState()

def s15():
    n=15;c=new(outpath(n),262)
    for pc,letter,left,lo,hi,yticks,col,ptext in [(1,'A',8,-4.1,6,[.5,1,2,4],RED,'Overall P <0.001; nonlinear P = 0.061'),(2,'B',257,-1.4,1.15,[.25,.5,1,2,4],BLUE,'Overall P = 0.002; nonlinear P = 0.150')]:
        rows=readcsv(DATA/f'pc{pc}_curve.csv');xx=[float(r[f'PC{pc}_Score']) for r in rows];lo=min(xx);hi=max(xx)
        title(c,letter,f'PC{pc}: adjusted restricted cubic spline',left,16);txt(c,left+19,32,ptext,size=6.5)
        x0=left+40;x1=left+230;y0=47;y1=220
        vals=[float(r[k]) for r in rows for k in ['lower','upper']];yl=min(vals)*.95;yu=max(vals)*1.05
        xm=lambda v:x0+(v-lo)/(hi-lo)*(x1-x0);ym=lambda v:y1-(math.log(v)-math.log(yl))/(math.log(yu)-math.log(yl))*(y1-y0)
        upper=[(xm(float(r[f'PC{pc}_Score'])),ym(float(r['upper']))) for r in rows];lower=[(xm(float(r[f'PC{pc}_Score'])),ym(float(r['lower']))) for r in rows]
        poly(c,upper+lower[::-1],fill=col,alpha=.15)
        line(c,x0,ym(1),x1,ym(1),GREY,1,[3,3]);poly(c,[(xm(float(r[f'PC{pc}_Score'])),ym(float(r['yhat']))) for r in rows],stroke=col)
        line(c,x0,y0,x0,y1);line(c,x0,y1,x1,y1)
        for v in yticks:
            if yl<v<yu:line(c,x0-3,ym(v),x0,ym(v));txt(c,x0-6,ym(v)+2.5,f'{v:g}',align='right')
        ticks=[-4,-2,0,2,4] if pc==1 else[-1,-.5,0,.5,1]
        for v in ticks:
            if lo<=v<=hi:line(c,xm(v),y1,xm(v),y1+3);txt(c,xm(v),y1+14,f'{v:g}',align='center')
        txt(c,(x0+x1)/2,249,f'PC{pc} score',align='center')
        c.saveState();c.translate(left+10,c._pagesize[1]-(y0+y1)/2);c.rotate(90);c.setFont(F,7);c.setFillColor(INK);c.drawCentredString(0,0,'Adjusted odds ratio');c.restoreState()
    finish(c,n)

def s16():
    n=16;c=new(outpath(n),332)
    title(c,'A','NHANES-derived associations (n = 2,688; 179 in Stages 3–4)',8,16)
    rows=[r for r in tab('External_OR') if r['Analysis'] in ['NHANES-derived PC1','NHANES-derived PC2']]
    forest(c,rows,[56,85],137,326,.35,2,[.4,.6,1,2],labelx=21,cix=354,px=482,axis_y=104,labels=['PC1','PC2'],head_y=34,color=[RED,BLUE],xlabel='Odds ratio per weighted SD')
    title(c,'B','Leave-one-cycle-out: fixed China-loading PC2',8,155)
    main=next(r for r in tab('External_OR') if r['Analysis']=='Fixed China-loading PC2');rr=[main]+tab('LOCO')
    labs=['All cycles (n = 2,688)','Exclude 2011–2012 (n = 2,003)','Exclude 2013–2014 (n = 1,910)','Exclude 2015–2016 (n = 2,018)','Exclude 2017–2018 (n = 2,133)']
    forest(c,rr,[190,213,236,259,282],177,326,.3,1.03,[.3,.5,.7,1],labelx=21,cix=354,px=482,axis_y=300,labels=labs,head_y=174,xlabel='Odds ratio per weighted SD')
    finish(c,n)

def s17():
    n=17;c=new(outpath(n),467)
    rows=readcsv(DATA/'paired_scores_no_identifiers.csv');progress=[r for r in rows if r['Progression_Status']=='Progressor'];stable=[r for r in rows if r['Progression_Status']=='Stable Severe'];assert len(progress)==37 and len(stable)==63
    a=np.array([float(r['PC2_Score_0']) for r in progress]);b=np.array([float(r['PC2_Score_1']) for r in progress]);delta=b-a
    assert (delta<0).sum()==27
    title(c,'A','Exploratory repeated-measures cohort',8,16)
    box(c,33,39,190,36,['Separate n = 100','Aged 30–79 years'],True)
    arrow(c,94,75,66,96);arrow(c,160,75,193,96)
    box(c,8,96,113,53,['Progressors: n = 37','Stages 0–2 to Stages 3–4'],False,'blue')
    box(c,139,96,106,53,['Reference: n = 63','Stages 3–4 at both visits'],False,'grey')
    txt(c,128,174,'Variable follow-up',True,align='center');txt(c,128,190,'Median 379.5 days',align='center')
    txt(c,128,204,'IQR 322.75–667.5; range 92–968',align='center')
    title(c,'B','PC2 summary in progressors',257,16);txt(c,279,32,'Median and IQR; n = 37')
    x0=294;x1=480;y0=45;y1=177;lo=-.8;hi=1.1;ym=lambda v:y1-(v-lo)/(hi-lo)*(y1-y0)
    line(c,x0,y0,x0,y1);line(c,x0,y1,x1,y1);line(c,x0,ym(0),x1,ym(0),GREY,1,[3,3])
    for v in [-.5,0,.5,1]:line(c,x0-3,ym(v),x0,ym(v));txt(c,x0-6,ym(v)+2.5,f'{v:g}',align='right')
    for x,v,lab in [(328,a,'Baseline'),(445,b,'Follow-up')]:
        q=np.quantile(v,[.25,.5,.75]);line(c,x,ym(q[0]),x,ym(q[2]),BLUE);line(c,x-10,ym(q[0]),x+10,ym(q[0]),BLUE);line(c,x-10,ym(q[2]),x+10,ym(q[2]),BLUE);dot(c,x,ym(q[1]),BLUE,3)
        txt(c,x,192,lab,align='center');txt(c,x,206,f'{q[1]:.2f}',True,align='center')
    txt(c,387,221,'Paired Wilcoxon P = 0.013',align='center')
    c.saveState();c.translate(269,c._pagesize[1]-(y0+y1)/2);c.rotate(90);c.setFont(F,7);c.setFillColor(INK);c.drawCentredString(0,0,'PC2 score');c.restoreState()
    title(c,'C','Paired PC2 trajectories',8,248);txt(c,38,264,'73% had lower PC2 at follow-up')
    x0=40;x1=232;y0=280;y1=418;lo=-1.12;hi=1.15;ym=lambda v:y1-(v-lo)/(hi-lo)*(y1-y0)
    line(c,x0,y0,x0,y1);line(c,x0,y1,x1,y1)
    for v in [-1,-.5,0,.5,1]:line(c,x0-3,ym(v),x0,ym(v));txt(c,x0-6,ym(v)+2.5,f'{v:g}',align='right')
    xa=67;xb=205
    for aa,bb in zip(a,b):
        col=BLUE if bb<aa else GREY;faint=Color(col.red,col.green,col.blue,alpha=.25);c.saveState();line(c,xa,ym(aa),xb,ym(bb),faint);c.restoreState();dot(c,xa,ym(aa),col,1.3);dot(c,xb,ym(bb),col,1.3)
    for x,vals in [(xa,a),(xb,b)]:
        q=np.quantile(vals,[.25,.5,.75]);c.saveState();rect(c,x-12,ym(q[2]),24,ym(q[0])-ym(q[2]),Color(BLUE.red,BLUE.green,BLUE.blue,alpha=.1));c.restoreState()
        for t1,t2 in [((x-12,ym(q[0])),(x+12,ym(q[0]))),((x-12,ym(q[2])),(x+12,ym(q[2]))),((x-12,ym(q[0])),(x-12,ym(q[2]))),((x+12,ym(q[0])),(x+12,ym(q[2]))),((x-12,ym(q[1])),(x+12,ym(q[1])))]:line(c,*t1,*t2,BLUE)
    txt(c,xa,433,'Baseline',align='center');txt(c,xb,433,'Follow-up',align='center')
    line(c,39,449,51,449,BLUE);txt(c,55,451.5,'Lower');line(c,116,449,128,449,GREY);txt(c,132,451.5,'Not lower')
    c.saveState();c.translate(13,c._pagesize[1]-(y0+y1)/2);c.rotate(90);c.setFont(F,7);c.setFillColor(INK);c.drawCentredString(0,0,'PC2 score');c.restoreState()
    title(c,'D','PC2 change by transition group',257,248);txt(c,290,264,'Wilcoxon rank-sum P = 0.002')
    x0=294;x1=480;y0=280;y1=418;lo=-1.4;hi=1.15;ym=lambda v:y1-(v-lo)/(hi-lo)*(y1-y0)
    line(c,x0,y0,x0,y1);line(c,x0,y1,x1,y1);line(c,x0,ym(0),x1,ym(0),GREY,1,[3,3])
    for v in [-1,-.5,0,.5,1]:line(c,x0-3,ym(v),x0,ym(v));txt(c,x0-6,ym(v)+2.5,f'{v:g}',align='right')
    for x,rs,lab,col in [(328,progress,'Progressors',BLUE),(445,stable,'Reference',GREY)]:
        vals=np.array([float(r['Delta_PC2']) for r in rs]);offset=[(((i*53)%len(vals))/(len(vals)-1)-.5)*18 for i in range(len(vals))]
        for v,j in zip(vals,offset):dot(c,x+j,ym(v),col,1.7)
        q=np.quantile(vals,[.25,.5,.75]);line(c,x,ym(q[0]),x,ym(q[2]),col)
        for v in q:line(c,x-11,ym(v),x+11,ym(v),col)
        txt(c,x,433,lab,align='center');txt(c,x,447,f'n = {len(rs)}',align='center')
    c.saveState();c.translate(269,c._pagesize[1]-(y0+y1)/2);c.rotate(90);c.setFont(F,7);c.setFillColor(INK);c.drawCentredString(0,0,'Change in PC2');c.restoreState()
    finish(c,n)
    (WORK/'S17_locked_data_validation.json').write_text(json.dumps({'source':'locked master, Chinese longitudinal only','progressors':len(progress),'reference':len(stable),'lower_pc2':int((delta<0).sum()),'baseline_q1_median_q3':np.quantile(a,[.25,.5,.75]).tolist(),'followup_q1_median_q3':np.quantile(b,[.25,.5,.75]).tolist(),'delta_q1_median_q3':np.quantile(delta,[.25,.5,.75]).tolist(),'new_models_or_tests':False},indent=2))

def qa():
    result=[]
    for n in range(1,18):
        p=outpath(n);pg=fitz.open(p)[0];sp=[s for b in pg.get_text('dict')['blocks'] for l in b.get('lines',[]) for s in l['spans']]
        bad=[s['text'] for s in sp if s['bbox'][0]<-.1 or s['bbox'][1]<-.1 or s['bbox'][2]>pg.rect.width+.1 or s['bbox'][3]>pg.rect.height+.1]
        result.append({'figure':n,'width_mm':pg.rect.width/72*25.4,'height_mm':pg.rect.height/72*25.4,'min_font_pt':min(s['size'] for s in sp),'max_font_pt':max(s['size'] for s in sp),'outside_page_text':bad,'image_count':len(pg.get_images()),'sha256':hashlib.sha256(p.read_bytes()).hexdigest()})
    (WORK/'supplementary_figures_technical_qa.json').write_text(json.dumps(result,indent=2));print(json.dumps(result,indent=2))

if __name__=='__main__':
    if any(SUPP.iterdir()): raise ValueError('Use an empty Supplementary_Figures directory.')
    audits=[]
    s1()
    for n in [2,3,5,6,7,8,9,10,11,12]:audits.append(normalize_source(n))
    s4();s13();s14();s15();s16();s17();qa()
    (WORK/'supplementary_vector_normalization_audit.json').write_text(json.dumps(audits,indent=2))
