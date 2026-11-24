"""Render final Nutrients Figure S10 from restricted-workflow aggregate exports.
Usage: python scripts/render_s10.py AGGREGATE_DIR NEW_OUTPUT_DIR FONT_REGULAR FONT_BOLD
Dependencies: reportlab, pypdfium2, Pillow. Supply locally licensed TrueType fonts.
The publication used Arial. No participant-level records are read by this renderer.
"""
import argparse
import csv
from pathlib import Path
from PIL import Image
from reportlab.pdfgen.canvas import Canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.colors import HexColor
import pypdfium2 as pdfium

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('input', type=Path)
parser.add_argument('output', type=Path)
parser.add_argument('regular_font', type=Path)
parser.add_argument('bold_font', type=Path)
args = parser.parse_args()
W = args.input
qadir = args.output
qadir.mkdir(parents=True, exist_ok=False)
def read_csv(name):
 with name.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def load_d(name):return read_csv(W/('s10_'+name+'.csv'))
curve=load_d('calibration_curves');dec=load_d('fdp_deciles');dca=load_d('dca')
coef=load_d('calibration_bootstrap')
events=load_d('event_counts')[0]
total=int(events['total'])
advanced=int(events['advanced'])
assert 0 < advanced < total and sum(int(d['N']) for d in dec)==total
models=['Baseline','BMI','FDP','VFA']
assert all(sum(d['Model']==model for d in curve)==150 and sum(d['Model']==model for d in dca)==76 for model in models)
colors=dict(Baseline='#6B7280',BMI='#C58A2A',FDP='#B33A3A',VFA='#2F5597')
ink='#1F2937';black='#20252A';gray=colors['Baseline'];white='#FFFFFF'
pdfmetrics.registerFont(TTFont('Arial',str(args.regular_font)))
pdfmetrics.registerFont(TTFont('ArialBold',str(args.bold_font)))
pw,ph=486,486*4250/4146
vw,vh=235,242
side=176
slots={'A':(4,0),'B':(247,0),'C':((pw-vw)/2,ph-vh)}
pdf=qadir/'Figure_S10.pdf';png=qadir/'Figure_S10.png'
c=Canvas(str(pdf),pagesize=(pw,ph),pageCompression=1)
c.setTitle('Figure S10. Calibration and decision-curve analyses')
def label(x,y,t,fontsize=8,color=ink,bold=False,align='left'):
 c.setFillColor(HexColor(color));c.setFont('ArialBold' if bold else 'Arial',fontsize)
 {'left':c.drawString,'right':c.drawRightString,'center':c.drawCentredString}[align](x,ph-y,t)
def line(pts,color,width=0.8,dash=()):
 c.setStrokeColor(HexColor(color));c.setLineWidth(width);c.setLineCap(0);c.setLineJoin(1);c.setDash(list(dash))
 p=c.beginPath();p.moveTo(pts[0][0],ph-pts[0][1])
 for x,y in pts[1:]:p.lineTo(x,ph-y)
 c.drawPath(p)
def panel_legend(ox,oy):
 start=ox+36;end=ox+vw-6
 for i,m in enumerate(models):
  x=start+(end-start)*i/4
  line([(x,oy+28),(x+9,oy+28)],colors[m],1.15)
  label(x+12,oy+30.5,m if m=='Baseline' else '+'+m,7.5)
def format_num(x):
 return str(x).replace('-','−') if x not in (0,1) else str(int(x))
rects={}
for k,title in [('A','Calibration of the +FDP model'),('B','Calibration across models'),('C','Decision-curve analysis')]:
 ox,oy=slots[k]
 x0,y0=ox+42,oy+40;x1,y1=x0+side,y0+side
 rects[k]=(x0,y0,x1,y1)
 label(ox+5,oy+13,k,11,bold=True)
 label(ox+24,oy+12.8,title,9.4,bold=True)
 if k!='A':panel_legend(ox,oy)
 if k=='A':xlim=ylim=(-0.04,0.83);xt=yt=[0,0.2,0.4,0.6,0.8]
 if k=='B':xlim=ylim=(0,1);xt=[0,0.25,0.5,0.75,1];yt=[0,0.5,1]
 if k=='C':
  xlim=(0.0125,0.8375);ylim=(-0.152,0.552);xt=[0.2,0.4,0.6,0.8];yt=[0,0.2,0.4]
 def point(x,y):return (x0+(x-xlim[0])/(xlim[1]-xlim[0])*side,y1-(y-ylim[0])/(ylim[1]-ylim[0])*side)
 c.saveState();clip=c.beginPath();clip.rect(x0,ph-y1,side,side);c.clipPath(clip,fill=0,stroke=0)
 if k in 'AB':
  line([point(xlim[0],xlim[0]),point(xlim[1],xlim[1])],gray,0.9,(3,2.6))
  for model in (['FDP'] if k=='A' else models):
   pts=[point(float(a['Predicted']),float(a['Observed'])) for a in curve if a['Model']==model]
   line(pts,colors[model],1.25 if k=='A' else 1.05)
  if k=='A':
   c.setLineWidth(0.85);c.setDash([]);c.setStrokeColor(HexColor(colors['FDP']));c.setFillColor(HexColor(white))
   for d in dec:
    x,y=point(float(d['Predicted']),float(d['Observed']))
    c.circle(x,ph-y,1.95,fill=1,stroke=1)
   for j,d in enumerate(coef):
    val=lambda k:format(float(d[k]),'.2f').replace('-','−')
    label(x1-2,y1-13.8+9*j,f"{d['Metric']} {val('Estimate')} ({val('Lower')} to {val('Upper')})",7.2,black,align='right')
 else:
  line([point(xlim[0],0),point(xlim[1],0)],ink,0.8)
  prev=advanced/total
  ts=sorted({float(a['threshold']) for a in dca})
  line([point(t,max(-0.12,prev-(1-prev)*t/(1-t))) for t in ts],gray,0.95,(4,2.6))
  for model in models:
   pts=[point(float(a['threshold']),float(a['net_benefit'])) for a in dca if a['Model']==model]
   line(pts,colors[model],1.05)
 c.restoreState()
 line([(x0,y0),(x0,y1),(x1,y1)],ink,0.85)
 for n in xt:
  tx,_=point(n,0);line([(tx,y1),(tx,y1+2.8)],ink,0.7);label(tx,y1+11.1,format_num(n),8,align='center')
 for n in yt:
  _,ty=point(0,n);line([(x0-2.8,ty),(x0,ty)],ink,0.7);label(x0-5.2,ty+2.65,format_num(n),8,align='right')
 label((x0+x1)/2,y1+23,'Threshold probability' if k=='C' else 'Predicted probability',8.6,align='center')
 c.saveState();c.translate(x0-26,ph-(y0+y1)/2);c.rotate(90);c.setFont('Arial',8.6);c.setFillColor(HexColor(ink));c.drawCentredString(0,0,'Net benefit' if k=='C' else 'Observed probability');c.restoreState()
c.showPage();c.save()
doc=pdfium.PdfDocument(str(pdf));p=doc[0];im=p.render(scale=4146/pw).to_pil().convert('RGB')
assert max(abs(i-j) for i,j in zip(im.size,(4146,4250)))<=1
if im.size!=(4146,4250):im=im.resize((4146,4250),Image.Resampling.LANCZOS)
im.save(png,dpi=(614.2,614.2))
