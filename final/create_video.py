from pathlib import Path
import subprocess as sp
import cv2
from PIL import Image, ImageDraw, ImageFont
import numpy as np
F=Path('final')
ff=next(Path('C:/Users/13001/AppData/Local/Packages').glob('OpenAI.Codex_*/LocalCache/Local/VibeGit/submission-tools/imageio_ffmpeg/binaries/ffmpeg*.exe'))
W,H=1280,720
def font(n,b=False):return ImageFont.truetype('C:/Windows/Fonts/segoeui'+('b' if b else '')+'.ttf',n)
def label(d,pos,s,n,color='#172e39',bold=False):d.text(pos,s,font=font(n,bold),fill=color)
def slide(title,kicker,foot,chart=None,rows=None,stats=None):
 p=Image.new('RGB',(W,H),'white');d=ImageDraw.Draw(p)
 label(d,(45,25),kicker,20,'#286c78',True);label(d,(45,65),title,43,bold=True)
 if chart:
  im=Image.open(F/chart).convert('RGB');im.thumbnail((1155,455))
  p.paste(im,((W-im.width)//2,135))
 if rows:
  for j,(left,right) in enumerate(rows):
   y=155+j*78
   d.line((55,y+63,1225,y+63),fill='#d7e1e4',width=1)
   label(d,(65,y),left,29,bold=True);label(d,(520,y+2),right,27)
 if stats:
  for j,(v,txt) in enumerate(stats):
   x=60+410*j;label(d,(x,225),v,70,'#206579',True);label(d,(x,325),txt,25)
 d.rectangle((0,604,1280,720),fill='#143845')
 for i,line in enumerate(foot.split('\n')):label(d,(46,619+i*36),line,26,'white')
 return np.array(p)
pages=[
(0,26,slide('How much roof water can one building use?','Hong Kong  /  SDSC 6008  /  Weihan Chen and Juwen Li','Coverage = annual rainwater supplied / annual non-potable demand.\nOne roof and one tank, driven by daily Hong Kong Observatory rainfall.',stats=[('500 m²','Example roof area'),('1.2 m³','Mean daily demand'),('0.75 mm','Example first flush')])),
(26,56,slide('Every year contains all 15 designs','Experimental design','Eight factorial corners, six axial points and one centre.\nTen independent demand trajectories per setting and rainfall year.',rows=[('Roof area','300, 500, 700 m²'),('Usable tank volume','5, 12.5, 20 m³'),('First-flush diversion','0, 0.75, 1.5 mm per event'),('Rainfall blocks','2021, 2022 and 2023'),('Main experiment','15 × 3 × 10 = 450 runs')])),
(107,130,slide('The first expansion delivers the larger gain','Direct simulation','At the centre roof and diversion settings, equal 7.5 m³ increments add\n14.28 and 6.41 percentage points of coverage.',chart='tank_curve.png')),
(130,155,slide('The roof–tank interaction changes by year','Factorial effects','Roof benefit with a 20 m³ tank minus roof benefit with a 5 m³ tank.\nJoint test of all treatment-by-year terms: F(14, 216) = 1053.5, p < 0.001.',stats=[('+4.35 pp','2021'),('+2.89 pp','2022'),('−1.52 pp','2023')])),
(155,175,slide('How close were the predictions at four new designs?','Response surface validation','The common surface missed coverage by 1.20 pp on average.\nThe two tank sizes near the target were only 0.68 pp apart in 2021.',chart='validation.png')),
(175,198,slide('Where does the worst-year mean cross 55%?','Tank sizing  /  500 m² roof, 0.75 mm diversion','Seventeen capacities share 30 demand paths in each year.\nThe blue curve is the lowest mean among the three historical years.',chart='capacity_decision.png')),
(198,217,slide('15 m³ crosses the illustrative 55% target','Tank sizing  /  direct simulation','95% intervals reflect the 30 demand paths under 2021 rainfall.\nNo tested size reached 60% in every historical year.',rows=[('14 m³ in 2021','54.90%   (95% interval 54.78–55.01%)'),('15 m³ in 2021','55.57%   (95% interval 55.46–55.69%)'),('Gain on paired paths','+0.68 pp   (95% interval 0.67–0.69 pp)')])),
(217,240,slide('Storage carries supply into the dry spell','Delivery-day analysis  /  5 versus 20 m³','On consecutive dry days 4–7, coverage rose from 9.93% to 49.05%.\nThe bars show the share of demand met within each group of days.',chart='dry_chart.png')),
(240,256,slide('Dry days 4–7 capture a third of the gain','Delivery-day analysis','Gain is the extra volume supplied by the 20 m³ tank over the 5 m³ tank.\nCoverage on day 8 and later still reached only 17.43% at 20 m³.',stats=[('15.67%','Share of all days'),('32.81%','Share of extra supply'),('39.13 pp','Coverage gain, days 4–7')])),
(256,280,slide('Would 15 m³ work in earlier years?','1990–2019  /  same roof, same target','Each pair uses the same annual demand total and weather. At 15 m³,\n27 of 30 years reached 55% at baseline, and 25 with higher dry-day demand.',chart='history_chart.png')),
(280,300,slide('How often can the building miss its target?','Tank decision  /  earlier weather check','In 2005, even 20 m³ covered only 53.90% of baseline demand.\nSite checks still need local rainfall, measured use, cost, space and water quality.',rows=[('Years reaching 55%','Baseline  /  more demand on dry days'),('14 m³','26 of 30   /   24 of 30'),('15 m³','27 of 30   /   25 of 30'),('20 m³','29 of 30   /   28 of 30')]))]
cap=cv2.VideoCapture(str(F/'RStudio_demo.mp4'));assert cap.isOpened()
frames=int(cap.get(7))
out=F/'Presentation_5min_add_voices.mp4'
proc=sp.Popen([str(ff),'-v','error','-y','-f','rawvideo','-pix_fmt','rgb24','-s','1280x720','-r','5','-i','pipe:0','-an','-c:v','libx264','-preset','veryfast','-crf','19','-pix_fmt','yuv420p','-movflags','+faststart',str(out)],stdin=sp.PIPE)
last=None
for i in range(1500):
 sec=i/5
 if 56<=sec<107:
  ret,im=cap.read()
  if ret:
   last=np.full((720,1280,3),255,np.uint8)
   last[:,64:1216]=cv2.cvtColor(cv2.resize(im,(1152,720)),cv2.COLOR_BGR2RGB)
  frame=last
 else:frame=next(im for a,b,im in pages if a<=sec<b)
 proc.stdin.write(frame.tobytes())
proc.stdin.close();assert proc.wait()==0
v=cv2.VideoCapture(str(out));assert int(v.get(7))==1500 and v.get(5)==5
print('Verified 300-second video, with the five filmed RStudio steps.')
