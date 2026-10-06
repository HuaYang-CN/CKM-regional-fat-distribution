# Rebuild all twenty approved panels and five composites from frozen displays.
# Run from the project root:
# Rscript 09_analysis_code/Stage10D_figure_redesign/Stage10D_Final_Main_Figures_v3.R
# Optional first argument or STAGE10D_PROJECT_ROOT selects another project root.
# Uses base R + grid and Cairo; no inferential analyses or package installation.
# Type-7 descriptive quartiles validate the paired coordinates against locked display
# summaries; the figure uses the locked IQR boxes without computed whiskers.
# Also reads retained QA CSVs Figure4a_4b_paired_values.csv and Figure4_summary_values.csv.
# Required retained inputs are in 07_outputs/Stage10D_FigureRedesign_v3/05_scripts:
# Locked_Approved_Panel_Display_Coordinates.tsv (approved vector primitives)
# Locked_Forest_Display_Values.tsv (18 approved OR/CI/P display rows).
# Locked_Tucker_Display_Values.tsv (2 conditional bootstrap display rows).
# The coordinate file includes participant-derived drawing geometry: keep local;
# it is deliberately excluded from the review ZIP. No PDF or PNG is imported.
# Preparation provenance and source reconciliation are in 06_audit.
args <- commandArgs(trailingOnly=TRUE)
if(.Platform$OS.type=='windows') invisible(Sys.setlocale('LC_CTYPE','English_United States.utf8'))
PROJECT_ROOT <- normalizePath(if(length(args)) args[1] else
  Sys.getenv('STAGE10D_PROJECT_ROOT', unset=getwd()), winslash='/', mustWork=TRUE)
OUT <- file.path(PROJECT_ROOT, '07_outputs', 'Stage10D_FigureRedesign_v3')
INPUT <- file.path(OUT,'05_scripts')
for(d in c('01_figures','02_panels','06_audit')) dir.create(file.path(OUT,d),recursive=TRUE,showWarnings=FALSE)
library(grid)
TUCKER <- read.delim(file.path(INPUT,'Locked_Tucker_Display_Values.tsv'), stringsAsFactors=FALSE, na.strings='',fileEncoding='UTF-8')
QA <- file.path(PROJECT_ROOT,'07_outputs','Stage7_5_Figure_Supplement_Production','qa')
PAIRED <- read.csv(file.path(QA,'Figure4a_4b_paired_values.csv'),stringsAsFactors=FALSE)
SUMMARY <- read.csv(file.path(QA,'Figure4_summary_values.csv'),stringsAsFactors=FALSE)
LOCKED <- setNames(SUMMARY$Value,SUMMARY$Metric)
stopifnot(capabilities('cairo'))
coords <- read.delim(file.path(INPUT,'Locked_Approved_Panel_Display_Coordinates.tsv'),
  check.names=FALSE, stringsAsFactors=FALSE, na.strings='', fileEncoding='UTF-8',quote='"')
forests <- read.delim(file.path(INPUT,'Locked_Forest_Display_Values.tsv'),
  check.names=FALSE, stringsAsFactors=FALSE, na.strings='',fileEncoding='UTF-8')
stopifnot(nrow(forests)==18, all(forests$lower>0),all(forests$lower<=forests$estimate),
  all(forests$estimate<=forests$upper))
BLACK <- '#15191E'; BLUE <- '#1465A0'; RED <- '#B44536'; GRAY <- '#4A4F57'
u <- function(x) unit(x,'bigpts')
text_at <- function(x,y,label,size=10.5,bold=FALSE,color=BLACK,align='left',center=FALSE,angle=0) {
  gp <- gpar(fontfamily='Arial',fontsize=size,fontface=if(bold) 'bold' else 'plain',col=color)
  if(center) grid.text(label,u(x),u(y),just=c(align,'center'),rot=angle,gp=gp)
  else {
    # Preserve the approved PDF baseline, including rotated axis text.
    tg <- textGrob(label,gp=gp)
    offset <- convertHeight(grobHeight(tg),'bigpts',valueOnly=TRUE)/2 -
      convertHeight(grobDescent(tg),'bigpts',valueOnly=TRUE)
    grid.text(label,u(x-offset*sin(angle*pi/180)),u(y+offset*cos(angle*pi/180)),
      just=c(align,'center'),rot=angle,gp=gp)
  }
}
line_at <- function(x,y,x2,y2,color=BLACK,width=.7,dashed=FALSE) {
  grid.segments(u(x),u(y),u(x2),u(y2),gp=gpar(col=color,lwd=width,lty=if(dashed) 'dashed' else 'solid',lineend='butt'))
}
draw_approved_panel <- function(figure,panel,approved=coords) {
  rows <- approved[approved$figure==figure & approved$panel==panel,,drop=FALSE]
  stopifnot(nrow(rows)>0)
  for(i in seq_len(nrow(rows))) {
    r <- rows[i,]; x <- r$x+r$tx; y <- r$y+r$ty
    switch(r$kind,
      text=text_at(x,y,r$text,r$size,r$font=='ArialBold',r$fill,r$align,angle=r$angle),
      line=line_at(x,y,r$x2+r$tx,r$y2+r$ty,r$stroke,r$width,!is.na(r$dash)),
      circle=grid.circle(u(x),u(y),r=u(r$r),gp=gpar(fill=r$fill,col=NA,alpha=r$alpha)),
      rect=grid.rect(u(x),u(y),u(r$w),u(r$h),just=c('left','bottom'),
        gp=gpar(fill=r$fill,col=if(r$drawstroke==1) r$stroke else NA,lwd=r$width)),
      polygon={
        xy <- do.call(rbind,strsplit(strsplit(r$points,';',fixed=TRUE)[[1]],',',fixed=TRUE))
        grid.polygon(u(as.numeric(xy[,1])+r$tx),u(as.numeric(xy[,2])+r$ty),
          gp=gpar(fill=r$fill,col=NA,alpha=r$alpha))
      },
      polyline={
        xy <- do.call(rbind,strsplit(strsplit(r$points,';',fixed=TRUE)[[1]],',',fixed=TRUE))
        grid.lines(u(as.numeric(xy[,1])+r$tx),u(as.numeric(xy[,2])+r$ty),
          gp=gpar(col=r$stroke,lwd=r$width,lineend='butt'))
      },stop('Unknown approved primitive'))
  }
}
# A single shared four-column layout. Text and marks share the same row y.
# Column allocations: analysis 21%, graphical forest 45%, OR/CI 23%, P 11%.
# Body/header 10.5 pt, sample sizes/ticks 9.5 pt, titles 11.5 pt, letters 16 pt.
make_forest_table_panel <- function(data,letter,title,height,axis_title,
  xlim,ticks,label_header='Analysis',colors=rep(BLUE,nrow(data)),width=468,scale='log',reference=1,
  statistic_header='OR (95% CI)',columns=c(.21,.45,.23,.11),
  split_statistic=FALSE,header_y=height-31,row_y=height-50,row_step=22,axis_y=NULL) {
  stopifnot(length(columns)==4,abs(sum(columns)-1)<1e-8,columns[2]>max(columns[-2]))
  boundaries <- width*c(0,cumsum(columns))
  gx0 <- boundaries[2]; gx1 <- boundaries[3]
  transform <- if(scale=='log') log else if(scale=='linear') identity else stop('Unknown display scale')
  fx <- function(v) gx0+(gx1-gx0)*(transform(v)-transform(xlim[1]))/diff(transform(xlim))
  stopifnot(all(data$lower>=xlim[1]),all(data$upper<=xlim[2]))
  if(scale=='log') stopifnot(xlim[1]>0)
  if(!is.null(reference)) stopifnot(reference>=xlim[1],reference<=xlim[2])
  text_at(0,height-12,letter,16,TRUE)
  text_at(23,height-11,title,11.5,TRUE)
  hy <- header_y
  text_at(0,hy,label_header,10.5,TRUE,center=TRUE)
  header_lines <- strsplit(statistic_header,'\n',fixed=TRUE)[[1]]
  for(j in seq_along(header_lines)) text_at((boundaries[3]+boundaries[4])/2,hy+(length(header_lines)-1)*6-(j-1)*12,header_lines[j],10.5,TRUE,align='center',center=TRUE)
  text_at(width,hy,'P value',10.5,TRUE,align='right',center=TRUE)
  line_at(0,hy-if(length(header_lines)>1) 15 else 10,width,hy-if(length(header_lines)>1) 15 else 10,'#B7C0C8',.6)
  ys <- row_y-(seq_len(nrow(data))-1)*row_step
  ay <- if(is.null(axis_y)) tail(ys,1)-if(any(!is.na(data$n))) 20 else 10 else axis_y
  if(!is.null(reference)) line_at(fx(reference),ay,fx(reference),ys[1]+9,GRAY,.8,TRUE)
  for(i in seq_len(nrow(data))) {
    yy <- ys[i]
    text_at(0,yy,data$label[i],10.5,center=TRUE)
    if(!is.na(data$n[i])) text_at(0,yy-10,paste0('n = ',data$n[i]),9.5,center=TRUE)
    line_at(fx(data$lower[i]),yy,fx(data$upper[i]),yy,colors[i],2.2)
    line_at(fx(data$lower[i]),yy-3,fx(data$lower[i]),yy+3,colors[i],1)
    line_at(fx(data$upper[i]),yy-3,fx(data$upper[i]),yy+3,colors[i],1)
    grid.circle(u(fx(data$estimate[i])),u(yy),u(3.3),gp=gpar(fill=colors[i],col=NA))
    if(split_statistic) {
      pieces <- strsplit(data$or_ci[i],' ',fixed=TRUE)[[1]]
      text_at((boundaries[3]+boundaries[4])/2,yy,pieces[1],10.5,align='center',center=TRUE)
      text_at((boundaries[3]+boundaries[4])/2,yy-12,paste(pieces[-1],collapse=' '),10.5,align='center',center=TRUE)
    } else text_at((boundaries[3]+boundaries[4])/2,yy,data$or_ci[i],10.5,align='center',center=TRUE)
    text_at(width,yy,data$p[i],10.5,align='right',center=TRUE)
  }
  line_at(gx0,ay,gx1,ay)
  for(t in ticks) {
    line_at(fx(t),ay,fx(t),ay-3)
    text_at(fx(t),ay-11,if(scale=='linear') sprintf('%.2f',t) else format(t,trim=TRUE),9.5,align='center',center=TRUE)
  }
  text_at((gx0+gx1)/2,ay-24,axis_title,10,align='center',center=TRUE)
}

# Descriptive checks only: no inferential test or percentage is recomputed.
validate_locked_pairs <- function(data,locked) {
  stopifnot(nrow(data)==37,length(unique(data$Participant))==37,
    all(is.finite(data$Baseline_PC2)),all(is.finite(data$Followup_PC2)))
  expected_baseline <- c(-.09,.31,.47); expected_followup <- c(-.26,.13,.46)
  baseline_locked <- unname(locked[c('Baseline Q1','Baseline median','Baseline Q3')])
  followup_locked <- unname(locked[c('Follow-up Q1','Follow-up median','Follow-up Q3')])
  stopifnot(identical(baseline_locked,expected_baseline),identical(followup_locked,expected_followup),
    locked[['Progressors']]==37,locked[['Lower at follow-up percent']]==73,
    locked[['Paired Wilcoxon P']]==.013,locked[['Paired t sensitivity P']]==.016)
  # Permitted descriptive validation against the authoritative two-decimal display.
  qb <- unname(quantile(data$Baseline_PC2,c(.25,.5,.75),type=7))
  qf <- unname(quantile(data$Followup_PC2,c(.25,.5,.75),type=7))
  if(!identical(round(qb,2),baseline_locked) || !identical(round(qf,2),followup_locked))
    stop('Descriptive quartiles do not match the locked reported display; export stopped.')
  # Classification is read from the retained flag, never redefined by differences.
  lower <- as.character(data$Lower_at_followup)=='TRUE'
  stopifnot(sum(lower)==27,sum(!lower)==10)
  writeLines(c('n paired participants: 37',
    'Baseline median / IQR: 0.31 / -0.09 to 0.47 — MATCH LOCKED DISPLAY',
    'Follow-up median / IQR: 0.13 / -0.26 to 0.46 — MATCH LOCKED DISPLAY',
    'Descriptive type-7 coordinate quartiles match locked values at two-decimal display precision: PASS',
    'IQR geometry uses locked rounded Q1 / median / Q3 only; no whiskers calculated or drawn.',
    'Lower-at-follow-up flags retained: 27 TRUE and 10 FALSE',
    'Locked 73% lower statement read unchanged; no new percentage substituted.',
    'Paired Wilcoxon 0.013 and paired t sensitivity 0.016 read unchanged; neither test rerun.'),
    file.path(OUT,'06_audit','Locked_Paired_Display_Validation.md'))
  invisible(TRUE)
}
make_paired_box_trajectory_panel <- function(data,locked) {
  validate_locked_pairs(data,locked)
  # Retain approved axis, title, category positions, annotation and legend.
  originals <- coords[coords$figure==5 & coords$panel=='C',,drop=FALSE]
  # Redraw approved text / axes / legend, removing participant trajectories/points.
  # The original long chart segments are precisely the 37 paired connections.
  keep <- originals$kind=='text' |
    (originals$kind=='line' & !(abs(originals$x2-originals$x)>100 & originals$width==.85))
  draw_approved_panel(5,'C',originals[keep,])
  fx <- function(v) 43+174*(v-.8)/1.4
  fy <- function(v) 43+133*(v+1.1)/2.25
  stopifnot(all(data$Baseline_PC2>=-1.1 & data$Baseline_PC2<=1.15),
    all(data$Followup_PC2>=-1.1 & data$Followup_PC2<=1.15))
  lower <- as.character(data$Lower_at_followup)=='TRUE'
  for(i in seq_len(nrow(data))) {
    color <- if(lower[i]) BLUE else GRAY
    grid.segments(u(fx(1)),u(fy(data$Baseline_PC2[i])),u(fx(2)),u(fy(data$Followup_PC2[i])),
      gp=gpar(col=color,lwd=.45,alpha=.22,lineend='butt'))
    for(j in 1:2) grid.circle(u(fx(j)),u(fy(if(j==1) data$Baseline_PC2[i] else data$Followup_PC2[i])),u(1.5),
      gp=gpar(fill=color,col=NA,alpha=.55))
  }
  for(j in 1:2) {
    prefix <- if(j==1) 'Baseline' else 'Follow-up'
    q1 <- locked[[paste(prefix,'Q1')]];med <- locked[[paste(prefix,'median')]];q3 <- locked[[paste(prefix,'Q3')]]
    grid.rect(u(fx(j)),u(fy(q1)),u(26),u(fy(q3)-fy(q1)),just=c('center','bottom'),
      gp=gpar(fill=adjustcolor(BLUE,alpha.f=.045),col=BLUE,lwd=1.2))
    line_at(fx(j)-13,fy(med),fx(j)+13,fy(med),BLUE,1.8)
  }
}
validate_locked_pairs(PAIRED,LOCKED)

titles <- c('Study design and Chinese adiposity structure','Chinese concurrent CKM associations',
  'Cross-cohort structure and fixed-loading transport','NHANES concurrent association replication',
  'Complementary repeated-measures evidence')
subtitles <- c('Chinese derivation n = 955 | Separate paired n = 100 | NHANES n = 2,254',
  'n = 955 | 451 in Stages 3-4 | Estimates, splines and 95% confidence intervals',
  'Independent PCA, conditional stability and transported-score correspondence',
  'Primary survey estimates and supportive fixed-loading PC2 robustness',
  'Selected cohort | Absolute paired changes | Hypothesis-generating evidence')
forest_specs <- list(
  '2A'=list(title='PC1 concurrent association',height=100,axis='OR per PC score unit',xlim=c(.35,1.35),ticks=c(.4,.6,1,1.3),header='Model',colors=c(RED,RED)),
  '2B'=list(title='PC2 concurrent association',height=100,axis='OR per PC score unit',xlim=c(.35,1.35),ticks=c(.4,.6,1,1.3),header='Model',colors=c(BLUE,BLUE)),
  '4A'=list(title='NHANES-derived associations',height=100,axis='Survey-weighted OR per SD',xlim=c(.3,2.7),ticks=c(.5,1,2),header='Analysis',colors=c(RED,BLUE)),
  '4B'=list(title='Fixed China-loading associations',height=100,axis='Survey-weighted OR per SD',xlim=c(.3,2.7),ticks=c(.5,1,2),header='Analysis',colors=c(RED,BLUE)),
  '4C'=list(title='Fixed-loading PC2 sensitivities',height=170,axis='Fixed-loading PC2 OR per SD',xlim=c(.3,1.15),ticks=c(.4,.6,1),header='Analysis',colors=rep(BLUE,5)),
  '4D'=list(title='Leave-one-cycle-out robustness',height=170,axis='Fixed-loading PC2 OR per SD',xlim=c(.3,1.15),ticks=c(.4,.6,1),header='Analysis',colors=rep(BLUE,5)))
panel_draw <- function(figure,panel) {
  s <- forest_specs[[paste0(figure,panel)]]
  if(figure==3 && panel=='C') {
    make_forest_table_panel(TUCKER,'C','Conditional loading stability',236,'Tucker congruence coefficient',
      c(.94,.99),c(.94,.96,.98),'Component',c(RED,BLUE),width=256,scale='linear',reference=NULL,
      statistic_header='Tucker φ\n(95% interval)',columns=c(59,86,68,43)/256,split_statistic=TRUE,
      header_y=178,row_y=140,row_step=52,axis_y=64)
    text_at(0,10,'1,000 conditional NHANES participant-bootstrap',9.5)
    text_at(0,-3,'intervals; Chinese loadings fixed;',9.5)
    text_at(0,-16,'no hypothesis-test P value.',9.5)
  } else if(figure==5 && panel=='C') make_paired_box_trajectory_panel(PAIRED,LOCKED)
  else if(is.null(s)) draw_approved_panel(figure,panel)
  else make_forest_table_panel(forests[forests$figure==figure & forests$panel==panel,],
    panel,s$title,s$height,s$axis,s$xlim,s$ticks,s$header,s$colors)
}
at_panel <- function(x,y,w,h,fn) {
  pushViewport(viewport(x=u(x),y=u(y),width=u(w),height=u(h),just=c('left','bottom'),clip='off'))
  fn();popViewport()
}
figure_height <- c(630,590,630,670,630)
draw_figure <- function(n) {
  hh <- figure_height[n]
  text_at(18,hh-24,titles[n],14,TRUE)
  text_at(18,hh-43,subtitles[n],10)
  if(n==2) {
    at_panel(18,430,468,100,function() panel_draw(2,'A'))
    at_panel(18,308,468,100,function() panel_draw(2,'B'))
    at_panel(18,40,222,236,function() panel_draw(2,'C'))
    at_panel(264,40,222,236,function() panel_draw(2,'D'))
  } else if(n==4) {
    for(i in 1:4) at_panel(18,c(520,404,214,24)[i],468,c(100,100,170,170)[i],
      local({p <- LETTERS[i];function() panel_draw(4,p)}))
  } else for(i in 1:4) at_panel(if(n==3 && i==3) 12 else if(n==3 && i==4) 276 else 18+((i-1)%%2)*246,
    if(i<=2) 330 else 55,if(n==3 && i==3) 256 else 222,236,
    local({p <- LETTERS[i];function() panel_draw(n,p)}))
}
export <- function(stem,w,h,draw) {
  cairo_pdf(paste0(stem,'.pdf'),width=w/72,height=h/72,family='Arial',bg='white',onefile=TRUE)
  grid.newpage();draw();dev.off()
  png(paste0(stem,'.png'),width=w/72,height=h/72,units='in',res=600,type='cairo',bg='white')
  grid.newpage();draw();dev.off()
}
for(n in 1:5) {
  export(file.path(OUT,'01_figures',paste0('Figure_',n,'_Final_v3')),504,figure_height[n],function() draw_figure(n))
  for(p in LETTERS[1:4]) {
    s <- forest_specs[[paste0(n,p)]]
    pw <- if(n==3 && p=='C') 256 else if(is.null(s)) 222 else 468; ph <- if(is.null(s)) 236 else s$height
    export(file.path(OUT,'02_panels',paste0('Figure_',n,p,'_Final_v3')),pw+20,ph+58,
      function() at_panel(10,48,pw,ph,function() panel_draw(n,p)))
  }
  cat('Created Figure',n,'and all four panels: vector PDF + 600-dpi PNG\n')
}
writeLines(c('Script parse: PASS','Full plotting run: PASS','All 5 PDFs created: YES',
  'All 5 PNGs created: YES','All 20 panels exported: YES','PNG resolution: 600 dpi',
  'Scientific models or inferential statistics recomputed: NO',
  paste('R version:',R.version.string),paste('Approved display primitives:',nrow(coords)),
  'Required local coordinate inputs are listed at the top of the standalone R script.'),
  file.path(OUT,'06_audit','R_Script_Validation.md'))
