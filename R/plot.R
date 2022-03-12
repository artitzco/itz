fixCol<-function(col){
  IF(is.null(col), color(), IF(is.character(col), color(col), col))
}

textRGB<- function(col) {
  code<-pst("\\text.color", col, sep="_")
  cls<-globalVar(code)
  if(!base::is.null(cls)) return(cls)
  file<-join(randFile(),".png")
  png(file, width=300, height=300)
  plot(.5, xlim=c(0,1), ylim=c(0,1), type="n")
  TRY(polygon(c(0,0,1,1), c(0,1,1,0), col=col),{
    dev.off()
    unlink(file)
    return(c(1,1,1))
  })
  dev.off()
  rgb<-png::readPNG(file)[150,150,]
  unlink(file)
  cls<-rgb[1:3]
  globalVar(code, cls)
  return(cls)
}

color <- function(r=NA, g=NA, b=NA, maxValue=1) {
  if(is.character(r)) { 
    tab<-NULL
    for (i in 1:length(r)) 
      tab<-rbind(tab, textRGB(r[i]))
    r<-tab[,1]
    g<-tab[,2]
    b<-tab[,3]
  }
  if(length(r)>1 |length(g)>1| length(b)>1)
    return(colorList(r,g,b,maxValue))
  if(is.na(r)){r<-runif(1)}else{r<-r/maxValue}
  if(is.na(g)){g<-runif(1)}else{g<-g/maxValue}
  if(is.na(b)){b<-runif(1)}else{b<-b/maxValue}
  RGB<- newVec(c(r,g,b))
  getRGB<- function(this=F) RGB$get()
  getCol<- function(alp=1, bri =1, this=F) {
    cls<-bri*getRGB()
    return(rgb(cls[1], cls[2], cls[3], alp))
  }
  combine<- function(col, r=0.5) {
    if(length(r)>1) {
      lst<-list()
      for (i in 1:length(r)) {
        cls <- (1-r[i])*getRGB()+r[i]*col$getRGB()
        lst[[i]] <- color(cls[1], cls[2], cls[3])
      }
      return(colorList(col=lst))
    }
    cls <- (1-r)*getRGB()+r*col$getRGB()
    return(color(cls[1], cls[2], cls[3]))
  }
  sequence<- function(col, n) {
    r<-if(n!=1)seq(0,1, 1/(n-1)) else 0.5
    colist<- newList()
    for (i in 1:n) colist$add(combine(col, r[i]))
    return(colorList(colist=colist))
  }
  copy<-function() color(r,g,b)
  inverse<-function() color(1-r, 1-g, 1-b)
  return(list(getRGB=getRGB, getCol=getCol, combine=combine, sequence=sequence,
              copy=copy, inverse=inverse, copiable="copiable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst", 
              class="color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT"))
}

colorList<- function(r=NA, g=NA, b=NA, maxValue=1, colist=NULL) {
  if(is.null(colist)) {
    colist<-newList()
    if(!is.na(r[1]) | !is.na(g[1]) | !is.na(b[1])) {
      if(is.na(r[1])){r<-runif(1)}else{r<-r/maxValue}
      if(is.na(g[1])){g<-runif(1)}else{g<-g/maxValue}
      if(is.na(b[1])){b<-runif(1)}else{b<-b/maxValue}
      n<-max(length(r),length(g),length(b))
      r<-r[1+((1:n)-1)%%length(r)]
      g<-g[1+((1:n)-1)%%length(g)]
      b<-b[1+((1:n)-1)%%length(b)]
      for (i in 1:n) colist$add(color(r[i],g[i],b[i])) 
    }
  }
  varIter<-newVec(colist$iter(cyclic=TRUE, color(0,0,0)))
  add <- function(col) {
    colist$add(col)
    varIter$set(colist$iter(cyclic=TRUE))
  }
  
  getColor <- function(this) IF(this, varIter$get()$This(), varIter$get()$Next())
  getRGB <- function(this=F) getColor(this)$getRGB()
  getCol<- function(alp=1, bri=1, this=F) getColor(this)$getCol(alp=alp, bri=bri)
  copy<-function() color(r,g,b)
  inverse<-function() color(1-r, 1-g, 1-b)
  return(list(getRGB=getRGB, getCol=getCol, add=add,length=colist$length,
              copy=copy, inverse=inverse, copiable="copiable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst", 
              class="color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT"))
}

Lines <- function(name, ...) {
  newGlobalParam(
    "x0000000mbf0N4sBjYI2uWKd29PXxpWMoUgANOR27kPg4h4JPEJ"
    ,function(add){
      add("alp.lines", 1)
      add("col.lines", NULL)
      add("lty.lines", 1)
      add("lwd.lines", 1)
    }
  ) (name, ...)
}


Points <- function(name, ...) {
  newGlobalParam(
    "x0000000XOj4zAq3BxojQ4xB5kdXREfiFpVerO9qR6XWoYkvC9r"
    ,function(add){
      add("alp.points" , 1)
      add("col.points" , NULL)
      add("lwd.points" , 1)
    }
  ) (name, ...)
}

Abline <- function(name, ...) {
  newGlobalParam(
    "x00000000wB8kBq0tLHhQYm1uVm3uQ5F7QN5nkC45U0ebFG8uhc"
    ,function(add){
      add("alp.abline", 1)
      add("col.abline", NULL)
      add("lty.abline", 1)
      add("lwd.abline", 1)
      add("sld.abline", TRUE)
    }
  ) (name, ...)
}

Plot <- function(name, ...) {
  newGlobalParam(
    "x00000s84o5c8Px2ImUj0GxXsQzq0rsUrq5oL2deyXPrw1lPxGw"
    ,function(add){
      add("alp"           , NULL)
      add("col"           , NULL)
      add("lwd"           , NULL)
      add("lty"           , NULL)
      add("alp.backlines" , 0.5)
      add("cex.axis"      , 1.1)
      add("cex.lab"       , 1.3)
      add("cex.main"      , 1.3)
      add("col.background", color("#161616"))
      add("col.backlines" , color("#808080"))
      add("labX"          , NA)
      add("labY"          , NA)
      add("lty.backlines" , 1)
      add("lwd.backlines" , 1)
      add("lx"            , NA)
      add("ly"            , NA)
      add("main"          , NA)
      add("plot.lines"    , TRUE)
      add("plot.points"   , FALSE)
      add("px"            , 6)
      add("py"            , 6)
      add("xlab"          , NA)
      add("xlim"          , NULL)
      add("ylab"          , NA)
      add("ylim"          , NULL)
      add("xdigits"       , NA)
      add("ydigits"       , NA)
    }
  ) (name, ...)
}

Bar <- function(name, ...) {
  newGlobalParam(
    "x000000bPQwasoSlZDX0Ka3Ef89h0d8s89g90hVqdwGw37B1tGu"
    ,function(add){
      add("alp.bar", 1)
      add("col.bar", color("#A6A6A6"))
      add("lwd.bar", 2)
      add("lty.bar", 1)
      add("horizontal", T)
    }
  ) (name, ...)
}

Hist <- function(name, ...) {
  newGlobalParam(
    "x00000000mF3zti9sP0msATjc7UEkz5BNIAQ4rr2fVGsy3NHzBX"
    ,function(add){
      add("relative"  , T)
      add("horizontal", T)
      add("new.plot" , T)
      add("alp.hist"  , 0.8)
      add("col.hist"  , color("#A6A6A6"))
      add("lty.hist"  , 1)
      add("lwd.hist"  , 2)
    }
  ) (name, ...)
}

Shadow <- function(name, ...) {
  newGlobalParam(
    "x0000000s95B0TU1PJbJ10FAcYgxrv7sbuv6yHd9d8g8DFf0Md4"
    ,function(add){
      add("alp.border" , 1)
      add("alp.shadow" , 0.5)
      add("alp.mean"   , 1)
      add("col.border" , NULL)
      add("col.shadow" , NULL)
      add("col.mean"   , NULL)
      add("lty.border" , 1)
      add("lty.mean"   , 1)
      add("lwd.border" , 1)
      add("lwd.mean"   , 1)
      add("plot.shadow", T)
      add("plot.mean", T)
    }
  ) (name, ...)
}

Legend <- function(name, ...) {
  newGlobalParam(
    "x0000000wTaxROM6a5g511AuPnDtqxelpLNHv4GYcw43tdsAder"
    ,function(add){
      add("pos.legend", "topright")
      add("cex.legend", 1)
      add("col.legend", "#000000")
      add("alp.legend", 1)
      add("col.lin"   , NULL)
      add("alp.lin"   , 1)
      add("lwd.lin"   , 1)
      add("lty.lin"   , 1)
      add("col.box"   , "#BEBEBE")
      add("alp.box"   , 1)
      add("lwd.box"   , 1)
      add("lty.box"   , 1)
      add("col.back"  , "#E6E6E6")
      add("alp.back"  , 1)
    }
  ) (name, ...)
}


mlines <- function(x=NULL, y=NULL,
                   alp.lines=nasd(),
                   col.lines=nasd(),
                   lty.lines=nasd(),
                   lwd.lines=nasd(),
                   param=NULL, ...)
{
  mlst<- c(list(...), param, Lines("\\get.list"))
  if(is.nasd(alp.lines))alp.lines<-mlst$alp.lines; if(is.nasd(col.lines))col.lines<-mlst$col.lines; if(is.nasd(lty.lines))lty.lines<-mlst$lty.lines; if(is.nasd(lwd.lines))lwd.lines<-mlst$lwd.lines
  col.lines<-fixCol(isnt.null(col.lines, mlst$col.lines.auxiliar))
  
  if(base::is.null(y) & !base::is.null(x)) {
    y<-x
    if(is.list(y)) {
      x<-list()
      for (i in 1:length(y)) x[[i]]<-1:length(y[[i]])
    }else {
      x <- 1:length(y)  
    }
  }else if(base::is.null(x) & base::is.null(y)) { 
    x<-0
    y<-0
  }
  MLINES<-function(x, y, alp.lines, col.lines, lty.lines, lwd.lines) {
    if(lwd.lines>=1) {
      sq <- seq(0, 1, 1.0/lwd.lines)[-1]
      for (i in 1:lwd.lines)
        lines(x, y, col=col.lines$getCol(bri=sq[i], alp=alp.lines, this=T), lty=lty.lines, lwd=2*(lwd.lines-i)+1)
      NO<-col.lines$getCol()
    }
  }
  
  if(!is.list(x) & !is.list(y)) {
    MLINES(x, y, alp.lines, col.lines, lty.lines, lwd.lines)
  }else {
    if(is.list(x) & is.list(y)) { 
      if(length(x)==1) {
        for (i in 1:length(y))
          MLINES(x[[1]], y[[i]], alp.lines, col.lines, lty.lines, lwd.lines)
      }else if(length(y)==1) {
        for (i in 1:length(x))
          MLINES(x[[i]], y[[1]], alp.lines, col.lines, lty.lines, lwd.lines)
      }else {
        for (i in 1:length(y))
          MLINES(x[[i]], y[[i]], alp.lines, col.lines, lty.lines, lwd.lines)
      }
    }else if(!is.list(x) & is.list(y)) {
      for (i in 1:length(y))
        MLINES(x, y[[i]], alp.lines, col.lines, lty.lines, lwd.lines)
    }else if(is.list(x) & !is.list(y)) {
      for (i in 1:length(x))
        MLINES(x[[i]], y, alp.lines, col.lines, lty.lines, lwd.lines)
    }
  }
}

mpoints <- function(x=NULL, y=NULL,
                    alp.points=nasd(),
                    col.points=nasd(),
                    lty.points=nasd(),
                    lwd.points=nasd(),
                    param=NULL, ...)
{
  mlst<- c(list(...), param, Points("\\get.list"))
  if(is.nasd(alp.points))alp.points<-mlst$alp.points; if(is.nasd(col.points))col.points<-mlst$col.points; if(is.nasd(lwd.points))lwd.points<-mlst$lwd.points
  col.points<-fixCol(isnt.null(col.points, mlst$col.points.auxiliar))
  if(base::is.null(y) & !base::is.null(x)) {
    y<-x
    if(is.list(y)) {
      x<-list()
      for (i in 1:length(y)) x[[i]]<-1:length(y[[i]])
    }else {
      x <- 1:length(y)  
    }
  }else if(base::is.null(x) & base::is.null(y)) { 
    x<-0
    y<-0
  }
  MPOINTS<-function(x, y, alp.points, col.points, lty.points, lwd.points) {
    if(lwd.points>=1) {
      sq <- seq(0, 1, 1.0/lwd.points)[-1]
      for (i in 1:lwd.points)
        points(x, y, col=col.points$getCol(bri=sq[i], alp=alp.points, this=T), lwd=2*(lwd.points-i)+1)
      NO<-col.points$getCol()
    }
  }
  
  if(!is.list(x) & !is.list(y)) {
    MPOINTS(x, y, alp.points, col.points, lty.points, lwd.points)
  }else {
    if(is.list(x) & is.list(y)) { 
      if(length(x)==1) {
        for (i in 1:length(y))
          MPOINTS(x[[1]], y[[i]], alp.points, col.points, lty.points, lwd.points)
      }else if(length(y)==1) {
        for (i in 1:length(x))
          MPOINTS(x[[i]], y[[1]], alp.points, col.points, lty.points, lwd.points)
      }else {
        for (i in 1:length(y))
          MPOINTS(x[[i]], y[[i]], alp.points, col.points, lty.points, lwd.points)
      }
    }else if(!is.list(x) & is.list(y)) {
      for (i in 1:length(y))
        MPOINTS(x, y[[i]], alp.points, col.points, lty.points, lwd.points)
    }else if(is.list(x) & !is.list(y)) {
      for (i in 1:length(x))
        MPOINTS(x[[i]], y, alp.points, col.points, lty.points, lwd.points)
    }
  }
}

mabline <- function(h=NULL, v=NULL,
                    alp.abline =nasd(),
                    col.abline =nasd(),
                    lty.abline =nasd(),
                    lwd.abline =nasd(),
                    sld.abline =nasd(),
                    param=NULL, ...)
{
  mlst<- c(list(...), param, Abline("\\get.list"))
  if(is.nasd(alp.abline))alp.abline<-mlst$alp.abline; if(is.nasd(col.abline))col.abline<-mlst$col.abline; if(is.nasd(lty.abline))lty.abline<-mlst$lty.abline; if(is.nasd(lwd.abline))lwd.abline<-mlst$lwd.abline; if(is.nasd(sld.abline))sld.abline<-mlst$sld.abline
  col.abline<-fixCol(isnt.null(col.abline, mlst$col.abline.auxiliar))
  
  sq <- if(lwd.abline!=1) seq(0, 1, 1.0/lwd.abline)[-1] else 1
  mab <- function(fn, x){
    for (j in 1:length(x)){
      if(lwd.abline>=1){
        for (i in 1:lwd.abline)
          fn(x[j], lwd=if(sld.abline)1+lwd.abline-i else 2*(lwd.abline-i)+1, col=col.abline$getCol(bri=sq[i], alp=alp.abline, this=T), lty=lty.abline)
        NO<-col.abline$getCol()  
      }
    }
  }
  mab(function(x, ...) abline(v=x, ...), v)
  mab(function(x, ...) abline(h=x, ...), h)
}

pol <- function(xlim, ylim, px, py,lx=NA,ly=NA,labX=NA,labY=NA, cex=1, background=color(0.086,0.086,0.086), col.backlines=color(.5,.5,.5), lwd=2, lty=1, alp=1, xdigits=NA, ydigits=NA){
  mx <- (xlim[2]-xlim[1])/2
  my <- (ylim[2]-ylim[1])/2
  x <-c(xlim[1]-mx, xlim[2]+mx)
  y <-c(ylim[1]-my, ylim[2]+my)
  
  fitDig <- function(lim, n, digits) {
    sq<-seq(lim[1],lim[2],(lim[2]-lim[1])/(n-1))
    if(is.na(digits)) return(sq)
    
    sqRef <-round(sq, digits)
    its.ok <- function(x){
      for (i in 2:length(x)) 
        if(x[i]==x[i-1])
          return(FALSE)
      return(TRUE)
    }
    while(!its.ok(sqRef)){
      digits<-digits+1
      sqRef <-round(sq, digits)
    }
    sqRef
  }
  if(is.na(lx[1])) lx<-fitDig(xlim, px, xdigits)
  if(is.na(ly[1])) ly<-fitDig(ylim, py, ydigits)
  if(is.na(labX[1])) labX<-T
  if(is.na(labY[1])) labY<-T
  
  polygon(c(x[1],x[1],x[2],x[2]),c(y[1],y[2],y[2],y[1]),col=background$getCol())
  mabline(v=lx, h=ly, col.abline=col.backlines, lwd.abline=lwd, lty.abline=lty, alp.abline=alp)
  axis(1,at=lx,labels=labX,cex.axis=cex)
  axis(2,at=ly,labels=labY,cex.axis=cex)
}

mplot <- function(x=NULL, y=NULL, 
                  alp            =nasd(),
                  col            =nasd(),
                  lwd            =nasd(),
                  lty            =nasd(),
                  alp.backlines  =nasd(),
                  cex.axis       =nasd(),
                  cex.lab        =nasd(),
                  cex.main       =nasd(),
                  col.background =nasd(),
                  col.backlines  =nasd(),
                  labX           =nasd(),
                  labY           =nasd(),
                  lty.backlines  =nasd(),
                  lwd.backlines  =nasd(),
                  lx             =nasd(),
                  ly             =nasd(),
                  main           =nasd(),
                  plot.lines     =nasd(),
                  plot.points    =nasd(),
                  px             =nasd(),
                  py             =nasd(),
                  xlab           =nasd(),
                  xlim           =nasd(),
                  ylab           =nasd(),
                  ylim           =nasd(),
                  xdigits        =nasd(),
                  ydigits        =nasd(),
                  param=NULL, ...){
  mlst<- c(list(...), param, Plot("\\get.list"))
  if(is.nasd(alp))alp<-mlst$alp; if(is.nasd(col))col<-mlst$col; if(is.nasd(lwd))lwd<-mlst$lwd; if(is.nasd(lty))lty<-mlst$lty; if(is.nasd(alp.backlines))alp.backlines<-mlst$alp.backlines; if(is.nasd(cex.axis))cex.axis<-mlst$cex.axis; if(is.nasd(cex.lab))cex.lab<-mlst$cex.lab; if(is.nasd(cex.main))cex.main<-mlst$cex.main; if(is.nasd(col.background))col.background<-mlst$col.background; if(is.nasd(col.backlines))col.backlines<-mlst$col.backlines; if(is.nasd(labX))labX<-mlst$labX; if(is.nasd(labY))labY<-mlst$labY; if(is.nasd(lty.backlines))lty.backlines<-mlst$lty.backlines; if(is.nasd(lwd.backlines))lwd.backlines<-mlst$lwd.backlines; if(is.nasd(lx))lx<-mlst$lx; if(is.nasd(ly))ly<-mlst$ly; if(is.nasd(main))main<-mlst$main; if(is.nasd(plot.lines))plot.lines<-mlst$plot.lines; if(is.nasd(plot.points))plot.points<-mlst$plot.points; if(is.nasd(px))px<-mlst$px; if(is.nasd(py))py<-mlst$py; if(is.nasd(xlab))xlab<-mlst$xlab; if(is.nasd(xlim))xlim<-mlst$xlim; if(is.nasd(ylab))ylab<-mlst$ylab; if(is.nasd(ylim))ylim<-mlst$ylim; if(is.nasd(xdigits))xdigits<-mlst$xdigits; if(is.nasd(ydigits))ydigits<-mlst$ydigits
  col.background<-fixCol(isnt.null(col.background, mlst$col.background.auxiliar))
  col.backlines<-fixCol(isnt.null(col.backlines, mlst$col.backlines.auxiliar))
  if(base::is.null(y) & !base::is.null(x)){
    y<-x
    if(is.list(y)){
      x<-list()
      for (i in 1:length(y)) x[[i]]<-1:length(y[[i]])
    }else{
      x <- 1:length(y)  
    }
  }else if(base::is.null(x) & base::is.null(y)){
    x<-0
    y<-0
  }
  IF(!is.list(x), x<-list(x))
  IF(!is.list(y), y<-list(y))
  if(is.null(xlim)){
    xmin <- Inf
    xmax <- -Inf
    for (i in 1:length(x)) {
      xmin <- min(xmin, x[[i]], na.rm=T)
      xmax <- max(xmax, x[[i]], na.rm=T)
    }
    xlim <- IF(xmin==xmax, c(xmin-1,xmin+1), c(xmin,xmax))
  }
  if(is.null(ylim)){
    ymin <- Inf
    ymax <- -Inf
    for (i in 1:length(y)) {
      ymin <- min(ymin, y[[i]], na.rm=T)
      ymax <- max(ymax, y[[i]], na.rm=T)
    }
    ylim <-IF(ymin==ymax, c(ymin-1,ymin+1), c(ymin,ymax))
  }
  plot(0, 0, xlim=xlim, ylim=ylim, axes=F, main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab, type="n")
  pol(xlim, ylim, px, py, lx, ly, labX,labY, cex.axis, col.background, col.backlines, lwd.backlines, lty.backlines, alp.backlines,xdigits,ydigits)
  
  if(plot.lines) {
    mlst.lines  <-Lines("\\get.list")
    alp.lines <- isnt.null(mlst$alp.lines,
                           isnt.null(mlst.lines$alp.lines, alp))
    lwd.lines <- isnt.null(mlst$lwd.lines,
                           isnt.null(mlst.lines$lwd.lines, lwd))
    lty.lines <- isnt.null(mlst$lty.lines,
                           isnt.null(mlst.lines$lty.lines, lty))
    col.lines <- isnt.null(mlst$col.lines,
                           isnt.null(mlst.lines$col.lines, col))
    IF(is.null(col.lines), {col.lines<-color(); mark<-TRUE})
    mlines(x, y, alp.lines=alp.lines, col.lines=col.lines, lty.lines=lty.lines, lwd.lines=lwd.lines)
  }
  
  if(plot.points){
    mlst.points <-Points("\\get.list")
    alp.points <- isnt.null(mlst$alp.points,
                            isnt.null(mlst.points$alp.points, alp))
    lwd.points <- isnt.null(mlst$lwd.points,
                            isnt.null(mlst.points$lwd.points, lwd))
    col.points <- isnt.null(mlst$col.points,
                            isnt.null(mlst.points$col.points, col))
    TRY(IF(is.null(col.points) & mark, col.points<-col.lines$copy()))
    mpoints(x, y, alp.points=alp.points, col.points=col.points, lwd.points=lwd.points)
  }
}

mbar <- function(x, y,#####Añadir plot
                 alp.bar =nasd(),
                 col.bar =nasd(),
                 lwd.bar =nasd(),
                 lty.bar =nasd(),
                 horizontal=nasd(),
                 param=NULL, ...)
{ 
  mlst<- c(list(...), param, Bar("\\get.list"))
  if(is.nasd(alp.bar))alp.bar<-mlst$alp.bar; if(is.nasd(col.bar))col.bar<-mlst$col.bar; if(is.nasd(lwd.bar))lwd.bar<-mlst$lwd.bar; if(is.nasd(lty.bar))lty.bar<-mlst$lty.bar; if(is.nasd(horizontal))horizontal<-mlst$horizontal
  col.bar<-fixCol(isnt.null(col.bar, mlst$col.bar.auxiliar))
  od <- order(x)
  x <- x[od]
  y <- y[od]
  w <- abs(diff(x)/2)
  w <- c(w[1], w, w[length(w)])
  x <- x - w[1:length(x)]
  w <- w + c(w[2:length(w)], 0)
  x[length(w)] <- x[length(x)] + w[length(x)]
  for (i in 1:length(y)) {
    X <- c(x[i],x[i],x[i+1],x[i+1])
    Y <- c(0, y[i], y[i], 0)
    if(horizontal){
      polygon(X,Y, col=col.bar$getCol(alp=alp.bar), lwd=lwd.bar, lty=lty.bar)
    }else{
      polygon(Y,X, col=col.bar$getCol(alp=alp.bar), lwd=lwd.bar, lty=lty.bar)
    }
  }
}

mhist <- function(x,
                  relative   =nasd(),
                  horizontal =nasd(),
                  new.plot   =nasd(),
                  alp.hist   =nasd(),
                  col.hist   =nasd(),
                  lty.hist   =nasd(),
                  lwd.hist   =nasd(),
                  param=NULL, ...)
{
  mlst<- c(list(...), param, Hist("\\get.list"))
  if(is.nasd(relative))relative<-mlst$relative; if(is.nasd(horizontal))horizontal<-mlst$horizontal; if(is.nasd(new.plot))new.plot<-mlst$new.plot; if(is.nasd(alp.hist))alp.hist<-mlst$alp.hist; if(is.nasd(col.hist))col.hist<-mlst$col.hist; if(is.nasd(lty.hist))lty.hist<-mlst$lty.hist; if(is.nasd(lwd.hist))lwd.hist<-mlst$lwd.hist
  col.hist<-fixCol(isnt.null(col.hist, mlst$col.hist.auxiliar))
  ht<-hist(x, plot=F)
  x<-ht$mids
  y<-ht$counts/(if(relative) sum(ht$counts) else 1)
  if(new.plot){
    lst <- list(...)
    xlim<-isnt.null(lst$xlim,
                    isnt.null(Plot("xlim"),
                              IF(horizontal, c(min(x)-(x[2]-x[1])/2, max(x)+(x[2]-x[1])/2), c(0,max(y)))))
    ylim<-isnt.null(lst$ylim,
                    isnt.null(Plot("ylim"),
                              IF(horizontal, c(0,max(y)), c(min(x)-(x[2]-x[1])/2, max(x)+(x[2]-x[1])/2))))
    mplot(param=mlst, plot.lines=F, plot.points=F, xlim=xlim, ylim=ylim)
  }
  mbar(x,y, horizontal=horizontal, col.bar=col.hist, alp.bar=alp.hist, lwd.bar=lwd.hist, lty.bar=lty.hist)
}

mshadow<- function(x1, y1=NULL, x2=NULL, y2=NULL, 
                   alp.border =nasd(),
                   col.border =nasd(),
                   lty.border =nasd(),
                   lwd.border =nasd(),
                   
                   alp.shadow =nasd(),
                   col.shadow =nasd(),
                   
                   alp.mean   =nasd(),
                   col.mean   =nasd(),
                   lwd.mean   =nasd(),
                   lty.mean   =nasd(),
                   
                   plot.mean   =nasd(),
                   plot.shadow =nasd(),
                   
                   param=NULL, ...)
{
  mlst<- c(list(...), param, Shadow("\\get.list"))
  if(is.nasd(alp.border))alp.border<-mlst$alp.border; if(is.nasd(alp.shadow))alp.shadow<-mlst$alp.shadow; if(is.nasd(alp.mean))alp.mean<-mlst$alp.mean; if(is.nasd(col.border))col.border<-mlst$col.border; if(is.nasd(col.shadow))col.shadow<-mlst$col.shadow; if(is.nasd(col.mean))col.mean<-mlst$col.mean; if(is.nasd(lty.border))lty.border<-mlst$lty.border; if(is.nasd(lty.mean))lty.mean<-mlst$lty.mean; if(is.nasd(lwd.border))lwd.border<-mlst$lwd.border; if(is.nasd(lwd.mean))lwd.mean<-mlst$lwd.mean; if(is.nasd(plot.shadow))plot.shadow<-mlst$plot.shadow; if(is.nasd(plot.mean))plot.mean<-mlst$plot.mean
  col.border<-fixCol(isnt.null(col.border, mlst$col.border.auxiliar))
  col.shadow<-fixCol(isnt.null(col.shadow, mlst$col.shadow.auxiliar))
  col.mean<-fixCol(isnt.null(col.mean, mlst$col.mean.auxiliar))
  table.exist<-FALSE
  if(base::is.null(x2)){
    table<-x1
    x1<-table$z
    x2<-x1
    y1<-table$min
    y2<-table$max
    table.exist<-TRUE
  }
  if(is.null(col.mean)) col.mean<-color()
  if(is.null(col.border)) col.border<-color()
  if(is.null(col.shadow)) col.shadow<-color()
  x<-c(x1,x2[length(x2):1])
  y<-c(y1,y2[length(y2):1])
  if(plot.shadow){
    polygon(x,y, col=col.shadow$getCol(alp.shadow), border= "#00000000")
    mlines(x,y, lty.lines=lty.border, lwd.lines=lwd.border, col.lines=col.border, alp.lines=alp.border)
  }
  if(plot.mean & table.exist)
    mlines(x1, table$mean, lty.lines=lty.mean, lwd.lines=lwd.mean, col.lines=col.mean, alp.lines=alp.mean)
}

mlegend <- function(legend="",
                    pos.legend=nasd(),
                    cex.legend=nasd(),
                    col.legend=nasd(),
                    alp.legend=nasd(),
                    col.lin   =nasd(),
                    alp.lin   =nasd(),
                    lwd.lin   =nasd(),
                    lty.lin   =nasd(),
                    col.box   =nasd(),
                    alp.box   =nasd(),
                    lwd.box   =nasd(),
                    lty.box   =nasd(),
                    col.back  =nasd(),
                    alp.back  =nasd(),
                    param=NULL, ...)
{
  mlst<- c(list(...), param, Legend("\\get.list"))
  if(is.nasd(pos.legend))pos.legend<-mlst$pos.legend; if(is.nasd(cex.legend))cex.legend<-mlst$cex.legend; if(is.nasd(col.legend))col.legend<-mlst$col.legend; if(is.nasd(alp.legend))alp.legend<-mlst$alp.legend; if(is.nasd(col.lin))col.lin<-mlst$col.lin; if(is.nasd(alp.lin))alp.lin<-mlst$alp.lin; if(is.nasd(lwd.lin))lwd.lin<-mlst$lwd.lin; if(is.nasd(lty.lin))lty.lin<-mlst$lty.lin; if(is.nasd(col.box))col.box<-mlst$col.box; if(is.nasd(alp.box))alp.box<-mlst$alp.box; if(is.nasd(lwd.box))lwd.box<-mlst$lwd.box; if(is.nasd(lty.box))lty.box<-mlst$lty.box; if(is.nasd(col.back))col.back<-mlst$col.back; if(is.nasd(alp.back))alp.back<-mlst$alp.back
  col.legend<-fixCol(isnt.null(col.legend, mlst$col.legend.auxiliar))
  col.lin<-fixCol(isnt.null(col.lin, mlst$col.lin.auxiliar))
  col.box<-fixCol(isnt.null(col.box, mlst$col.box.auxiliar))
  col.back<-fixCol(isnt.null(col.back, mlst$col.back.auxiliar))
  if(is.character(pos.legend)){
    x<-pos.legend
    y<-NULL
  }else{
    x<-pos.legend[1]
    y<-pos.legend[2]
  }
  colNA<-"#00000000"
  graphics::legend(x, y, legend=legend, lty=lty.lin, lwd=lwd.lin,cex=cex.legend, box.lty=lty.box, bg=col.back$getCol(alp=alp.back), box.lwd=lwd.box, text.col=col.legend$getCol(alp=alp.legend), box.col=colNA, col=colNA)
  n<-max(lwd.box,lwd.lin)
  if(n>0){
    lwd<-lwd.box
    lwd.box<-rep(0, n)
    if(lwd>0) lwd.box[1:lwd]<-1+lwd-(1:lwd)
    col<-col.box
    col.box<-rep(colNA, n)
    if(lwd>0){
      sq<-seq(0, 1, 1.0/lwd)[-1]
      for (i in 1:lwd) {
        col.box[i]<-col$getCol(bri=sq[i], alp=alp.box,this = T)
      }
      NO<-col$getCol()
    }
    
    lwd.lin.mat<-NULL
    col.lin.mat<-NULL
    lwd.iter<-iterVector(lwd.lin, cyclic = T)
    col.iter<-iterCol(col.lin, cyclic = T)
    for (i in 1:length(legend)) {
      lwd<-lwd.iter$Next()
      lwd.lin<-rep(0, n)
      if(lwd>0) lwd.lin[1:lwd]<-2*(lwd-(1:lwd))+1
      col<-col.iter$Next()
      col.lin<-rep(colNA, n)
      if(lwd>0){
        sq<-seq(0, 1, 1.0/lwd)[-1]
        for (i in 1:lwd) {
          col.lin[i]<-col$getCol(bri=sq[i],alp=alp.lin, this=T)
        }
        NO<-col$getCol()
      }
      
      lwd.lin.mat<-cbind(lwd.lin.mat, lwd.lin)
      col.lin.mat<-cbind(col.lin.mat, col.lin)
    }
    for (i in 1:n) {
      graphics::legend(x, y, legend=legend, lty=lty.lin, cex=cex.legend,
                       box.lty=lty.box, bg=colNA, text.col=colNA,
                       lwd=lwd.lin.mat[i,], col=col.lin.mat[i,],
                       box.lwd=lwd.box[i], box.col=col.box[i])
    }
  }
}

substrae.LIST.DAT <-function(..., CONDITION, TRANSFORM){
  lst<-list(...)
  dat<-newList()
  ind<-NULL
  for (i in 1:length(lst)) {
    if(CONDITION(lst[[i]])){
      dat$add(TRANSFORM(lst[[i]]))
      ind<-c(ind, i)
    }
  }
  lst[ind]<-NULL
  return(list(lista.123=lst, data.123=dat))
}

linesGroup <- function(..., plot.lines=F, plot.points=F, plot.shadow=F, plot.mean=F, col=NULL){
  lst<-list(...)
  if(is.null(lst$lista.123)){
    ret<-substrae.LIST.DAT(...,
                           CONDITION=is.group.class,
                           TRANSFORM=function(x) x)
    lst<-ret$lista.123
    dat<-ret$data.123
  }else{
    dat<-lst$data.123
    lst<-lst$lista.123
  }
  ##Shadow
  plot.shadow<-iterVector(plot.shadow, cyclic=T)
  iter<-dat$iter()
  iter.col<-iterCol(col, cyclic=T, defVal=nasd())
  iter.col.border<-iterCol(lst$col.border, cyclic=T, defVal=nasd())
  iter.alp.border<-iterVector(lst$alp.border, cyclic=T, defVal=nasd())
  iter.lwd.border<-iterVector(lst$lwd.border, cyclic=T, defVal=nasd())
  iter.lty.border<-iterVector(lst$lty.border, cyclic=T, defVal=nasd())
  iter.col.shadow<-iterCol(lst$col.shadow, cyclic=T, defVal=nasd())
  iter.alp.shadow<-iterVector(lst$alp.shadow, cyclic=T, defVal=nasd())
  while (iter$HasNext()) {
    grupo<-iter$Next()
    col<-iter.col$Next()
    col.border<-iter.col.border$Next()
    alp.border<-iter.alp.border$Next()
    lwd.border<-iter.lwd.border$Next()
    lty.border<-iter.lty.border$Next()
    col.shadow<-iter.col.shadow$Next()
    alp.shadow<-iter.alp.shadow$Next()
    if(plot.shadow$Next()){
      mshadow(grupo$table,
              alp.border=alp.border,lwd.border=lwd.border,
              lty.border=lty.border, col.border=col.border,
              alp.shadow=alp.shadow, col.shadow=col.shadow,
              col.border.auxiliar=isnt.nasd(col, grupo$col$copy()),
              col.shadow.auxiliar=isnt.nasd(col, grupo$col$copy()),
              plot.shadow=T, plot.mean=F)
    }
  }
  ##lines
  plot.lines<-iterVector(plot.lines, cyclic=T)
  iter$Restart()
  iter.col$Restart()
  iter.col.lines<-iterCol(lst$col.lines, cyclic=T, defVal=nasd())
  iter.alp.lines<-iterVector(lst$alp.lines, cyclic=T, defVal=nasd())
  iter.lwd.lines<-iterVector(lst$lwd.lines, cyclic=T, defVal=nasd())
  iter.lty.lines<-iterVector(lst$lty.lines, cyclic=T, defVal=nasd())
  while (iter$HasNext()) {
    grupo<-iter$Next()
    col<-iter.col$Next()
    col.lines<-iter.col.lines$Next()
    alp.lines<-iter.alp.lines$Next()
    lwd.lines<-iter.lwd.lines$Next()
    lty.lines<-iter.lty.lines$Next()
    if(plot.lines$Next())
      mlines(grupo$X, grupo$Y,
             alp.lines=alp.lines,lwd.lines=lwd.lines,
             lty.lines=lty.lines, col.lines=col.lines,
             col.lines.auxiliar=isnt.nasd(col, grupo$col$copy()))
  }
  ##points
  plot.points<-iterVector(plot.points, cyclic=T)
  iter$Restart()
  iter.col$Restart()
  iter.col.points<-iterCol(lst$col.points, cyclic=T, defVal=nasd())
  iter.alp.points<-iterVector(lst$alp.points, cyclic=T, defVal=nasd())
  iter.lwd.points<-iterVector(lst$lwd.points, cyclic=T, defVal=nasd())
  while (iter$HasNext()) {
    grupo<-iter$Next()
    col<-iter.col$Next()
    col.points<-iter.col.points$Next()
    alp.points<-iter.alp.points$Next()
    lwd.points<-iter.lwd.points$Next()
    if(plot.points$Next())
      mpoints(grupo$X, grupo$Y, alp.points=alp.points,
              lwd.points=lwd.points, col.points=col.points,
              col.points.auxiliar=isnt.nasd(col, grupo$col$copy()))
  }
  ##mean
  plot.mean<-iterVector(plot.mean, cyclic=T)
  iter$Restart()
  iter.col$Restart()
  iter.col.mean<-iterCol(lst$col.mean, cyclic=T, defVal=nasd())
  iter.alp.mean<-iterVector(lst$alp.mean, cyclic=T, defVal=nasd())
  iter.lwd.mean<-iterVector(lst$lwd.mean, cyclic=T, defVal=nasd())
  iter.lty.mean<-iterVector(lst$lty.mean, cyclic=T, defVal=nasd())
  while (iter$HasNext()) {
    grupo<-iter$Next()
    col<-iter.col$Next()
    col.mean<-iter.col.mean$Next()
    alp.mean<-iter.alp.mean$Next()
    lwd.mean<-iter.lwd.mean$Next()
    lty.mean<-iter.lty.mean$Next()
    if(plot.mean$Next()){
      mshadow(grupo$table,
              alp.mean=alp.mean,lwd.mean=lwd.mean,
              lty.mean=lty.mean, col.mean=col.mean,
              col.mean.auxiliar=isnt.nasd(col, grupo$col$copy()),
              plot.mean=T, plot.shadow=F)
    }
  }
}

plotGroup<-function(..., new.plot=T, plot.lines=F, plot.points=F, plot.shadow=F, plot.mean=F, col=NULL) {
  if(!new.plot){
    linesGroup(..., plot.lines=plot.lines, plot.points=plot.points, 
               plot.shadow=plot.shadow, plot.mean=plot.mean, col=col)
  }else{
    lst<-list(...)
    if(is.null(lst$lista.123)){
      ret<-substrae.LIST.DAT(...,
                             CONDITION=is.group.class,
                             TRANSFORM=function(x) x)
      lst<-ret$lista.123
      dat<-ret$data.123
    }else{
      dat<-lst$data.123
      lst<-lst$lista.123
    }
    if(base::is.null(lst$xlim) | base::is.null(lst$ylim)){
      xmin<-Inf
      xmax<--Inf
      ymin<-Inf
      ymax<--Inf
      iter<-dat$iter()
      while (iter$HasNext()) {
        grupo<-iter$Next()
        x<-grupo$X
        y<-grupo$Y
        for (i in 1:length(x)) {
          xmin<-min(xmin, x[[i]], na.rm = T)
          xmax<-max(xmax, x[[i]], na.rm = T)
          ymin<-min(ymin, y[[i]], na.rm = T)
          ymax<-max(ymax, y[[i]], na.rm = T)
        }
      }
    }
    xlim <- isnt.null(lst$xlim, 
                      IF(xmin==xmax, c(xmin-1,xmin+1), c(xmin,xmax)))
    ylim <- isnt.null(lst$ylim,
                      IF(ymin==ymax, c(ymin-1,ymin+1), c(ymin,ymax)))
    mplot(xlim=xlim, ylim=ylim, plot.lines=F, plot.points=F, param=lst)
    linesGroup(lista.123=lst, data.123=dat,
               plot.lines=plot.lines, plot.points=plot.points, plot.shadow=plot.shadow, plot.mean=plot.mean, col=col)
  }
}

printer <- function(prop=3/4, width=9, height=NULL, size=c(1,1)){
  ACTIVE <- newVec(T)
  PROP <- newVec(prop)
  WIDTH <- newVec(width)
  HEIGTH <- newVec(if(is.numeric(height)) height else NA)
  SIZE <- newVec(size)
  
  PDF <- function(file=NULL, prop=NULL, width=NULL, height=NULL, size=NULL){
    if(ACTIVE$get()){
      if(base::is.null(file)){
        i <- 1
        while(file.exists(paste("output.plot_",i,".pdf",sep=""))){
          i <- i+1
        }
        file<-paste("output.plot_",i,".pdf",sep="")
      }
      prop <- if(is.null(prop)) PROP$get() else prop
      width <- if(is.null(width)) WIDTH$get() else width
      height <- if(is.null(height)) if(is.na(HEIGTH$get())) prop*width else HEIGTH$get() else height
      size <- if(is.null(size)) SIZE$get() else size
      pdf(file = file, width=width*size[2], height=height*size[1])
    }
  }
  OFF <- function(){
    if(ACTIVE$get()) dev.off()
  }
  
  setActive <- function(value) ACTIVE$set(value)
  setProp <- function(prop) PROP$set(prop)
  setWidth <- function(width) WIDTH$set(width)
  setHeight <- function(height) HEIGTH$set(if(is.numeric(height)) height else NA)
  setSize <- function(size) SIZE$set(size)
  return(list(PDF=PDF, OFF=OFF, setActive=setActive, setProp=setProp, setWidth=setWidth, setHeight=setHeight, setSize=setSize))
}
