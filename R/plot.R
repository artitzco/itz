#' @export
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

#' @export
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

#' @export
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

#' @export
Plot <- function(name, ...) {
  newGlobalParam(
    "x00000s84o5c8Px2ImUj0GxXsQzq0rsUrq5oL2deyXPrw1lPxGw"
    ,function(add){
      add("alp.backlines" , 0.5)
      add("alp.background", 1)
      add("alp.border"    , 0)
      add("cex.axis"      , 1.1)
      add("cex.lab"       , 1.3)
      add("cex.main"      , 1.3)
      add("col.background", "#E0E0E0")
      add("col.backlines" , "white")
      add("col.border"    , "black")
      add("labX"          , NA)
      add("labY"          , NA)
      add("lty.backlines" , 1)
      add("lwd.backlines" , 1)
      add("lx"            , NA)
      add("ly"            , NA)
      add("plot.lines"    , TRUE)
      add("plot.points"   , FALSE)
      add("px"            , 6)
      add("py"            , 6)
      add("xdigits"       , NA)
      add("ydigits"       , NA)
      add("xaxis"         , T)
      add("yaxis"         , T)
      add("xaxis.pos"     , T)
      add("yaxis.pos"     , T)
    }
  ) (name, ...)
}

#' @export
Bar <- function(name, ...) {
  newGlobalParam(
    "x000000bPQwasoSlZDX0Ka3Ef89h0d8s89g90hVqdwGw37B1tGu"
    ,function(add){
      add("alp.bar", 1)
      add("col.bar", "#A6A6A6")
      add("lwd.bar", 2)
      add("lty.bar", 1)
      add("horizontal", T)
    }
  ) (name, ...)
}

#' @export
Hist <- function(name, ...) {
  newGlobalParam(
    "x00000000mF3zti9sP0msATjc7UEkz5BNIAQ4rr2fVGsy3NHzBX"
    ,function(add){
      add("relative"  , T)
      add("horizontal", T)
      add("new.plot" , T)
      add("alp.hist"  , 0.7)
      add("col.hist"  , NULL)
      add("lty.hist"  , 1)
      add("lwd.hist"  , 2)
    }
  ) (name, ...)
}

#' @export
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
      add("lwd.border" , 0)
      add("lwd.mean"   , 1)
      add("plot.shadow", T)
      add("plot.mean"  , T)
    }
  ) (name, ...)
}

#' @export
Legend <- function(name, ...) {
  newGlobalParam(
    "x0000000wTaxROM6a5g511AuPnDtqxelpLNHv4GYcw43tdsAder"
    ,function(add){
      add("cex.legend", 1.3)
      add("col.legend", "#000000")
      add("alp.legend", 1)
      add("lin"       , T)
      add("col.lin"   , NULL)
      add("alp.lin"   , 1)
      add("lwd.lin"   , 1)
      add("lty.lin"   , 1)
      add("col.box"   , "#BEBEBE")
      add("alp.box"   , 1)
      add("lwd.box"   , 1)
      add("lty.box"   , 1)
      add("col.back"  , "#E6E6E6")
      add("alp.back"  , 0.8)
    }
  ) (name, ...)
}

#' @title mlines
#' @description This is a modification to the \code{graphics::lines} function.
#' @param x \code{x}-coordinate.
#' @param y \code{y}-coordinate.
#' @param col.lines lines color.
#' @param alp.lines lines color transparency.
#' @param lty.lines lines type.
#' @param lwd.lines lines width.
#' @details \code{x} and \code{y} could be vectors or lists.
#' @seealso \code{itz::Lines}, \code{itz::mplot} & \code{itz::color}.
#' @examples
#' x<--10:10
#' y0<-x^2
#' y1<-2*x+20
#' 
#' mplot(xlim=c(-10,10), ylim=c(0,100))
#' 
#' mlines(x, y0)
#' 
#' mlines(x, list(y0, y1), col.lines="#AA33AA", lty.lines=2)
#' 
#' mlines(x, list(y0, y1), col.lines=c("red","blue"),lwd.lines=3)
#' 
#' mlines(x, y0+30, col=color(0.3,0.7,0.5), lty=2, lwd=3)
#' 
#' mlines(x, list(y0, y1), col=c("red","blue"), lwd=5)
#' @export
mlines <- function(x=NULL, y=NULL,
                   col.lines=nasd(),
                   alp.lines=nasd(),
                   lty.lines=nasd(),
                   lwd.lines=nasd(),
                   ...)
{
  lst<-list(...); param<-lst$param; lst$param<-NULL
  mlst<- c(lst, param, Lines("\\get.list"))
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

#' @title mpoints
#' @description This is a modification to the \code{graphics::points} function.
#' @param x \code{x}-coordinate.
#' @param y \code{y}-coordinate.
#' @param col.points points color.
#' @param alp.points points color transparency.
#' @param lwd.points points width.
#' @details \code{x} and \code{y} could be vectors or lists.
#' @seealso \code{itz::Points}, \code{itz::mplot} & \code{itz::color}.
#' @examples
#' x<--10:10
#' y0<-x^2
#' y1<-2*x+20
#' 
#' mplot(xlim=c(-10,10), ylim=c(0,100))
#' 
#' mpoints(x, y0)
#' 
#' mpoints(x, list(y0, y1), col.points="#AA33AA")
#' 
#' mpoints(x, list(y0, y1), col.points=c("red","blue"),lwd.points=3)
#' 
#' mpoints(x, y0+30, col=color(0.3,0.7,0.5), lwd=3)
#' 
#' mpoints(x, list(y0, y1), col=c("red","blue"), lwd=5)
#' @export
mpoints <- function(x=NULL, y=NULL,
                    alp.points=nasd(),
                    col.points=nasd(),
                    lwd.points=nasd(),
                    ...)
{
  lst<-list(...); param<-lst$param; lst$param<-NULL
  mlst<- c(lst, param, Points("\\get.list"))
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
  MPOINTS<-function(x, y, alp.points, col.points, lwd.points) {
    if(lwd.points>=1) {
      sq <- seq(0, 1, 1.0/lwd.points)[-1]
      for (i in 1:lwd.points)
        points(x, y, col=col.points$getCol(bri=sq[i], alp=alp.points, this=T), lwd=2*(lwd.points-i)+1)
      NO<-col.points$getCol()
    }
  }
  
  if(!is.list(x) & !is.list(y)) {
    MPOINTS(x, y, alp.points, col.points, lwd.points)
  }else {
    if(is.list(x) & is.list(y)) { 
      if(length(x)==1) {
        for (i in 1:length(y))
          MPOINTS(x[[1]], y[[i]], alp.points, col.points, lwd.points)
      }else if(length(y)==1) {
        for (i in 1:length(x))
          MPOINTS(x[[i]], y[[1]], alp.points, col.points, lwd.points)
      }else {
        for (i in 1:length(y))
          MPOINTS(x[[i]], y[[i]], alp.points, col.points, lwd.points)
      }
    }else if(!is.list(x) & is.list(y)) {
      for (i in 1:length(y))
        MPOINTS(x, y[[i]], alp.points, col.points, lwd.points)
    }else if(is.list(x) & !is.list(y)) {
      for (i in 1:length(x))
        MPOINTS(x[[i]], y, alp.points, col.points, lwd.points)
    }
  }
}

#' @title mabline
#' @description This is a modification to the \code{graphics::abline} function.
#' @param h the \code{y}-values for horizontal lines.
#' @param v the \code{x}-values for vertical lines.
#' @param col.lines lines color.
#' @param alp.lines lines color transparency.
#' @param lty.lines lines type.
#' @param lwd.lines lines width.
#' @param sld.lines lines density.
#' @seealso \code{itz::Abline}, \code{itz::mplot} & \code{itz::color}.
#' @examples
#' mplot()
#' mabline(h=0, v=c(-.5,.5), col.abline="red")
#' 
#' mplot()
#' mabline(h=(-1):1, v=c(-.5,.5), col.abline=c("blue","red"), lwd.abline=5)
#' 
#' mplot()
#' mabline(h=.5, col.abline=color(1,0,1), lwd.abline=6)
#' mabline(h=-.5, col.abline=color(1,0,1), lwd.abline=6, sld=F)
#' 
#' sq<- seq(-1,1,.1)
#' mplot()
#' mabline(h=sq, v=sq, col.abline=color(1:0,0:1,1))
#' 
#' mplot()
#' mabline(h=(-1):1, v=c(-.5,.5), col.abline=c("blue","red"), lwd.abline=5)
#' 
#' mplot()
#' mabline(h=.5, col.abline=color(1,0,1), lwd.abline=6)
#' mabline(h=-.5, col.abline=color(1,0,1), lwd.abline=6, sld=F)
#' 
#' sq<- seq(-1,1,.1)
#' mplot()
#' mabline(h=sq, v=sq, col.abline=color(1:0,0:1,1), lty.abline=3)
#' @export
mabline <- function(h=NULL, v=NULL,
                    col.abline =nasd(),
                    alp.abline =nasd(),
                    lty.abline =nasd(),
                    lwd.abline =nasd(),
                    sld.abline =nasd(),
                    ...)
{
  lst<-list(...); param<-lst$param; lst$param<-NULL
  mlst<- c(lst, param, Abline("\\get.list"))
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
  if(!is.null(h)) mab(function(x, ...) abline(h=x, ...), h)
  if(!is.null(v)) mab(function(x, ...) abline(v=x, ...), v)
}


pol <- function(xlim, ylim, px, py,lx=NA,ly=NA,labX=NA,labY=NA, cex=1, background=color(0.086,0.086,0.086), col.backlines=color(.5,.5,.5), lwd=2, lty=1, alp=1, xdigits=NA, ydigits=NA, alp.background=alp.background, xaxis=T, yaxis=T, xaxis.pos=T, yaxis.pos=T, col.border="black", alp.border= 0){
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
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col=background$getCol(alp=alp.background),
       border=NA)
  mabline(v=lx, h=ly, col.abline=col.backlines, lwd.abline=lwd, lty.abline=lty, alp.abline=alp)
  if(xaxis) axis(if(xaxis.pos) 1 else 3,at=lx,labels=labX,cex.axis=cex)
  if(yaxis) axis(if(yaxis.pos) 2 else 4,at=ly,labels=labY,cex.axis=cex)
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col=NA,
       border=col.border$getCol(alp=alp.border))
  globalVar("xlim.0000000k1DTt5P2hOzSqOZFzG04ip6a3xfdFA9DrDp8Ffilq", xlim)
  globalVar("ylim.0000000k1DTt5P2hOzSqOZFzG04ip6a3xfdFA9DrDp8Ffilq", ylim)
}

#' @title mplot
#' @description This is a modification to the \code{graphics::plot} function.
#' @param x \code{x}-coordinate.
#' @param y \code{y}-coordinate.
#' @param xlim limits for \code{x}-axis.
#' @param ylim limits for \code{x}-axis.
#' @param col color for \code{itz::mlines} and \code{itz::mpoints}.
#' @param main main title.
#' @param xlab \code{x}-axis legend.
#' @param ylab \code{y}-axis legend.
#' @param alp.background background color transparency.
#' @param alp.backlines backlines color transparency.
#' @param alp.border border color transparency.
#' @param cex.main scale for main title.
#' @param cex.lab scale for \code{x}-axis legend.
#' @param cex.axis scale for \code{y}-axis legend.
#' @param col.background background color.
#' @param col.backlines backlines color.
#' @param col.border border color.
#' @param labX legends of \code{x}-axis backlines.
#' @param labY legends of \code{y}-axis backlines.
#' @param lty.backlines backlines type.
#' @param lwd.backlines backlines width.
#' @param lx values of \code{x}-axis backlines.
#' @param ly values of \code{y}-axis backlines.
#' @param plot.lines to plot lines graphic.
#' @param plot.points to plot points graphic.
#' @param px number of \code{x}-axis backlines.
#' @param py number of \code{y}-axis backlines.
#' @param xdigits \code{x}-axis rounding digits.
#' @param ydigits \code{y}-axis rounding digits.
#' @param xaxis to show the \code{x}-axis.
#' @param yaxis to show the \code{y}-axis.
#' @param xaxis.pos to show the \code{x}-axis on bottom side.
#' @param yaxis.pos to show the \code{y}-axis on left side.
#' @param ... supports \code{mlines} and \code{mpoints} parameters
#' @seealso \code{itz::Plot}, \code{itz::mlines}, \code{itz::mpoints} & \code{itz::color}.
#' @examples
#' mplot()
#' 
#' mplot(0:1)
#' 
#' sq<-seq(0, 4*pi, 0.6)
#' mplot(sq, sin(sq), plot.points=T, ydigits=2, col="magenta", col.points="cyan")
#' 
#' t<-seq(0, 1, 0.05)
#' mplot(t, l(sqrt(t),t, t^2), plot.points=T, plot.lines=F, col=c("red","blue","green"), lwd.points=2)
#' 
#' mplot(rnorm(100), px=3, py=4, ylim=c(-2.5,2.5), ydigits=0, xdigits=0)
#' 
#' mplot(runif(100),rnorm(100), px=2, py=2, xaxis=F, yaxis=F, col.background=color(.21,.05,.2), col.backlines="#C9F2CC",xlab="runif", ylab="rnorm", main="title", lwd.backlines=4)
#' @export
mplot <- function(x              =NULL, 
                  y              =NULL, 
                  xlim           =NULL, 
                  ylim           =NULL, 
                  col            =NULL,
                  main           =NA,
                  xlab           =NA,
                  ylab           =NA,
                  alp.background =nasd(),
                  alp.backlines  =nasd(),
                  alp.border     =nasd(),
                  cex.axis       =nasd(),
                  cex.lab        =nasd(),
                  cex.main       =nasd(),
                  col.background =nasd(),
                  col.backlines  =nasd(),
                  col.border     =nasd(),
                  labX           =nasd(),
                  labY           =nasd(),
                  lty.backlines  =nasd(),
                  lwd.backlines  =nasd(),
                  lx             =nasd(),
                  ly             =nasd(),
                  plot.lines     =nasd(),
                  plot.points    =nasd(),
                  px             =nasd(),
                  py             =nasd(),
                  xdigits        =nasd(),
                  ydigits        =nasd(),
                  xaxis          =nasd(),
                  yaxis          =nasd(),
                  xaxis.pos      =nasd(),
                  yaxis.pos      =nasd(),
                  ...)
{
  lst<-list(...); param<-lst$param; lst$param<-NULL
  mlst<- c(lst, param, Plot("\\get.list"))
  if(is.nasd(alp.backlines))alp.backlines<-mlst$alp.backlines; if(is.nasd(alp.background))alp.background<-mlst$alp.background; if(is.nasd(alp.border))alp.border<-mlst$alp.border; if(is.nasd(cex.axis))cex.axis<-mlst$cex.axis; if(is.nasd(cex.lab))cex.lab<-mlst$cex.lab; if(is.nasd(cex.main))cex.main<-mlst$cex.main; if(is.nasd(col.background))col.background<-mlst$col.background; if(is.nasd(col.backlines))col.backlines<-mlst$col.backlines; if(is.nasd(col.border))col.border<-mlst$col.border; if(is.nasd(labX))labX<-mlst$labX; if(is.nasd(labY))labY<-mlst$labY; if(is.nasd(lty.backlines))lty.backlines<-mlst$lty.backlines; if(is.nasd(lwd.backlines))lwd.backlines<-mlst$lwd.backlines; if(is.nasd(lx))lx<-mlst$lx; if(is.nasd(ly))ly<-mlst$ly; if(is.nasd(plot.lines))plot.lines<-mlst$plot.lines; if(is.nasd(plot.points))plot.points<-mlst$plot.points; if(is.nasd(px))px<-mlst$px; if(is.nasd(py))py<-mlst$py; if(is.nasd(xdigits))xdigits<-mlst$xdigits; if(is.nasd(ydigits))ydigits<-mlst$ydigits; if(is.nasd(xaxis))xaxis<-mlst$xaxis; if(is.nasd(yaxis))yaxis<-mlst$yaxis; if(is.nasd(xaxis.pos))xaxis.pos<-mlst$xaxis.pos; if(is.nasd(yaxis.pos))yaxis.pos<-mlst$yaxis.pos
  col.background<-fixCol(isnt.null(col.background, mlst$col.background.auxiliar))
  col.backlines<-fixCol(isnt.null(col.backlines, mlst$col.backlines.auxiliar))
  col.border<-fixCol(isnt.null(col.border, mlst$col.border.auxiliar))
  xlab<- isnt.na(xlab,isnt.null(mlst$xlab,NA))
  ylab<- isnt.na(ylab,isnt.null(mlst$ylab,NA))
  main<- isnt.na(main,isnt.null(mlst$main,NA))
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
  pol(xlim, ylim, px, py, lx, ly, labX,labY, cex.axis, col.background, col.backlines, lwd.backlines, lty.backlines, alp.backlines,xdigits,ydigits,alp.background, xaxis, yaxis, xaxis.pos, yaxis.pos, col.border, alp.border)
  col<-fixCol(col)
  if(plot.lines) {
    mlst.lines  <-Lines("\\get.list")
    alp.lines <- isnt.null(mlst$alp.lines, mlst.lines$alp.lines)
    lwd.lines <- isnt.null(mlst$lwd.lines, mlst.lines$lwd.lines)
    lty.lines <- isnt.null(mlst$lty.lines, mlst.lines$lty.lines)
    col.lines <- isnt.null(mlst$col.lines, mlst.lines$col.lines)
    mlines(x, y, alp.lines=alp.lines, col.lines.auxiliar=col, col.lines=col.lines, lty.lines=lty.lines, lwd.lines=lwd.lines)
  }
  
  if(plot.points){
    mlst.points <-Points("\\get.list")
    alp.points <- isnt.null(mlst$alp.points, mlst.points$alp.points)
    lwd.points <- isnt.null(mlst$lwd.points, mlst.points$lwd.points)
    col.points <- isnt.null(mlst$col.points, mlst.points$col.points)
    mpoints(x, y, alp.points=alp.points, col.points.auxiliar=col, col.points=col.points, lwd.points=lwd.points)
  }
}

#' @title mbar
#' @description This generates a bar plot.
#' @param x \code{x}-coordinate.
#' @param y bar values.
#' @param col.bar bar color.
#' @param alp.bar bar color transparency.
#' @param lty.bar bar type.
#' @param lwd.bar bar width.
#' @param horizontal horizontal bar.
#' @seealso \code{itz::Bar} \code{itz::mplot}, & \code{itz::color}.
#' @examples
#' x<--10:10 
#' y0<-x^2
#' y1<-2*x+20
#' 
#' mplot(xlim=c(-10,10), ylim=c(0,100))
#' 
#' mbar(x, y0)
#' 
#' mbar(x, y0, col.bar="#AA33AA", lty.bar=2)
#' 
#' mbar(x, y0, col.bar=c("red","blue"),lwd.bar=3)
#' mbar(x, y1, col.bar=c("blue","red"),lwd.bar=3)
#' 
#' mbar(x, y0+30, col=color(0.3,0.7,0.5), lty=2, lwd=3)
#' mbar(x, y0, col=c("red","blue"), lwd=5)
#' mbar(x, y1, col=c("blue","red"), lwd=5)
#' @export
mbar <- function(x, y,#####Añadir plot
                 col.bar =nasd(),
                 alp.bar =nasd(),
                 lwd.bar =nasd(),
                 lty.bar =nasd(),
                 horizontal=nasd(),
                 ...)
{
  lst<-list(...); param<-lst$param; lst$param<-NULL
  mlst<- c(lst, param, Bar("\\get.list"))
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

#' @title mhist
#' @description This is a modification to the \code{graphics::hist} function.
#' @param x \code{x}-coordinate.
#' @param relative relative values.
#' @param horizontal horizontal histogram.
#' @param new.plot to generate a new plot.
#' @param col.hist histogram color.
#' @param alp.hist histogram color transparency.
#' @param lty.hist histogram type.
#' @param lwd.hist histogram width.
#' @param ... supports \code{mplot} parameters.
#' @seealso \code{itz::Hist}, \code{itz::mplot} & \code{itz::color}.
#' @examples
#' set.seed(1993)
#' x<-rnorm(100)
#' 
#' mhist(x)
#' 
#' par(mfrow=c(1,2))
#' mhist(x, ydigits=0, lty.hist=2)
#' mhist(x, relative=F, lty.hist=3, ydigits=0)
#' par(mfrow=c(1,1))
#' 
#' col=color.rainbow(11, abs=T)
#' mhist(x, ydigits=0, col.hist=col, lwd.hist=3)
#' 
#' set.seed(1990)
#' y<-rnorm(100, 2)
#' colx<-"#FF4D33"
#' coly<-"#334DFF"
#' mhist(x, relative=F, col.hist=colx, alp.hist=0.5, xlim=c(-2.5,4.5), ydigits=0)
#' mhist(y, relative=F, col.hist=coly, alp.hist=0.5, new.plot=F)
#' 
#' mplot((1:100)/100, l(x,y), col=c(colx, coly), ylim=c(-2.5,4.5), ydigits=1, xaxis=F, lwd.lines=2)
#' mhist(x, horizontal=F, col.hist=colx, lwd.hist=2, alp.hist=0.4, new.plot=F)
#' mhist(y, horizontal=F, col.hist=coly, lwd.hist=2, alp.hist=0.4, new.plot=F)
#' @export
mhist <- function(x,
                  relative   =nasd(),
                  horizontal =nasd(),
                  new.plot   =nasd(),
                  alp.hist   =nasd(),
                  col.hist   =nasd(),
                  lty.hist   =nasd(),
                  lwd.hist   =nasd(),
                  ...)
{
  lst<-list(...); param<-lst$param; lst$param<-NULL
  mlst<- c(lst, param, Hist("\\get.list"))
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

#' @title mshadow
#' @description Shadow plot between two curves \code{(x1, y1)} and \code{(x2, y2)}, and the mean curve.
#' @param x1 \code{x1}-coordinate or table with values.
#' @param y1 \code{y1}-coordinate.
#' @param x2 \code{x2}-coordinate.
#' @param y2 \code{y2}-coordinate.
#' @param col.border border color.
#' @param alp.border border color transparency.
#' @param lty.border border type.
#' @param lwd.border border width.
#' @param col.shadow shadow color.
#' @param alp.shadow shadow color transparency.
#' @param col.mean mean color.
#' @param alp.mean mean color transparency.
#' @param lty.mean mean type.
#' @param lwd.mean mean width.
#' @param plot.mean to plot mean.
#' @param plot.shadow to plot shadow.
#' @seealso \code{itz::Shadow} ,\code{itz::mplot} & \code{itz::color}.
#' @examples
#' x<--10:10
#' y1<-x^2
#' y2<-2*x+20
#'
#' mplot(xlim=c(-10,10), ylim=c(0,100))
#' mshadow(x,y1,x,y2, col.shadow="#AA33AA", lty.border=3,lwd.border=1, col.border="white")
#'
#' x<-seq(0,1.3,0.01)
#' mplot(xlim=c(0,1.4), ylim=c(0,3))
#' mshadow(x, x^4, x, sqrt(x), col.shadow="red")
#' mshadow(x, 0.2+x^2, x, 0.2+sqrt(sqrt(x)), col.shadow="blue")
#'
#' x1<-seq(0,2*pi,.1)
#' y1<-(2+sin(x1))*x1
#' x2<-x1+1
#' y2<-20+(-2+cos(x2))*x2
#' mplot(xlim=c(min(x1,x2), max(x1,x2)), ylim=c(min(y1,y2), max(y1,y2)))
#' mshadow(x1,y1,x2,y2, col.border = "white", lwd.border=1, col.shadow="#991A99")
#' mlines(l(x1,x2),l(y1,y2), col=c("#FF1A33","#331AFF"))
#'
#' z<-seq(0,1,0.01)
#' min<-sqrt(z)
#' max<-z^2
#' mean<-(min+max)/2
#' tab<-data.frame(z, min, max, mean)
#' mplot(xlim=c(0,1), ylim=c(0,1))
#' mshadow(tab, lwd.border=T)
#' @export
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
                   
                   ...)
{
  lst<-list(...); param<-lst$param; lst$param<-NULL
  mlst<- c(lst, param, Shadow("\\get.list"))
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
  x<-c(x1,x2[length(x2):1],x1[1])
  y<-c(y1,y2[length(y2):1],y1[1])
  if(plot.shadow){
    polygon(x,y, col=col.shadow$getCol(alp.shadow), border= "#00000000")
    mlines(x,y, lty.lines=lty.border, lwd.lines=lwd.border, col.lines=col.border, alp.lines=alp.border)
  }
  if(plot.mean & table.exist)
    mlines(x1, table$mean, lty.lines=lty.mean, lwd.lines=lwd.mean, col.lines=col.mean, alp.lines=alp.mean)
}


posLegend <- function(pos, inf){
  x<-inf$rect$left
  y<-inf$rect$top
  usr<-par("usr")
  X<-abs(c(usr[1],usr[2])-globalVar("xlim.0000000k1DTt5P2hOzSqOZFzG04ip6a3xfdFA9DrDp8Ffilq"))
  Y<-abs(c(usr[3],usr[4])-globalVar("ylim.0000000k1DTt5P2hOzSqOZFzG04ip6a3xfdFA9DrDp8Ffilq"))
  if(pos=="bottomleft"|pos=="bottom"|pos=="bottomright") y<-y+Y[1]
  if(pos=="topleft"|pos=="top"|pos=="topright") y<-y-Y[2]
  if(pos=="bottomleft"|pos=="left"|pos=="topleft") x<-x+X[1]
  if(pos=="bottomright"|pos=="right"|pos=="topright") x<-x-X[2]
  list(x=x, y=y)
}

#' @title mlegend
#' @description This is a modification to the \code{graphics::legend} function.
#' @param pos location of legend.
#' @param legend text of legend.
#' @param cex.legend scale for legend.
#' @param col.legend legend color.
#' @param alp.legend legend color transparency.
#' @param lin to draw a line or a point.
#' @param col.lin lines color.
#' @param alp.lin lines color transparency.
#' @param lty.lin lines type.
#' @param lwd.lin lines width.
#' @param col.box box color.
#' @param alp.box box color transparency.
#' @param lty.box box type.
#' @param lwd.box box width.
#' @param col.back back color.
#' @param alp.back back color transparency.
#' @seealso \code{itz::Legend}, \code{itz::mplot} & \code{itz::color}.
#' @examples
#' mplot()
#' mlegend(legend=c("a","b"), col.lin=c("blue","red"), lty.lin=c(1,2))
#'
#' mlegend("topleft", legend=c("a","b","c"), col.lin=c("blue","red","green"), lty.lin=c(3,2,1), lwd.lin=c(1,2,3))
#'
#' mlegend("topright", legend=c("a","b"), col.lin=c("purple","magenta"), lwd.lin=4, lin=c(F,T))
#'
#' mplot()
#' mlegend(c(0,1),legend=c("a","b"), col.lin=c("blue","red"), cex.legend=2, col.box="orange", lwd.box=5, lty.box=1)
#' @export
mlegend <- function(pos       ="center",
                    legend    ="",
                    cex.legend=nasd(),
                    col.legend=nasd(),
                    alp.legend=nasd(),
                    lin       =nasd(),
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
                    ...)
{
  lst<-list(...); param<-lst$param; lst$param<-NULL
  mlst<- c(lst, param, Legend("\\get.list"))
  if(is.nasd(cex.legend))cex.legend<-mlst$cex.legend; if(is.nasd(col.legend))col.legend<-mlst$col.legend; if(is.nasd(alp.legend))alp.legend<-mlst$alp.legend; if(is.nasd(lin))lin<-mlst$lin; if(is.nasd(col.lin))col.lin<-mlst$col.lin; if(is.nasd(alp.lin))alp.lin<-mlst$alp.lin; if(is.nasd(lwd.lin))lwd.lin<-mlst$lwd.lin; if(is.nasd(lty.lin))lty.lin<-mlst$lty.lin; if(is.nasd(col.box))col.box<-mlst$col.box; if(is.nasd(alp.box))alp.box<-mlst$alp.box; if(is.nasd(lwd.box))lwd.box<-mlst$lwd.box; if(is.nasd(lty.box))lty.box<-mlst$lty.box; if(is.nasd(col.back))col.back<-mlst$col.back; if(is.nasd(alp.back))alp.back<-mlst$alp.back
  col.legend<-fixCol(isnt.null(col.legend, mlst$col.legend.auxiliar))
  col.lin<-fixCol(isnt.null(col.lin, mlst$col.lin.auxiliar))
  col.box<-fixCol(isnt.null(col.box, mlst$col.box.auxiliar))
  col.back<-fixCol(isnt.null(col.back, mlst$col.back.auxiliar))
  if(is.character(pos)){
    inf<-graphics::legend(pos,legend=legend, lty=lty.lin, lwd=lwd.lin,cex=cex.legend, box.lty=lty.box, box.lwd=lwd.box, pch=NA, plot = FALSE)
    pl<-posLegend(pos, inf)
    x<-pl$x
    y<-pl$y
  }else{
    x<-pos[1]
    y<-pos[2]
  }
  colNA<-"#00000000"
  graphics::legend(x, y, legend=legend, lty=lty.lin, lwd=lwd.lin,cex=cex.legend, box.lty=lty.box, bg=col.back$getCol(alp=alp.back), box.lwd=lwd.box, text.col=col.legend$getCol(alp=alp.legend), box.col=colNA, col=colNA, pch=NA)
  n<-max(lwd.box,lwd.lin)
  if(n>0){
    lwd<-lwd.box
    lwd.box<-rep(0, n)
    if(lwd>0) lwd.box[1:lwd]<-1+lwd-(1:lwd)
    col<-col.box
    col.box<-rep(colNA, n)
    if(lwd>0){
      sq<-seq(0, 1, 1.0/lwd)[-1]
      for (i in 1:lwd){
        col.box[i]<-col$getCol(bri=sq[i], alp=alp.box, this=T)
      }
      NO<-col$getCol()
    }
    
    lin.iter<-iterVector(lin, cyclic = T)
    lwd.iter<-iterVector(lwd.lin, cyclic = T)
    col.iter<-iterCol(col.lin, cyclic = T)
    lty.lin.iter<-iterVector(lty.lin, cyclic = T)
    alp.lin.iter<-iterVector(alp.lin, cyclic = T)
    lwd.lin.mat<-NULL
    col.lin.mat<-NULL
    pch<-NULL
    lty.lin<-NULL
    alp.lin<-NULL
    for (i in 1:length(legend)) {
      pch[i]<-if(lin.iter$Next()) NA else 1
      lty.lin[i]<-lty.lin.iter$Next()
      alp.lin[i]<-alp.lin.iter$Next()
      if(!is.na(pch[i])) lty.lin[i]<-0
      lwd<-lwd.iter$Next()
      lwd.lin<-rep(0, n)
      if(lwd>0) lwd.lin[1:lwd]<-2*(lwd-(1:lwd))+1
      col<-col.iter$Next()
      col.lin<-rep(colNA, n)
      if(lwd>0){
        sq<-seq(0, 1, 1.0/lwd)[-1]
        for (j in 1:lwd){
          col.lin[j]<-col$getCol(bri=sq[j], alp=alp.lin[i], this=T)
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
                       box.lwd=lwd.box[i], box.col=col.box[i], pch=pch)
    }
  }
}

#' @export
subgraphic <- function(plots,
                       xmar=NULL, ymar=NULL, 
                       xfig=NULL, yfig=NULL,
                       overlap=T){
  mar<-par("mar")
  fig<-par("fig")
  xmar<-if(!is.null(xmar[1])) xmar else mar[c(2,4)]
  ymar<-if(!is.null(ymar[1])) ymar else mar[c(1,3)]
  xfig<-if(!is.null(xfig[1])) xfig else fig[c(1,2)]
  yfig<-if(!is.null(yfig[1])) yfig else fig[c(3,4)]
  
  newMar<-c(ymar[1], xmar[1], ymar[2], xmar[2])
  newFig<-c(xfig, yfig)
  par(mar=newMar, fig=newFig, new=overlap)
  plots
  par(mar=mar, fig=fig)
}

#' @export
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

#' @export
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

#' @export
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

#' @export
printer <- function(prop=0.68, width=9, height=NULL, size=c(1,1)){
  ACTIVE <- newVec(T)
  PROP <- newVec(prop)
  WIDTH <- newVec(width)
  HEIGTH <- newVec(if(is.numeric(height)) height else NA)
  SIZE <- newVec(size)
  
  PDF <- function(file=NULL, prop=NULL, width=NULL, height=NULL, size=NULL){
    if(ACTIVE$get()){
      OFF()
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
    if(ACTIVE$get()){
      ret<-ret<-TRY(dev.off(),"")
      while(ret!=""){
        print(ret)
        ret<-TRY(dev.off(),"")
      }
    } 
  }
  
  setActive <- function(value) ACTIVE$set(value)
  return(list(PDF=PDF, OFF=OFF, setActive=setActive))
}
