#' @export
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

#' @export
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
  
  getRGB<- function(this=F) c(r,g,b)
  
  getCol<- function(alp=1, bri=1, this=F)
    rgb(bri*r, bri*g, bri*b, alp)
  
  getRGBs<- function() 
    cbind(r,g,b)
  
  getCols<- function(alp=1, bri =1) {
    n<-max(length(alp), length(bri))
    alp=iterVector(alp, cyclic=T)
    bri=iterVector(bri, cyclic=T)
    color1<-NULL
    for (i in 1:n)
      color1[i]<- rgb(bri$This()*r, bri$This()*g, bri$Next()*b, alp$Next())
    color1<-rbind(color1)
    colnames(color1)<-paste("t",1:n,sep="")
    color1
  }
  
  combine<- function(col, r=0.5) {
    if(length(r)==1){
      rgb<-(1-r)*getRGB()+r*col$getRGB()
      return(color(rgb[1], rgb[2], rgb[3]))
    }
    rgb<-NULL
    for (i in 1:length(r))
      rgb<-rbind(rgb, (1-r[i])*getRGB()+r[i]*col$getRGB())
    color(rgb[,1], rgb[,2], rgb[,3])
  }
  
  sequence<- function(col, n) 
    combine(col, if(n!=1)seq(0,1, 1/(n-1)) else 0.5)
  
  copy<-function() color(r,g,b)
  
  inverse<-function() color(1-r, 1-g, 1-b)
  return(list(getRGB=getRGB, getCol=getCol, getRGBs=getRGBs, getCols=getCols,
              combine=combine, sequence=sequence, copy=copy, inverse=inverse,
              copiable="copiable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst", 
              class="color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT"))
}

colorList<- function(r=NA, g=NA, b=NA, maxValue=1) {
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
  varIter<-newVec(colist$iter(cyclic=TRUE, color(0,0,0)))
  add <- function(col) {
    colist$add(col)
    varIter$set(colist$iter(cyclic=TRUE))
  }
  
  getColor <- function(this) 
    IF(this, varIter$get()$This(), varIter$get()$Next())
  
  getRGB <- function(this=F) 
    getColor(this)$getRGB()
  
  getCol<- function(alp=1, bri=1, this=F) 
    getColor(this)$getCol(alp=alp, bri=bri)
  
  getRGBs<- function() cbind(r,g,b)
  
  getCols<- function(alp=1, bri =1) {
    cls<- colist$iter()
    col<-NULL
    while (cls$HasNext())
      col<-rbind(col,cls$Next()$getCols(alp=alp,bri=bri))
    rownames(col)<-paste("color",1:colist$length(), sep="")
    col
  }
  
  copy<-function() color(r,g,b)
  
  inverse<-function() color(1-r, 1-g, 1-b)
  
  return(list(getRGB=getRGB, getCol=getCol, getRGBs=getRGBs, getCols=getCols,
              add=add,length=colist$length, copy=copy, inverse=inverse,
              copiable="copiable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst", 
              class="color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT"))
}

#' @export
color.combine<-function(..., r, abs=F){
  lst<-list(...)
  d<- function(x ,y) sqrt(sum((x-y)^2))
  RGB<-list(fixCol(lst[[1]])$getRGB())
  dRGB<- NULL
  for (i in 2:length(lst)) {
    RGB[[i]]<-fixCol(lst[[i]])$getRGB()
    dRGB[i-1]<-if(abs) 1 else d(RGB[[i-1]], RGB[[i]])
  }
  dRGB<-cumsum(dRGB)
  dRGB<-c(0, dRGB/dRGB[length(dRGB)])
  n<-length(dRGB)
  retRGB<-NULL
  for (r in r) {
    for (i in 2:n) {
      if(r<=dRGB[i]) {
        ri<-reparam(r,c(dRGB[i-1],dRGB[i]),c(0,1))
        retRGB<- rbind(retRGB, (1-ri)*RGB[[i-1]]+ri*RGB[[i]])
        break()
      }
    }
  }
  color(retRGB[,1],retRGB[,2],retRGB[,3])  
}


#' @export
color.sequence<-function(..., n, abs=F, inv=F){
  r<- if(n==1) 0.5 else seq(0,1,1/(n-1))
  if(inv) r<-1-r
  color.combine(..., r=r, abs=abs)
}

#' @export
color.rainbow<- function(n, abs=F, inv=F){
  color.sequence("#FF0000",
                 "#FF7F00",
                 "#FFFF00",
                 "#00FF00",
                 "#0000FF",
                 "#4B0082",
                 "#9400D3",n=n, inv=inv, abs=abs)
}

