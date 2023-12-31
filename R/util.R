#' @export
create.dir<- function(path){
  spath<-strsplit(path,"/")[[1]]
  P<-""
  for (i in 1:length(spath)) {
    P<-if(i==1) spath[1] else pst(P, "/",spath[i])
    if(!dir.exists(P)) dir.create(P)
  }
}

#' @export
csv.scanner<- function(file, lista=NULL, basis=""){
  return(csv.scanner_(file, lista, basis)$get())
}

csv.scanner_<- function(file, lista, basis){
  if(is.null(lista)){
    basis<-file
    lista<-newList(basis)
    file<-""
  }
  df<-dir(if(file!="")pst(basis,"/",file) else basis)
  if(length(df)!=0){
    for(d in df){
      nfile<-if(file!="") pst(file,"/",d) else d
      if(file.info(pst(basis,"/",nfile))$isdir){
        csv.scanner(nfile, lista=lista, basis=basis)
      }else{
        nc<-nchar(d)
        if(nchar(d)>4){
          if(substr(d, nc-3, nc)==".csv"){
            lista$add(list(file=file, name=nfile, value=read.csv(pst(basis,"/",nfile))))
          }
        }
      }
    }
  }
  lista
}

#' @export
csv.printer<- function(lista, basis=NULL){
  if(is.null(basis)) basis<-lista[[1]]
  
  if(basis!="") basis<-pst(basis,"/")
  
  lista[[1]]<-NULL
  n<-length(lista)
  if(n>0){
    for (i in 1:length(lista)) {
      create.dir(pst(basis,lista[[i]]$file))
      write.csv(lista[[i]]$value, pst(basis, lista[[i]]$name), na="",  row.names=F)
    }
  }  
}

#' @export
l<-function(...) list(...)

#' @export
auxiliarWD<-function(file=""){
  code<-"WD.00000000wNZ8piTdFAWCqzbqnRBXJB5K9A9vmRqSc2"
  if(file==""){
    dir<- globalVar(code)
    if(!base::is.null(dir))
      if(dir.exists(dir))
          return(dir)
    return(getwd())
  }
  globalVar(code, file)
  cat("Working auxiliary directory set:")
  cat("\n\n    ", auxiliarWD(),"\n")
}

#' @export
listToVec <- function(List, index) {vec<-NULL;for (i in index) vec<-c(vec, List[[i]]);return(vec)}

#' @export
subList<-function(List, index){listy <-list();for (i in 1:length(index)) listy[[i]]<-List[[index[i]]];return(listy)}

#splitz <- function(text) {spl<-NULL; for (i in 1:nchar(text)) spl[i]<-substring(text, i, i); return(spl)}

#displitz <- function(spl,sp="") {text<-""; for(i in 1:length(spl)) text<-paste(text, spl[i],sp, sep="");return(substring(text,1,nchar(text)-nchar(sp)))}

#' @export
reparam <- function(x0, x, y) return(y[1]+(x0-x[1])*(y[2]-y[1])/(x[2]-x[1]))

#' @export
getSeconds <- function() as.numeric(proc.time()[3])

#' @export
pst<- function(..., sep="") paste(..., sep=sep)

#' @export
join<- function(..., sep="") {
  xjoin<-NULL
  y<-list(...)
  for (j in 1:length(y)) {
    x<-y[[j]]
    for (i in 1:length(x)) {
      if(i==1 & j==1) {
        xjoin<-pst(xjoin, x[i],sep="")
      }else {
        xjoin<-pst(xjoin, x[i],sep=sep)
      }
    }
  }
  return(xjoin)
}

#' @export
randFile<- function() join("0x",rep(0,runif(1,5,10)),sample(c(0,1,2,3,4,5,6,7,8,9,letters,LETTERS), runif(1,30,50),replace = T))

#' @export
TRY <- function(x,y=NULL) tryCatch(x, error=function(cond) y)

#' @export
IF <- function(cond, x, y=NULL){
  if(cond) {x} else {y}
}

#' @export
list.compareItem <- function(lista, name, item){
  item_ <- TRY(lista[[name]])
  if(!base::is.null(item_))
    if(item_==item)
      return(TRUE)
  return(FALSE)
}

#' @export
isnt.null<-function(x, y) if(is.null(x)) y else x

#' @export
isnt.na<-function(x, y) if(is.na(x)) y else x

#' @export
isnt.nasd<-function(x, y) if(is.nasd(x)) y else x

#' @export
nasd <- function() "nasd.00000000n3F8zpfpCoa5NnO6ELu8GhgA86qVNRnGw0gp93ekXeYgaPxu"

#' @export
null <- function() "null.000000000GBxRkEka3IDpOK5h0FNBnzLXMq1AIewoVOn64wincCVyl"

#' @export
is.null <- function(x) {
  if(base::is.null(x))
    return(TRUE)
  if(is.character(x))
    TRY(if(x[1]=="null.000000000GBxRkEka3IDpOK5h0FNBnzLXMq1AIewoVOn64wincCVyl")return(TRUE))
  return(FALSE)
}

#' @export
is.nasd <- function(x) {
  if(is.character(x))
    TRY(if(x[1]=="nasd.00000000n3F8zpfpCoa5NnO6ELu8GhgA86qVNRnGw0gp93ekXeYgaPxu")return(TRUE))
  return(FALSE)
}

#' @export
is.color.class <- function(x) list.compareItem(x,"class","color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT")

#' @export
is.group.class <- function(x) list.compareItem(x,"class","group.00000004intEgVdJq9HfQwRwDXWRlZYxDSX646IH6s51zkcT1B")

#' @export
is.copiable <- function(x) list.compareItem(x,"copiable","copiable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst")

#' @export
color.class <- function() "color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT"

#' @export
group.class <- function() "group.00000004intEgVdJq9HfQwRwDXWRlZYxDSX646IH6s51zkcT1B"

#' @export
newVec <- function(x=NULL){
  TRY(is.list(list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57)
      ,list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57<<-list())
  n<-length(list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57)+1
  list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57[[n]] <- x
  
  LENGTH<- function() length(get())
  
  get<- function(index=NA){
    TRY({
      if(is.na(index[1])) return(list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57[[n]])
      return(list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57[[n]][index])
    })
  }
  
  set <- function(x, index=NA){
    if(is.na(index[1])) list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57[[n]]<<-x
    else TRY({
      if(is.null(x)){
        list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57[[n]]<<-
          list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57[[n]][-index]
      }else{
        list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57[[n]][index]<<-x
      }
    },{
      var<-c()
      var[index]<-x
      list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57[[n]]<<-var
    })
  }
  
  add <- function(x) set(x, LENGTH()+1)
  
  iter <- function(cyclic=FALSE, defVal=NA) iterVector(get(), cyclic, defVal)
  
  return(list(get=get, set=set, add=add, length=LENGTH, iter=iter, class="vector"))
}

#' @export
newList <- function(...){
  lista<- newVec(list(...))
  
  LENGTH<- function() lista$length()
  
  get<- function(name="", index=NA){
    if(is.numeric(name)){
      index <- name
      name<-""
    }
    TRY({
      if(name!=""){
        item<-lista$get()[[name]]
        if(is.null(item)) return(NULL)
        return(item)
      }
      if(is.na(index)) return(lista$get())
      item<-lista$get()[[index]]
      if(is.null(item)) return(NULL)
      return(item)
    },{
      if(name!="") unlink(fun$file)
      return(NULL)
    })
  }
  
  set <- function(x, name="", index=NA){
    if(is.numeric(name)){
      index <- name
      name<-""
    }
    
    TRY({
      if(is.null(x)) x<-null()
      if(name!=""){
        lst<-lista$get()
        lst[[name]]<-x
        lista$set(lst) 
      }else if(is.na(index)){
        lista$set(x)
      }else{
        lst<-lista$get()
        lst[[index]]<-x
        lista$set(lst)
      }
    },{
      if(name!="") unlink(fun$file)
      cat(join("Error, invalid name: name=\"",name,"\""))
    }
    )
  }
  
  add <- function(x) set(x, index=LENGTH()+1)
  
  iter <- function(cyclic=FALSE, defVal=NULL) iterList(get(), cyclic, defVal)
  
  copy <- function(){
    lst <- newList()
    if(LENGTH()>0){
      this.lista <-lista$get()
      names <- names(this.lista)
      for (i in 1:LENGTH()) {
        item<-this.lista[[i]]
        ###Recordar crear una función para copiar items
        if(names[i]==""){
          lst$add(item)
        }else{
          lst$set(item, name=names[i])
        }
      }
    }
    return(lst)
  }
  
  return(list(get=get, set=set, add=add, length=LENGTH, iter=iter, copy=copy, copiable="copiable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst"))
}

#' @export
newFunction <- function(...){
  file <- pst(randFile(),".R")
  while(file.exists(file)) file <- pst(randFile(),".R")
  code<-newVec(pst("function(",join(..., sep=", "),"){"))
  add <- function(...) code$add(join(...))
  getCode<- function() join(code$get(),"}",sep="\n")
  getFunction <- function(){
    write(getCode(), file)
    fun<- source(file)$value
    unlink(file)
    return(fun)
  }
  run <- function(...) getFunction()(...)
  return(list(add=add, getCode=getCode, getFunction=getFunction, run=run, file=file))
}

#' @export
globalVar <- function(name, ...){
  TRY(is.list(list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57)
      ,list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57<<-list())
  if(base::is.null(
    list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57$
    x00000000Izt1yRF1SYvtSO2AHge1WegOIYLzoOn))
    list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57$
    x00000000Izt1yRF1SYvtSO2AHge1WegOIYLzoOn<<-list()
  
  x<-list(...)
  if(length(x)==0)
    return(list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57$
             x00000000Izt1yRF1SYvtSO2AHge1WegOIYLzoOn[[name]])
  list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57$
    x00000000Izt1yRF1SYvtSO2AHge1WegOIYLzoOn[[name]]<<-x[[1]]
}

newGlobalParam <- function(ID, DEFAULT) {
  if(base::is.null(globalVar(ID))) {
    Names<- newVec()
    List <- newList()
    DEFAULT(function(name, x)
    {
      Names$add(name)
      List$set(x, name=name)
    })
    globalVar(ID, list(Names=Names, List=List))
    
  }
  return(
    function(name, ...) {
      if(name=="\\default.values") {
        globalVar(ID, NULL)
        print("Default values were set")
      }else if(name=="\\get.list") {
        return(globalVar(ID)$List$get())
      }else if(name=="\\get.names") {
        return(globalVar(ID)$Names$get())
      }else if(name=="\\get.code") {
        names <- globalVar(ID)$Names$get()
        return(join(paste("if(is.nasd(",names,"))",names,"<-mlst$",names,"",sep=""),sep="; "))
      }else {
        if(length(list(...))==0)
          return(globalVar(ID)$List$get(name))
        globalVar(ID)$List$set(..., name)
      }
    }
  )
}

#' @export
newCount <- function(start=NA, step=1, limits=NA){
  if(is.na(limits[1])){
    if(is.na(start)) start<-0
    cont <- newVec(start)
    plus<-function() cont$set(cont$get()+step)
    less<-function() cont$set(cont$get()-step)
  }else{
    mod<- limits[2]-limits[1]+1
    start<-IF(is.na(start), limits[1],
              limits[1]+((start-limits[1])%%mod))
    cont <- newVec(start)
    plus<-function() cont$set(limits[1]+((cont$get()+step-limits[1])%%mod))
    less<-function() cont$set(limits[1]+((cont$get()-step-limits[1])%%mod))
  }
  get<-function() cont$get()
  getPlus<-function(){
    GET<-get()
    plus()
    return(GET)
  }
  getLess<-function(){
    GET<-get()
    less()
    return(GET)
  }
  plusGet<-function(){
    plus()
    return(get())
  }
  lessGet<-function(){
    less()
    return(get())
  }
  restart <- function(){
    if(is.na(limits[1])){
      cont$set(IF(is.na(start), 0, start))
    }else{
      cont$set(IF(is.na(start), limits[1],
                  limits[1]+((start-limits[1])%%(limits[2]-limits[1]+1))))
    }
  }
  return(list(get=get, plus=plus, less=less, getPlus=getPlus, getLess=getLess, plusGet=plusGet, lessGet=lessGet, restart=restart))
}

#' @export
iterList <- function(lista, cyclic=FALSE, defVal=NULL, decreasing=F){
  if(length(lista)>0){
    index<-newCount(start=IF(decreasing,length(lista),1),
                    limits=IF(cyclic,c(1,length(lista)),NA))
  }else{
    index<-newCount(1)
  }
  HasNext <- function() index$get()>0 & index$get()<=length(lista)
  if(decreasing){
    Next<- function() IF(HasNext(), lista[[index$getLess()]], defVal)
    Prev<- function() IF(HasNext(), lista[[index$getPlus()]], defVal)
  }else{
    Next<- function() IF(HasNext(), lista[[index$getPlus()]], defVal)
    Prev<- function() IF(HasNext(), lista[[index$getLess()]], defVal)
  }
  This<- function() IF(HasNext(), lista[[index$get()]], defVal)
  Restart<-function() index$restart()
  return(list(HasNext=HasNext, Next=Next, Prev=Prev, This=This, Restart=Restart))
}

#' @export
iterVector <- function(vector, cyclic=FALSE, defVal=NA, decreasing=F){
  if(length(vector)>0){
    index<-newCount(start=IF(decreasing,length(vector),1),
                    limits=IF(cyclic,c(1,length(vector)),NA))
  }else{
    index<-newCount(1)
  }
  HasNext <- function() index$get()>0 & index$get()<=length(vector)
  if(decreasing){
    Next<- function() IF(HasNext(), vector[index$getLess()], defVal)
    Prev<- function() IF(HasNext(), vector[index$getPlus()], defVal)
  }else{
    Next<- function() IF(HasNext(), vector[index$getPlus()], defVal)
    Prev<- function() IF(HasNext(), vector[index$getLess()], defVal)
  }
  This<- function() IF(HasNext(), vector[index$get()], defVal)
  Restart<-function() index$restart()
  return(list(HasNext=HasNext, Next=Next, Prev=Prev, This=This, Restart=Restart))
}

#' @export
iterCol <- function(col, cyclic=FALSE, defVal=NULL, decreasing=F){
  if(is.color.class(col)) col<-list(col)
  iterList(lista=col, cyclic=cyclic, defVal=defVal, decreasing=decreasing)
}

#' @export
funVec<- function(fun, x, ...){
  vec<-NULL
  for (i in 1:length(x)) 
    vec[i] <- fun(x[i], ...)
  return(vec)
}

#' @export
funList<- function(fun, x, ...){
  lis<-list()
  for (i in 1:length(x)) 
    lis[[i]] <- fun(x[i], ...)
  return(lis)
}

#' @export
linesInter <- function(x,y,z){
  ret<-rep(NA, length(z))
  for (i in 1:length(z)) {
    if(z[i]>=x[1] & z[i]<=x[length(x)]){
      ret[i] <- 0
      for (j in 1:length(x)) {
        if(x[j]<=z[i]) a<-j
        if(x[j]>=z[i]) {b<-j;break}
      }
      ret[i]<- if(a==b)y[a]else reparam(z[i],c(x[a],x[b]),c(y[a],y[b]))
    }
  }
  return(ret)  
}

#' @export
tableLinesInter <- function(X, Y){
  z <- NULL
  for (i in 1:length(X)) {
    notNA <- !is.na(X[[i]]) & !is.na(Y[[i]])
    X[[i]]<-X[[i]][notNA]
    Y[[i]]<-Y[[i]][notNA]
    ord <- order(X[[i]])
    X[[i]]<-X[[i]][ord]
    Y[[i]]<-Y[[i]][ord]
    z<-c(z, X[[i]])
  }
  z<-z[order(z)]
  z<-unique(z)
  tab<-NULL
  for (i in 1:length(X)) {
    tab<-cbind(tab, linesInter(X[[i]],Y[[i]],z))
  }
  return(list(z=z,tab=tab,
              min=apply(tab, 1, min, na.rm=T),
              max=apply(tab, 1, max, na.rm=T),
              mean=apply(tab, 1, mean, na.rm=F),
              sd=apply(tab, 1, sd, na.rm=F)))
}

#' @export
test <- function(n, X, Y,...){
  v<-newVec()
  for (i in 1:(2*n)) {
    a<-getSeconds()
    X(...)
    b<-getSeconds()
    Y(...)
    c<-getSeconds()
    v$add(b-a)
    v$add(c-b)  
  }
  return(list(x=v$get()[2*(1:n)-1], y=v$get()[2*(1:n)]))
}

#' @export
optima <- function(fun, param, interval, ...) {
  if(!is.list(interval)) interval<-list(interval)
  code<-join("\\optimitz",param, listToVec(interval,1:length(interval)),sep="_")
  op<-globalVar(code)
  if(!base::is.null(op)) return(op(fun, interval, ...))
  n<-length(param)
  optim<-newFunction("fun, interval, ...")
  jprm<-join(pst(param,"=",param), sep=", ")
  optim$add("f0<-function(",join(param,sep=", "),") fun(",jprm,", ...)")
  if(n>1){
    for (i in 1:(n-1)){
      parm<-param[(i+1):n]
      optim$add("f",i,"<-function(",join(parm, sep=", "),") optimize(f",i-1,", ",
                join(pst(parm,"=",parm), sep=", "),", interval=interval[[",i,"]])$objective")    
    }
  }
  optim$add(param[n],"<-optimize(f",n-1,", interval=interval[[",n,"]])$minimum")
  if(n>1){
    for (i in (n-1):1){
      parm<-param[(i+1):n]
      optim$add(param[i],"<-optimize(f",i-1,", ",
                join(pst(parm,"=",parm), sep=", "),", interval=interval[[",i,"]])$minimum")
    }
  }
  optim$add("list(",jprm,", min=f0(",jprm,"))")
  op<-optim$getFunction()
  globalVar(code, op)
  op(fun, interval, ...)
}

#' @export
simSample <- function(x, Nsim=NA){
  if(!is.na(Nsim)) return(as.numeric(quantile(ecdf(x), runif(Nsim))))
  ecdf<-ecdf(x)
  function(Nsim) as.numeric(quantile(ecdf, runif(Nsim)))
}

#' @export
simTable <- function(tab, Nsim=NA){
  if(is.null(ncol(tab))) tab<-cbind(tab)
  if(!is.na(Nsim)){
    simtab<-NULL
    for (i in 1:ncol(tab))
      simtab<- cbind(simtab, simSample(tab[,i], Nsim))
    colnames(simtab)<-colnames(tab)
    return(as.data.frame(simtab))
  }
  simsamp<-NULL
  for (i in 1:ncol(tab))
    simsamp[[i]]<- simSample(tab[,i])
  function(Nsim){
    simtab<-NULL
    for (i in 1:ncol(tab))
      simtab<- cbind(simtab, simsamp[[i]](Nsim))
    colnames(simtab)<-colnames(tab)
    return(as.data.frame(simtab))
  }
}