#' Cut values and create category
#'
#' @param data Data frame 
#' @param var Variable to be cut
#' @param breaks break points 
#' @param labels category name. If fancy, the categories will be created according to the break points 
#' @param right If false, right value will be exclusive
#'  @param newvar vector name of the categorical. If default, the var with suffix"cat" will be used as default name
#' @keywords lhcut()
#' @export
#' @examples 

lhcut<-function(data,var,breaks,labels="fancy",right=F,newvar="default"){
  brk=c(min(data[,var]),breaks,max(data[,var])^2)
  if(newvar=="default"){nvar=paste0(var,"cat")}else{nvar=newvar}
  if(labels=="fancy"){
    lab1=c(paste0("<",breaks))
    lab2<-c(paste0(">=",breaks))
    lab3<-c(paste0("<=",breaks))
    lab4<-c(paste0(">",breaks))
    lab11<-NULL;lab22<-NULL
    for(i in 1:(length(breaks)-1)){
      lab11<-c(lab11,paste(lab2[i],"&",lab1[i+1]))
      lab22<-c(lab22,paste(lab4[i],"&",lab3[i+1]))
    }
    if(right){
      labels<-c(lab3[1],lab22,lab4[length(lab4)])}else{ 
        labels<-c(lab1[1],lab11,lab2[length(lab2)])
      } }
  
  data[,nvar]<-cut(data[,var],breaks=brk,labels=labels,right=right)
  print(addvar(data,nvar,var,"range(x)","no"))
  data}


#' Change factor level of a variable using matched level of another variable in the same dataset
#'
#' @param data Data frame 
#' @param leader lead Variable to be used for factor level of the follower variable
#' @param follower follower Variable  
#' @keywords lhfactor()
#' @export
#' @examples 

lhfactor<-function(data=test,leader="AGEcat",follower="catt"){
  lab<-nodup(data,c(leader,follower),"var");lab<-lab[order(lab[,leader]),follower]
  data<-reflag(data,follower,lab)
}




#' Combine variables in the same column
#'
#' @param data Data frame 1 and 2 with long vectors and values. Note: no duplicated sorting vector allowed 
#' @param combine.var Variable name, c(var1,var2). var1 will be stacked over var 2
#' @keywords stackvar()
#' @export
#' @examples
stackvar<-function(data,combine.var=c("xxx","variable")){
  z$dum<-seq(nrow(z))
  z1<-z
  z1$dum<-z1$dum-1
  z1<-nodup(z1,combine.var[1],"all")
  keep<-c(combine.var[1],combine.var[2],"dum")
  z1[,combine.var[2]]<-z1[,combine.var[1]];z1[,!names(z1)%in%keep]<-""
  z<-rbind(z,z1[,names(z)]); z<-z[order(z$dum),]
  z[,c(combine.var[1],"dum")]<-NULL
  z}



#' mutate variable names
#'
#' @param data Data frame 1 and 2 with long vectors and values. Note: no duplicated sorting vector allowed 
#' @param mutate Vector to be mutated ex. "xxx=yyy" for renaming xxx as yyy
#' @keywords lhmutate()
#' @export
#' @examples
lhmutate<-function(data,mutate){
  keep<-sub("=.*","",mutate)%in%names(data)
  imp<-sub(".*=","",mutate)[keep]
  bimp<-sub("=.*","",mutate)[keep]
  print(c("Not found:",sub("=.*","",mutate)[!sub("=.*","",mutate)%in%names(data)]))
  
  for(i in 1:length(bimp)){
    names(data)[names(data)==bimp[i]]<-imp[i]
  }
  data
}



#' merge and make long to wide data frame
#'
#' @param dat1,dat2 Data frame 1 and 2 with long vectors and values. Note: no duplicated sorting vector allowed 
#' @param long.vector Vector containing wide variable names
#' @param value Column containing values associated with each wide variable. 
#' @keywords lhmerge_long_data()
#' @export
#' @examples
lhmerge_long_data<-function (dat1=dd1, dat2=dd2, long.vector1="PARAMCD", long.vector2="PARAMCD", value1="AVAL", value2="CHG") 
{
  names(dat1)[names(dat1) == long.vector1] <- "x"
  names(dat1)[names(dat1) == value1] <- "y"
  names(dat2)[names(dat2) == long.vector2] <- "x"
  names(dat2)[names(dat2) == value2] <- "y"
  dat2$x[dat2$x == dat1$x] <- paste0(dat2$x[dat2$x == dat1$x], 
                                     "dat2")
  datall <- lhrbind(dat1, dat2)
  head(datall)
  if(nrow(dup1(datall, names(datall)[!names(datall)%in%"y"],"all"))>0){
    print("duplicated data")
    output<-dup1(datall, names(datall)[!names(datall)%in%"y"],"all")
  }else{output <- lhwide(datall, "y", "x")}
}

#' merge wide data frames
#'
#' @param dat1,dat2 Data frame 1 and 2 with long vectors and values. Note: no duplicated sorting vector allowed 
#' @param by1,by2 Vectors containing matched values from each data frames. 
#' @keywords lhmerge_long_data()
#' @export
#' @examples
lhmerge_wide_data<-function(dat1,dat2,by1,by2){
  d1<-chclass(dat1,names(dat1),"char");d2<-chclass(dat2,names(dat2),"char")
  d1<-lhlong(d1,names(d1)[!names(d1)%in%by1]);d2<-lhlong(d2,names(d2)[!names(d2)%in%by2])
  head(d1)
  d1<-chclass(d1,names(d1),"char");d2<-chclass(d2,names(d2),"char")
  d2$variable<-as.character(d2$variable);d1$variable<-as.character(d1$variable)
  d2$variable[d2$variable%in%d1$variable]<-paste0(d2$variable[d2$variable%in%d1$variable],"d2")
  datall<-lhrbind(d1,d2)
  head(datall)
  unique(datall$variable)
  test<-nodup(datall,c(by1,by2,"variable"),"all")
  test<-test[order(test$USUBJID,as.Date(test$date)),]
  output<-lhwide(test,"value","variable")
}


#' Reshape wide
#'
#' @param data Dataset
#' @param wide.data Name of vector containing data to be dcasted
#'  @param wide.vector Name of vector to be reshape as heading 
#' @param data Dataset 
#' @keywords lhwide()
#' @export
#' @examples
#' lhwide()
lhwide<-function(data,wide.data,wide.vector){
  b <- function(x) {}
  x1<-paste(paste(names(data)[!names(data)%in%c(wide.data,wide.vector)],collapse="+"),"~",wide.vector)
  body(b) <- parse(text = x1)
  z1<-dcast(data,b())}

#' Reshape long
#'
#' @param data Dataset
#' @param long.vector List of vector to be melted
#'
#' @keywords lhlong()
#' @export
#' @examples
#' lhlong()
lhlong<-function(data,long.vector){
  z1<-melt(data,names(data)[!names(data)%in%long.vector])
}


#' find different values between two datasets
#'
#' @param dat1,dat2 Dataset 1 and 2" 
#' @keywords findiff()
#' @export
#' @examples
#' findiff()

findiff<-function(dat1,dat2){
 # stopifnot(nrow(dat1)==nrow(dat2))
  dum1a<-""
  nm1<-"dat1"
  dum2a<-""
  nm2<-"dat2"
  for(i in 1:length(names(dat1))){
    nm1<-paste(nm1,names(dat1)[i],sep="/")
    dum1a<-paste(dum1a,dat1[,names(dat1)[i]],sep="/")
    nm2<-paste(nm2,names(dat2)[i],sep="/")
    dum2a<-paste(dum2a,dat2[,names(dat2)[i]],sep="/")
  }
  a<-setdiff(dum1a,dum2a)
  b<-setdiff(dum2a,dum1a)
  out<-data.frame(nm1=unique(a));names(out)<-nm1
  out1<-data.frame(nm2=unique(b));names(out1)<-nm2
  row1<-data.frame(N1=length(dum1a),N2=length(dum2a))
  out3<-lhcbind(out,out1)
  out3<-lhcbind(out3,row1)
  out3 }

#' date and time format funtion
#'Join two datasets and print report of joining procedure
#' @param dattime Date and time data ex: "%d-%b-%y %H:%M:%S" 
#' @keywords d010101()
#' @export
#' @examples
#' d010101()
d010101<-function(dattime){
  #%Y=year 2000;%B=January; %b=Jan; %m=month 01;%H= 24-H; %p= AM/PM  
  strftime(strptime(dattime, format ="%d-%b-%y %H:%M:%S",tz = "GMT"), format = "%Y-%m-%d %H:%M", tz = "GMT") 
}



#' lhjoin funtion
#'Join two datasets and print report of joining procedure
#' @param dat1,by1 Data frame 1 and variables to be matched. If NULL, match="all"
#' @param dat2,by2 Data frame 2 and variables to be matched. If by1=NULL then by2=NULL then match="all"
#' @param type could be "full", "left","right" or "inner"
#' @keywords lhjoin()
#' @export
#' @examples
#' lhjoin()
lhjoin<-function(dat1,by1=NULL,dat2,by2=NULL,type="full"){
  invar<-intersect(names(dat1),names(dat2))
  if(is.null(by1)){
    by1=invar}else{
      by1=by1}
  if(is.null(by2)){
    by2=invar
  }else{
    by2=by2
    names(dat2)[names(dat2)%in%invar]<-paste0("df2_",names(dat2)[names(dat2)%in%invar])
  }
  
  if(length(by1)>1){
    dat1[,"dum"]<-dat1[,by1[1]]
    for(i in 2:length(by1)){
      dat1[,"dum"]<-paste(dat1[,"dum"],dat1[,by1[i]],sep="-")
    }}else{dat1[,"dum"]<-dat1[,by1[1]]}
  
  by2[!by2%in%by1]<-paste0("df2_",by2[!by2%in%by1])
  
  if(length(by2)>1){
    dat2[,"dum"]<-dat2[,by2[1]]
    for(i in 2:length(by2)){
      dat2[,"dum"]<-paste(dat2[,"dum"],dat2[,by2[i]],sep="-")
    }}else{dat2[,"dum"]<-dat2[,by2[1]]}
  
  dat<-plyr::join(dat1,dat2,by="dum",type=type)
  
  report<-data.frame(nrow_data1=nrow(dat1),
                     nrow_data2=nrow(dat2),
                     nrow_joint=nrow(dat))
  for(c in 1:length(by1)){    
    x<-data.frame(z=setdiff(dat1[,by1[c]],dat2[,by2[c]]))
    names(x)<-paste0(by1[c],"_not_in_data2")
    y<-data.frame(z=setdiff(dat2[,by2[c]],dat1[,by1[c]]))
    names(y)<-paste0(by1[c],"_not_in_data1")
    zz<-lhcbind(x,y)
    report<-lhcbind(report,zz)
  }
  print(report)
  dat}



#' lhorder funtion
#'
#' Make simple table. Use data frame created by addvar2
#' @param dat Datframe
#' @param var Order by variables. ex: ":Trt,:Agegr"
#' @keywords lhorder()
#' @export
#' @examples
#' lhorder()

lhorder<-function(dat,var){
  data<-dat
  x<-paste0("data[order(",gsub(":","data$",var),"),]")
  b<- function(x) {}
  body(b) <- parse(text = x)
  data<-b()
}



#' lhtab funtion
#'
#' Make simple table. Use data frame created by addvar2
#' @param data Datframe
#' @param vh Vertical and horizontal headers. ex: "Trt+Agegr~Param"
#' @param value Values. Example: c("mean","SD","Mean (CV)")
#' @param ord Order variables. ex: ":Trt,:Agegr"
#' @param save.name Save table as word document. Enter the file name: ex "test.docx"
#' @param output output="csv" for csv output, else output will be in FlexTable format
#' @keywords lhtab()
#' @export
#' @examples
#' lhtab()

lhtab<-function (data, vh, value, ord = NULL, save.name = NULL, output = "csv") 
{
  library(reshape)
  b <- function(x) {
  }
  body(b) <- parse(text = vh)
  
  #vh<-"group~label+ss+test"
  v <- gsub("+", ":", sub("~.*", "", vh), fixed = T)
  v <- unlist(strsplit(v, ":")) 
  h <- gsub("+", ":", sub(".*~", "", vh), fixed = T)
  list(gsub(":", ",", h))
  h <- unlist(strsplit(h, ":"))
  data$dum<-""
  for(i in h){
    if(i==h[1]){
      data$dum<-data[,i]}else{data$dum<-paste(data$dum,data[,i],sep="_")}
  }  
  
  
  hd<-nodup(data,c("dum",v,h),"var")
  w<-NULL
  for(uu in value){
    data[,"stats"]<-uu
    w1 <- reshape(data[,c(v,"dum",uu,"stats")], 
                  timevar ="dum",
                  idvar =c(v,"stats"),
                  direction = "wide")
    for(u in names(data[,c(v,"dum",uu,"stats")])){
      rm<-paste0(u,".")
      names(w1)<-gsub(rm,"",names(w1),fixed = T)
    }
    w<-rbind(w,w1)}
  hw<-NULL
  
  for(d in h){
    hd[,"stats"]<-"stats"
    hw1 <- reshape(hd[,c("dum",v,d,"stats")], 
                   timevar ="dum",
                   idvar =c(v,"stats"),
                   direction = "wide")
    
    for(u in names(hd[,c("dum",v,d,"stats")])){
      rm<-paste0(u,".")
      names(hw1)<-gsub(rm,"",names(hw1),fixed = T)
    }
    hw1<-nodup(hw1,names(hw1)[!names(hw1)%in%c(v,"stats")],"all")
    hw<-rbind(hw,hw1)
  }
  hw<-hw[,unique(names(hw))]
  for(vv in v){
    hw[,vv]<-vv 
  }
  
  setdiff(names(w),names(hw))
  
  hw1<-rbind(hw,w)
  head(hw1,10)
  
  
  if (!is.null(ord)) {
    y <- paste0(ord, ",:stats")
  }else {
    y <- ":stats"
  }
  stor<-c("stats",value)
  hw1[,"stats"]<-factor(hw1[,"stats"],level=stor)
  head(hw1)
  
  hw1 <- lhorder(hw1,y)
  
  hw1 <- chclass(hw1, names(hw1), "char")
  tab <- ReporteRs::FlexTable(hw1, header.columns = FALSE)
  
  for (y in c(v,"stats")) {
    tab = ReporteRs::spanFlexTableRows(tab, j = y, runs = as.character(hw1[, 
                                                                           y]))
  }
  t4 <- hw1
  colnames(t4) <- NULL
  rownames(t4) <- NULL
  for (z in 1:length(h)) {
    tab = ReporteRs::spanFlexTableColumns(tab, i = z, runs = paste(t4[z, 
                                                                      ]))
  }
  tab[1:length(h), ] = ReporteRs::textProperties(font.weight = "bold")
  tab[, names(hw1)] = ReporteRs::parCenter()
  if (!is.null(save.name)) {
    doc <- ReporteRs::docx()
    doc <- ReporteRs::addFlexTable(doc, tab)
    ReporteRs::writeDoc(doc, save.name)
    ReporteRs::writeDoc(doc, save.name)
  }
  if (output != "csv") {
    res <- tab
  }
  else {
    res <- hw1
  }
  res
}




#' txt funtion
#'
#' Clone expression function for adding special formats and symbol to plots.
#' @param c text. Example: c("Concentration mg L","-1::s"," AUC::u"," Delta::i","moles::e"). s=subscript, u=underline, Delta= capital greek delta letter, i= italic, e=superscript
#' @keywords txt()
#' @export
#' @examples
#' txt()

txt<-function(c){
  z1<-""
  for(j in 1:length(c)){
    if(length(grep("::",sub(".*::","::", c[j])))==0){z=c[j]}else{
      if(length(grep(":e",sub(".*:e",":e", c[j])))!=0){
        z=paste0("^{",gsub(sub(".*::", "::",c[j]),"",c[j]),"}")}
      if(length(grep(":s",sub(".*:s",":s", c[j])))!=0){
        z=paste0("[",gsub(sub(".*::", "::",c[j]),"",c[j]),"]")}
      if(length(grep(":u",sub(".*:u",":u", c[j])))!=0){
        z=paste0(" underline(",gsub(sub(".*::", "::",c[j]),"",c[j]),")")}
      if(length(grep(":i",sub(".*:i",":i", c[j])))!=0){
        z=paste0(" italic(",gsub(sub(".*::", "::",c[j]),"",c[j]),")")}}
    z1=paste0(z1,z)}
  z1=gsub(" ","~",z1)
  z1=paste0("expression(",z1,")")
  b <- function(x) {}
  body(b)<-parse(text=z1)
  text<-b()
}




#' install.pack
#'
#' To install require packages. Use ipak function to install desired packages.
#' @param packages pre-define packages list.
#' @keywords install.pack
#' @export
#' @examples
#' install.pack()

install.pack<-function(...){
  packages <- c("SASxport", "reshape", "Hmisc", "tidyr","ReporteRs","plyr","downloader")
  ipak(packages)}


#' lhtemplate
#'
#' To install require packages. Use ipak function to install desired packages.
#' @param repo leonpheng
#' @param user logo.style
#' @keywords lhtemplate
#' @export
#' @examples
#' lhtemplate()
lhtemplate <- function(){
  require(downloader)
  dir.create("c:/lhtemplate")
  wf<-"c:/lhtemplate"
  url <- sprintf("https://github.com/%s/%s/archive/master.zip", "leonpheng","logo.style")
  tmp <- tempfile(fileext = "styleapp.zip",tmpdir=wf)
  download(url, tmp)
  unzip(tmp,exdir=wf)

  #download_repo("logo.style","leonpheng")
  zipF<-paste0(wf,"/logo.style-master/styleapp.zip")
  unzip(zipF,exdir=wf)
  zipF<-paste0(wf,"/logo.style-master/logostyle.zip")
  unzip(zipF,exdir=wf)
  frm<-dir(wf)
  index<-c(grep("zip",frm))
  frm<-frm[index]
  for(i in frm){
    file.remove(paste(wf,i,sep="/"))
  }
  unlink(paste0(wf,"/logo.style-master"), recursive = T)
}

#' lhdoc
#'
#' Create doc for word document using this fonction or you can do it manually.
#' Type lhtext and copy the template to R workspace and start writing.
#' @param t template name see at the end of lhtext function.
#' @param toc.level maximimum toc level
#' @param template Word document template could be used for styles. Styles should be mapped in style.to.map. Template is also available at github: to load it, just run  lhtemp() once to download and store the templates in your PC at "c:lhtemplate. Note that the templates and logo are also used in xptdef package.

#' @param TOC Set to F if no TOC wanted
#' @param style.to.map Map the styles in template to be used. Ex: mypar is for footnote (font size)
#' @keywords lhdoc
#' @export
#' @examples
lhdoc<-function(toc.level=4,template="default",TOC=F,
        style.to.map="default",logo.certara=F){

  library(ReporteRs)
  library(flextable)
  library(dplyr)

  if(template=="default"){
  doc<-docx(template = "c:/lhtemplate/styleapp.docx", empty_template = TRUE)}else{doc<-docx(template =template, empty_template = TRUE)}
if("default"%in%style.to.map){
  doc <- map_title(doc, stylenames =c("Heading1",
                                     "Heading2", "Heading2", "fnt"))}else{doc <- map_title(doc, stylenames =style.to.map)}
  if(logo.certara){
    doc<-addImage(doc,"c:/lhtemplate/logo.jpg", par.properties = parProperties(text.align = "center"),width = 3.35, height = 1.6)
  }
  if(TOC){
    doc <-addTOC(doc,level_max =toc.level)
  }
  doc
  }


#' lhrbind
#'
#' r bind 2 data frames regardless number of columns.
#' @param dat1,dat2 data frames.
#'
#' @keywords lhrbind
#' @export
#' @examples
#' lhrbind()

lhrbind<-function (dat1, dat2, na.replace = NA, all.character = T) 
{
  dat1[, setdiff(names(dat2), names(dat1))] <- na.replace
  dat2[, setdiff(names(dat1), names(dat2))] <- na.replace
  if (all.character) {
    dat <- rbind(chclass(dat1, names(dat1), "char"), chclass(dat2, 
                                                             names(dat2), "char"))
    print("Warning: all vectors in new dataset are character")
    dat
  }
  else (dat <- rbind(dat1, dat2))
}

#' lhcbind
#'
#' C bind 2 data frames regardless number of row length.
#' @param dat1,dat2 data frames.
#'
#' @keywords lhcbind
#' @export
#' @examples
#' lhcbind()

lhcbind<-function(dat1,dat2){
  dat1=as.data.frame(dat1)
  dat2=as.data.frame(dat2)
  r1<-nrow(dat1)
  r2<-nrow(dat2)
  if(r1>r2){
    r3=as.data.frame(matrix(ncol=ncol(dat2),nrow=r1-r2,data=""))
    names(r3)<-names(dat2)
    r3=rbind(dat2,r3)
    dat=cbind(dat1,r3)
  }
  if(r1<r2){
    r3=as.data.frame(matrix(ncol=ncol(dat1),nrow=r2-r1,data=""))
    names(r3)<-names(dat1)
    r3=rbind(dat1,r3)
    dat=cbind(r3,dat2)
  }
  if(r1==r2){dat=cbind(dat1,dat2)}
  dat
}

#' lhloess
#'
#' Compute the LOESS data for ploting.
#' @param data data.
#' @param x Independent variable
#' @param y Dependent variable
#' @param by Sort by. Only one sorting variabele is accepted. If more than 1 variables, create a unique sorting using paste(var1,var2,etc)
#' @param span LOESS stiffness
#' @keywords lhloess
#' @export
#' @examples
#' lhloess()


lhloess<-function(data,x,y,by,span=1){
  library(plyr)
  data$x=data[,x]
  data$y=data[,y]
  data$by=data[,by]
  head(data)
  dat=NULL
  for(i in unique(data$by)){
    tmp<-data[data$by==i,c(x,"x","y")]
    head(tmp)
    tmp1<-with(tmp,unlist(predict(loess(y~x,tmp,span=span),x)))
    tmp$loess<-tmp1
    dat<-rbind(dat,tmp)
  }
  #data$x<-data$y<-data$by<-NULL
  data<-join(data,dat)
}



#' load.pack1
#'
#' To install require packages. Use ipak function to install desired packages.
#' @param packages pre-define packages list.
#' @keywords load.pack1
#' @export
#' @examples
#' load.pack1()

loadpack<-function(...){
  library("ReporteRs")
  require(dplyr)
  library(plyr)
  require(stats)     # To format summary
  require(PCSmisc)     #
     #This package is needed to add a dataset label,it can only be used  with the 32bit version of R. If dataset label is not needed, the package SASxport can be used
  require(tidyr)
}


#' Ben's function
#'
#' internal used
#' @param packages pre-define packages list.
#' @keywords blk.locf
#' @export
#' @examples
#' blk.locf2()

blk.locf2<-function (x, id, na.action = c("fill", "carry.back"), fill = NA)
{
  .checkID(id)
  if (length(x) != length(id)) {
    stop("Lengths of x and id must match")
  }
  na.action <- match.arg(na.action)
  ii <- ifelse(is.na(x), NA, .myseqalong(x))
  ii <- unlist(tapply(ii, id, function(y, na.action) {
    if (all(is.na(y)))
      return(y)
    z <- cumsum(!is.na(y))
    if (na.action == "carry.back") {
      z[z == 0] <- 1
    }
    ifelse(z == 0, NA, y[!is.na(y)][ifelse(z > 0, z, 1)])
  }, na.action = na.action, simplify = FALSE))
  y <- x
  y[!is.na(ii)] <- x[ii[!is.na(ii)]]
  y[is.na(ii)] <- fill
  y
}



#########
#' TAD from ADDL
#'
#' This function allows you to derive time after dose from ADDL.
#' @param data data frame
#' @param id ID vector
#' @param ii dose interval vector
#' @param addl additional dose vector
#' @param rtime relative time after first dose vector
#' @param evid EVID vector
#' @param dose amount adminstered (ex: AMT) vector
#' @param dose.expand If "yes", all dosing rows in ADDL will be outputed
#' @keywords tad
#' @export
#' @examples
#' tad_addl()

tad_addl<-function (data=df, id="usubjid", ii="ii", addl="addl", rtime="rtime", evid="evid", dose.expand = "yes") 
{
  data <- chclass(data, c(rtime, evid, addl, ii), "num")
  data[,addl][is.na(data[,addl])]<-0
  data[,ii][is.na(data[,ii])]<-0
  data[, "TAD"] <- data[, "tad"] <- NULL
  nam <- names(data)
  data <- data[order(data[, id], data[, rtime]), ]
  dose <- data[data[, evid] == 1, ]
  dose[, "TAD"] <- 0
  datp <- data[data[, evid] != 1, ]
  dat0 <- data[data[, evid] == 1, ]
  datr <- NULL
  
  for (i in 1:nrow(dat0)) {
    dat1 <- dat0[i, ]
    if (dat1[, addl] == 0) {
      dat2 <- dat1
    }  else {
      dat2 <- as.data.frame(matrix(ncol = ncol(dat1), nrow = dat1[, 
                                                                  addl]))
      names(dat2) <- names(dat1)
      dat2[, names(dat2)] <- dat1
      dat2$dum <- seq(0, nrow(dat2) - 1, 1)
      dat2[, rtime] <- dat2[, rtime] + (dat2[, ii] * dat2$dum)
      dat2$dum <- NULL
    }
    datr <- rbind(datr, dat2)
  }
  dup1(datr, names(datr), "all")
  datr <- nodup(datr, names(datr), "all")
  datr[, addl] <- datr[, ii] <- 0
  setdiff(names(datr), names(datp))
  datr$loc1 <- datr[, rtime]
  datr$lhdose <- "yes"
  datp$loc1 <- NA
  datp$lhdose <- "no"
  datp1 <- rbind(datp, datr)
  datp1 <- datp1[order(datp1[, id], datp1[, rtime]), ]
  head(datp1)
  datp1 <- locf2(datp1, id, "loc1")
  datp1$TAD <- datp1[, rtime] - datp1$loc1
  datp1$TAD[datp1$TAD < 0] <- 0
  range(datp1$TAD)
  
  if (dose.expand != "yes") {
    d1 <- datp1[datp1$lhdose == "no", ]
    d1$loc1 <- d1$lhdose <- NULL
    data <- rbind(d1, dose)
    data <- data[order(data[, id], data[, rtime]), ]
  } else {
    data <- datp1[, !names(datp1) %in% c("loc1", "lhdose")]
  }
  data
}


###########
#' BLQ M6 Method
#'
#' This function allows you to create data with BLQ using M6 method.
#' @param data data frame
#' @param id ID vector
#' @param evid EVID vector
#' @keywords m6
#' @export
#' @examples
#' tad_addl()
m6<-function(data,id,evid,mdv,blq.flag,time,dv,lloq){
  dat<-data
  #id="id";time="RTIME";mdv="mdv";evid="evid";blq.flag="blqf";dv="dv";lloq=0.01
  dat$cum<-cumsum(dat[,evid])
  dat$cum1<-cumsum(dat[,blq.flag])

  good<-addvar(dat[dat[,evid]==0&dat[,mdv]==0,],c(id,"cum"),time,"max(x)","no","good")
  good1<-addvar(dat[dat[,time]>0&dat[,blq.flag]==1,],c(id,"cum1"),time,"min(x)","no","good1")
  good1[,time]<-good1$good1
  good

  m4<-plyr::join(dat,good)
  m4<-plyr::join(m4,good1)
  good2<-addvar(m4[m4$good<=m4$good1,],c(id,"cum"),"good1","min(x)","no","good2")
  good2[,time]<-good2$good2

  good3<-addvar(m4[m4$good>=m4$good1,],c(id,"cum"),"good1","max(x)","no","good3")
  good3[,time]<-good3$good3

  m4<-plyr::join(m4,good2)
  m4<-plyr::join(m4,good3)
  m4$dvm6<-m4[,dv]
  m4$mdvm6<-m4[,mdv]

m4$dvm6[m4[,time]==m4$good2|m4[,time]==m4$good3]<-lloq/2
m4$mdvm6[m4[,time]==m4$good2|m4[,time]==m4$good3]<-0
m4$cum<-m4$cum1<- m4$good<-m4$good1<-m4$good2<-m4$good3<-NULL
  m4
}


#-------------------------
#' Reflag variables
#'
#' This function allows you to change variable name (ex: "M" to "Male").
#' @param dat data frame
#' @param var Vector to be changed
#' @param orignal.flag Original names (ex:c("M","F"))
#' @param new.flag New names (ex:c("Male","Female"))
#' @param newvar Create new vector
#' @keywords reflag
#' @export
#' @examples
#' reflag(dat,var="SEX",c("M","F"),c("Male","Female"),"SEXCH"))
reflag<-function (dat, var, orignal.flag, new.flag=NULL,newvar=NULL,to.factor=T,missing=c("",".","NA",NA)) 
{
  if(is.null(new.flag)){
    new.flag=orignal.flag
  }else{new.flag}
  forgot<-setdiff(dat[,var],orignal.flag)
  forgot<-forgot[!forgot%in%missing]
  print(paste("forgot:",forgot))
  stopifnot(length(forgot)==0)
  dat[,var]<-as.character(dat[,var])
  dat[dat[,var]%in%missing,var]<-"missing or unknown"
  orignal.flag<-as.character(orignal.flag)
  new.flag<-as.character(new.flag)
  dat[,var]<-as.character(dat[,var])
  if(!is.null(newvar)){
    dat[,newvar]<-factor(dat[,var],levels=c(orignal.flag,"missing or unknown"),
                         labels=c(new.flag,"missing or unknown"))
    if(to.factor==F){
      dat[,newvar]<-as.character(dat[,newvar])
    }}else{dat[,var]<-factor(dat[,var],levels=c(orignal.flag,"missing or unknown"),
                             labels=c(new.flag,"missing or unknown"))
    if(to.factor==F){
      dat[,var]<-as.character(dat[,var])
    }}
  dat
}
#-------------------------
#' Derived 1 variable and 1 function
#'
#' This function allows you to add derived variable (ex: add mean value by ID).
#' @param dat data frame
#' @param sort sort derived variable by (ex:c("ID","SEX"))
#' @param var variable to be derived
#' @param fun deriving funtion ex:"mean(x)")
#' @param add.to.data if "yes" result will be appended to dat
#' @param name column name of derived variable
#' @keywords addvar
#' @export
#' @examples
#' addvar()
addvar<-function(dat,sort,var,fun,add.to.data="yes",name=NULL){
  library(plyr)
  d<-dat
  a<-fun
  if(is.null(name)){name=paste0(gsub("(x)",var,fun))}
  b<-function(x){}
  body(b)<-parse(text=a)

  if(length(sort)>1){dd<-(aggregate(d[,var],d[,sort],b))}else{dd<-(aggregate(d[,var],list(d[,sort]),b))}
  names(dd)<-c(sort,name)
  if(add.to.data=="yes"){out<-plyr::join(dat,dd,type="left")}else{out<-dd}}

#-------------------------
#' Derived more variables and functions
#'
#' This function allows you to add derived variable (ex: add mean value by ID).
#' @param dat data frame
#' @param sort sort derived variable by (ex:c("ID","SEX"))
#' @param var variable to be derived
#' @param fun deriving funtion ex:c("mean(x)=mean","length(x[is.na(x)])")
#'
#' @keywords addvar
#' @export
#' @examples
#' addvar()

addvar2<-function (dat, sort, var, fun, rounding = "sigfig(x,3)") 
{
  tmp1 <- NULL
  stn <- NULL
  for (z in 1:length(fun)) {
    fy = gsub("=", "", sub(".*=", "=", fun[z]))
    fx <- gsub(sub(".*=", "=", fun[z]), "", fun[z])
    tmp <- NULL
    stn <- c(stn, fy)
    for (v in var) {
      t <- addvar(dat = dat, sort = sort, var = v, fun = fx, 
                  add.to.data = "no", name = fy)
      t$var <- v
      tmp <- rbind(tmp, t)
      tmp[, fy] <- as.numeric(as.character(tmp[, fy]))
      rounding1 <- "round(x,10)"
      if (!fy %in% c("N", "n")) {
        a <- gsub("x", "tmp[,fy]", rounding1)
        b <- function(x) {
        }
        body(b) <- parse(text = a)
        tmp[, fy] <- b()
      }
      else {
        tmp
      }
    }
    if (z == 1) {
      tmp1 <- tmp
    }
    else {
      tmp1 <- join(tmp1, tmp)
    }
  }
  a <- rounding
  b <- function(x) {
  }
  body(b) <- parse(text = a)
  for (z in stn[!stn %in% c("N", "n")]) {
    for(x in 1:nrow(tmp1)){
      tmp1[,z] <- b(as.numeric(tmp1[, z]))
    }
  }
  tmp1
}


#######ADD TIME########
#-------------------------
#' Add time in hour to calendar date/time
#'
#' This function allows you to add derived variable (ex: add mean value by ID).
#' @param datetime date/time vector to be computed
#' @param timehour Time to be added in hour
#' @param format date and time format
#' @param tz Time zone (default="GMT")
#' @param add.to.data if "yes" result will be appended to dat
#' @param name column name of derived variable
#' @keywords addtime
#' @export
#' @examples
#' addtime()

addtime<-function(datetime,timehour,format="%Y-%m-%d %H:%M",tz="GMT"){
  output<-substring(strptime(datetime,format=format,tz=tz)+timehour*60*60,1,16)
  output}


###TAD calculation using elapsed RTIME#########
#-------------------------
#' Derive TAD from RTIME
#'
#' This function allows you to derive Time after dose from Time after first dose.
#' @param data data frame
#' @param id subject id
#' @param time time after first dose
#' @param evid evid
#' @keywords rt2tad
#' @export
#' @examples
#' rt2tad()
rt2tad<-function(data,id,time,evid){
  #id="uid";time="T";evid="evid"
  data$cumsum<-unlist(tapply(data[,evid],list(data[,id]),cumsum))
  nrow(data)
  data$time<-data[,time]
  d<-data[data[,evid]==1,c(id,time,"cumsum")]
  names(d)<-c(id,"time1","cumsum")
  data$sort<-seq(1,nrow(data),1)
  d<-nodup(d,c(id,"cumsum"),"all")
  data1<-plyr::join(data,d,type="left")
  data1<-chclass(data1,c("time","time1"),"num")
  data1$tad<-with(data1,time-time1)
  data1$ndose<-data1$cumsum
  data1<-data1[order(data1$sort),];data1$sort<-data1$time1<-data1$time<-data1$cumsum<-NULL
  data1
}
####create NMEM UNIQUE SUBJECT####
#-------------------------
#' NMID
#'
#' This function allows you to create NMID.
#' @param data data frame
#' @param id subject id
#' @param varname column name
#' @keywords nmid
#' @export
#' @examples
#' nmid()
nmid<-function(data,id,varname="NMID"){
  id="USUBJID";varname="X.NMID"
  data<-pcf1
  data$ord<-seq(1,nrow(data),1)
  idat<-data.frame(id=unique(data[,id]),varname=seq(1,length(unique(data[,id])),1))
  names(idat)<-c(id,varname)
  data<-merge(data,idat)
  data<-data[order(data[,varname],data$ord),]
  data$ord<-NULL
  data
}
###Elapse Time###
#-------------------------
#' Compute delta using calendar date and time
#'
#' This function allows you to compute the delta (time1-time2).
#' @param tm1 data frame
#' @param tm2 subject id
#' @param form1 date/time format 1
#' @param form2 date/time format 2
#' @keywords diftm
#' @export
#' @examples
#' diftm()
diftm<-function(tm1,tm2,unit="hour",form1="%Y-%m-%d %H:%M",form2="%Y-%m-%d %H:%M",tz="GMT"){
dat<- as.numeric(difftime(strptime(tm1,format=form1,tz=tz),strptime(tm2,format=form2,tz=tz), units=unit))
 dat}

######change date and time format
#-------------------------
#' Reformat calendar date/time
#'
#' This function allows you to change the date/time format.
#' @param dttm original date/time
#' @param tm2 subject id
#' @param form1 date/time format 1 to be changed
#' @param form2 new date/time format
#' @keywords format_time
#' @export
#' @examples
#' format_time()

format_time<-function(dttm,form1,form2="%Y-%m-%d %H:%M",tz="GMT"){
  strftime(strptime(dttm,format=form1,tz=tz),format=form2,tz=tz)
}
####SUMMARY table
#-------------------------
#' Quick table
#'
#' This function allows you to create quick and dirty table.
#' @param data original date/time
#' @param var1 var1
#' @param var2 var2
#' @keywords tab
#' @export
#' @examples
#' tab()
tab<-function(data,var1,var2){
  data[,"var1"]<-data[,var1]
  data[,"var2"]<-data[,var2]
  xtabs(~var1+var2,data)
}
##Change class
#-------------------------
#' Change variable class
#'
#' This function allows you to change variable class ("num" or "char").
#' @param data data
#' @param var variable (ex:c("DV","MDV"))
#' @param class class ("char" or "num")
#' @keywords chclass
#' @export
#' @examples
#' chclass()
chclass<-function(data,var,class="char"){
   for(i in var){
    if (class=="num"){
      data[,i]<-as.numeric(as.character(data[,i]))}
    else {data[,i]<-as.character(data[,i])}
  }
  data
}
#print unique variable only
#-------------------------
#' one
#'
#' one.
#' @param data data
#' @param var variable
#' @keywords one
#' @export
#' @examples
#' one()
one<-function(data,var){
  for(i in var){
    print(i)
    print(data[!duplicated(data[,i]),i])
  }
}
# keep unique only
#-------------------------
#' No duplicate
#'
#' This function allows you to remove duplicates.
#' @param data data
#' @param var variable (ex:c("DV","MDV"))
#' @param all if all="all", all columns in data will be kept (ex:all=c("ID","DV"))
#' @keywords nodup
#' @export
#' @examples
#' nodup()
nodup<-function(data,var,all,item){
  if(all=="all"){d1<-data[!duplicated(data[,var]),names(data)]}else{
    if(all=="var"){d1<-data[!duplicated(data[,var]),var]}else{
      d1<-data[!duplicated(data[,var]),c(var,item)]}}
  d1
}
#Output duplicated row for checking or remove duplicates if remove is set to non-NULL
#-------------------------
#' Check duplicates
#'
#' This function allows you to check duplicates.
#' @param data data
#' @param var variable (ex:c("DV","MDV"))
#' @param remove if remove="yes", duplicates will be removed)
#' @keywords duprow
#' @export
#' @examples
#' duprow()
duprow<-function(data,var=NULL,remove=NULL){
  flag="flag"
  data[,flag]<-""
  if(is.null(var)){
    var=names(data)}
  for(i in 1:length(var)){
    data[,flag]<-paste(data[,flag],data[,var[i]],sep="")
  }
  if(is.null(remove)){
    data[duplicated(data[,"flag"]),]}
  else{data1<-data[!duplicated(data[,"flag"]),]
       data1[,"flag"]<-NULL
       data1}
}


########Compute Rtime and tad#########
#-------------------------
#' Derive TAD and RTIME
#'
#' This function allows you to derive TAD and RTIME from calendar date/time.
#' @param data data
#' @param id subject id
#' @param date date variable "%Y-%m-%d"
#' @param time time variable  "%H:%M")
#' @param EVID evid variable (evid>0 for dose)
#' @keywords tadRT
#' @export
#' @examples
#' tadRT()
tadRT<-function (data, id, date, time, EVID, tz = "UTC") 
{
  locf <- function(x) {
    good <- !is.na(x)
    positions <- seq(length(x))
    good.positions <- good * positions
    last.good.position <- cummax(good.positions)
    last.good.position[last.good.position == 0] <- NA
    x[last.good.position]
  }
  data$DTTM <- data$TAD <- data$RTIME <- NULL
  data$DTTM <- as.character(paste(data[, date], data[, time], 
                                  sep = " "))
  data <- chclass(data, c(date, time), "char")
  data$tadtm <- NA
  data <- data[order(data[, id], as.Date(data[, date]), data[, 
                                                             time]), ]
  head(data)
  dtm <- data[data[, EVID] > 0, ]
  rtime <- dtm[!duplicated(dtm[, id]), c(id, "DTTM")]
  names(rtime)[2] <- "FDDTM"
  nodose <- data[data[, EVID] == 0, ]
  dose <- data[data[, EVID] > 0, ]
  dose$tadtm <- as.character(dose$DTTM)
  data <- rbind(dose, nodose)
  data$tadtm <- as.character(data$tadtm)
  head(data)
  data$DTTM <- strftime(strptime(data$DTTM, format = "%Y-%m-%d %H:%M", 
                                 tz = tz), format = "%Y-%m-%d %H:%M", tz = tz)
  data <- data[order(data[, id], data$DTTM), ]
  data$WT1 <- unlist(tapply(data$tadtm, data[, id], locf))
  data$tadtm <- rev(locf(rev(data$WT1)))
  data <- data[order(data[, id], as.Date(data[, date]), data[, 
                                                             time]), ]
  head(data)
  data$DTTM <- strftime(strptime(data$DTTM, format = "%Y-%m-%d %H:%M", 
                                 tz = tz), format = "%Y-%m-%d %H:%M", tz = tz)
  data$tadtm <- strftime(strptime(data$tadtm, format = "%Y-%m-%d %H:%M", 
                                  tz = tz), format = "%Y-%m-%d %H:%M", tz = tz)
  data$TAD <- as.numeric(difftime(strptime(data$tadtm, format = "%Y-%m-%d %H:%M", 
                                           tz = tz), strptime(data$DTTM, format = "%Y-%m-%d %H:%M", 
                                                              tz = tz), units = "hour")) * (-1)
  data <- merge(data, rtime, all.x = T)
  data$RTIME <- as.numeric(difftime(strptime(data$DTTM, format = "%Y-%m-%d %H:%M", 
                                             tz = tz), strptime(data$FDDTM, format = "%Y-%m-%d %H:%M", 
                                                                tz = tz), units = "hour"))
  data$WT1 <- NULL
  data$tadtm <- NULL
  data$FDDTM <- NULL
  data <- data[order(data[, id], as.Date(data[, date]), data[, 
                                                             time]), ]
  data$RTIME <- round(data$RTIME, 4)
  data$TAD <- round(data$TAD, 4)
  data
}


#' LOCF and LOCB
#'
#' LOCF LOCB function
#' @param data data
#' @param var variable to locf
#' @param by sort variable
#' @param locb carry backward
#' @keywords locb2
#' @export
#' @examples
#' locf2()
locf2<-function (data, by, var, locb = T) 
{
  locf <- function(x) {
    good <- !is.na(x)
    positions <- seq(length(x))
    good.positions <- good * positions
    last.good.position <- cummax(good.positions)
    last.good.position[last.good.position == 0] <- NA
    x[last.good.position]
  }
  data$dumy<-seq(1,nrow(data))
  data[, var] <- unlist(tapply(data[, var], data[, by], 
                               locf))
  
  if (locb) {
    data <- data[order(data[,by],rev(data$dumy)),]
    data[, var] <- unlist(tapply(data[, var], data[, by], 
                                 locf))
    data <- data[order(data[,by],data$dumy),]
  }
  data$dumy<-NULL
  data
}


########
# 1 cpt
#' One compartment micro constants and HL
#'
#' This function allows you to derive TAD and RTIME from calendar date/time.
#' @param data data
#' @keywords hl1cpt
#' @export
#' @examples
#' hl1cpt()
hl1cpt<-function(data,cl,v,output){
all<-c("HL","k")
  ifelse(output=="all",output<-all,output)

  k<-data[,cl]/data[,v]
  HL<-log(2)/k
  datf<-data.frame(k=k,HL=HL)
  data[,output]<-datf[,output]
  data
  }

#df<-data.frame(id=1:5,cla=1:5/2,v=2:6*4,cl2=3:7/20,v2=3:7*100,cl3=3:7/10,v3=3:7*50)
#df<-hl3cpt(df,"cla","cl2","cl3","v","v2","v3","all")


#Two-compartment
#' Two compartment micro constants and HL
#'
#' This function allows you to derive micro constants and HL.
#' @param data data
#' @keywords hl2cpt
#' @export
#' @examples
#' hl2cpt(pkdat1,"cl","cl2","v","v2","all")
#
hl2cpt<-function(data,cl,cl2,v,v2,output){
  all<-c("HLa","HLb","alfa","beta","k","k12","k21")
  ifelse(output=="all",output<-all,output)
df<-data
  k<-df[,cl]/df[,v]
  k12<-df[,cl2]/df[,v]
  k21<-df[,cl2]/df[,v2]
beta1<-(1/2)*(k12+k21+k-(sqrt((k12+k21+k)^2-(4*k21*k))))
alfa<-k21*k/beta1
alfaHL<-log(2)/alfa    # to be verify with excel
betaHL<-log(2)/beta1    # to be verified with excel
datf<-data.frame(k=k,k12=k12,k21=k21,alfa=alfa,beta=beta1,HLa=alfaHL,HLb=betaHL)
  data[,output]<-datf[,output]
  data
}

# Three CPT
#' Three compartment micro constants and HL
#'
#' This function allows you to derive micro constants and HL.
#' @param data data
#' @keywords hl3cpt
#' @export
#' @examples
#' hl3cpt(pkdat1,"cl","cl2",,"cl3","v","v2","v3","all")
#
hl3cpt<-function(data,Cl,Cl2,Cl3,V,V2,V3,output){
  all<-c("HLa","HLb","HLg","A","B","C","alpha","beta","gama")
  ifelse(output=="all",output<-all,output)
  df<-data
  k<-df[,Cl]/df[,V]
  k12<-df[,Cl2]/df[,V]
  k21<-df[,V]*k12/df[,V2]
  k13<-df[,Cl3]/df[,V]
  k31<-df[,V]*k13/df[,V3]
  a0<-k*k21*k31
  a1<-(k*k31) + (k21*k31) + (k21*k13) + (k*k21) + (k31*k12)
  a2<-k + k12 + k13 + k21 + k31
  p<-a1 - (a2^2)/3
  q<-2*(a2^3)/27 - a1*a2/3 + a0
  r1<-sqrt((-1)*p^3/27)
  r2<-2*(r1^(1/3))
  phi<-acos(-1*q/(2*r1))/3
  gama<-(-1)*((cos(phi)*r2)-(a2/3))             # gama instead of alpha: Formula error found in Ref. Dubois A. et al., "Mathematical Expressions of the Pharmacokinetic and Pharmacodynamic Models implemented in the PFIM"
  alpha<-(-1)*(cos(phi+(2*pi/3))*r2-a2/3)       # alpha instead of beta: Formula error found in Ref. Dubois A. et al., "Mathematical Expressions of the Pharmacokinetic and Pharmacodynamic Models implemented in the PFIM"
  beta<-(-1)*(cos(phi+(4*pi/3))*r2-a2/3)        # beta instead of gamma: Formula error found in Ref. Dubois A. et al., "Mathematical Expressions of the Pharmacokinetic and Pharmacodynamic Models implemented in the PFIM"
  alfaHL<-log(2)/alpha
  betaHL<-log(2)/beta
  gamaHL<-log(2)/gama
  A=(1/df[,V])*((k21-alpha)/(alpha-beta))*((k31-alpha)/(alpha-gama))
  B=(1/df[,V])*((k21-beta)/(beta-alpha))*((k31-beta)/(beta-gama))
  C=(1/df[,V])*((k21-gama)/(gama-beta))*((k31-gama)/(gama-alpha))
  datf<-data.frame(HLa=alfaHL,HLb=betaHL,HLg=gamaHL,alpha=alpha,beta=beta,gama=gama,A=A,B=B,C=C)
  data[,output]<-datf[,output]
  data
}


#Round old method
#' Rounding as per Excel Internal use
#'
#' This function allows you to round value as per Excel method.
#' @keywords cround
#' @export
#' @examples
#' cround1()
cround1= function(x,n,asnum=T){
  vorz = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
ifelse(is.na(x),output<-NA,
  output<-sprintf(paste("%.",n,"f",sep=""),z*vorz))
if(asnum){output<-as.numeric(as.character(output))}else{
output}
output
}

#' Round up 
#'
#' This function allows you to round value as in Excel.
#' @param z Vector or single value to be rounded
#' @param y number of significant figure 
#' @keywords rounding
#' @export
#' @examples
#' cround()
cround<-function (z, y) 
{
  if(length(z)>1){
    output<-NULL
    for(i in 1:length(z)){
      output1<-cround1(as.numeric(z[i]),y)
      output<-rbind(output,output1)}
    output
  }else{output<-cround1(as.numeric(z),y)
  output
  }}


#sigfig Internal Use
#' Significant figure
#'
#' This function allows you to round value in significant figure.
#' @keywords sigfig
#' @export
#' @examples
#' sigfig1()
sigfig1<-function (x, y) 
{
  sround = function(x, n) {
    vorz = sign(x)
    z = abs(x) * 10^n
    z = z + 0.5
    z = trunc(z)
    z = z/10^n
    ifelse(is.na(x), sro <- NA, sro <- z * vorz)
    sro
  }
  nround <- ifelse(x == 0, y - 1, y - 1 - floor(log10(abs(x))))
  if (!is.na(x) & ceiling(log10(abs(x))) >= 3) {
    output <- as.character(cround(x, 0))
  }else {
    if (!is.na(x) & ceiling(log10(abs(x)))<3) {
      output <- sprintf(paste("%.", nround, "f", sep = ""), 
                        sround(x, nround))
    }else{
      output <- NA
    }
  }
  output
}

#Sigfig 
#' Significant figure
#'
#' This function allows you to round value in significant figure.
#' @param z Vector or single value to be rounded
#' @param y number of significant figure 
#' @keywords sigfig
#' @export
#' @examples
#' sigfig()
sigfig<-function (z, y) 
{
  if(length(z)>1){
    output<-NULL
    for(i in 1:length(z)){
      output1<-sigfig1(as.numeric(z[i]),y)
      output<-rbind(output,output1)}
    output
  }else{output<-sigfig1(as.numeric(z),y)
  output
  }}

#Only output unique duplicate item
#' Filter unique duplicated row
#'
#' This function allows you to filter duplicated rows but only show unique row
#' @param data data
#' @param data data
#' @param all display all columns (all="all")
#' @param select display selected variables only
#' @keywords dup1
#' @export
#' @examples
#' dup1()
dup1<-function(data,var,all,select){
  d1<-data[duplicated(data[,var]),]
  if(all=="all"){d1<-d1}else{
    if(all=="var"){d1<-d1[,var]}else{
      d1<-d1[,c(var,select)]}}
  d1
}

#Output duplicated items with all or partial variabes
#' Filter all duplicated rows
#'
#' This function allows you to filter duplicated rows but only show unique row
#' @param data data
#' @param data data
#' @param all display all columns (all="all")
#' @param select display selected variables only
#' @keywords dup2
#' @export
#' @examples
#' dup2()
dup2<-function(data,var,all,select){
  d1<-data
  d1$dum<-""
  for(i in var){
    d1$dum<-paste(d1$dum,d1[,i],sep="-")
  }
  dup<-d1[duplicated(d1$dum),"dum"]
  d1<-d1[d1$dum%in%dup,]
  if(all=="all"){d1<-d1[,names(data)]}else{
    if(all=="var"){d1<-d1[,var]}else{
      d1<-d1[,c(var,select)]}}
  d1
}

#TABLE FUNCTIONS###############
#' bround Table function
#' @param data data
#' @keywords bround
#' @export
#' @examples
#' bround()
bround<-function(data,var,rtype="sigfig",dec=3){
  data<-chclass(data,var,"num")
  for(i in var){
    data[is.na(data[,i]),i]<-9999999999999
    if(rtype=="sigfig"){data[,i]<-sigfig(data[,i],dec)}else{data[,i]<-cround(data[,i],dec)}
    data[data[,i]=="9999999999999",i]<-"NA"
  }
  data
}

#' geom Table function
#'
#' @param x data
#' @keywords geom
#' @export
#' @examples
#' geom()


geom <- function(x) {
  exp(mean(log(x[x > 0]), na.rm=TRUE))
}

#' geocv Table function
#' @param x data
#' @keywords geocv
#' @export
#' @examples
#' geocv()

geocv <- function(x) {
  100*sqrt(exp(var(log(x[x > 0]), na.rm=TRUE)) - 1)
}

#' cv Table function
#' @param x data
#' @keywords cv
#' @export
#' @examples
#' cv()

cv <- function(x) {
  abs(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100)
}

#########
# mean<-"mean(x,na.rm=T)=mean"
# sd<-"sd(x,na.rm=T)=sd"
# cv<-"cv(x)=cv"
# qt05<-"quantile(x,0.05,na.rm=TRUE)=qt05"
# qt95<-"quantile(x,0.95,na.rm=TRUE)=qt05"

#per95<-function(x){quantile(x,percentile,na.rm=TRUE)}

#' se Table function
#' internal use.
#' @param x data
#' @keywords se
#' @export
#' @examples
#' se()

se<-function(x){sd(x,na.rm=TRUE)/(length(x))^0.5}

#' cilow Table function
#'
#' internal use
#' @param x data
#' @keywords generic
#' @export
#' @examples
#' cilow()

cilow<-function(x){mean(x,na.rm=TRUE)-((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}    #1.96)}

#' ciup Table function
#' internal use.
#' @param x data
#' @keywords ciup
#' @export
#' @examples
#' ciup()
ciup<-function(x){mean(x,na.rm=TRUE)+((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}

#' nmiss Table function
#' internal use.
#' @param x data
#' @keywords nmiss
#' @export
#' @examples
#' nmiss()
nmiss<-function(x){length(x[is.na(x)])}

#nmiss<-function(x){length(x[is.na(x)])}
#upci<-function(x){mean(x,na.rm=TRUE)+((sd(x,na.rm=TRUE)/(length(x#))^0.5)*qt(0.975,df=length(x)-1))}
#loci<-function(x){mean(x,na.rm=TRUE)-((sd(x,na.rm=TRUE)/(length(x#))^0.5)*qt(0.975,df=length(x)-1))}    #1.96)}
#se<-function(x){sd(x,na.rm=TRUE)/(length(x))^0.5}
#per95<-function(x){quantile(x,percentile,na.rm=TRUE)}
#per05<-function(x){quantile(x,1-percentile,na.rm=TRUE)}
#Gmean <- function(x) {
 # exp(mean(log(x[x > 0]), na.rm=TRUE))
#}
# Gcv <- function(x) {
#   100*sqrt(exp(var(log(x[x > 0]), na.rm=TRUE)) - 1)
# }
# 
# }
#####################TABLE STATS##################

#' conti<-function(input=input,var=var,by=by,round.type="sigfig",digit=3,quanti){
#'   statsfun()
#'   cv<-function(x){
#'     abs(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100)
#'   }
#'   nmiss<-function(x){length(x[is.na(x)])}
#'   upci<-function(x){mean(x,na.rm=TRUE)+((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}
#'   loci<-function(x){mean(x,na.rm=TRUE)-((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}    #1.96)}
#'   se<-function(x){sd(x,na.rm=TRUE)/(length(x))^0.5}
#'   per95<-function(x){quantile(x,percentile,na.rm=TRUE)}
#'   per05<-function(x){quantile(x,1-percentile,na.rm=TRUE)}
#'   Gmean <- function(x) {
#'     exp(mean(log(x[x > 0]), na.rm=TRUE))
#'   }
#'   Gcv <- function(x) {
#'     100*sqrt(exp(var(log(x[x > 0]), na.rm=TRUE)) - 1)
#'   }
#' 
#'   input<-input[,c(var,by)]
#'   l<-stats::reshape(input,
#'              varying = var,
#'              v.names = "score",
#'              timevar = "var",
#'              times = var,
#'              direction = "long")
#'   l1<-l[!is.na(l$score),]
#'   l2<-l[is.na(l$score),]
#'   l1$qt<-quanti
#'   sum<-plyr::ddply(l1,c(by,"var"),summarise,
#'              n=length(score),
#'              #nmiss=nmiss(score),
#'              mean=mean(score,na.rm=T),
#'              cv=cv(score),
#'              sd=sd(score,na.rm=T),
#'              se=se(score),
#'              median=median(score,na.rm=T),
#'              min=min(score,na.rm=T,na.rm=T),
#'              max=max(score),
#'              lo_qt975=quantile(score,0.025,na.rm=T),
#'              hi_qt975=quantile(score,0.975,na.rm=T),
#'              lo_qt95=quantile(score,0.05,na.rm=T),
#'              hi_qt95=quantile(score,0.95,na.rm=T),
#'              lo_qtxx=quantile(score,1-unique(qt),na.rm=T),
#'              hi_qtxx=quantile(score,unique(qt),na.rm=T),
#'              lowCI=loci(score),
#'              HiCI=upci(score),
#'              GeoMean=Gmean(score),
#'              GeoCV=Gcv(score)
#'   )
#' 
#'   variable=c("mean","cv","sd","se","median","min","GeoMean", "GeoCV",
#'              "max","lo_qt975", "hi_qt975", "lo_qt95",  "hi_qt95","lo_qtxx","hi_qtxx","lowCI","HiCI")
#' 
#'   l<-reshape(sum,
#'              varying = variable,
#'              v.names = "value",
#'              timevar = "toround",
#'              times = variable,
#'              direction = "long")
#'   l<-l[!is.na(l$value),]
#'   if(round.type=="sigfig"){
#'     l$value<-sigfig(l$value,digit)}else{l$value<-cround(l$value,digit)}
#'   l$id<-NULL
#'   keep<-names(l)[!names(l)%in%c("toround","value")]
#'   w <- stats::reshape(l,
#'                timevar = "toround",
#'                idvar = keep,
#'                direction = "wide")
#'   names(w)<-gsub("value.","",names(w))
#'   if(nrow(l2)>0){
#'     sum1<-plyr::ddply(l2,c(by,"var"),summarise,
#'                 nmiss=nmiss(score))
#'     w<-plyr::join(w,sum1)
#'     w$nmiss[is.na(w$nmiss)]<-0}else{w$nmiss=0}
#'   w$score<-w$id<-NULL
#'   w
#' }


#' Funtion for Descriptove stats of continuous covariate
#'
#'
#' Descriptove stats of continuous covariates
#' @param data datset or data frame (ex:data=PKdatat)
#' @param var List of continuous covariates (ex:c("CRCL","WT"))
#' @param by  Stratification variable (ex: by="study")
#' @param colby Specify sorting variable to be displayed vertically. (ex: colby=by or colby="var")
#' @param rowby Specidy sorting variable horizontally. If by > 1, set rowby=by.
#' @param sumry Result summary format to be displayed. Type summary.set to see different options of summary sets.
#' @param round.type rounding method, sigfig by default. (ex:"round" for rounding)
#' @param digit Number of round digits or significant figures
#' @keywords con.tab
#' @export
#' @examples
#' con.tab(data=dat,var=c("WT","crcl","ALT"),by=c("study"),colby="var",rowby=by,sumry=set1)

lhcontab<-function(dat,var,by,format="by.var",sumry=1,round.type="sigfig",digit=3,quant=0.90){
  statsfun()
  cv<-function(x){
    abs(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100)
  }
  nmiss<-function(x){length(x[is.na(x)])}
  upci<-function(x){mean(x,na.rm=TRUE)+((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}
  loci<-function(x){mean(x,na.rm=TRUE)-((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}    #1.96)}
  se<-function(x){sd(x,na.rm=TRUE)/(length(x))^0.5}
  per95<-function(x){quantile(x,percentile,na.rm=TRUE)}
  per05<-function(x){quantile(x,1-percentile,na.rm=TRUE)}
  Gmean <- function(x) {
    exp(mean(log(x[x > 0]), na.rm=TRUE))
  }
  Gcv <- function(x) {
    100*sqrt(exp(var(log(x[x > 0]), na.rm=TRUE)) - 1)
  }

  p1<-"(";p2<-")"
  b1<-"[";  b2<-"]"
  a<-"{"; a<-"}"
  d<-"-"; com<-","
  bsl<-"/";lb<-"\n"
  sp<-" "
  set=NULL
  set[[1]]=c("paste0(mean,sp,p1,cv,p2,lb,median,sp,b1,min,d,max,b2)","Mean(CV)Med[min-max]")
  set[[2]]=c("paste0(mean,sp,p1,sd,p2,lb,median,sp.b1,min,d,max,b2)","Mean(SD)Med[min-max]")
  set[[3]]=c("paste0(mean,sp,p1,cv,com,sp,n,p2,lb,median,sp,b1,min,d,max,b2)","Mean(CV,N) Med[min-max]")
  set[[4]]=c("paste0(mean,sp,p1,sd,com,sp,n,p2,lb,median,sp,b1,min,d,max,b2)","Mean(SD,N) Med[min-max]")
  set[[5]]=c("paste0(n,sp,p1,nmiss,p2,lb,mean,sp,p1,cv,p2,lb,median,sp,b1,min,d,max,b2)","N(Nmis)Mean(CV) Med[min-max]")
  set[[6]]=c("paste0(n,sp,p1,nmiss,p2,lb,mean,sp,p1,sd,p2,lb,median,sp,b1,min,d,max,b2)","N(Nmis)Mean(SD) Med[min-max]")
  set[[7]]=c("paste0(mean,sp,p1,cv,p2,lb,median,sp,b1,lowCI,d,HiCI,b2)","Mean(CV)Med[95CI]")
  set[[8]]= c("paste0(mean,sp,p1,cv,p2,lb,median,sp,b1,lo_qt95,d,hi_qt95,b2)","Mean(CV)Med[95PI]")
set[[9]]=c("paste0(mean,sp,p1,cv,p2,lb,median,sp,b1,lo_qt975,d,hi_qt975,b2)","Mean(CV)Med[97.5PI]")
set[[10]]=c("paste0(mean,sp,p1,cv,p2,lb,median,sp,b1,lo_qtxx,d,hi_qtxx,b2)","Mean(CV) Median[xxPI]")



  data<-dat[,c(var,by)]
    data<-data[,c(var,by)]
    data<-chclass(data,var,"num")
  #  lo_qtxx<-paste0("loqt",quant)
  #  hi_qtxx<-paste0("hiqt",quant)

    con1<-conti(data,var=var,by=by,round.type=round.type,digit=digit,quanti=quant)
    data1<-data
    head(data1)
    data1[,by]<-"Overall"
    con2<-conti(data1,var=var,by=by,round.type=round.type,digit=digit,quanti=quant)
    con1<-rbind(con1,con2)
   # names(con1)[names(con1)=="lo_qtxx"]<-lo_qtxx
  #  names(con1)[names(con1)=="hi_qtxx"]<-hi_qtxx
    grp<-sumry

    head(con1)
    if(is.null(grp)){w<-con1}else{
      groupstats<-set[[grp]][1]
      con1$summary<-with(con1,eval(parse(text=groupstats)))
      con1<-con1[,c(by,"var","summary")]
      if(format=="by.var"){
        w <- reshape(con1,
                            timevar = "var",
                            idvar = by,
                            direction = "wide")
        names(w)<-gsub("summary.","",names(w))
        nm<-names(w)
        w$stats<-set[[grp]][2]
        w<-w[,c("stats",nm)]
        }else{
            tx<-NULL
            for(i in var){
            ty<-t(con1[con1$var==i,c("var","summary")])
            tx<-rbind(tx,ty)}
            tx<-tx[row.names(tx)!="var",]
            row.names(tx)<-var
            dim(tx)
            con2<-chclass(con1,by,"char")
            byby<-nodup(con2,by,"var")
            t1<-t(byby)
            dim(t1)
            row.names(t1)<-by
            w<-as.data.frame(rbind(t1,tx))
            names(w)
            names(w)<-c(paste0(rep(set[[grp]][2],dim(w)[2]-1),1:dim(w)[2]-1))
            nm<-names(w)
           w$Covariate<-row.names(w)
           w<-w[,c("Covariate",nm)]
        }}
    row.names(w)<-NULL
    w
      }

################################################
#' roundbatch
#'
#' internal use
#' @keywords roundbatch
#' @export
#' @examples

roundbatch<-function(data,variable,toround,nb){
  head(data)
  data<-sum
  l<-stats::reshape(data,
             varying = variable,
             v.names = "value",
             timevar = "toround",
             times = variable,
             direction = "long")
  l<-l[!is.na(l$value),]
  if(toround=="sigfig"){
    l$value<-sigfig(l$value,nb)}else{l$value<-cround(l$value,nb)}
  l$id<-NULL
  keep<-names(l)[!names(l)%in%c("toround","value")]
  w <- stats::reshape(l,
               timevar = "toround",
               idvar = keep,
               direction = "wide")
  names(w)<-gsub("value.","",names(w))
  w
}


################COUNTS CATEGORICAL###############

#' Funtion for Descriptove Stats of Categorical Covariate
#'
#'
#' Descriptove stats of categorical covariates
#' cat.tab(data,var,by,colby="var",rowby=by)
#' @param data datset or data frame (ex:data=PKdatat)
#' @param var List of continuous covariates (ex:c("SEX","RACE"))
#' @param by  Stratification variable (ex: by="study")
#' @keywords cat.tab
#' @export
#' @examples
#' cat.tab(data=dat,var=c("SEX","RACE"),by=c("study"),colby="var",rowby=by)

lhcattab<-function (data, var, by)
  {
  rowby = by
  dat1 <- chclass(data[, c(var, by)], c(var, by), "char")

  tot <- stats::reshape(dat1, varying = var, v.names = "value",
                        timevar = "var", times = var, direction = "long")
  tot$id <- NULL
  tot1 <- addvar(tot, c(by, "var"), "var", "length(x)", "no",
                 "tot")
  tot2 <- addvar(tot, c(by, "var", "value"), "var", "length(x)",
                 "no", "subt")
  tot11 <- nodup(tot1, c(by), "all")
  names(tot11)[names(tot11) == "tot"] <- "N="
  tot12 <- addvar(tot, c("var", "value"), "var", "length(x)",
                  "no", "Overall")
  tot13 <- addvar(tot, c("var"), "var", "length(x)", "no",
                  "tot")
  tot12 <- plyr::join(tot12, tot13)
  tot12$Overall <- with(tot12, paste0(Overall, " (", sigfig(Overall/tot *
                                                              100, 3), "%)"))
  tot12$tot <- NULL
  tot11$"N=" <- paste0(tot11$"N=", " (", sigfig(tot11$"N="/max(tot13$tot) *
                                                  100, 3), "%)")
  tot4 <- plyr::join(tot2, tot1)
  tot4$summary <- with(tot4, paste0(subt, " (", sigfig(subt/tot *
                                                         100, 3), "%)"))
  tot3 <- tot4[, c(by, "var", "value", "summary")]

  tto<-addvar(tot4,c(rowby),"tot","max(x)","yes","all")
  tto[,c("var","value")]<-"all"
  tto$all<-paste0(tto$all," (100%)")

  tto0<-tto;tto0$tot<-tto0$subt<-tto0$summary<-NULL
  w0 <- stats::reshape(tto0, timevar =rowby, idvar = c("var",
                                                       "value"), direction = "wide")
  names(w0)<-gsub("all.","",names(w0))

  tto1<-tot4;tto1$tot<-tto1$subt<-NULL
  w <- stats::reshape(tto1, timevar =rowby, idvar = c("var",
                                                      "value"), direction = "wide")
  names(w)<-gsub("summary.","",names(w))

  w<-rbind(w,w0)

  w1<-addvar(tot4,c("var","value"),"subt","sum(x)","no","overall")
  w2<-addvar(tot4,c("var"),"subt","sum(x)","no","overall1")
  w1<-plyr::join(w1,w2)

  w1$overall<-paste0(w1$overall," (",sigfig(w1$overall/w1$overall1*100,3),"%)")
  w1a<-w1[1,]
  w1a$var<-w1a$value<-"all"
  w1a$overall<-paste0(w1a$overall1," (100%)")
  w1<-rbind(w1,w1a)

  w<-plyr::join(w,w1[,c("var",  "value","overall")])
  w<-w[order(w$var),]
  w
}



#' Individual table with descriptive statse
#'
#' Listing of individual data and descriptove stats
#' @param data datset or data frame (ex:data=PKdatat)
#' @param id unique identifier
#' @param by  Stratification variable (ex: by="study")
#' @param variables Specify sorting variable to be displayed vertically. (ex: colby=by or colby="var")
#' @param rtype rounding type. (sigfig by default)
#' @param dec round decimal or number of significant figures
#' @keywords ind.tab
#' @export
#' @examples
#' ind.tab(data=dat,id="NMID",by=c("study"))
indiv.tab<-function(data,id,by,variables,rtype="sigfig",dec=3){
  id<-id#
  data<-data[,c(id,by,variables)]#[!duplicated(data$id),]
  strat1<-by#c("phase")# mandatory
  convar<-variables #mandatory
  d1<-data[,c(id,strat1,convar)]
  d1<-chclass(d1,convar,"num")
  head(d1)

  t1<-NULL
  for(i in unique(d1[,strat1])){
    d0<-d1[d1[,strat1]%in%i,]
    l<-stats::reshape(d0,
               varying = c(convar),
               v.names = "score",
               timevar = "subj",
               times = c(convar),
               #new.row.names = 1:1000,
               direction = "long")
    head(l)
    l$id<-NULL
    str(l)
    st<-plyr::ddply(l,c(by,"subj"),summarise,
              N=round(length(score),0),
              Nmiss=round(length(score[is.na(score)]),0),
              Means=sigfig(mean(score,na.rm=T),3),
              SD=sigfig(sd(score,na.rm=T),3),
              cv=sigfig(cv(score),3),
              Median=sigfig(median(score,na.rm=T),3),
              Minimum=sigfig(min(score,na.rm=T),3),
              Maximum=sigfig(max(score,na.rm=T),3),
              GeoMean=sigfig(Gmean(score),3),
              GeoCV=sigfig(Gcv(score),3))
    keep<-names(st[,3:length(names(st))])
    l1<-stats::reshape(st,
                varying = c(keep),
                v.names = "Stats",
                timevar = "Results",
                times = c(keep),
                #new.row.names = 1:1000,
                direction = "long")
    l1$id<-NULL

    w<-stats::reshape(l1,
               timevar = "subj",
               idvar = c(strat1, "Results"),
               direction = "wide")
    names(w)<-gsub("Stats.","",names(w))
    head(d0)
    x1<-setdiff(names(d0),names(w))
    x2<-setdiff(names(w),names(d0))
    w[,x1]<-""
    d0[,x2]<-""
    d0<-d0[,c(id,strat1,x2,convar)]
    #d0<-chclass(d0,convar,"num")
    if(!is.null(rtype)){
      d0<-bround(d0,convar,rtype=rtype,dec=dec)}
    t<-rbind(d0,w)
    t<-t[,c(id,strat1,x2,convar)]
    t1<-rbind(t1,t)
  }
  t1
}

#' Calculate AUC Using the Trapezoidal Method
#'
#' Calculate the area under the curve (AUC) for each subject over the time interval for dv using the trapezoidal rule.
#' nca.analysis(data,n_lambda=3,id,time,dv,ss,partialAUC="no")
#' data
#' @param data.frame containing the data to use for the AUC calculation
#' @param time	chronologically ordered time variable present in data
#' @param id	variable in data defining subject level data
#' @param dv	dependent variable used to calculate AUC present in data
#' @keywords AUC
#' @export
#' @examples
#' AUC(data, time = 'TIME', id = 'ID', dv = 'DV')

AUC<-function (data, time = "TIME", id = "ID", dv = "DV")
{
  if (any(is.na(data[[id]])))
    warning("id contains NA")
  if (any(is.na(data[[time]])))
    warning("time contains NA")
  if (any(is.na(data[[dv]])))
    warning("dv contains NA")
  data <- data[order(data[[id]], -data[[time]]), ]
  nrec <- length(data[[time]])
  data$diff <- c(data[[time]][-nrec] - data[[time]][-1], 0)
  data$meanDV <- c((data[[dv]][-1] + data[[dv]][-nrec])/2,
                   0)
  data$dAUC <- data$diff * data$meanDV
  data <- data[order(data[[id]], data[[time]]), ]
  data <- data[duplicated(data[[id]]), ]
  AUC <- aggregate.data.frame(data$dAUC, by = list(data[[id]]),
                              FUN = sum)
  names(AUC) <- c(id, "AUC")
  return(AUC)
}

#' Derive Common NCA parameters using single and multiple profiles
#'
#' nca.cal()
#' @param data datset or data frame (ex:data=PKdatat)
#' @param id unique subject identifier
#' @param n_lambda  number of points for estimating the Lambda
#' @param time Sampling time after dose (TAD)
#' @param dv Concentration
#' @param dosing.regimen PK profile profile flag (ex: single, sd, md ,ss).As vector or numeric. Note that only one prefile per dose is to be analyzed.
#' @param label.for.sd Label to identify sd profile used in dosing.regimen. Need to compute the effective half-life
#' @param label.for.ss Label to identify ss profile used in dosing.regimen. Need to compute the effective half-life
#' @param tau Dose interval. As vector or numeric.
#' @param dose Dose administered. As vector or numeric. Required to estimate CL and Vss 
#' @param partialAUC Time interval for partial AUC. Ex: c(0,6,0,12,6,12) for AUC0-6, AUC0-12 and AUC6-12
#' @param partialConc Concentration at particular time (Ex:c(1,4) for concentration after 1 and 4 h)
#' @keywords nca.cal
#' @export
#' @examples 
#' p<-nca.cal(data=data, n_lambda = 3, id = "id", time = "time", dv = "dv", 
#' tau="ii",dose="dose",dosing.regimen = "ss",label.for.sd="single", partialAUC = NULL, partialConc = NULL)
#' 

nca.cal<-function (data, n_lambda = 3, id = "id", time = "time", dv = "dv",sort.by=NULL, ss.variable = "ss", sd_label_in_ss = NULL, ss_label_in_ss = NULL,tau = NULL, dose = NULL, partialAUC = NULL, partialConc = NULL) 
{
  dat <- data
  dat$time1 <- dat[, time]
  dat$time <- dat[, time]
  dat[, "tad"] <- NULL
  dat$id <- dat[, id]
  dat$dv <- dat[, dv]
  if (is.numeric(ss.variable)) {
    dat$ss <- ss.variable
  } else {
    dat$ss <- dat[,ss.variable]
  }
  if (is.numeric(dose)) {
    dat$dose <- dose
  } else {
    if (!is.null(dose)) {
      dat$dose <- dat[, dose]
    } else {
      dat$dose <- "dose required"
    }
  }
  if (is.numeric(tau) & !is.null(tau)) {
    dat$tau <- tau
  }else{
    if (!is.null(tau)) {
      dat$tau <- dat[, tau]
    }else {dat}
  }
  dat$idss <- paste(dat$id, dat$ss, sep = "-")
  if(!is.null(sort.by)){
    for(i in sort.by){
      dat$idss <- paste(dat$idss, dat[,i], sep = "-")  
    }}else{dat$idss<-dat$idss}
  
  dat <- dat[order(dat$idss, dat$time), ]
  dat <- addvar(dat, "idss", "time", "min(x)", "yes", "tad")
  dat$time <- dat$time - dat$tad
  idss <- nodup(dat, c("idss", "id", "ss", "dose",sort.by), "var")
  dat$dvtm <- dat$dv * dat$time
  datauc <- dat
  auclast <- AUC(datauc, time = time, id = "idss", dv = dv)
  names(auclast) <- c("idss", "AUClast")
  auclast <- plyr::join(auclast, idss)
  aucmlast <- AUC(datauc, time = time, id = "idss", dv = "dvtm")
  names(aucmlast) <- c("idss", "AUMClast")
  aucmlast <- plyr::join(aucmlast, idss)
  head(dat)
  dat$tad1 <- dat$time
  aucpart <- NULL
  if (!is.null(partialAUC)) {
    nauc <- length(partialAUC)/2
    for (z in seq(1, length(partialAUC), 2)) {
      tm1 <- partialAUC[z]
      tm2 <- partialAUC[z + 1]
      auc <- AUC(dat[dat[, "tad1"] >= tm1 & dat[, "tad1"] <= 
                       tm2, ], time = "tad1", id = "idss", dv = dv)
      names(auc) <- c("idss", paste0("AUC", tm1, "-", tm2))
      if (z == 1) {
        aucpart <- rbind(aucpart, auc)
      } else {
        aucpart[, paste0("AUC", tm1, "-", tm2)] <- auc[, 
                                                       2]
      }
    }
    aucpart <- join(aucpart, idss)
    aucpart$idss <- NULL
    aucpart
  } else {
    aucpart <- NULL
  }
  Cpart <- NULL
  if (!is.null(partialConc)) {
    nauc <- length(partialConc)
    for (z in 1:length(partialConc)) {
      tm1 <- partialConc[z]
      partc <- dat[dat[, "tad1"] == tm1, c("idss", "dv")]
      names(partc) <- c("idss", paste0("C", tm1))
      if (z == 1) {
        Cpart <- rbind(Cpart, partc)
      } else {
        Cpart[, paste0("C", tm1)] <- partc[, 2]
      }
    }
    Cpart
    Cpart <- join(Cpart, idss)
    Cpart$idss <- NULL
    Cpart
  }else {
    Cpart <- NULL
  }
  if (n_lambda != "no") {
    dat1 <- dat
    dat1$tmp <- seq(nrow(dat1))
    dat1 <- addvar(dat1, "idss", "tmp", "max(x)", "yes", 
                   "tmp2")
    head(dat1)
    dat1$tmp <- dat1$tmp2 - dat1$tmp
    dat1 <- dat1[dat1$tmp < n_lambda, ]
    str(dat1)
    dat1[, c("idss", "time", "dv")]
    test1 <- ddply(dat1[, c("idss", "time", "dv")], .(idss), 
                   summarize, interc = lm(log(dv) ~ time)$coef[1], Lambda = lm(log(dv) ~ time)$coef[2] * -1, R2 = summary(lm(log(dv) ~ time))$r.squared, HL = (log(2)/lm(log(dv) ~ time)$coef[2]) *  -1, that = max(time))
    test1$n_lambda <- n_lambda
    test1$Clast_hat <- with(test1, exp(-Lambda * that + interc))
    test1a <- ddply(dat1[, c("idss", "time", "dvtm")], .(idss), 
                    summarize, intercc = lm(log(dvtm) ~ time)$coef[1], 
                    Lambdac = lm(log(dvtm) ~ time)$coef[2] * -1, 
                    R2c = summary(lm(log(dvtm) ~ time))$r.squared, 
                    HLc = (log(2)/lm(log(dvtm) ~ time)$coef[2]) * -1, 
                    thatc = max(time))
    test1a$n_lambdac <- n_lambda
    test1a$Clast_hatc <- with(test1a, exp(-Lambdac * thatc + 
                                            intercc))
  }else {
    test1 <- NULL
  }
  if (TRUE %in% c(test1$HL < 0)) {
    test1$Warning.HL.Negative = ifelse(test1$HL, "yes", "")
  }
  max <- ddply(dat[, c("idss", "dv", "time", "time1")], .(idss), 
               summarize, Cmax = max(dv), Tmax = time1[dv == max(dv)], 
               Cmin = min(dv[time >= time[dv == max(dv)]]), Tlast = max(dat$time1), 
               Clast = dv[time == max(time)])
  maxa <- ddply(dat, .(idss), summarize, Clastc = dvtm[time == 
                                                         max(time)])
  head(dat)
  test <- plyr::join(max, idss)
  test <- plyr::join(test, maxa)
  test <- plyr::join(test, auclast)
  test <- plyr::join(test, aucmlast)
  if (n_lambda != "no") {
    test <- join(test, test1)
    test <- join(test, test1a)
    test$AUCinf_obs <- abs(as.numeric(as.character(test$AUClast)) + 
                             test$Clast/test$Lambda)
    test$AUMCinf_obs <- abs(as.numeric(as.character(test$AUMClast)) + 
                              test$Clastc/test$Lambdac)
    test$AUCinf_pred <- abs(as.numeric(as.character(test$AUClast)) + 
                              test$Clast_hat/test$Lambda)
    test$AUMCinf_pred <- abs(as.numeric(as.character(test$AUMClast)) + 
                               test$Clast_hatc/test$Lambdac)
    test$MRTlast <- test$AUMClast/test$AUClast
    test$MRTobs <- test$AUMCinf_obs/test$AUCinf_obs
    test$MRTpred <- test$AUMCinf_pred/test$AUCinf_pred
  }else {
    test$MRTlast <- test$AUMClast/test$AUClast
  }
  if (!is.null(Cpart)) {
    test <- plyr::join(test, Cpart)
  }
  if (!is.null(aucpart)) {
    test <- plyr::join(test, aucpart)
  }
  test$idss <- test$interc <- test$that <- NULL
  n <- names(test)
  n <- n[!n %in% c("id", "ss")]
  test <- test[, c("id", "ss", n)]
  if ("AUCinf_pred" %in% names(test) & is.numeric(test$dose)) {
    test$CLobs <- with(test, dose/AUCinf_obs)
    test$CLpred <- with(test, dose/AUCinf_pred)
    test$Vss_obs <- with(test, CLobs * AUMCinf_obs)
    test$Vss_pred <- with(test, CLpred * AUMCinf_pred)
    test <- test[, !names(test) %in% c("Lambdac", "R2c", 
                                       "HLc", "thatc", "n_lambdac", "Clast_hatc", "intercc")]
    names(test)[names(test) == "HL"] <- "HL_Lambda_z"
  }else {
    test$CLobs <- "Dose required"
    test$CLpred <- "Dose required"
    test$Vss_obs <- "Dose required"
    test$Vss_pred <- "Dose required"
  }
  if (!is.null(ss_label_in_ss) | !is.null(sd_label_in_ss)) {
    if (!is.null(ss_label_in_ss) & !is.null(sd_label_in_ss)) {
      head(dat)
      ss1 <- dat[dat$ss == ss_label_in_ss & dat$time <= dat$tau, 
                 ]
      single <- dat[dat$ss == sd_label_in_ss & dat$time <= 
                      dat$tau, ]
    }
    if (!is.null(sd_label_in_ss) & is.null(ss_label_in_ss)) {
      ss1 <- dat[dat$ss != sd_label_in_ss & dat$time <= dat$tau, 
                 ]
      single <- dat[dat$ss == sd_label_in_ss & dat$time <= 
                      dat$tau, ]
    }
    if (is.null(sd_label_in_ss) & !is.null(ss_label_in_ss)) {
      ss1 <- dat[dat$ss == ss_label_in_ss & dat$time <= dat$tau, 
                 ]
      single <- dat[dat$ss != ss_label_in_ss & dat$time <= 
                      dat$tau, ]
    }
    
    auctau <- AUC(ss1, time = "time", id = "idss", dv = "dv")
    names(auctau) <- c("idss", "AUCtau")
    aucsdtau <- AUC(single, time = "time", id = "idss", dv = "dv")
    names(aucsdtau) <- c("idss", "AUCsd_tau")
    test<-join(test,idss,type="left")
    #test$idss <- paste(test$id, test$ss, sep = "-")
    test <- join(test, aucsdtau, type = "left")
    test <- join(test, auctau, type = "left")
    ident <- nodup(test, c("id", "idss"), "var")
    a <- join(auctau, ident)
    a$idss <- NULL
    b <- join(aucsdtau, ident)
    b$idss <- NULL
    EHL <- join(a, b)
    EHL$Rc <- with(EHL, AUCtau/AUCsd_tau)
    tau1 <- nodup(single, c("id", "tau"), "var")
    EHL <- join(EHL, tau1)
    EHL$EHL <- with(EHL, log(2) * tau/(log(Rc/(Rc - 1))))
    test <- join(test, EHL[, c("id", "tau", "EHL")], type = "left")
  }
  if ("AUCinf_obs" %in% names(test)) {
    single <- dat[dat$time <= dat$tau, ]
    aucsdtau <- AUC(single, time = "time", id = "idss", dv = "dv")
    names(aucsdtau) <- c("idss", "AUCsd_tau")
    head(test)
    test$idss <- paste(test$id, test$ss, sep = "-")
    test <- join(test, aucsdtau, type = "left")
    test$Rc <- with(test, AUCinf_obs/AUCsd_tau)
    test$EHL <- with(test, log(2) * tau/(log(Rc/(Rc - 1))))
    test <- join(test, nodup(dat, c("idss", "tau"), "var"))
  } else {
    if (!c("tau") %in% names(dat)) {
      test$EHL <- "AUCinf or TAU is missing"
    }
  }
  keep <- names(test)[!names(test) %in% c("Lambdac", "R2c", 
                                          "HLc", "thatc", "n_lambdac", "Clast_hatc", "intercc", 
                                          "idss", "Rc")]
  test <- test[, keep]
}
###########################
