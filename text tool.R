
c<-c("txtc","This is ::b:i"," X","2::e:u" ,"bold::b:i:s","fwb::e","this::i","fsti::b:i")


library(ReporteRs)
if()
all<-""
z11<-z22<-z33<-z44<-cc<-""
for(i in 2:length(c)){
if(length(grep(":i",sub(".*:i",":i", c[i])))!=0){
  z1="italic"}else{z1="normal"}
  z11<-c(z11,z1)
if(length(grep(":b",sub(".*:b", ":b", c[i])))!=0){
  z2="bold"}else{z2="normal"}
  z22<-c(z22,z2)
if(length(grep(":s",sub(".*:s", ":s", c[i])))!=0){
  z3="subscript"}
  if(length(grep(":e",sub(".*:e", ":e", c[i])))!=0){
      z3="superscript"}else{z3="baseline"}
  z33<-c(z33,z3)
if(length(grep(":u",sub(".*:u", ":u", c[i])))!=0){
  z4=T}else{z4=F}
  z44<-c(z44,z4)
  if(length(grep("::",sub(".*::", "::", c[i])))==0){
    c1<-c[i]}else{c1<-gsub(sub(".*::", "::",c[i]),"",c[i])}
  cc<-c(cc,c1)
all[[i]]<-paste0("font.size = 12 ,font.weight =z22[",i,"],font.style =z11[",i,"],underlined =",z44[i],",vertical.align =z33[",i,"])")
}

z1<-""
  for(i in 2:(length(c))){
  if(i<length(c)){
  z1<-paste0(z1,paste0("pot(cc[",i,"],textProperties(",all[[i]],")+"))
  }else{z1<-paste0(z1,paste0("pot(cc[",i,"],textProperties(",all[[i]],")"))}
  }
b <- function(x) {}
body(b)<-parse(text =z1)

text<-b()
doc<-docx()
doc <-addParagraph(doc,text)


writeDoc(doc, file ="doc.docx")
