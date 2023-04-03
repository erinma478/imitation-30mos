MyData=read.csv(file="X:/1D_LooxcieKidsDay/6_ToyPlay/Analysis/Spatial_Master_7_28_22.csv", header=T)
MyData = MyData[!is.na(MyData$X),]

subjects= unique(MyData$subject)
subjects

library(tidyr)
library(dplyr)

MyData$container_c = 0
MyData$supporter_c = 0
MyData$handleable_c = 0
MyData$container_p = 0
MyData$supporter_p = 0
MyData$handleable_p = 0

container=c(1,11,13,14,16,17,24)
supporter=c(11,12,13,14,16,21,22,25)
handleable=c(2,3,4,5,6,7,8,9,10,15,18,19,20,22,23)

for(i in 1:nrow(MyData)){
  child = MyData$child_in_hand._[i]
  parent = c(MyData$parent_in_hand._[i],MyData$parent2_in_hand._[i])
  
  cobj = unlist(strsplit(child,","))
  pobj = unlist(strsplit(parent,","))
  if(any(cobj%in%container)){
    MyData$container_c[i] = 1}
  if(any(pobj%in%container)){
    MyData$container_p[i] = 1}
  
  if(any(cobj%in%supporter)){
    MyData$supporter_c[i] = 1}
  if(any(pobj%in%supporter)){
    MyData$supporter_p[i] = 1}
  
  if(any(cobj%in%handleable)){
    MyData$handleable_c[i] = 1}
  if(any(pobj%in%handleable)){
    MyData$handleable_p[i] = 1}
}

MyData$child_in = ""
MyData$child_on = ""
MyData$parent_in = ""
MyData$parent_on = ""

for(i in 1:nrow(MyData)){
  inobj = MyData$in_objects._[i]
  onobj = MyData$on_objects._[i]
  child = MyData$child_in_hand._[i]
  parent = c(MyData$parent_in_hand._[i],MyData$parent2_in_hand._[i])
  
  inobj = unlist(strsplit(inobj,","))
  onobj = unlist(strsplit(onobj,","))
  child = unlist(strsplit(child,","))
  parent = unlist(strsplit(parent,","))
  
  ci = inobj[inobj %in% child]
  pi = inobj[inobj %in% parent]
  co = onobj[onobj %in% child]
  po = onobj[onobj %in% parent]
  
  if(length(ci) > 0){MyData$child_in[i] = ci[1]}
  if(length(co) > 0){MyData$child_on[i] = co[1]}
  if(length(pi) > 0){MyData$parent_in[i] = pi[1]}
  if(length(po) > 0){MyData$parent_on[i] = po[1]}
}
for(i in 1:nrow(MyData)){
  if(is.na(MyData$child_in[i])){MyData$child_in[i] = ""}
  if(is.na(MyData$child_on[i])){MyData$child_on[i] = ""}
  if(is.na(MyData$parent_in[i])){MyData$parent_in[i] = ""}
  if(is.na(MyData$parent_on[i])){MyData$parent_on[i] = ""}
}
# see if imitations by child in 3-second window
MyData$in_imit = 0
MyData$on_imit = 0
MyData$in_kind = 0
MyData$on_kind = 0

for(i in 1:(nrow(MyData)-3)){
if(MyData$subject[i] == MyData$subject[i+3]){
  a = MyData$parent_in[i]
  b = MyData$parent_on[i]
  c = MyData$child_in[i:(i+3)]
  d = MyData$child_on[i:(i+3)]

  if(a != "" & any(c != "")){
     MyData$in_imit[i] = 1
      if(any(c %in% a)){ MyData$in_kind[i] = 1} # near relation
      else {MyData$in_kind[i] = 2} # far relation
    }
  if(b != "" & any(d != "")){
     MyData$on_imit[i] = 1
      if(any(d %in% b)){ MyData$on_kind[i] = 1}
      else {MyData$on_kind[i] = 2}
    }
  }
}

ondata = MyData %>% 
  group_by(subject,on_kind) %>%
  summarise(n = sum(on_imit))
ondata = ondata[which(ondata$on_kind != 0),]

indata = MyData %>% 
  group_by(subject,in_kind) %>%
  summarise(n = sum(in_imit))
indata = indata[which(indata$in_kind != 0),]

library(ggplot2)
ggplot(indata, aes(x=n, fill=as.factor(subject) )) + 
  geom_histogram() +
  facet_wrap(~in_kind)+
  xlab("# in imitations") +
  theme_bw()

ggplot(ondata, aes(x=n, fill=as.factor(subject) )) + 
  geom_histogram() +
  facet_wrap(~on_kind)+
  xlab("# on imitations") +
  theme_bw()
