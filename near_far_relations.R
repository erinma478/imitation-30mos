# read in base object df
# change this to your local path after downloading the google doc as a csv
df = read.csv("C:/Users/erinm/Downloads/base_objects_master - Sheet1.csv")
df$placed_object = as.character(df$placed_object)
df$base_object = as.character(df$base_object)

# subset to only row that have base objects
df2 = df[which(df$base_object != ""),]
df2$relation = ""
for(i in 1:nrow(df2)){
  rel = df2$who_relation[i]
  end = nchar(rel)
  df2$relation[i] = substr(rel,end-1,end)
}
# get unique subjects
usubs = unique(df2$subject)

# if you need to re-run the loop, run this line first
if(exists("Master")){rm(Master)}
# loop through to look for generalization
for(i in 1:length(usubs)){
  # subset to this subject
  subdf = df2[which(df2$subject == usubs[i]),]
  # stick together total play time? or do generalization per each day?
  
  rels = c("in","on")
  for(j in 1:2){
    subdf2 = subdf[which(subdf$relation == rels[j]),]
    if(nrow(subdf2)>1){
    subdf2$instance = 1:nrow(subdf2)
    subdf2$placed = 0
    subdf2$base = 0
    # record placed & base for first in/on
    p1 = subdf2$placed_object[1]
    b1 = subdf2$base_object[1]
    # for the rest of ins/ons, see if new base, new placed or both
    samep = which(subdf2$placed_object == p1)
    sameb = which(subdf2$base_object == b1)
    subdf2$placed[samep] = 1
    subdf2$base[sameb] = 1
    
    if(!exists("Master")){Master = subdf2} 
    else{Master = rbind(Master,subdf2)}
    }
  } 
}

Master$nsims = Master$placed + Master$base
Master$nsims = factor(Master$nsims, labels = c("far","near","exact"))
# defines as far = both placed and base differ, exact = both same
# near = either place or base differ from first relation

library(ggplot2)
ggplot(Master, aes(x = instance, y = nsims, color = nsims))+
  geom_jitter()+
  facet_wrap(~relation)


