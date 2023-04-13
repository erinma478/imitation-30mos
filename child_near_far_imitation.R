library(tidyr)
library(dplyr)
MyData = base_objects_freeplay %>%
  select(subject,datavyu_bin,who_relation,placed_object,base_object) %>%
  filter(!is.na(base_object))
subj_list = unique(MyData$subject)

alldata = c()
for(s in 1:length(subj_list)){
  subj1 = MyData[which(MyData$subject == subj_list[s]),]
  subj1$on_imitation = 0
  subj1$near_far = ""
  parent_inds = subj1$datavyu_bin[which(subj1$who_relation == "parent_on")]
  parent_placed = subj1$placed_object[which(subj1$who_relation == "parent_on")]
  parent_base = subj1$base_object[which(subj1$who_relation == "parent_on")]
  
  for (i in 1:nrow(subj1)){
    if(subj1$who_relation[i] == "child_on" & length(parent_inds)>0){
      if(any(subj1$datavyu_bin[i]>parent_inds)){ 
        
        subj1$on_imitation[i] = 1
        child_placed = subj1$placed_object[i]
        child_base = subj1$base_object[i]
        
        if(child_placed == parent_placed[1] & child_base == parent_base[1]){
          subj1$near_far[i] = "near"} else {
            subj1$near_far[i] = "far"}
      }
    }
  }
  
  subj1$in_imitation = 0
  parent_inds = subj1$datavyu_bin[which(subj1$who_relation == "parent_in")]
  parent_placed = subj1$placed_object[which(subj1$who_relation == "parent_in")]
  parent_base = subj1$base_object[which(subj1$who_relation == "parent_in")]
  
  for (i in 1:nrow(subj1)){
    if(subj1$who_relation[i] == "child_in" & length(parent_inds)>0){
      if(any(subj1$datavyu_bin[i]>parent_inds)){ 
        subj1$in_imitation[i] = 1
        child_placed = subj1$placed_object[i]
        child_base = subj1$base_object[i]
        
        if(child_placed == parent_placed[1] & child_base == parent_base[1]){
          subj1$near_far[i] = "near"} else {
            subj1$near_far[i] = "far"}
        
      }
    }
  }
  alldata = rbind(alldata,subj1)
} 

alldata = alldata %>%
  mutate(imitation = ifelse(in_imitation == 1 | on_imitation == 1, 1,0))

data2 = alldata %>%
  filter(imitation == 1)

data_sum = data2 %>%
  group_by(near_far) %>%
  summarise(n = n(), proportion = n()/nrow(data2))

install.packages("ggplot2")  
library(ggplot2)  

ggplot(data = data_sum, aes(x = near_far, y = proportion)) +
  geom_col() + 
  theme_classic( ) + 
  xlab("Type of Imitation") + 
  ylab("Proportion of Imitations Made by Infants")

