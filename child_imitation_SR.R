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
  parent_inds = subj1$datavyu_bin[which(subj1$who_relation == "parent_on")]
  for (i in 1:nrow(subj1)){
    if(subj1$who_relation[i] == "child_on" & length(parent_inds)>0){
      if(any(subj1$datavyu_bin[i]>parent_inds)){ 
        subj1$on_imitation[i] = 1}
    }
  }
  
  subj1$in_imitation = 0
  parent_inds = subj1$datavyu_bin[which(subj1$who_relation == "parent_in")]
  for (i in 1:nrow(subj1)){
    if(subj1$who_relation[i] == "child_in" & length(parent_inds)>0){
      if(any(subj1$datavyu_bin[i]>parent_inds)){ 
        subj1$in_imitation[i] = 1}
    }
  }
  alldata = rbind(alldata,subj1)
}

alldata = alldata %>%
  mutate(imitation = ifelse(in_imitation == 1 | on_imitation == 1, 1,0))

Semantically_Rich = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 23, 26, 27, 28, 29, 32)
Abstract = c(15, 16, 24, 25, 30 ,31)

data2 = alldata %>%
  mutate(SR = ifelse(placed_object %in% Semantically_Rich & base_object %in% Semantically_Rich, 1, 0)) %>%
  filter(who_relation == "child_on" | who_relation == "child_in")
  
data_sum = data2 %>%
  group_by(SR) %>%
  summarise(n = n(), proportion = n()/nrow(data2))

install.packages("ggplot2")  
library(ggplot2)  

data_sum$SR = factor(data_sum$SR, levels = c(0,1), labels = c("Abstract", "Semantically Rich"))

ggplot(data = data_sum, aes(x = SR, y = proportion)) +
  geom_col() + theme_classic( ) + xlab("Type of Toy") + ylab("Proportion of Imitations Made by Infants")



