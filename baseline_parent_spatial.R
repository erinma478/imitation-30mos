# read in data (base_objects_freeplay)
# go to File > Import Dataset > From Excel... 

#assign dataset to MyData
MyData = base_objects_freeplay

# data2 = filter out rows with child_in or child_on / keep only rows with parent_in or parent_on
# keep only rows where base_object in NOT "NA"
# keep only these columns: who_relation, placed_object & base_object
library(tidyr)
library(dplyr)
data2 = MyData %>%
  select(who_relation,placed_object,base_object) %>%
  filter(who_relation == "parent_on" | who_relation == "parent_in") %>%
  filter(!is.na(base_object))

# assign to semantically rich or abstract
Semantically_Rich = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 23, 26, 27, 28, 29, 32)
Abstract = c(15, 16, 24, 25, 30 ,31)

data2 = data2 %>%
  mutate(SR = ifelse(placed_object %in% Semantically_Rich & base_object %in% Semantically_Rich, 1, 0),
         PlacedS = ifelse(placed_object %in% Semantically_Rich, 1, 0),
         EitherS = ifelse(placed_object %in% Semantically_Rich | base_object %in% Semantically_Rich, 1, 0
         ))
#onlySR = data2 %>%
#  filter(placed_object %in% Semantically_Rich & base_object %in% Semantically_Rich)

data_sum = data2 %>%
  group_by(SR) %>%
  summarise(n = n(), proportion = n()/nrow(data2))

install.packages("ggplot2")
library(ggplot2)

data_sum$SR = factor(data_sum$SR, levels = c(0,1), labels = c("Abstract", "Semantically Rich"))
ggplot(data = data_sum, aes(x = SR, y = proportion)) +
  geom_col()

data_sum2 = data2 %>%
  group_by(PlacedS) %>%
  summarise(n = n(), proportion = n()/nrow(data2))

data_sum2$PlacedS = factor(data_sum2$PlacedS, levels = c(0,1), labels = c("Abstract", "Semantically Rich"))
ggplot(data = data_sum2, aes(x = PlacedS, y = proportion)) +
  geom_col()

data_sum3 = data2 %>%
  group_by(EitherS) %>%
  summarise(n = n(), proportion = n()/nrow(data2))

data_sum3$EitherS = factor(data_sum3$EitherS, levels = c(0,1), labels = c("Abstract", "Semantically Rich"))
ggplot(data = data_sum3, aes(x = EitherS, y = proportion)) +
  geom_col()
