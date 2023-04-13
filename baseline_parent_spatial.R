# Step 1: read in data (base_objects_freeplay)
#   in the right-hand corner of your screen, find base_objects_freeplay and double-click / right-click
#   then select Import Dataset

# assign base_objects_freeplay to MyData
MyData = base_objects_freeplay

# Step 2: install and load required packages
install.packages("tidyr")  # this installs - you only need to run this once
install.packages("dplyr") # this installs - you only need to run this once

library(tidyr) # this opens the package - you need to run this every time you open R
library(dplyr) # this opens the package - you need to run this every time you open R

# Step 3: select a subset of the data that we care about
#   keep only rows with parent_in or parent_on
#   keep only rows where base_object in NOT "NA"
#   keep only these columns: who_relation, placed_object & base_object
data2 = MyData %>%
  select(who_relation,placed_object,base_object) %>%
  filter(who_relation == "parent_on" | who_relation == "parent_in") %>%
  filter(!is.na(base_object))

# Step 3: assign objects to semantically rich or abstract categories
Semantically_Rich = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 23, 26, 27, 28, 29, 32)
Abstract = c(15, 16, 24, 25, 30 ,31)

data2 = data2 %>%
  mutate(SR = ifelse(placed_object %in% Semantically_Rich & base_object %in% Semantically_Rich, 1, 0),
         PlacedS = ifelse(placed_object %in% Semantically_Rich, 1, 0),
         EitherS = ifelse(placed_object %in% Semantically_Rich | base_object %in% Semantically_Rich, 1, 0
         ))

# Step 4: calculate the proportion of parent spatial relations in each category
data_sum = data2 %>%
  group_by(SR) %>%
  summarise(n = n(), proportion = n()/nrow(data2))

data_sum2 = data2 %>%
  group_by(PlacedS) %>%
  summarise(n = n(), proportion = n()/nrow(data2))

data_sum3 = data2 %>%
  group_by(EitherS) %>%
  summarise(n = n(), proportion = n()/nrow(data2))

# Step 5: Plot proportions by object category
install.packages("ggplot2")  # this installs - you only need to run this once
library(ggplot2)  # this opens the package - you need to run this every time you open R

# give category labels
data_sum$SR = factor(data_sum$SR, levels = c(0,1), labels = c("Abstract", "Semantically Rich"))
# create bar plot
ggplot(data = data_sum, aes(x = SR, y = proportion)) +
  geom_col()+
  theme_classic()+
  xlab("Toy Type")+
  ylab("Proportion of spatial relations made by parent")

# give category labels
data_sum2$PlacedS = factor(data_sum2$PlacedS, levels = c(0,1), labels = c("Abstract", "Semantically Rich"))
# create bar plot
ggplot(data = data_sum2, aes(x = PlacedS, y = proportion)) +
  geom_col()

# give category labels
data_sum3$EitherS = factor(data_sum3$EitherS, levels = c(0,1), labels = c("Abstract", "Semantically Rich"))
# create bar plot
ggplot(data = data_sum3, aes(x = EitherS, y = proportion)) +
  geom_col()+
  theme_classic()+
  xla
