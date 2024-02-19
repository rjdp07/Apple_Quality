library(tidyverse)
library(data.table)

gd_data = fread("Cleaned_DS_Jobs.csv") %>% as_tibble()

#Check cardinality of the data
#Lot of Unique values for Job Title
gd_data$`Job Title` %>% unique()


#Can we create a new column for Job Title with fewer categories??
gd_data_mod = gd_data %>% 
  mutate(
    DS_Title = case_when(
      str_detect(`Job Title`,"Science|Scientist|scientist|SCIENTIST|Model|Modeler|Data Management") ~ "Data Scientist",
      str_detect(`Job Title`,"Analyst|Analytics") ~ "Data Analyst",
      str_detect(`Job Title`,"Engineer|Architect") ~ "Data Engineer",
      str_detect(`Job Title`,"Machine Learning") ~ "Machine Learning Engineer",
      .default =`Job Title` 
    )
  ) 
gd_data_mod %>% 
  count(
    DS_Title, sort = TRUE
  )

gd_data_mod %>% 
  group_by(
    DS_Title, seniority
  ) %>% 
  reframe(
    Count = n()
  )

gd_data_mod %>% 
  filter(DS_Title == "Data Analyst" & seniority == "na")


gd_data_mod %>% 
  group_by(
    DS_Title, python
  ) %>% 
  reframe(
    Count = n()
  )

gd_data_mod %>% 
  ggplot(aes(x = DS_Title, y = avg_salary, fill = as.factor(python) )) +
  geom_boxplot()


gd_data_mod %>% 
  ggplot(aes(x = as.factor(big_data), y = avg_salary )) +
  geom_boxplot()


gd_data_mod %>% 
  count(Industry, sort = TRUE)

gd_data_mod = gd_data_mod %>% 
  filter(Industry != "-1")


gd_data_mod %>% 
  group_by(Industry) %>% 
  reframe(
    avg_salary = mean(avg_salary,na.rm = TRUE) 
  ) %>% 
  ggplot(aes(x = reorder(Industry, avg_salary), y = avg_salary)) +
  coord_flip()+
  geom_bar(stat = "identity")


