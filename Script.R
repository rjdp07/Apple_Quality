library(tidyverse)
library(data.table)
library(factoextra)

apple_data = fread("apple_quality.csv") %>% as_tibble()

#Check ID Cardinalities
nrow(apple_data) == length(unique(apple_data$A_id))
#True


#Check count of response variable
apple_data %>% 
  count(Quality)


apple_data = apple_data %>% 
  mutate(
    Acidity = as.numeric(Acidity)
  ) %>%
  filter(!is.na(Acidity))
#Check Pairs

pairs(apple_data[2:8])

#Looks like none of the numerical values have some sort of linear relationship with the other

#Check distribution
apple_data %>% 
  ggplot(aes(x = Size)) +
  geom_histogram()
#Size is normal, visually. Can we test it?
#Using Shapiro-Wilk Test
shapiro.test(apple_data$Size)

#Check distribution based on quality
apple_data %>% 
  ggplot(aes(x = Size,fill = Quality)) +
  geom_histogram()


apple_data %>% 
  ggplot(aes(x = Weight,fill = Quality)) +
  geom_histogram()

apple_data %>% 
  ggplot(aes(x = Sweetness,fill = Quality)) +
  geom_histogram()

apple_data %>% 
  ggplot(aes(x = Crunchiness,fill = Quality)) +
  geom_histogram()

apple_data %>% 
  ggplot(aes(x = Juiciness,fill = Quality)) +
  geom_histogram()

#Cluster Analysis

#Use K-Means
kmeans(apple_data[2:8],centers = 2, iter.max = 100, nstart = 100)

fviz_nbclust(apple_data[2:8],kmeans,method = "wss")
fviz_nbclust(apple_data[2:8],kmeans,method = "silhouette")
fviz_nbclust(apple_data[2:8],kmeans,method = "gap_stat")



#Create cluster Plot
fviz_cluster(kmeans(apple_data[2:8], centers = 2, iter.max = 100, nstart = 100), data = apple_data[2:8])


clusters = kmeans(apple_data[2:8],centers = 2, iter.max = 100, nstart = 100)
apple_data = apple_data %>% 
  mutate(
    Cluster = clusters$cluster
  )

apple_data %>% 
  ggplot(aes(x = Size,y = Acidity, col = as.factor(Cluster))) +
  geom_point()


apple_data %>% 
  group_by(Quality,Cluster) %>% 
  reframe(
    Member_Count = n()
  )


bad_v1 = apple_data %>% 
  filter(Cluster == 1 & Quality == "bad")

bad_v1 %>% 
  ggplot(aes(x = Size)) +
  geom_histogram()


apple_data = apple_data %>% 
  mutate(
    Cluster_Quality = paste(Quality,Cluster,sep = "_")
  )
apple_data %>% 
  ggplot(aes(x = Cluster_Quality, y = Acidity)) +
  geom_boxplot()



