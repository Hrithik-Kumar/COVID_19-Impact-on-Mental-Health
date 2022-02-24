library(readxl)
install.packages("ggpubr")
library("ggpubr")
install.packages("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
require(dplyr)
install.packages("umx")
install.packages("psych")
library(psych)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

data = read_excel("UTM 5th Semester/STA 304/Project/STA304_Group_10_Responses.xlsx")

# Processing the data. Removing first two columns.
df <- data[-c(1,2)]

# Ordering the data by Gender
df = df[order(df$Gender),]
View(df)

# Performing Cronbach's alpha to check internal consistency.
alpha(df[c(2:11)]) #raw_alpha = 0.88. This also tells us to remove the colum "Fear of contracting covid-19"

#Removing the column gives us raw_alpha = 0.90
df = df[c(-10)] 
alpha(df[c(2:10)]) # Raw alpha of 0.9
View(df)

# Using PCA(Prinipal Component Analysis) to factor out the variables which explains the most variance.
df.pca = prcomp(df[,c(2:10)], center = TRUE,scale. = TRUE)
summary(df.pca)
df.group <- c(rep("Female", 21), rep("Male",26))

ggbiplot(df.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(df), groups=df.group) +
  scale_colour_manual(name="Gender", values= c("red3", "dark blue"))+
  ggtitle("PCA of Sampled data")+
  theme_minimal()+
  theme(legend.position = "bottom")


# Using Boxplots to visualize the difference between two time periods of all the variables.
# Plot each variable by group and color by group

var_plot <- function(before,during,var_name){
  my_data <- data.frame( 
    group = rep(c("before", "during"), each = 47),
    var_vec = c(before,  during)
  )
  
  ggboxplot(my_data, x = "group", y = "var_vec", 
            color = "group", palette = c("#00AFBB", "#E7B800"),
            order = c("before", "during"),
            ylab = var_name, xlab = "Groups")
  
}

result <- function(during,before){
  res <- t.test(during, before, paired = TRUE,alternative = "greater")
  CI = t.test(during, before, paired = TRUE,conf.level = 0.95)
  return (list(res,CI))
}
# n > 30 so using CLT, we can assume that sampling distribution of differences of the variables before and during COVID follow Normal distribution
# Since we can assume normality for differences, we can use two sample paired t-test to analyze the data.
# d = var_during_covid - var_before_covid.
# Ho = d_bar = 0. Ha: d_bar > 0.

# Mental health
before = df$`Mental health before Covid-19`
during = df$`Mental health during Covid-19`
var_plot(before,during,"Mental_health")

# check for significance and CI
res = result(during,before)
res




# Stress Level
before = df$`Stress levels before Covid-19`
during = df$`Stress levels during Covid-19`
var_plot(before,during,"Stress_level")

# check for significance and CI
res = result(during,before)
res

# Anxiety Level
before = df$`Anxiety levels before Covid-19`
during = df$`Anxiety levels during Covid-19`
var_plot(before,during,"Anxiety_level")

# check for significance and CI
res = result(during,before)
res

# Depression Level
before = df$`Depression levels before Covid-19`
after = df$`Depression levels during Covid-19`
var_plot(before,during,"Depression_level")

# check for significance and CI
res = result(during,before)
res

# Contracting Covid-19
personal_fear = df$`Fear of contracting COVID-19.`
family_fear = df$`Fear of loved ones contracting COVID-19.`
my_data <- data.frame( 
  group = rep(c("personal_fear", "family_fear"), each = 47),
  var_vec = c(personal_fear,  family_fear)
)
ggboxplot(my_data, x = "group", y = "var_vec", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("personal_fear", "family_fear"),
          ylab = "Fear_level", xlab = "Groups")

#correlation matrix
reduced_data = df[c(2:11)]
mymatrix = cor(reduced_data)
View(mymatrix)

#Partition the data by gender
gender_data = df[c(1:11)]
f_data = gender_data[gender_data$Gender=="Female",]
m_data = gender_data[gender_data$Gender=="Male",]

f_updated_data = f_data[c(2:11)]
m_updated_data = m_data[c(2:11)]
m_matrix = cor(m_updated_data)
f_matrix = cor(f_updated_data)
View(m_matrix)
View(f_matrix)

# Partition data by people whose family members contracted Covid.

family_covid = df[df$`Family members contracted COVID-19` == "Yes",]
family_covid_data = family_covid[c(2:11)]
cor_mat = cor(family_covid_data)
View(cor_mat)

family_no_covid=df[df$`Family members contracted COVID-19` == "No",]
family_no_covid_data = family_no_covid[c(2:11)]
cor_matrix = cor(family_no_covid_data)
View(cor_matrix)




