#public health: national nutritional health
#importing data set
nhanes.data <- (read.csv("https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/nhanes.csv"))
#understanding the structure of the dataset
str(nhanes.data)
#the dataset is a data frame composed of 5000 observations across 32 variables amounting to a total of 160,000 variables

#determining the total number of missing data signified by 'N/A' in the datafame
sum(is.na(nhanes.data))
#there are a total of 33,931 missing data from the data set 

#replacing all missing data signified by 'N/A' in the dataframe with '0' 
cleaned_nhanes <- replace(nhanes.data, is.na(nhanes.data), 0)

#viewing data summary 
summary(cleaned_nhanes)

#creating histograms to visualize the distribution of BMI, Weight(in kg), Weight (in lbs) and Age
#firstly create a new column in the df to represent Weight in pounds i.e Weight in kg * 2.2
cleaned_nhanes$WeightInPounds <- cleaned_nhanes$Weight * 2.2
#using par(mfrow) to create a grid layout for the four plots
par (mfrow=c(2,2), mar= c(4.2,4.2,2.5,1.0), mgp= c(2.8,0.7,0))

#Histogram 1: BMI
hist(cleaned_nhanes$BMI, 
     breaks= "Sturges", 
     main="BMI Distribution", 
     xlab="BMI", ylab='Frequency', 
     col="steelblue", 
     border= "black")
#Histogram 2: Weight in kg
hist(cleaned_nhanes$Weight, 
     breaks= "Sturges", 
     main="Weight (in kg) Distribution", 
     xlab="Weight (kg)", 
     ylab='Frequency', 
     col="skyblue", 
     border= "black")
#Histogram 3: Weight in lbs(pounds)
hist(cleaned_nhanes$WeightInPounds, 
     breaks="Sturges", 
     main='Weight (in lbs) Distribution', 
     xlab='Weight (in lbs)', 
     ylab='Frequency', 
     col='blue', 
     border='black')
#Histogram 4: Age
hist(cleaned_nhanes$Age, 
     breaks="Sturges", 
     main='Age Distribution', 
     xlab='Age', 
     ylab='Frequency', 
     col='steelblue', 
     border='black')
#the grid of the distributions has been saved in the repository as 'DistributionsGrid'

#determining the mean pulse for all participants
mean_pulse <- mean(cleaned_nhanes$Pulse)
#the mean pulse across all participants is 63.06

#determining the range of the Diastolic Blood Pressure across all participants
range(cleaned_nhanes$BPDia)
#the range of Diastolic Blood Pressure is 0-116

#determining the variance and standard deviation of income across all participants 
income_variance <- var(cleaned_nhanes$Income)
print (income_variance)
income_sd <- sd(cleaned_nhanes$Income)
print (income_sd)

#visualizing the relationship between weight and height using scatterplot
#color the points according to:gender,diabetes & smoking status
library(ggplot2)
ggplot(cleaned_nhanes, 
       aes(x=Height, 
           y=Weight, 
           colour=Gender, 
           shape=Diabetes)) 
+ geom_point(size=3,alpha=0.7) 
+ facet_wrap(~SmokingStatus) 
+ labs(title='Weight vs Height Colored by Diabetes Status', 
       x='Height', y='Weight') 
+ theme_minimal()
#the scatterplot of the relationship has been saved in the repository as 'WHScatterplot'

#conducting t-test between the following variables:
#Age and Gender
#BMI and Diabetes
#Alcohol Year and Relationship Status
#and making conclusions on the relationship between them based on P-Value

#Age and Gender
t.test(Age ~ Gender, data = cleaned_nhanes)

#BMI and Diabetes
#due to the presence of missing data denoted by "0" in the Diabetes column
#it will be assumed that all missing data are indicative of no diabetes
#to determine the total number of missing data
sum(nhanes.data$Diabetes == '0')
#the total number of missing data within the Diabetes column is 64
cleaned_nhanes$Diabetes <- factor(ifelse(cleaned_nhanes$Diabetes %in% c("0", "No"), "No", "Yes"))
t.test(BMI ~ Diabetes, data = cleaned_nhanes)

#AlcoholYear and RelationshipStatus
#due to the presence of missing data denoted by "0" in the Relationship Status column
#it will be assumed that all missing data are indicative of not being in a relationship
#to determine the total number of missing data
sum(nhanes.data$RelationshipStatus == '0')
#the total number of missing data within the Relationship Status is 1415
cleaned_nhanes$RelationshipStatus <- factor(ifelse(cleaned_nhanes$RelationshipStatus %in% c('0','Single'), 'Single', 'Committed'))
t.test (AlcoholYear ~ RelationshipStatus, data= cleaned_nhanes)

#final discussion of the results will be captured in the report within the repository






