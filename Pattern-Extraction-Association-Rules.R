####Pattern Extraction using the Association Rules Model####

# Use required libraries
library(arules)
library(ROSE)

# Clear the global environment
rm(list=ls(all=T)) 

# Set Working directory
setwd('C:/Users/sa/Desktop/sa/Adarsh Data/2019/Common files/Big Data/BD/PHD/PHD Part 2/TrainData-1564147787554/20190728_6pm_TrainData/') 


# Import data
train = read.csv('traintoput.csv' ,header = T,na.strings = '?')

#save targetin a varibale
target=train$readmitted

#Check Structure
str(train)

# Descritize the numeric columns
library(arules)
train$num_lab_procedures=discretize(train$num_lab_procedures)
train$num_procedures=as.factor(train$num_procedures)
train$num_medications=discretize(train$num_medications)
train$num_diagnoses=discretize(train$num_diagnoses)
train$No_of_days_Stayed=discretize(train$No_of_days_Stayed)
train$admission_source_id=discretize(train$admission_source_id)
train$Month_of_year=discretize(train$Month_of_year)
train$admission_type_id=as.factor(train$admission_type_id)

#Over sample the data
data=train
data=ROSE(readmitted~.,data=data,seed=3)$data
str(data)

#Apriori
rules <- apriori(data,
                 parameter = list(minlen=2, supp=0.02, conf=0.6),
                 appearance = list(rhs=c("readmitted=Within30days"),
                                   default="lhs"),
                 control = list(verbose=F))

#Sort
rules.sorted <- sort(rules, by="lift",decreasing = T)

#Print
inspect(head(rules.sorted,20))
rules


dev.off()
library(arulesViz)

#Plot the graphs
plot(head(rules.sorted,20))
plot(head(rules.sorted,20), method="graph",engine="html", control=list(type="items"))
plot(head(rules.sorted,20), method="paracoord", control=list(type="items"))

#Save in CSV file
x=as(head(rules.sorted,20), "data.frame")
write.csv(x,"rules.csv")


