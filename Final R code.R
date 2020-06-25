#Data Munging
#sImporting the file into a Dataframe using JSON
#df <- getURL("https://s3.us-east-1.amazonaws.com/blackboard.learn.xythos.prod/59566 21d575cd/8606160?response-content-disposition=inline%3B%20filename%2A%3D UTF-8%27%27fall2019-survey-M09.json&response-content-type=application%2Fjs on&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20191205T023408Z&X- Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAIL7WQ YDOOHAZJGWQ%2F20191205%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Sign ature=080af8a024ff530299823a713fcf4a4e8a827d5990bbf6048bd64945fbdc2629") Dataset <- jsonlite::fromJSON('dataset.json')
View(Dataset)

#Cleaning the dataset
#Checking for NA's is.na(Dataset)
#checking for any rows which are not complete sum(!complete.cases(Dataset))
ncol(Dataset)
nrow(Dataset)
#Converting columns into number
Dataset$Age <- as.numeric(Dataset$Age) Dataset$Flight.time.in.minutes <- as.numeric(Dataset$Flight.time.in.minutes) Dataset$Day.of.Month <- as.numeric(Dataset$Day.of.Month) Dataset$Flight.Distance <- as.numeric(Dataset$Flight.Distance)
#checking for any rows which are not complete for the new dataset
sum(!complete.cases(Dataset)) sum(is.na(Dataset$Arrival.Delay.in.Minutes)) sum(is.na(Dataset$Flight.time.in.minutes)) sum(is.na(Dataset$Departure.Delay.in.Minutes))
#replacing na values with mean values of adjacent cells
Dataset <- na.interpolation(as.numeric(Dataset),option = "linear",maxgap = Inf)
#Validating that the data is cleaned sum(!complete.cases(Dataset)) is.na(Dataset)
is.null(Dataset)
View(Dataset) 
nrow(Dataset) 
str(Dataset)

#Descriptive Statistics
#a) Customers’ likelihood to recommend with type of travel.
Total_Customers <- nrow(CleanData)
travel <- ggplot(CleanData, aes(x=Likelihood.to.recommend, y=Total_Customers ,fill=Type.of.Travel)) + geom_col()

#b) Customers’ likelihood to recommend with Age group
age<-agefunction(dataLowSatisfaction$Age) 
ggplot(dataLowSatisfaction,aes(x=age,fill=Likelihood.to.recommend))+geom_bar (position='dodge')

#c) Customers’ Likelihood to recommend with Airline Status.
Airline_status_satis <- ggplot(CleanData, aes(x=CleanData$Airline.Status, y=Total_Customers ,fill=Likelihood.to.recommend)) + geom_col() Airline_status_satis

#d) Customers likelihood to recommend with Delay in Arrival
ADD <- quantile(dataLowSatisfaction$Arrival.Delay.in.Minutes,0.96) SatisfactionDelayinArr <-ggplot(dataLowSatisfaction[dataLowSatisfaction$Arrival.Delay.in.Minutes<ADD ,],aes(x=Arrival.Delay.in.Minutes))
SatisfactionDelayinArr <- SatisfactionDelayinArr+geom_histogram(aes(fill=Likelihood.to.recommend,color= "white"),binwidth = 1,position = "dodge")
SatisfactionDelayinArr <- SatisfactionDelayinArr+ggtitle("Satisfaction versus Delay in Arrival")
SatisfactionDelayinArr

#e) Customer’s likelihood to recommend vs Delay in Departure
SDD <- quantile(dataLowSatisfaction$Departure.Delay.in.Minutes,0.96) SatisfactionDelayInDept <- ggplot(dataLowSatisfaction[dataLowSatisfaction$Departure.Delay.in.Minutes<SD D,],aes(x=Departure.Delay.in.Minutes))
SatisfactionDelayInDept <- SatisfactionDelayInDept+geom_histogram(aes(fill=Likelihood.to.recommend,colo r="white"),binwidth = 10,position = "dodge")
SatisfactionDelayInDept <- SatisfactionDelayInDept+ggtitle("Satisfaction versus Delay in Departure")
SatisfactionDelayInDept

#f)Customer’s likelihood to recommend based on Gender
ggplot(Dataset,aes(x=Gender,fill=Likelihood.to.recommend))+geom_bar(position ='dodge')

#Linear Modelling:
str(CleanData)
#Removing columns with lot of factors as they are not useful in lm. Took all the other columns as lm's job is to remove all the not important columns lm_data <- CleanData[,c(3,4,5,6,8,9,10,11,12,13,14,15,21,22,23,25,26,27)]
str(lm_data)
# chnaging chr to factors as lm only runs on int, factors>2 and num lm_data$Airline.Status <- as.factor(lm_data$Airline.Status) lm_data$Class <- as.factor(lm_data$Class)
lm_data$Gender <- as.factor(lm_data$Gender) lm_data$Type.of.Travel <- as.factor(lm_data$Type.of.Travel) str(lm_data)
#removing rows NA - If there are no NAs in our dataset then please remove the following code
colSums(is.na(lm_data)) #check NA with this code
lm_data <- filter(lm_data, !is.na(Flight.time.in.minutes))
lm_data <- filter(lm_data, !is.na(Departure.Delay.in.Minutes)) lm_data <- filter(lm_data, !is.na(Arrival.Delay.in.Minutes))
#running 1st lm with no direction wrt likelihood to recommend Linear.model <- lm(formula = Likelihood.to.recommend ~., data = lm_data) library(MASS)
stepAIC(Linear.model) #this tells us the best columns to pick summary(Linear.model)
Model_1 <- lm(formula = Likelihood.to.recommend ~ Airline.Status + Age + Type.of.Travel + Total.Freq.Flyer.Accts +
                Shopping.Amount.at.Airport +
                Eating.and.Drinking.at.Airport + Day.of.Month +
                Departure.Delay.in.Minutes +
                Arrival.Delay.in.Minutes + Flight.time.in.minutes +
                Flight.Distance,
              data = lm_data)
summary(Model_1)

#running lm with backwards null<-lm(Likelihood.to.recommend~1,lm_data) stepAIC(Linear.model, direction='backward')
back <-lm(formula = Likelihood.to.recommend ~ Airline.Status + Age + Type.of.Travel + Total.Freq.Flyer.Accts +
            Shopping.Amount.at.Airport +
            Eating.and.Drinking.at.Airport + Day.of.Month +
            Departure.Delay.in.Minutes +
            Arrival.Delay.in.Minutes + Flight.time.in.minutes + Flight.Distance,
          data = lm_data) summary(back)
#running lm forward model stepAIC(null,direction='forward',scope=list(upper=Linear.model,lower=null))
forward <- lm(formula = Likelihood.to.recommend ~ Type.of.Travel + Airline.Status +Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes + Age +Day.of.Month + Total.Freq.Flyer.Accts +
Model_common <- lm(formula = Likelihood.to.recommend ~ Airline.Status + Age + Type.of.Travel +
Total.Freq.Flyer.Accts + Eating.and.Drinking.at.Airport + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes,
data = summary(Model_common)
#Corelation Model
lm_data <- CleanData[,c(3,4,5,6,8,9,10,11,12,13,14,15,21,22,23,25,26,27)]
colSums(is.na(lm_data)) #check NA with this code
lm_data <- filter(lm_data, !is.na(Flight.time.in.minutes))
lm_data <- filter(lm_data, !is.na(Departure.Delay.in.Minutes)) 
lm_data <- filter(lm_data, !is.na(Arrival.Delay.in.Minutes))
str(CleanData) 
str(lm_data) 
cor_df <- lm_data Day.of.Month + lm_data)

# converting categorical variables into dummy variables 
cor_df$Airline.Status[cor_df$Airline.Status == 'Blue'] <- 1,
cor_df$Airline.Status[cor_df$Airline.Status == 'Silver'] <- 2,
cor_df$Airline.Status[cor_df$Airline.Status == 'Gold'] <- 3, 
cor_df$Airline.Status[cor_df$Airline.Status == 'Platinum'] <- 4,
cor_df$Gender[cor_df$Gender == "Female"] <- 0, 
cor_df$Gender[cor_df$Gender == "Male"] <- 1,
cor_df$Type.of.Travel[cor_df$Type.of.Travel == 'Mileage tickets'] <- 1, 
cor_df$Type.of.Travel[cor_df$Type.of.Travel == 'Personal Travel'] <- 2,
cor_df$Type.of.Travel[cor_df$Type.of.Travel == 'Business travel'] <- 3,
cor_df$Class[cor_df$Class == 'Eco'] <- 1, 
cor_df$Class[cor_df$Class == 'Eco Plus'] <- 2, 
cor_df$Class[cor_df$Class == 'Business'] <- 3, 
#cor_df <- as.matrix(cor_df)
#Converting dummy variables into numeric format 
cor_df$Airline.Status <- as.numeric(cor_df$Airline.Status),
cor_df$Gender <- as.numeric(cor_df$Gender),
cor_df$Type.of.Travel <- as.numeric(cor_df$Type.of.Travel),
cor_df$Class <- as.numeric(cor_df$Class),
#Creating a correlation matrix str(cor_df)
cor_model <- cor(cor_df),
cor_model,
install.packages("corrplot"),
library(corrplot),


#Visualizing correlation matrix
corrplot(cor_model, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.5),
str(cor_df),

#Association Rules Mining:
ruleset_satisfied <- apriori(data_flight_categorizedX, 
                             parameter = list(support =0.3, confidence = 0.2), 
                             appearance = list(default="lhs",rhs=("custsatis=Promoters"))),
ruleset_unsatisfied <- apriori(data_flight_categorizedX,
                               parameter = list(support =0.1, confidence = 0.4), 
                               appearance = list(default="lhs",rhs=("custsatis=Detractors")))




