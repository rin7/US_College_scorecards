#US college scorecards 
#reading Data 

#intalling packages
#install.packages("ggplot2")
library(ggplot2)

setwd("C:/Users/ARIN/Google Drive/DaViS/Quarters/Fall17/STA 141A/HMW/hmw1")
scores <- readRDS("college_scorecard_2013.rds") 

#determining number of observations and features
dim(scores) 

#determining number of distinct colleges
length(which(scores$main_campus == "TRUE"))

#converting from non-factor to factor

#converting from non-factor to factor
scores$unit_id <- factor(scores$unit_id)
scores$ope_id <- factor(scores$ope_id)
scores$city <- factor(scores$city)
scores$zip <- factor(scores$zip)
scores$branches <- factor(scores$branches)

#checking how many factors are there
sum(sapply(1:51, function(x) is.factor(scores[,x]))) #sum to get how many

str(scores) #observe other feature
#checking for missing values
missingObservations <- sum(is.na(scores))

#checking which feature has most missing values
names(which.max(colSums(is.na(scores)))) 

#ratio of private and public schools

#creating new ownership variable with two factors: Public and Private
scores$ownership2 <- scores$ownership
levels(scores$ownership2)[c(2,3)] = c("Private", "Private")

#shows the numbers of private and public schools
table(scores$ownership2)      

#obtaining numbers of highest degree issued by each category of ownership
degrees <- table(scores$ownership2, scores$highest_degree)
degreeProportions <- prop.table(degrees)

#plotting the proportions of highest degrees issued
par(xpd = NA)
barplot(degreeProportions, main = "Proportions of Highest Degree Issued",
        ylab = "Frequency",
        xlab = "Type of Degree",
        col = c("skyblue2", "thistle4"))
legend("topleft", levels(scores$ownership2), fill = c("skyblue", "thistle4"))
 
#determinigh number of Undegraduate Population
#checking the distribution of the undergraduate population
summary(scores$undergrad_pop)               
quantile(scores$undergrad_pop, na.rm = TRUE) #comparing quantile() with summary()

#getting deciles for undegrad population
undergrad_pop_deiciles <- quantile(scores$undergrad_pop, probs = seq(0,1, 0.1), na.rm = 1)

scores[which.max(scores$undergrad_pop), "name"]     ##shows the school with the highes number of undergraduate students

#plotting result for undergraduate population
par(oma = c(2,2,2,2), mar = c(2,2,6,2))
b <- boxplot(scores$undergrad_pop,
             horizontal = TRUE,
             lwd = 2,
             col = "skyblue",
             ylim = c(0,10000),
             main = "Undergraduate Population",
             xlab = "Number of Students")
axis(side = 3, at = c(undergrad_pop_deciles, b$stats), labels = c(undergrad_pop_deciles, b$stats))
abline(v = undergrad_pop_deciles, col = "thistle4", lwd = 3, lty = 2)

#determining the most populous states, per Wikipedia:https://simple.wikipedia.org/wiki/List_of_U.S._states_by_population
#California
#Texas
#Florida
#New York
#Illinois

#subsetting the dataset to only display the schools from the most populous states.
MostPopulatedStates <- subset(scores, state %in% c("CA", "TX", "FL","NY", "IL")) ## a better way to subset
MostPopulatedStates <- droplevels(MostPopulatedStates)  ## removing values for states, in which we are not interested.
boxplot(tuition ~ state, MostPopulatedStates,
        ylim = c(0,50000), 
        main = "Tuition for Five Most Populous States", col.main = "thistle3",
        ylab = "Tuition", xlab = "State", col.lab = "orange4",
        col.axis = "skyblue4",
        col = "grey")


#Relationship between school and spending per student and 10-year earning


#plotting the relationship between spending per student and averagae salary in 10 years
p2 <- ggplot(scores, na.rm = T, aes(x = spend_per_student,  y = avg_10yr_salary, col = ownership))
p2 + geom_point( pch = 5, size = 1, alpha = .6, na.rm = T) +  
  geom_smooth(method = "lm", se = FALSE, linetype = 2, lwd = 2,na.rm = T) + 
  facet_grid(scores$ownership ~.) + 
  scale_y_continuous(labels = scales::comma) +   #changes y-axis ticks labels from scientific into comma format
  scale_x_continuous(labels = scales::comma) +   #changes x-axis ticks labels from scientific into comma format
  labs(title = "Spending Per Student vs Average Salary in 10 Years from Starting School",
       x = "Spending per Student", 
       y = "Avg. Salary in 10 Years") 

#Earing income vs Cost of attendance
# creating a new variable to estimate earnings per cost
scores$earnings_per_cost <- round(scores$avg_10yr_salary / scores$cost, digits = 2)
summary(scores$earnings_per_cost)
#getting the top 10 schools with the highest earings per cost
earnings <- sort.list(scores$earnings_per_cost, decreasing = T)   #sorting in the descending order since the largest values are desired. 
top_ten_earnings <- subset(scores[earnings[1:10],])
top_ten_earnings[, c("name", "state", "earnings_per_cost")] #shows the top 10 schoos w/highest earnings per cost

# Diversity amopung students
#subsetting dataframe for the race percentage variables
races <-subset(scores[,c(44:49,51)])
#computing the variance among race percentage variables for each school
scores$race_variance <- round(apply(races, 1, var), digits = 4)
#selecting schools with the lowest varience in the percentage of different races
diversity <- sort.list(scores$race_variance)            #sorting in ascending order since the lowest variances are desired
top_ten_diversity <- subset(scores[diversity[6:15],])   #the first five school in this list are omited, as they have varience of 0 due to only containing zeros or missing values in all of their race percentage variables
top_ten_diversity[, c("name", "state", "race_variance")] #shows 10 most racially diverse schools

#compare UC Davis vs Other Schools
#Using variables for cost of attendance w/o fin. aid, average 10-year salary, and spending per student

#plotting the results for UC Davis vs Other schools for the above three variables
par(mfrow = c(1, 3), oma = c(0,0,2,0)) # using three plots in one

#plotting Average 10-year salary for UC Davis vs other schools.
d1 <- boxplot(scores$avg_10yr_salary, col = "skyblue", xlab = "Average 10 Year Salary", ylim = c(0, 80000))
abline(h = scores[which(scores$name == "University of California-Davis"), "avg_10yr_salary"], lwd = 2, col = "red", lty = 3)
axis(side = 4, at = scores[which(scores$name == "University of California-Davis"), "avg_10yr_salary"], labels = scores[which(scores$name == "University of California-Davis"), "avg_10yr_salary"])
legend("top", legend = "UC Davis", col = "red", lty = 3, lwd = 2)
#plotting the cost of attendance for UC Davis vs other schools.
d2 <- boxplot(scores$cost, col = "orange3", xlab = "Cost of Attendance")
abline(h = scores[which(scores$name == "University of California-Davis"), "cost"], lwd = 2, col = "red", lty = 3)
axis(side = 4, at = scores[which(scores$name == "University of California-Davis"), "cost"], labels = scores[which(scores$name == "University of California-Davis"), "cost"])
legend("top", legend = "UC Davis", col = "red", lty = 3, lwd = 2)

#plotting the spending per student for UC Davis vs other schools. 
d3 <- boxplot(scores$spend_per_student, col = "thistle2", xlab = "Spending per Student", ylim = c(0, 25000)) 
abline(h = scores[which(scores$name == "University of California-Davis"), "spend_per_student"], lwd = 2, col = "red", lty = 3)
axis(side = 4, at = scores[which(scores$name == "University of California-Davis"), "spend_per_student"],labels = scores[which(scores$name == "University of California-Davis"), "spend_per_student"])
legend("top", legend = "UC Davis", col = "red", lty = 3, lwd = 2)

title(main = "UC Davis vs Other Schools", outer = T, cex.main = 3)

#quantile(scores$spend_per_student, probs = seq(0,1, 0.1), na.rm = 1) #shows that UC Davis spending per student within highest 10% among schools in the dataframe. 
