#changing work directory
setwd("Input/Working/Directory")

#Installing libraries
install.packages("readxl")
install.packages("car") 

#read the excel file
library(readxl)
df = read_excel("Questionnarie (Responses) (2).xlsx")

##########################################################
# Splitting our data into relevant sub data frames
##########################################################

df_CS <- df[df$Discipline == "CS", ]
df_Math <- df[df$Discipline == "Math", ]
df_Economics <- df[df$Discipline == "Economics", ]
df_Other <- df[df$Discipline == "Other", ]

df_CS_Math <- df[df$Discipline %in% c("CS", "Math"), ]
df_Economics_Other <- df[df$Discipline %in% c("Economics", "Other"), ]


#####################################################################
# Sample Size Computations (SRS)                                    
#####################################################################

N = 200
p = 0.5
q = 1-p
B = 0.116

D = B^2 / 4
(N * p * q) / ((N-1) * D + p * q)

#####################################################################
# Initial Data Analysis                                    
#####################################################################

nrow(df_CS)/nrow(df) #Proportion of students with "CS" Discipline

nrow(df_Math)/nrow(df) #Proportion of students with "Math" Discipline

nrow(df_Economics)/nrow(df) #Proportion of students with "Economics" Discipline

nrow(df_Other)/nrow(df) #Proportion of students with "Other" Discipline


###########################################################################
#(RQ1) Does the perceived usefulness of generative 
#AI for grasping course content differ across different disciplines?
###########################################################################


##########################################################################
#COMPARISON OF MEANS FOR PERCIEVED USEFULNESS FOR GRASPING COURSE CONTENT 
#BETWEEN STUDENTS OF [CS AND MATH] AND [ECONOMICS AND OTHER] STUDENTS
###########################################################################

shapiro.test(df_Economics_Other$AIDisciplineCourseContent) #p-value < 0.05 so not normal.
#Proceed to use wilcox test (Mann-Whitney U Test (for two groups))

df_combined <- data.frame( value = c(df_CS_Math$AIDisciplineCourseContent, 
                                     df_Economics_Other$AIDisciplineCourseContent), 
                           group = rep(c("Group1", "Group2"), 
                                       times = c(length(df_CS_Math$AIDisciplineCourseContent), 
                                                 length(df_Economics_Other$AIDisciplineCourseContent))) ) 

# Load the car package 
library(car)

# Perform Levene's test for equal variances
leveneTest(value ~ group, data = df_combined) # Checking if variances are same
#(if p-value>0.05, then they are approximately equal). 
# We find p-value>0.05 so they are equal variances
wilcox.test(df_CS_Math$AIDisciplineCourseContent, df_Economics_Other$AIDisciplineCourseContent, 
            alternative = "two.sided", var.equal = TRUE)

#LOGISTIC REGRESSION
#Logistic Regression between predictor discipline and percieved usefulness of AI for course content
df$Discipline <- as.factor(df$Discipline)
model <- glm(AIDisciplineCourseContent/10 ~ Discipline, data = df,)
summary(model)

#ANOVA
anova_results = aov(df$AIDisciplineCourseContent ~ df$Discipline)
summary(anova_results)

boxplot(df$AIDisciplineCourseContent ~ df$Discipline, 
        data = df, main = "Boxplot of AIDisciplineCourseContent by Discipline", 
        xlab = "Discipline", ylab = "AIDisciplineCourseContent", 
        col = c("lightblue", "lightgreen", "lightcoral", "lightyellow"), 
        border = "black")

stripchart(df$AIDisciplineCourseContent ~ df$Discipline, 
           data = df, method = "jitter", # Jitters points to prevent overlap
           ,vertical = TRUE, # Draws the chart vertically 
           pch = 16, # Symbol type 
           col = c("blue", "green", "coral", "red"), # Colors for each group 
           main = "Usefulness of AI by Discipline", xlab = "Discipline", ylab = "Perceived Usefulness of AI") 

#####################################################################
#COMPARISON OF MEANS FOR PERCIEVED USEFULNESS FOR HELPING IN ASSIGNMENTS 
#BETWEEN STUDENTS OF [CS AND MATH] AND [ECONOMICS AND OTHER] STUDENTS
#####################################################################

shapiro.test(df_Economics_Other$AIDisciplineAssignment) #p-value < 0.05 so not normal. 
#Proceed to use wilcox test (Mann-Whitney U Test (for two groups))

df_combined_2 <- data.frame( value = c(df_CS_Math$AIDisciplineAssignment,
                                       df_Economics_Other$AIDisciplineAssignment), 
                             group = rep(c("Group1", "Group2"), 
                                         times = c(length(df_CS_Math$AIDisciplineAssignment), 
                                                   length(df_Economics_Other$AIDisciplineAssignment))) ) 

# Load the car package 
library(car)

# Perform Levene's test for equal variances
leveneTest(value ~ group, data = df_combined_2) # Checking if variances are same
#(if p-value>0.05, then they are approximately equal). 
#We find p-value>0.05 so they are equal variances

wilcox.test(df_CS_Math$AIDisciplineAssignment, df_Economics_Other$AIDisciplineAssignment, 
            alternative = "two.sided", var.equal = TRUE)

#LOGISTIC REGRESSION
#Logistic Regression between predictor discipline and percieved usefulness of AI for course content
df$Discipline <- as.factor(df$Discipline)
model <- glm(AIDisciplineAssignment/10 ~ Discipline, data = df,)
summary(model)

#ANOVA
anova_results = aov(df$AIDisciplineAssignment ~ df$Discipline)
summary(anova_results)
boxplot(df$AIDisciplineAssignment ~ df$Discipline, data = df, 
        main = "Boxplot of AIDisciplineCourseContent by Discipline", 
        xlab = "Discipline", ylab = "AIDisciplineCourseContent", 
        col = c("lightblue", "lightgreen", "lightcoral", "lightyellow"), 
        border = "black")

stripchart(df$AIDisciplineAssignment ~ df$Discipline, 
           data = df, method = "jitter", # Jitters points to prevent overlap
           ,vertical = TRUE, # Draws the chart vertically 
           pch = 16, # Symbol type 
           col = c("blue", "green", "coral", "red"), # Colors for each group 
           main = "Usefulness of AI by Discipline", xlab = "Discipline", ylab = "Perceived Usefulness of AI")

#####################################################################


# OVERALL, THERE IS NO WAY TO REJECT THE FACT THAT THE MEAN PERCIEVED USEFULNESS OF AI 
# BY [CS AND MATH] STUDENTS IS THE SAME AS [ECONOMICS AND OTHER] STUDENTS 
# FOR BOTH COURSE CONTENT AND ASSIGNMENTS

#####################################################################
# (RQ2) How does the proportion of assessments completed with the assistance 
#  of generativeAI differ across academic disciplines?
#####################################################################

# Proportion of graded assignments done with help of AI 
#for [CS AND MATH] students
mean(df_CS_Math$AIGraded)/5 

# Proportion of graded assignments done with help of AI 
#for [ECONOMICS AND OTHER] students
mean(df_Economics_Other$AIGraded)/5 

#2-sample test for equality of proportions of graded assignments done 
#with help of AI for [CS AND MATH] students and Proportion of graded 
#assignments done with help of AI for [ECONOMICS AND OTHER] students
prop.test(c(mean(df_CS_Math$AIGraded)*36, mean(df_Economics_Other$AIGraded)*19), 
          c(5*36,5*19), conf.level = 0.95, correct = FALSE)

# Proportion of non graded assignments done with help of AI 
#for [CS AND MATH] students
mean(df_CS_Math$AINonGraded)/5
# Proportion of non graded assignments done with help of AI 
#for [ECONOMICS AND OTHER] students
mean(df_Economics_Other$AINonGraded)/5

# 2-sample test for equality of proportions of non graded assignments done 
# with help of AI for [CS AND MATH] students and # Proportion of graded 
#assignments done with help of AI for [ECONOMICS AND OTHER] students
prop.test(c(mean(df_CS_Math$AINonGraded)*36, mean(df_Economics_Other$AINonGraded)*19), 
          c(5*36,5*19), conf.level = 0.95, correct = FALSE)

#####################################################################
# (RQ3) Does the preference of using generative AI compared to traditional resources for
#helping with course content differ across academic disciplines?                                                   
#####################################################################

#Proportion of CS and Math students who prefer generative AI over 
#traditional sources like Ta's, Professors, piazza, etc.
sum(sum(df_CS_Math$AskFirst == "I only ask generative AI for help on course content"),
    sum(df_CS_Math$AskFirst == "I ask generative AI before I ask the professors, TA's, piazza, etc"))/36

#Proportion of Economics and Other students who prefer generative AI over 
#traditional sources like Ta's, Professors, piazza, etc.
sum(sum(df_Economics_Other$AskFirst == "I only ask generative AI for help on course content"),
    sum(df_Economics_Other$AskFirst == "I ask generative AI before I ask the professors, TA's, piazza, etc"))/19

# 2-sample test for equality of proportions between: CS and Math students 
#who prefer generative AI over traditional sources like Ta's, Professors, piazza, etc. and
#Proportion of Economics and Other students who prefer generative AI over 
#traditional sources like Ta's, Professors, piazza, etc.
prop.test(c(sum(sum(df_CS_Math$AskFirst == "I only ask generative AI for help on course content"),
                sum(df_CS_Math$AskFirst == "I ask generative AI before I ask the professors, TA's, piazza, etc")), 
            sum(sum(df_Economics_Other$AskFirst == "I only ask generative AI for help on course content"),
                sum(df_Economics_Other$AskFirst == "I ask generative AI before I ask the professors, TA's, piazza, etc"))), 
          c(36,19), conf.level = 0.95, correct = FALSE)

table(df$AIDisciplineCourseContent)

