library(haven)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

    ### DATA MANIPULATION###

barometer <- read_sav("3269.sav") #load in data

#subset to get the relevant variables to my study
mydata <- select(barometer, C12,
                 C12A,B22, C3, C9, C10, C4_1,C4_2,C4_3,C4_4,C4_6,
                 A9_1, A9_2, A9_3)

#rename the variables so I have an easier time navigating the data
mydata <- mydata %>%
  rename(religion = C12,
         rel_attendance =C12A,
         partyvote = B22,
         voter_ideology = C3,
         sex = C9,
         age = C10,
         psoe_ideology = C4_1,
         pp_ideology = C4_2,
         vox_ideology = C4_3,
         podemos_ideology = C4_4,
         cs_ideology = C4_6,
         mip1 = A9_1,
         mip2 = A9_2,
         mip3 = A9_3)

####For Each Variable, I want to make NAs uniform (in the dataset, they are 
#often as numbers and differ by variable)

#IDEOLOGY
table(mydata$voter_ideology)
mydata <- mutate(mydata,
                 voter_ideology = case_when(
                   voter_ideology == 1~1, voter_ideology == 2~2,
                   voter_ideology == 3~3, voter_ideology == 4~4,
                   voter_ideology == 5~5, voter_ideology == 6~6,
                   voter_ideology == 7~7, voter_ideology == 8~8,
                   voter_ideology == 9~9, voter_ideology == 10~10))

#SIMPLIFIED IDEOLOGY
mydata$simple_ideology <- mydata$voter_ideology
mydata <- mutate(mydata,
                 simple_ideology = case_when(
                   voter_ideology == 1~0, voter_ideology == 2~0,
                   voter_ideology == 3~0, voter_ideology == 4~0,
                   voter_ideology == 5~1, voter_ideology == 6~1,
                   voter_ideology == 7~2, voter_ideology == 8~2,
                   voter_ideology == 9~2, voter_ideology == 10~2))

#RELIGION
table(mydata$religion)
mydata <- mutate(mydata,
                 religion = case_when(
                   religion == 1~ "Practicing Catholic", 
                   religion == 2~ "Non-Practicing Catholic",
                   religion == 3~ "Other Religion",
                   religion == 4~ "Agnostic/Indifferent/Athiest"))

#RELIGIOUS ATTENDANCE
table(mydata$rel_attendance)
mydata <- mutate(mydata,
                 rel_attendance = case_when(
                   rel_attendance == 1~1, rel_attendance == 2~2,
                   rel_attendance == 3~3, rel_attendance == 4~4,
                   rel_attendance == 5~5, rel_attendance == 6~6))

#PARTY VOTE
mydata <- mutate(mydata,
                 partyvote = case_when(
                   partyvote == 1 ~ "PP", partyvote == 2 ~ "PSOE",
                   partyvote == 4 ~ "Cs",partyvote == 21 ~ "Podemos",
                   partyvote == 18 ~ "Vox"))

#SEX
table(mydata$sex) 
mydata <- mutate(mydata,
                 sex = case_when(
                   sex == 1~0, sex == 2~1)) # MALE = 0, FEMALE = 1

#AGE
table(mydata$age)

#RELIGION
mydata$practicing_cath <- mydata$religion
mydata <- mutate(mydata,
                 practicing_cath = case_when(
                   practicing_cath == "Practicing Catholic" ~ 1, 
                   practicing_cath == "Non-Practicing Catholic" ~ 0,
                   practicing_cath == "Other Religion"~ 0,
                   practicing_cath == "Agnostic/Indifferent/Athiest" ~ 0))

### Now, to look at individual issues that correlate with Vox votes:

#IMMIGRATION

#coding: if a respondent says immigration is the most important problem facing the country
# (whether 1st, 2nd, or 3rd most important), they are coded as a 1, meaning higher concern
mydata$immigration1 <- mydata$mip1
mydata <- mutate(mydata,
                 immigration1 = ifelse(mydata$immigration1 == 18, 1, 0))
mydata$immigration2 <- mydata$mip2
mydata <- mutate(mydata,
                 immigration2 = ifelse(mydata$immigration2 == 18, 1, 0))
mydata$immigration3 <- mydata$mip3
mydata <- mutate(mydata,
                 immigration3 = ifelse(mydata$immigration3 == 18, 1, 0))
mydata$immigration <- mydata$immigration1 + mydata$immigration2 + mydata$immigration3
#the following step accounts for respondents who put the issue down twice
mydata <- mutate(mydata,
                 immigration = ifelse(mydata$immigration == 0, 0, 1)) 
mydata = subset(mydata, select = -c(17:19) ) #clean data, don't need immigration1-3


#ECONOMY
#coding: if a respondent says the economy is the most important problem facing the country
# (whether 1st, 2nd, or 3rd most important), they are coded as a 1, meaning higher concern
mydata$economy1 <- mydata$mip1
mydata <- mutate(mydata,
                 economy1 = ifelse(mydata$economy1 == 8, 1, 0))
mydata$economy2 <- mydata$mip2
mydata <- mutate(mydata,
                 economy2 = ifelse(mydata$economy2 == 8, 1, 0))
mydata$economy3 <- mydata$mip3
mydata <- mutate(mydata,
                 economy3 = ifelse(mydata$economy3 == 8, 1, 0))
mydata$economy <- mydata$economy1 + mydata$economy2 + mydata$economy3
#the following step accounts for respondents who put the issue down twice
mydata <- mutate(mydata,
                 economy = ifelse(mydata$economy == 0, 0, 1)) 
mydata = subset(mydata, select = -c(18:20) ) #clean data, don't need economy1-3


## CORRUPTION
mydata$corruption1 <- mydata$mip1
mydata <- mutate(mydata,
                 corruption1 = ifelse(mydata$corruption1 == 11, 1, 0))
mydata$corruption2 <- mydata$mip2
mydata <- mutate(mydata,
                 corruption2 = ifelse(mydata$corruption2 == 11, 1, 0))
mydata$corruption3 <- mydata$mip3
mydata <- mutate(mydata,
                 corruption3 = ifelse(mydata$corruption3 == 1, 1, 0))
mydata$corruption <- mydata$corruption1 + mydata$corruption2 + mydata$corruption3
#the following step accounts for respondents who put the issue down twice
mydata <- mutate(mydata,
                 corruption = ifelse(mydata$corruption == 0, 0, 1)) 
mydata = subset(mydata, select = -c(19:21) ) #clean data, don't need economy1-3


## CRISIS OF VALUES
mydata$values1 <- mydata$mip1
mydata <- mutate(mydata,
                 values1 = ifelse(mydata$values1 == 21, 1, 0))
mydata$values2 <- mydata$mip2
mydata <- mutate(mydata,
                 values2 = ifelse(mydata$values2 == 21, 1, 0))
mydata$values3 <- mydata$mip3
mydata <- mutate(mydata,
                 values3 = ifelse(mydata$values3 == 21, 1, 0))
mydata$values <- mydata$values1 + mydata$values2 + mydata$values3
#the following step accounts for respondents who put the issue down twice
mydata <- mutate(mydata,
                 values = ifelse(mydata$values == 0, 0, 1)) 
mydata = subset(mydata, select = -c(20:22) ) #clean data, don't need economy1-3


## CATALONIA
mydata$catalonia1 <- mydata$mip1
mydata <- mutate(mydata,
                 catalonia1 = ifelse(mydata$catalonia1 == 45, 1, 0))
mydata$catalonia2 <- mydata$mip2
mydata <- mutate(mydata,
                 catalonia2 = ifelse(mydata$catalonia2 == 45, 1, 0))
mydata$catalonia3 <- mydata$mip3
mydata <- mutate(mydata,
                 catalonia3 = ifelse(mydata$catalonia3 == 45, 1, 0))
mydata$catalonia <- mydata$catalonia1 + mydata$catalonia2 + mydata$catalonia3
#the following step accounts for respondents who put the issue down twice
mydata <- mutate(mydata,
                 catalonia = ifelse(mydata$catalonia == 0, 0, 1)) 
mydata = subset(mydata, select = -c(21:23) ) #clean data, don't need economy1-3


library("Rmisc")
library("magrittr")
library("broom")
library("estimatr")
library("modelsummary")
library("stargazer")
library("naniar")
?stargazer
                  ###DESCRIPTIVE STATISTICS###

#Numeric Variables
datasummary((`Religious Attendance` = na.omit(rel_attendance)) + 
              (`Practicing Catholic` = na.omit(practicing_cath)) +
              (`Voter Ideology` = na.omit(voter_ideology)) +
              (`Simplified Ideology` = na.omit(simple_ideology)) +
              (`Sex` = na.omit(sex)) + (`Age` = na.omit(age)) 
            ~ (`N` = length) + Mean + SD + Min + Max,
            data = mydata,
            output = 'markdown', #change to latex for paper
            title = "Descriptive Statistics for Numeric Variables")



#Descriptive Stats for Categorical Religion Variable
datasummary_skim(mydata$religion, type='categorical', 
                 title = "Religion", output = 'markdown') #change to latex for paper

#Descriptive Stats for Categorical Party Vote Variable
datasummary_skim(mydata$partyvote, type='categorical', 
                 title = "Party Supported", output = 'markdown') #change to latex for paper



                  ###STATISTICAL TESTS###


#REGRESSION ATTENDANCE ON IDEOLOGY
attendance_no_control <- lm(mydata$voter_ideology ~ mydata$rel_attendance)
practicing_no_control <- lm(mydata$voter_ideology ~ mydata$practicing_cath)
attendance_control <- lm(mydata$voter_ideology ~ mydata$rel_attendance + mydata$age + mydata$sex)
practicing_control <- lm(mydata$voter_ideology ~ mydata$practicing_cath + mydata$age + mydata$sex)

stargazer(attendance_no_control, practicing_no_control,
          attendance_control, practicing_control,  type = "text", #change to latex and do 'asis'
          title = "Effect of Religion on Voter Ideology",
          covariate.labels = c("Religious Attendance Level", "Practicing Catholic",
                               "Age", "Sex"),
          dep.var.labels   = "Voter Ideology",
          header = FALSE)



#INTERESTING TABLES TO CONSIDER, don't use
prop.table(table(mydata$religion, mydata$simple_ideology), margin = 2)
table(mydata$religion, mydata$partyvote)

prop.table(table(mydata$religion, mydata$partyvote), margin = 2)



#DATAFRAME WITH MEANS AND CIS (REALLY DONT NEED THIS)

party <- c("Podemos", "PSOE", "Cs", "PP", "Vox")

number <- c(nrow(subset(mydata, mydata$partyvote=="Podemos")),
            nrow(subset(mydata, mydata$partyvote=="PSOE")),
            nrow(subset(mydata, mydata$partyvote=="Cs")),
            nrow(subset(mydata, mydata$partyvote=="PP")),
            nrow(subset(mydata, mydata$partyvote=="Vox")))

mean_attendance <- c(mean(subset(mydata, mydata$partyvote=="Podemos")$rel_attendance,
                           na.rm = T),
                      mean(subset(mydata, mydata$partyvote=="PSOE")$rel_attendance, 
                           na.rm = T),
                      mean(subset(mydata, mydata$partyvote=="Cs")$rel_attendance, 
                           na.rm = T),
                      mean(subset(mydata, mydata$partyvote=="PP")$rel_attendance, 
                           na.rm = T),
                      mean(subset(mydata, mydata$partyvote=="Vox")$rel_attendance, 
                           na.rm = T))


sd_attendance <- c(sd(subset(mydata, mydata$partyvote=="Podemos")$rel_attendance,
                      na.rm = T),
                   sd(subset(mydata, mydata$partyvote=="PSOE")$rel_attendance, 
                      na.rm = T),
                   sd(subset(mydata, mydata$partyvote=="Cs")$rel_attendance, 
                      na.rm = T),
                   sd(subset(mydata, mydata$partyvote=="PP")$rel_attendance, 
                      na.rm = T),
                   sd(subset(mydata, mydata$partyvote=="Vox")$rel_attendance, 
                      na.rm = T))

CI_lo <- c(mean_attendance[1] -1.96*(sd_attendance[1]/sqrt(number[1])),
           mean_attendance[2] -1.96*(sd_attendance[2]/sqrt(number[2])),
           mean_attendance[3] -1.96*(sd_attendance[3]/sqrt(number[3])),
           mean_attendance[4] -1.96*(sd_attendance[4]/sqrt(number[4])),
           mean_attendance[5] -1.96*(sd_attendance[5]/sqrt(number[5])))

CI_hi <- c(mean_attendance[1] +1.96*(sd_attendance[1]/sqrt(number[1])),
           mean_attendance[2] +1.96*(sd_attendance[2]/sqrt(number[2])),
           mean_attendance[3] +1.96*(sd_attendance[3]/sqrt(number[3])),
           mean_attendance[4] +1.96*(sd_attendance[4]/sqrt(number[4])),
           mean_attendance[5] +1.96*(sd_attendance[5]/sqrt(number[5])))

means_data <- data.frame(party, number, mean_attendance, sd_attendance, CI_lo, CI_hi)



library(Hmisc)

#Creating Graphic for mean religious attendance
means_test <- subset(mydata, mydata$partyvote!="NA")
means_test <- subset(means_test, means_test$rel_attendance!= "NA")
means_test$partyvote <- as.factor(means_test$partyvote)
means_test$partyvote <- factor(means_test$partyvote,
                               levels = c("Podemos", "PSOE", "Cs", "PP", "Vox"))

colorfill <- c( "purple2", "red2", "darkorange2", "dodgerblue1", "springgreen3" )
names(colorfill) <- levels(means_test$partyvote)
colScale <- scale_colour_manual(name = "partyvote",values = colorfill)

table(mydata$rel_attendance)

g0 <- ggplot(means_test,aes(x=rel_attendance ,y=partyvote))
g_mean<-g0+stat_summary(fun=mean,geom="point")
g_mean +
  stat_summary(fun.data=mean_cl_normal,geom="errorbar", aes(col=factor(partyvote)),
               show.legend = F) + 
  colScale +
  geom_vline(xintercept = mean(mydata$rel_attendance, na.rm = T), col="dark gray") +
  labs(title = "Mean Religious Attendance by Party", 
       y = "Party",
       x= "Religious Attendance (1-6 Scale)", 
       subtitle = "95% confidence intervals, line indicates overall mean attendance ",
       caption = "Data from CIS Barometer, December 2019")


#Creating Graphic for mean practicing catholic
means_test2 <- subset(mydata, mydata$partyvote!="NA")
means_test2 <- subset(means_test2, means_test2$practicing_cath!="NA")
means_test2$partyvote <- as.factor(means_test2$partyvote)
means_test2$partyvote <- factor(means_test2$partyvote,
                               levels = c("Podemos", "PSOE", "Cs", "PP", "Vox"))

colorfill <- c( "purple2", "red2", "darkorange2", "dodgerblue1", "springgreen3" )
names(colorfill) <- levels(means_test2$partyvote)
colScale <- scale_colour_manual(name = "partyvote",values = colorfill)

g2 <- ggplot(means_test2,aes(x=practicing_cath ,y=partyvote))
g_mean2<-g2+stat_summary(fun=mean,geom="point")
g_mean2 +
  stat_summary(fun.data=mean_cl_normal,geom="errorbar", aes(col=factor(partyvote)),
               show.legend = F) + 
  colScale +
  geom_vline(xintercept = mean(mydata$practicing_cath, na.rm = T), col="dark gray") +
  labs(title = "Proportion Practicing Catholics by Party", 
       y = "Party",
       x= "Proportion of Voters who are Practicing Catholics", 
       subtitle = "95% confidence intervals, line is overall proportion practicing ",
       caption = "Data from CIS Barometer, December 2019")

## TAKEAWAY: Vox is not winning the religious vote



#Graphic for relationship between religious attendance and ideology (DONT NEED)
ggplot(data=mydata, aes(x=rel_attendance, y = voter_ideology))+
  geom_smooth(method = "lm") +
  labs(title = "Relationship Between Religious Attendence and Ideology", 
       y = "Left-Right Ideology (10 = Extreme Right)",
       x= "Religious Attendance (1-6 Scale)", 
       subtitle = "December 2019", caption = "Data from CIS Barometer, December 2019")

cor(mydata$voter_ideology, mydata$rel_attendance, use = "pairwise.complete.obs")

## REGRESSION: Who is Vox winning?

issues <- subset(mydata, mydata$partyvote!="NA")
datasummary((`Immigration` = immigration) + 
              (`Economy` = economy) +
              (`Corruption` = corruption) +
              (`Crisis of Values` = values) +
              (`Catalan Independence` = catalonia) +
              (`Sex` = na.omit(sex)) + (`Age` = na.omit(age)) 
            ~ (`N` = length) + mean + SD + Min + Max,
            data = issues,
            output = 'markdown', #change to latex for paper
            title = "Descriptive Statistics for Numeric Variables, Most Important Problem")

# Set up 3 models (all binary DV, different subsets)
issues$allparties_vote <- issues$partyvote
issues <- mutate(issues,
                 allparties_vote= ifelse(issues$allparties_vote == "Vox", 1, 0))
issues$rightparties_vote <- issues$partyvote
issues <- mutate(issues,
                 rightparties_vote = case_when(
                   rightparties_vote == "Vox" ~ 1,
                   rightparties_vote == "PP" ~ 0,
                   rightparties_vote == "Cs" ~ 0))
issues$vox_pp_vote <- issues$partyvote
issues <- mutate(issues,
                 vox_pp_vote = case_when(
                   vox_pp_vote == "Vox" ~ 1,
                   vox_pp_vote == "PP" ~ 0))

#Doing logit regressions (binary DV) for 3 models
allparties <- glm(issues$allparties_vote ~ issues$immigration + issues$economy +
                   issues$corruption + issues$values + issues$catalonia +
                   issues$sex + issues$age)
ideology_test <- glm(issues$allparties_vote ~ issues$voter_ideology + issues$immigration + issues$economy +
                       issues$corruption + issues$values + issues$catalonia +
                       issues$sex + issues$age, family = "binomial")

rightparties <- glm(issues$rightparties_vote ~ issues$immigration + issues$economy +
                      issues$corruption + issues$values + issues$catalonia +
                      issues$sex + issues$age)
justpp <- glm(issues$vox_pp_vote ~ issues$immigration + issues$economy +
                issues$corruption + issues$values + issues$catalonia +
                issues$sex + issues$age)

# Copying code from https://cimentadaj.github.io/blog/2016-08-22
#-producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/
#producing-stargazer-tables-with-odds-ratios-and-standard-errors-in-r/

stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}

#Making table with odds ratios for logit regression (USE THIS)
models <- list(allparties, rightparties, justpp)
stargazer2(models, odd.ratio = T, type = "text",
           title = "Odds Ratios: Issue perception and Vox Support",
           covariate.labels = c("Immigration", "Economy", "Corruption", 
                                "Crisis of Values", "Catalan Independence",
                                "Sex", "Age"),
           dep.var.labels   = c("All Parties", "Right Party Voters", "PP, Vox Voters"), 
           header = FALSE)

