#surveydata=read.csv("C:/Xu Huaying/Teaching/STAT 183 Spring 20/Homework/HCI survey/all responses data 1.csv")
surveydata=read.csv("all responses data 1.csv")

attach(surveydata)

N=nrow(surveydata) # total No. of observations

# 1. Use the Question 1 as example to show how to make horizontal barchart for "choose all that apply"
#    question
Q1 <- NULL
for (i in 1:33) { # Column 1-33 are different concerns in Question 1
  Q1 <- c(Q1,table(surveydata[,i])[2])  # total No. of people who chose concern in the ith column
}
Q1_pecentage <- Q1/N  # Percentage of people who chose each concerns
Q1_pecentage <- sort(Q1_pecentage,decreasing = TRUE)  # Sort concerns from highest to lowest percentage

###### overall barchart ####
# barchart of top 10 concerns
bp<-barplot(Q1_pecentage[10:1],
        main = "Top 10 concerns",
        xlab = "Percentage",
#        ylab = "Day",
#        names.arg = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        col = "darkred",
        horiz = TRUE,
        axisnames=FALSE, xlim=c(0, 1.1*max(Q1_pecentage)))
# Add name of concern related to each bar
axis(2, at=bp, labels=names(Q1_pecentage)[10:1], tick=FALSE, las=1, line=-0.8, cex.axis=0.5)
# Add percentage of each concern to the related bar
text(Q1_pecentage[10:1], bp,round(Q1_pecentage[10:1], 2), cex=1, pos=4)


##### female subset ####
table(surveydata$What.is.your.gender.)

# subset data - female only, and then follow the steps above to plot the barchart for female
female <- subset(surveydata, What.is.your.gender.=='Female')

attach(female)

N.f=nrow(female) # total No. of observations

for (i in 1:33) { # Column 1-33 are different concerns in Question 1
  Q1.f <- c(Q1.f,table(female[,i])[2])  # total No. of people who chose concern in the ith column
}
Q1.f_pecentage <- Q1.f/N.f  # Percentage of people who chose each concerns
Q1.f_pecentage.sort <- sort(Q1.f_pecentage,decreasing = TRUE) 
Q1.f_pecentage.sort[1:10]

##### male subset #####


male <- subset(surveydata, What.is.your.gender.=='Male')

attach(male)

N.m=nrow(male) # total No. of observations

# 1. Use the Question 1 as example to show how to make horizontal barchart for "choose all that apply"
#    question
Q1.m <- NULL
for (i in 1:33) { # Column 1-33 are different concerns in Question 1
  Q1.m <- c(Q1.m,table(male[,i])[2])  # total No. of people who chose concern in the ith column
}
Q1.m_pecentage <- Q1.m/N.m  # Percentage of people who chose each concerns
Q1.m_pecentage.sort <- sort(Q1.m_pecentage,decreasing = TRUE) 
Q1.m_pecentage.sort[1:10]

##### chisq test example ####
# Test if the percentage of faculty, staff and students who are concerned on anxiety are the same.
# We are only interested in faculty, staff and students, therefore we subset data with UCR affiliation
# only be faculty, staff and students.
UCRAffiliation <- subset(surveydata, What.is.your.UCR.affiliation.=='Faculty' 
                         | What.is.your.UCR.affiliation.=='Staff' 
                         | What.is.your.UCR.affiliation.=='Student')
tbl = table(UCRAffiliation$What.is.your.UCR.affiliation.,UCRAffiliation$Anxiety)
chisq.test(tbl)
# The Chi square test of independence is significant, which means there is significant difference 
# on the percentage of facult, staff and studens who are concerned on anxiety. We further did 
# pairwise comparison to see how the percentages are different from each other. If you are doing 
# stratified analysis by gender, you don't need this step because there are only two levels for 
# gender, female and male.
library(rcompanion)
pairwiseNominalIndependence(tbl,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method = "bonferroni")

##### chisq test by gender ####
# removing nonbinary and blank responses
UCRGender <- subset(surveydata, 
                    What.is.your.gender.=='Male' 
                    | What.is.your.gender. == "Female")
# we only want the top 10 concerns for each gender
as.data.frame("Category" = names(Q1.f_pecentage), 
              "Percentage" = Q1.f_pecentage)

merge(Q1.f_pecentage[1:10], Q1.m_pecentage[1:10], all = F)

###### create a blank table ########
# gender_tbl <- data.frame("Category" = rep(0,33),
#                          "Male" = rep(0, 33),
#                          "Female" = rep(0,33))
# for (i in 1:33) {
#   gender_tbl[i,] <- c(
#     names(table(UCRGender[,i]))[2], # a convoluted way to get the category name
#     table(subset(UCRGender, What.is.your.gender. == "Male")[,i])[2],
#     table(subset(UCRGender, What.is.your.gender. == "Female")[,i])[2]
#     )
# }


#### a for loop that gives you just the pvalue for each category ####
chisq_tbl <- data.frame("Category" = rep(0, 33),
                        "Percent.female" = as.numeric(Q1.f_pecentage),
                        "Percent.male" = as.numeric(Q1.m_pecentage),
                        "pvalue" = rep(0,33)) 
for (i in 1:33) {
  temp.col <- names(table(UCRGender[,i]))[2]
  chisq_tbl[i, 1] <- temp.col
  temp.tbl <- table(UCRGender$What.is.your.gender.,
                    UCRGender[,i])
  chisq.gender <- chisq.test(temp.tbl)
  chisq_tbl[i, 4] <- chisq.gender$p.value
  
}
chisq_tbl

write.csv(chisq_tbl, "q1_chisq_tbl.csv", row.names = T)

gender_tbl
chisq.test(gender_tbl[1,2:3])

tbl = table(UCRAffiliation$What.is.your.UCR.affiliation.,UCRAffiliation$Anxiety)
chisq.test(tbl)
chisqtoy <- chisq.test(tbl)

##### question 2 #####
# 2. Example for the single answer (select one) question
What.is.your.gender. <- factor(What.is.your.gender., levels=c("Female","Male"))
What.is.your.UCR.affiliation. <- factor(What.is.your.UCR.affiliation., levels=c("Faculty","Staff","Student"))

mytable <- table(What.is.your.UCR.affiliation., What.is.your.gender.)
mytable

per_table <-prop.table(mytable, 1)
per_table

bp<-barplot(per_table, main="UCR Affliliation by Gender",
        xlab="Gender", legend = rownames(mytable), beside=TRUE,
        ylim = c(0, 1.1*max(per_table)))
text(bp, per_table,round(per_table, 2), cex=1, pos=3)

names(Q1_pecentage[1:10])


