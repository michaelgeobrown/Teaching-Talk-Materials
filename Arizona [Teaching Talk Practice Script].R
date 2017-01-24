library(foreign)
library(ggplot2)

df09<-read.csv("https://raw.githubusercontent.com/michaelgeobrown/Teaching-Talk-Materials/master/AZ_09.csv") #Data frame for College Score Card Data; AZ Institutions 2009-2010
df10<-read.csv("https://raw.githubusercontent.com/michaelgeobrown/Teaching-Talk-Materials/master/AZ_10.csv")
df11<-read.csv("https://raw.githubusercontent.com/michaelgeobrown/Teaching-Talk-Materials/master/AZ_11.csv")
df12<-read.csv("https://raw.githubusercontent.com/michaelgeobrown/Teaching-Talk-Materials/master/AZ_12.csv")
df13<-read.csv("https://raw.githubusercontent.com/michaelgeobrown/Teaching-Talk-Materials/master/AZ_13.csv")
df14<-read.csv("https://raw.githubusercontent.com/michaelgeobrown/Teaching-Talk-Materials/master/AZ_14.csv")


library(tidyverse) #Loads a collection of data cleaning library
df09<-tbl_df(df09) #Converts data frame into a well formatted table
df10<-tbl_df(df10)
df11<-tbl_df(df11)
df12<-tbl_df(df12)
df13<-tbl_df(df13)
df14<-tbl_df(df14)

#For this script, I focus on academic year 2009-2010. You can reproduce all of these analyses for
#each year in the data set. 

#Let's start by subsetting the data by institutional type
df09BABS<-subset(df09, df09$highdeg==3) #Highest Degree: Bachelors
df09GRAD<-subset(df09, df09$highdeg==4) #Highest Degree: Graduate
df09CC<-subset(df09, df09$highdeg==2) #Highest Degree: Associates

#Let's also look at different institutional typese
summary(df09$hbcu) #Historically Black Colleges and Universities
summary(df09$hsi) #Hispanic Serving Institutons
#there are no HBCUs in this dataset. 
hsi09<-subset(df09, subset=df09$hsi==1) #Create an HSI dataframe

summary(hsi09) #there should now be a data frame in the global environment called 'hsi09' with 
#11 observations (schools) and 130 variables.

hsi09<-tbl_df(hsi09) #Convert data frame into a well formatted table

hsi09 #print the table out to the console. LOTS OF VARIABLES!

#Quick graph of Tuition revenue vs instructional expenditure at AZ HSIs in 2009-2010
qplot(tuitfte, inexpfte, data=hsi09)
#Lots of interesting variability here. Let's see what happens when we add in info about 
#the percentage of pell recipients and the highest degree awarded
qplot(tuitfte, inexpfte, data=hsi09, col=as.factor(highdeg), size=pctpell)
#Ok, we want to know what those 'highdeg' colors mean. 
hsi09$highdeg<-as.factor(hsi09$highdeg) #Make the variable a factor variable for graphing
hsi09$highdeg
levels(hsi09$highdeg)<-c("associate","bachelors", "graduate") #We've assigned names to the numerical values
qplot(tuitfte, inexpfte, data=hsi09, col=highdeg)+geom_text(aes(label=hsi09$instnm))

#We can start to see those graphs, but it would be helpful if we could zoom and click around. 
#install.packages(plotly) If you have not used plotly before you will need to install it
library(plotly)

Plot1<-qplot(tuitfte, inexpfte, data=hsi09, col=highdeg)+geom_text(aes(label=hsi09$instnm, size=2))


ggplotly(Plot1)

#To make this print ready, let's use Grammar of Graphics which gives us more lattitude in formatting
plot2<-ggplot(hsi09, aes(tuitfte, inexpfte, col=as.factor(highdeg)))+
  geom_point()+geom_text(aes(label=hsi09$instnm))+
  scale_size(range = c(5, 10))+labs(title="Tuition Revenue vs Instructional Expeniture", subtitle="AZ Hispanic Serving Institutions (2009-2010", x="Tuition Revenue (per student)", y="Instructional Expenditure per student")

plot2

#Interesting. Let's look at a different institutional type. 
#install.packages("ggthemes") If you have not installed the themes package, you'll need to do so now
library(ggthemes)
df09CC$public<-factor(df09CC$control, labels=c("public", "private for profit")) #Let's make a categorical variable
#public/private for our Community College dataset. 

#Normally, you would want to do all this recoding with the original df09 data so that any subsets would 
#carry over your new variables. But we're just exploring the data for now. 

CCg<-ggplot(df09CC, aes(tuitfte, inexpfte, size=(pctpell), col=public))+
  geom_point()+geom_text(aes(label=df09CC$instnm), check_overlap = TRUE, vjust = 0, nudge_x = 0.5,
                         size=4)+
  scale_size(range = c(0, 10))

CCg+theme_light()
CCg+theme_bw() #I like this one
CCg+theme_classic()

#It looks like only one school costs more than $35,000 a year. For now let's make remove them 
#from the data. 

df09CC1<-subset(df09CC, subset=df09CC$tuitfte<=35000)
levels(df09CC1$public)<-c("Public","Private for-profit")

CCg1<-ggplot(df09CC1, aes(tuitfte, inexpfte, size=(pctpell), col=public))+
  geom_point()+geom_text(aes(label=df09CC1$instnm), check_overlap = TRUE, vjust = 0, nudge_x = 0.5,
                         size=4)+
  scale_size(range = c(0, 10))
CCg1+theme_light()+labs(title="Tuition Revenue vs Instructional Expenditure", sub="AZ Associates Granting (2009-2010",
                        x="Tuition Revenue (per student)", y="Instructional Expenditure (per student)")


#Let's compare public to private
CCg2<-ggplot(df09CC1, aes(tuitfte, inexpfte, size=(pctpell)))+
  geom_point()+geom_text(aes(label=df09CC1$instnm), check_overlap = TRUE, vjust = 0, nudge_x = 0.5,
                         size=4)+
  scale_size(range = c(0, 10))+facet_grid(.~df09CC1$public)
CCg2+theme_light()+labs(title="Tuition Revenue vs Instructional Expenditure", sub="AZ Associates Granting (2009-2010",
                        x="Tuition Revenue (per student)", y="Instructional Expenditure (per student)")

#Looks like a lot more students are receiving pell grants at Public Community Colleges in comparison
#to for profit associates granting institutions. 

CCg3<-ggplot(df09CC1, aes(tuitfte, inexpfte, col=pctpell))+
  geom_point(size=4, alpha=0.75)+
  scale_size(range = c(0, 10))+facet_grid(.~df09CC1$public)

#Let's double check that by adding some color to the plot. 

CCg3+theme_light()+labs(title="Cost of Tuition vs Instructional Expenditure per Student",
                        subtitle="Arizona Associate Granting Institutions (2009-2010)",
                        x="Average Cost of Tuition",
                        y="Instructional Expenditure")+
  scale_color_gradient("% Pell Recipients",low="gray", high="purple")


CCg4<-ggplot(df09CC1, aes(tuitfte, inexpfte, size=pctpell, col=public))+
  geom_point(size=4, alpha=0.75)+
  scale_size(range = c(0, 10))

CCg4+theme_light()+labs(title="Cost of Tuition vs Instructional Expenditure per Student",
                        subtitle="Arizona Associate Granting Institutions (2009-2010)",
                        x="Average Cost of Tuition",
                        y="Instructional Expenditure")

#There's a pretty clear split between public and private for profit Associate granting institutions 
#on these two variables. 

#If you want to explore these two variables across multiple years, run the following code to:

#1. Create the data frame

dftime_table<-data.frame(df09$instnm, df09$inexpfte, df09$tuitfte,
                         df10$instnm, df10$inexpfte, df10$tuitfte,
                         df11$instnm, df11$inexpfte, df11$tuitfte,
                         df12$instnm, df12$inexpfte, df12$tuitfte,
                         df13$instnm, df13$inexpfte, df13$tuitfte,
                         df14$instnm, df14$inexpfte, df14$tuitfte)

#2 Write the data frame to a comma seperated file that you can save locally.
#This second step isn't neccesary unless you want to do ongoing work with the file. 
write.csv(dftime_table, "Arizona Institutions 2009-2014.csv")

