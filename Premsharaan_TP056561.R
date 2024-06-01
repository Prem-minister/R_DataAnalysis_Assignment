#Premsharaan A/L Selva
#TP056561

cat("\014")

#====Installing various packages====#
install.packages("tidyverse")
install.packages("plotrix")
install.packages("ggpubr")
install.packages("ggthemes")
install.packages("treemapify")
install.packages("grid")
install.packages("gridExtra")


#====Load the installed packages====#
library(tidyverse) #ggplot2, dplyr, readr, tidyr, tibble
library(plotrix)
library(ggpubr)
library(ggthemes)
library(treemapify)
library(grid)
library(gridExtra)

#=======Data Importing========#
#========Import the dataset========#
#Importing the dataset from CSV file while naming the object to dsap_data
#dsap stands for Degree Students Academic Performance 
dsap_data = read.csv("C:\\Users\\User\\OneDrive - Asia Pacific University\\APU degree studies\\Sem1\\PFDA\\Assignment\\student.csv") 
dsap_data

#=======Data Processing========#
#========View the dataset structure========#
glimpse(dsap_data)


#========Check for any null or empty values inside the dataset========#
nulldata = sapply(dsap_data,function(x) sum(is.na(x)))
nulldata


#========Check for any data duplication inside the dataset========#
sum(duplicated(dsap_data)==TRUE)


#========Data Transformation========#
#no - 1 || yes  - 2
dsap_data$schoolsup = unclass(factor(dsap_data$schoolsup))
dsap_data$famsup = unclass(factor(dsap_data$famsup))
dsap_data$paid = unclass(factor(dsap_data$paid))
dsap_data$activities = unclass(factor(dsap_data$activities))
dsap_data$nursery = unclass(factor(dsap_data$nursery))
dsap_data$higher = unclass(factor(dsap_data$higher))
dsap_data$internet = unclass(factor(dsap_data$internet))
dsap_data$romantic = unclass(factor(dsap_data$romantic))

#========Create a new column for the mean of the math grade for three period========#
dsap_data$avgGradeNotRoundedOff = rowMeans(subset(dsap_data, select = c(G1,G2,G3)))
dsap_data$avgGrade = round(dsap_data$avgGradeNotRoundedOff)


#========Assigning the average grade into a new range column========#
dsap_data = dsap_data %>% mutate(avgGradeRange = case_when(avgGrade <=5 ~ "00-05", 
                                                 avgGrade <=10 ~ "06-10", 
                                                 avgGrade <=15 ~ "11-15", 
                                                 avgGrade <=20 ~ "16-20",))


#========Average grade of all students combining together========#
avgMeanGrade = round(mean(dsap_data$avgGrade))
avgMeanGrade


#=======Data Exploration========#
#========View the summary of the dataset========#
summary(dsap_data)

#View all the data from the dataset in a table form
View(head(dsap_data, 10)) #Seeing the first 10 rows of data



#=======Question 1========#
#Question 1: How do personal relationships impact a student's grades?
#Analysis 1-1 
#Finding the correlation between students' romantic relationship status and their average grades.
Q1A1R1<- dsap_data %>% group_by(romantic,avgGradeRange) %>% summarise(counts = n())
Q1A1R1
Q1A1V1<- ggplot(Q1A1R1, aes(avgGradeRange, y=counts, fill = as.factor(romantic))) +
         geom_bar(stat = "identity", position=position_dodge2(), width = 0.5, color="black") + 
         ggtitle("The number of Students with their Average Student Marks grouped by their Romantic Relationship Status") +
         theme(plot.title = element_text(size = 15, face = "bold")) + 
         labs(fill = "Student Romantic Relationship Status", x="Average Student Marks Range", y = "Student Counts")+
         facet_wrap(~romantic, labeller = as_labeller(c(`2`="Yes", `1`="No"))) +
         geom_text(aes(label=counts, vjust=-0.3)) +
         scale_fill_manual(values = c("#FFD700", "#B8860B"),labels = c("1 - No", "2 - Yes"))
Q1A1V1


Q1A1R2 <- dsap_data %>% group_by(romantic) %>% filter(avgGrade > 15) %>%
          summarise(counts = n(), percentage= n()/length(which(dsap_data$avgGrade>15))*100)
Q1A1R2
Q1A1V2 <- ggplot(Q1A1R2, aes(x="", y =percentage, fill=as.factor(romantic))) +
          geom_col(color = "black") + coord_polar("y", start = 0) +
          theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +
          geom_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                    color = c("white"), position = position_stack(vjust=0.5)) + 
          ggtitle("Shows the percentage of the Students that scored\n> 15 marks and their Romantic Relantionship\nStatus.") +
          labs(fill="Romantic Relationship Status")+ scale_fill_manual(values = c("#228B22","#CD5C5C"), 
                                                                       labels = c("1 - No", "2 - Yes"))
Q1A1V2


Q1A1R3 <- dsap_data %>% group_by(romantic) %>% filter(avgGrade <5) %>% 
          summarise(counts = n(), percentage= n()/length(which(dsap_data$avgGrade<5))*100)
Q1A1R3
Q1A1V3 <- ggplot(Q1A1R3, aes(x="", y =percentage, fill=as.factor(romantic))) + geom_col(color = "black") + 
          coord_polar("y", start = 0) +
          theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +
          geom_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), color = c("white"), 
                    position = position_stack(vjust=0.5)) + 
          ggtitle("Shows the percentage of the Students that scored\n< 5 marks and their Romantic Relantionship\nStatus.") +
          labs(fill="Romantic Relationship Status") + 
          scale_fill_manual(values = c("#228B22","#CD5C5C"), labels = c("1 - No", "2 - Yes"))
Q1A1V3

#Show combined two pies into one view
ggarrange(Q1A1V2, Q1A1V3, nrow = 2, ncol = 1)


#Analysis 1-2
#Finding the correlation between students' going on outings with their friends and their average grades.
Q1A2R1<- dsap_data %>%  group_by(goout,avgGradeRange) %>% summarise(counts = n())
Q1A2R1
Q1A2V1<- ggplot(Q1A2R1, aes(x=avgGradeRange, y=counts, fill=as.factor(goout))) +
         geom_bar(stat="identity",width = 0.5, color="black") +
         ggtitle("The number of Students with their average score grouped by the level of them going out.")+
         labs(x="Average Student Marks Range", y = "Student Counts", fill="Students Going Out With Friends")+ 
         theme(plot.title = element_text(size = 15, face = "bold")) +
         scale_fill_manual(values=c("#FF6347", "#FFA500", "#DAA520", "#7CFC00", "#2E8B57", "#1E90FF"),
                           labels = c("1 - Very Low", "2 - Low", "3 - Medium", "4 - High", "5 - Very High"))+
         geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q1A2V1

Q1A2R2 <- dsap_data %>% group_by(goout) %>% filter(avgGrade>15) %>% 
          summarise(counts = n(),percentage= n()/length(which(dsap_data$avgGrade>15))*100) 
                                                                         
Q1A2R2
Q1A2V2 <- ggplot(Q1A2R2, aes(x="", y =percentage, fill=as.factor(goout))) + geom_col(color = "black") + coord_polar("y", start = 0) +
          theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) + 
          geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                    color = c("white"), position = position_stack(vjust=0.5)) + 
          ggtitle("Shows the percentage of the Students that scored\n> 15 marks and their frequency of Going Out\nwith Friends.") +
          labs(fill="Students Going Out With Friends")+ 
          scale_fill_manual(values = c("#4B0082","#DA70D6","#8B4513", "#191970","#2F4F4F"), 
                             labels = c("1 - Very Low", "2 - Low", "3 - Medium", "4 - High", "5 - Very High"))
Q1A2V2

Q1A2R3<- dsap_data %>% group_by(goout) %>% filter(avgGrade <=5) %>%
         summarise(counts = n(), percentage= n()/length(which(dsap_data$avgGrade<=5))*100)
Q1A2R3
Q1A2V3 <- ggplot(Q1A2R3, aes(x="", y =percentage, fill=as.factor(goout))) + geom_col(color = "black") + coord_polar("y", start = 0) +
          theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +
          geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                    color = c("white"), position = position_stack(vjust=0.5)) + 
          ggtitle("Shows the percentage of the Students that scored\n<= 5 marks and their frequency of Going Out\nwith Friends.") +
          labs(fill="Students Going Out With Friends")+ 
          scale_fill_manual(values = c("#4B0082","#DA70D6","#8B4513", "#191970","#2F4F4F"), 
                            labels = c("1 - Very Low", "2 - Low", "3 - Medium", "4 - High", "5 - Very High"))
Q1A2V3

ggarrange(Q1A2V2, Q1A2V3, nrow = 2, ncol = 1)


#Analysis 1-3
#Finding the relationship between the quality of the student's family relationships and the average grades.
Q1A3R1<- dsap_data %>% group_by(famrel,avgGradeRange) %>% summarise(counts = n())
Q1A3R1
Q1A3V1<- ggplot(Q1A3R1, aes(x= avgGradeRange, y=counts, fill = as.factor(famrel))) + 
         geom_bar(stat = "identity", position = position_dodge2(preserve = 'single'), width=0.9)+ 
         ggtitle("The number of Students with their average score grouped by their Family Relantionship quality.")+
         theme(plot.title = element_text(size = 15, face = "bold")) + 
         labs(fill = "Students Family Relationship Level", x="Average Students Marks Range", y = "Student Counts")+
         geom_text(aes(label=counts), position = position_dodge2(1), vjust=-0.5) +
         scale_fill_manual(values = c("#40E0D0","#191970","#FF1493", "#DEB887","#6A5ACD"),
                           labels = c("1 - Very Bad", "2 - Bad", "3 - Okay", "4 - Good", "5 - Excellent"))
Q1A3V1


Q1A3R2 <- dsap_data %>% group_by(famrel) %>% filter(avgGrade>15)%>% 
          summarise(counts = n(), percentage= n()/length(which(dsap_data$avgGrade>15))*100)
Q1A3R2
Q1A3V2<- ggplot(Q1A3R2, aes(x="", y =percentage, fill=as.factor(famrel)))+ 
         geom_col(color = "black") + coord_polar("y", start = 0)+
         theme(panel.background = element_blank(),
               axis.title = element_blank(),
               axis.text = element_blank(),
               axis.line = element_blank(),
               axis.ticks= element_blank(),
         plot.title = element_text(size = 20, face = "bold")) +
         geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                   color = c("white"), position = position_stack(vjust=0.5)) + 
         ggtitle("Shows the percentage of the students that scored > 15 marks 
         and their quality of relationship with their family.") +
         labs(fill="Students Family Relationship Level")+ 
         scale_fill_manual(values = c("#40E0D0","#191970","#FF1493", "#DEB887","#6A5ACD"), 
                           labels = c("1 - Very Low", "2 - Low", "3 - Medium", "4 - High", "5 - Very High"))
Q1A3V2


Q1A3R3 <- dsap_data %>% group_by(famrel) %>% filter(avgGrade<=5)%>% 
          summarise(counts = n(), percentage= n()/length(which(dsap_data$avgGrade<=5))*100)
Q1A3R3
Q1A3V3<- ggplot(Q1A3R3, aes(x="", y =percentage, fill=as.factor(famrel)))+ 
         geom_col(color = "black") + coord_polar("y", start = 0) +
         theme(panel.background = element_blank(),
               axis.title = element_blank(),
               axis.text = element_blank(),
               axis.line = element_blank(),
               axis.ticks= element_blank(),
         plot.title = element_text(size = 20, face = "bold"))+
         geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                  color = c("white"), position = position_stack(vjust=0.5)) + 
         ggtitle("Shows the percentage of the students that scored <= 5 marks 
         and their quality of relationship with their family.") +
         labs(fill="Students Family Relationship Level")+ 
         scale_fill_manual(values = c("#40E0D0","#191970","#FF1493", "#DEB887","#6A5ACD"), 
                           labels = c("1 - Very Low", "2 - Low", "3 - Medium", "4 - High", "5 - Very High"))
Q1A3V3


ggarrange(Q1A3V2, Q1A3V3, nrow = 2, ncol = 1)


#Analysis 1-4
#Finding the relationship between the students' guardian and their average marks.
Q1A4R1<- dsap_data %>% group_by(guardian, avgGradeRange) %>%  summarise(counts= n())
Q1A4R1
Q1A4V1 <-  ggplot(Q1A4R1, aes(x=avgGradeRange, y = counts, fill = guardian)) +
           geom_bar(stat = "identity", width = 0.9, color="black", position = "dodge") +
           labs(fill="Guardian", x = "Average Student Marks Range", y="Student Counts") + 
           ggtitle("The number of Students with their average score grouped by Guardian.") +
           theme(plot.title = element_text(size = 15, face = "bold")) + 
           geom_text(aes(label=counts), position = position_dodge2(1), vjust=-0.5) 
Q1A4V1

Q1A4R2<-  dsap_data %>% group_by(guardian) %>% filter(avgGrade>=avgMeanGrade)%>% 
          summarise(counts = n(), percentage= n()/length(which(dsap_data$avgGrade>=avgMeanGrade))*100)
Q1A4R2
Q1A4V2 <- ggplot(Q1A4R2, aes(x="", y =percentage, fill=guardian)) + 
          geom_col(color = "black") + coord_polar("y", start = 0) +
          theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold"))+
          geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")),
                    color = c("white"), position = position_stack(vjust=0.5)) + 
          ggtitle("Shows the percentage of the students that scored >= 
          average mean grade and their Guardian.") +
          labs(fill="Guardian")+
          scale_fill_manual(values = c("#191970","#FF1493", "#00008B"))
Q1A4V2


Q1A4R3<-  dsap_data %>% group_by(guardian) %>% filter(avgGrade<avgMeanGrade)%>% 
          summarise(counts = n(), percentage= n()/length(which(dsap_data$avgGrade<avgMeanGrade))*100)
Q1A4R3
Q1A4V3 <- ggplot(Q1A4R3, aes(x="", y =percentage, fill=guardian))+ 
          geom_col(color = "black") + coord_polar("y", start = 0) +
          theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold"))+
          geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                    color = c("white"), position = position_stack(vjust=0.5)) + 
          ggtitle("Shows the percentage of the students that scored <
          average mean grade and their Guardian.") +
          labs(fill="Guardian")+ scale_fill_manual(values = c("#191970","#FF1493", "#00008B"))
Q1A4V3

ggarrange(Q1A4V2, Q1A4V3, nrow = 2, ncol = 1)


#Analysis 1-5
#Finding the relationship between students' romantic relationship status, their study time and their average marks.
Q1A5R1 <- dsap_data %>% group_by(romantic,avgGradeRange, studytime)  %>%  summarise(counts= n())
Q1A5R1
Q1A5V1<- ggplot(Q1A5R1, aes(x=avgGradeRange, y=counts, fill = as.factor(studytime)))+ 
         geom_bar(stat = "identity", width = 0.5, color="black") + 
         ggtitle("The number of Students with their Average Student Marks
         grouped by their Romantic Relationship Status and the total Study Time") +
         theme(plot.title = element_text(size = 15, face = "bold")) + 
         labs(fill = "Student Study Time (Hours)", x="Average Student Marks Range", y = "Student Counts")+
         facet_wrap(~romantic, labeller = as_labeller(c(`1`="Relationship: No", `2`="Relationship: Yes"))) +
         geom_text(aes(label=counts), position = position_stack(vjust = 0.5)) +
         scale_fill_manual(values=rainbow(10))
Q1A5V1



#=======Question 2========#
#Question 2: How does time management impact the students' grades?
#Analysis 2 - 1
#Finding the relationship between the students' study time and their average marks.
Q2A1R1 <- dsap_data %>% group_by(studytime, avgGradeRange)  %>%  summarise(counts= n())
Q2A1R1
Q2A1V1<-  ggplot(Q2A1R1, aes(avgGradeRange, y=counts, fill = as.factor(studytime))) + 
          geom_bar(stat = "identity", position = position_dodge2(preserve = 'single'), width=0.9) + 
          ggtitle("The number of Students with their average score grouped by their Study Time.") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs(fill = "Student Study Time (Hours)", x="Average Students Marks Range", y = "Student Counts")+
          geom_text(aes(label=counts), position = position_dodge2(1), hjust = -0.1) + 
          ylim(0,250) +
          scale_fill_manual(values = c("#40E0D0","#191970","#FF1493", "#DEB887","#6A5ACD")) +
          coord_flip() + facet_wrap(~studytime)
Q2A1V1


Q2A1R2 <- dsap_data %>% group_by(studytime) %>% filter(avgGrade>15) %>% summarise(counts = n())
Q2A1R2
Q2A1V2 <- ggplot(Q2A1R2, aes(studytime, counts)) + geom_point(aes(color=as.factor(studytime))) + 
          geom_line(aes(studytime)) +
          ggtitle("The number of Students scored > 15 average mark grouped by their Study Time.") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs( x="Student Study Time (Hours)", y = "Student Counts")+
          geom_text(aes(label=counts), position = position_dodge2(1), hjust = -0.5)  + 
          scale_color_manual(name = "Student Study Time (Hours)", 
                             values = c(`4` = "darkblue", `3` = "red", `2` = "black", `1`= "green" ))
Q2A1V2

Q2A1R3 <- dsap_data %>% group_by(studytime) %>% filter(avgGrade<=5) %>% summarise(counts = n())
Q2A1R3
Q2A1V3 <- ggplot(Q2A1R3, aes(studytime, counts)) + geom_point(aes(color=as.factor(studytime))) +
          geom_line(aes(studytime)) +
          ggtitle("The number of Students scored <= 5 average mark grouped by their Study Time.") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs( x="Student Study Time (Hours)", y = "Student Counts")+
          geom_text(aes(label=counts), position = position_dodge2(1), hjust = -0.5)  + 
          scale_color_manual(name = "Student Study Time (Hours)",
                             values = c(`4` = "darkblue", `3` = "red", `2` = "black", `1`= "green" ))

Q2A1V3

ggarrange(Q2A1V2, Q2A1V3, nrow = 2, ncol = 1)


#Analysis 2-2
#Finding the relationship between students' free time and their average grades.
Q2A2R1<- dsap_data %>% group_by(freetime, avgGradeRange)  %>%  summarise(counts= n())
Q2A2R1
Q2A2V1<-  ggplot(Q2A2R1, aes(x=avgGradeRange, y=counts,fill=as.factor(freetime))) + 
          geom_bar(stat="identity",width = 0.5, color="black") +
          ggtitle("The number of Students with their average score grouped by the frequency of them having Free Time (Hours).")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="Students Free Time (Hours)")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          scale_fill_manual(values=c("#FF6347", "#FFA500", "#DAA520", "#7CFC00", "#2E8B57", "#1E90FF"),
                            labels = c("1", "2", "3", "4", "5"))+
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q2A2V1


Q2A2R2<- dsap_data %>% group_by(freetime, avgGrade) %>% filter(avgGrade>15) %>% summarise(counts = n())
Q2A2R2
Q2A2V2 <- ggplot(Q2A2R2, aes(avgGrade, counts)) + 
          geom_point(aes(color=as.factor(freetime)), position = position_dodge(width = 0.02)) + 
          geom_line(aes(color=as.factor(freetime))) +
          ggtitle("The number of Students scored > 15 average mark grouped by their Free Time (Hours).") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs( x="Average Grade", y = "Student Counts")+
          geom_text(aes(label=counts), hjust = -0.6)  + 
          ylim(0,20)+
          scale_color_manual(name = "Student Free Time (Hours)", 
                             values = c(`5` = "yellow",`4` = "darkblue", `3` = "red", `2` = "green", `1`= "black" ))
Q2A2V2


Q2A2R3<- dsap_data %>% group_by(freetime, avgGrade) %>% filter(avgGrade<=5) %>% summarise(counts = n())
Q2A2R3
Q2A2V3 <- ggplot(Q2A2R3, aes(avgGrade, counts)) + 
          geom_point(aes(color=as.factor(freetime)), position = position_dodge(width = 0.02)) + 
          geom_line(aes(color=as.factor(freetime))) +
          ggtitle("The number of Students scored <= 5 average mark grouped by their Free Time (Hours).") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs( x="Average Grade", y = "Student Counts")+
          geom_text(aes(label=counts), hjust = -0.5)  + 
          ylim(0,20)+
          scale_color_manual(name = "Student Free Time (Hours)", 
                             values = c(`5` = "yellow",`4` = "darkblue", `3` = "red", `2` = "green", `1`= "black" ))
Q2A2V3

ggarrange(Q2A2V2, Q2A2V3, nrow = 2, ncol = 1)


#Analysis 2-3
#Finding the correlation between students' extra-curricular activities participation and their average grades.
Q2A3R1<- dsap_data %>%  group_by(activities,avgGradeRange) %>% summarise(counts = n())
Q2A3R1
Q2A3V1<-  ggplot(Q2A3R1, aes(avgGradeRange, y=counts, fill = as.factor(activities))) +
          geom_bar(stat = "identity", position=position_dodge2(), width = 0.5, color="black") + 
          ggtitle("The number of Students with their Average Student Marks grouped by\ntheir Extra-curricular Activities Status") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs(fill = "Student Extra-curricular Activities Status", 
               x="Average Student Marks Range", y = "Student Counts")+
          facet_wrap(~activities, labeller = as_labeller(c(`2`="Participated", `1`="Not Participated"))) +
          geom_text(aes(label=counts), vjust=-0.3) + 
          scale_fill_manual(values = c("#800080", "#1E90FF"),labels = c("1 - No", "2 - Yes"))
Q2A3V1     

Q2A3R2 <- dsap_data %>% filter(activities==2, avgGrade>15) %>% 
          group_by(activities, studytime, avgGrade) %>% summarise(counts = n())
Q2A3R2 
Q2A3V2 <- ggplot(Q2A3R2, aes(avgGrade, y=counts, fill=as.factor(studytime))) + geom_bar(stat = "identity",width = 0.5, color="black") +
          ggtitle("Students that have scored > 15 average mark and been joined extra-curricular activites\ngrouped by their Study Time (Hours).") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs(fill = "Student Time (Hours)", x="Average Student Marks", y = "Student Counts")  +
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5)) +
          scale_fill_manual(values=c("#FF6347", "#FFA500", "#DAA520", "#7CFC00", "#2E8B57"))
Q2A3V2


Q2A3R3 <- dsap_data %>% filter(activities==1, avgGrade>15) %>% 
          group_by(activities, studytime, avgGrade) %>% summarise(counts = n())
Q2A3R3 
Q2A3V3 <- ggplot(Q2A3R3, aes(avgGrade, y=counts, fill=as.factor(studytime))) + geom_bar(stat = "identity",width = 0.5, color="black") +
          ggtitle("Students that have scored > 15 average mark and not been joined \nextra-curricular activites grouped by their Study Time (Hours).") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs(fill = "Student Time (Hours)", x="Average Student Marks", y = "Student Counts")  +
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5)) +
          scale_fill_manual(values=c("#FF6347", "#FFA500", "#DAA520", "#7CFC00", "#2E8B57"))
Q2A3V3

ggarrange(Q2A3V2, Q2A3V3, nrow = 2, ncol = 1)




#Analysis 2-4
#Finding the correlation between students' study time, free time and their average grades.
Q2A4R1<- dsap_data %>%  group_by(freetime, studytime, avgGradeRange) %>% 
         summarise(counts = n())
Q2A4R1

Q2A4V1<- ggplot(Q2A4R1, aes(x=avgGradeRange, y=counts, fill = as.factor(studytime))) + 
         geom_bar(stat = "identity", position = position_dodge2(),width = 0.5, color="black") + 
         ggtitle("The number of Students with their Average Student Marks grouped by\ntheir Study Time and Free Time") +      
         theme(plot.title = element_text(size = 15, face = "bold")) + 
         labs(fill = "Student Study Time (Hours)", x="Average Student Marks Range", y = "Student Counts")+
         facet_grid(studytime~freetime, labeller = labeller(.cols=label_both,.rows =label_both)) + 
         geom_text(aes(label=counts), vjust=-0.3) +
         scale_fill_manual(values=c("#800080", "#F4A460", "#00BFFF", "#FFA500"))
Q2A4V1        
        
Q2A4R2 <-  dsap_data %>% group_by(freetime, studytime) %>% filter(avgGrade>15) %>%
           summarise(counts = n()) 
View(Q2A4R2)



#=======Question 3========#
#Question 3: How do resource availability and the comfort impact students' marks?
#Analysis 3-1
#Finding the relationship between students' joining additional paid classes and their average grades.
Q3A1R1 <- dsap_data %>% group_by(paid, avgGradeRange) %>% summarise(counts = n())
Q3A1R1
Q3A1V1 <-  ggplot(Q3A1R1, aes(avgGradeRange, y=counts, fill = as.factor(paid))) +
           geom_bar(stat = "identity", width=0.9) + 
           ggtitle("The number of Students with their average score grouped by their joining Paid Additional Classes status.") +
           theme(plot.title = element_text(size = 15, face = "bold")) + 
           labs(fill = "Student Paid Additional Classes", x="Average Students Marks Range", y = "Student Counts")+
           geom_text(aes(label=counts), position = position_dodge2(1), hjust = -0.1) + ylim(0,250) +
           scale_fill_manual(values = c("#B22222","#7CFC00"),labels = c("1 - No", "2 - Yes")) +
           coord_flip() + facet_wrap(~paid, labeller = as_labeller(c(`2`="Joined", `1`="Not Joined")))
Q3A1V1


Q3A1R2 <- dsap_data %>% group_by(paid) %>% filter(avgGrade>avgMeanGrade) %>% 
          summarise(counts = n(), percentage = n()/length(which(dsap_data$avgGrade>avgMeanGrade))*100)
Q3A1R2
Q3A1V2 <- ggplot(Q3A1R2, aes(x=percentage, y="", fill = as.factor(paid), area = percentage))+ geom_treemap()+ 
          labs(title = "") +  theme(panel.background = element_blank(),
                                    axis.title = element_blank(),
                                    axis.text = element_blank(),
                                    axis.line = element_blank(),
                                    axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +
          geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                            color = c("white"), place = "centre") +
          ggtitle("Shows the percentage of the Students that scored\n> average mean mark and their status of attending for\npaid Additional Class.") +
          labs(fill="Student Paid Additional Classes")+ scale_fill_manual(values = c("#4B0082","#DA70D6"), 
                                                                          labels = c("1 - No", "2 - Yes"))
Q3A1V2
 
 

Q3A1R3 <- dsap_data %>% group_by(paid) %>% filter(avgGrade<avgMeanGrade) %>% 
          summarise(counts = n(), percentage = n()/length(which(dsap_data$avgGrade<avgMeanGrade))*100)
Q3A1R3
Q3A1V3 <- ggplot(Q3A1R3, aes(x=percentage, y="", fill = as.factor(paid), area = percentage)) +geom_treemap()+ 
          labs(title = "") +  theme(panel.background = element_blank(),
                                    axis.title = element_blank(),
                                    axis.text = element_blank(),
                                    axis.line = element_blank(),
                                    axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +
          geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                            color = c("white"), place = "centre") +
          ggtitle("Shows the percentage of the Students that scored\n< average mean mark and their status of attending for\npaid Additional Class.") +
          labs(fill="Student Paid Additional Classes")+ scale_fill_manual(values = c("#4B0082","#DA70D6"), 
                                                                          labels = c("1 - No", "2 - Yes"))
Q3A1V3

ggarrange(Q3A1V2, Q3A1V3, nrow = 2, ncol = 1)


Q3A1R4 <- dsap_data %>% group_by(paid)%>% 
          filter(avgGrade>15) %>% summarise(counts = n(), percentage = n()/length(which(dsap_data$avgGrade>15))*100)
Q3A1R4
Q3A1V4 <- ggplot(Q3A1R4, aes(x="", y =percentage, fill=as.factor(paid))) + geom_col(color = "black") + coord_polar("y", start = 0) +
          theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                                                                          color = c("white"), position = position_stack(vjust=0.5)) + 
          ggtitle("Shows the percentage of the students that scored\n> 15 marks and their status of attending for\npaid Additional Class.") +
          labs(fill="Paid Additional Class Status")+ scale_fill_manual(values = c("#191970","#FF1493"), 
                                                                       labels=c("1 - No", "2 - Yes"))

Q3A1V4
  

Q3A1R5 <- dsap_data %>% group_by(paid) %>% filter(avgGrade<=5) %>% 
          summarise(counts = n(), percentage = n()/length(which(dsap_data$avgGrade<=5))*100)
Q3A1R5  
Q3A1V5 <- ggplot(Q3A1R5, aes(x="", y =percentage, fill=as.factor(paid))) + geom_col(color = "black") + coord_polar("y", start = 0) +
          theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")),
                                                                          color = c("white"), position = position_stack(vjust=0.5)) + 
          ggtitle("Shows the percentage of the students that scored\n<= 5 marks and their status of attending for\npaid Additional Class.") +
          labs(fill="Paid Additional Class Status")+scale_fill_manual(values = c("#191970","#FF1493"), 
                                                                      labels=c("1 - No", "2 - Yes"))

Q3A1V5

ggarrange(Q3A1V4, Q3A1V5, nrow = 2, ncol = 1)



#Analysis 3-2
#Finding the relationship between students' additional educational support from the school and their average marks.
Q3A2R1<- dsap_data %>% group_by(schoolsup, avgGradeRange) %>%  summarise(counts = n())
Q3A2R1
Q3A2V1<- ggplot(Q3A2R1, aes(x = avgGradeRange, y=counts, color= as.factor(schoolsup))) + geom_point(stat="identity",alpha = 0.8) +
         ggtitle("The number of Students with their average score grouped\nby their Extra Education Support status.") +
         theme(plot.title = element_text(size = 15, face = "bold")) +
         labs(x="Average Students Marks Range", y = "Student Counts")+
         geom_text(aes(label=counts), vjust=-1.0)  + ylim(0, 450) +
         scale_color_manual(name = "Student Additional Educational Support\n from school status", 
                            values = c(`1` = "black",`2` = "red" ), labels=c("1 - No", "2 - Yes"))
        
Q3A2V1

Q3A2R2<-  dsap_data %>% group_by(schoolsup)%>% 
          filter(G1<=5) %>% summarise(counts = n(), percentage = n()/length(which(dsap_data$G1<=5))*100)
Q3A2R2  
Q3A2V2 <-  ggplot(Q3A2R2, aes(x=percentage, y="", fill = as.factor(schoolsup), area = percentage)) +
           geom_treemap()+ 
           labs(title = "") +  theme(panel.background = element_blank(),
                                     axis.title = element_blank(),
                                     axis.text = element_blank(),
                                     axis.line = element_blank(),
                                     axis.ticks= element_blank(),
           plot.title = element_text(size = 20, face = "bold")) +
           geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                             color = c("white"), place = "centre") +
           ggtitle("Shows the percentage of the Students that scored <= 5 marks in their G1, 
                   grouped by Student Additional Educational Support from school status") +
           labs(fill="Student Additional Educational Support\nfrom school status")+ 
          scale_fill_manual(values = c("#7FFF00","#008B8B"), labels = c("1 - No", "2 - Yes"))
Q3A2V2


Q3A2R3<-  dsap_data %>% group_by(schoolsup)%>% 
          filter(G2<=5) %>% summarise(counts = n(), percentage = n()/length(which(dsap_data$G2<=5))*100)
Q3A2R3  
Q3A2V3 <-  ggplot(Q3A2R3, aes(x=percentage, y="", fill = as.factor(schoolsup), area = percentage)) +
           geom_treemap()+ 
           labs(title = "") +  theme(panel.background = element_blank(),
                                     axis.title = element_blank(),
                                     axis.text = element_blank(),
                                     axis.line = element_blank(),
                                     axis.ticks= element_blank(),
           plot.title = element_text(size = 20, face = "bold")) +
           geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")),
                             color = c("white"), place = "centre") +
           ggtitle("Shows the percentage of the Students that scored <= 5 marks in their G2, 
                   grouped by Student Additional Educational Support from school status") +
           labs(fill="Student Additional Educational Support\nfrom school status")+
          scale_fill_manual(values = c("#7FFF00","#008B8B"), labels = c("1 - No", "2 - Yes"))
Q3A2V3


Q3A2R4<-  dsap_data %>% group_by(schoolsup)%>% 
          filter(G3<=5) %>% summarise(counts = n(), percentage = n()/length(which(dsap_data$G3<=5))*100)
Q3A2R4  
Q3A2V4 <-  ggplot(Q3A2R4, aes(x=percentage, y="", fill = as.factor(schoolsup), area = percentage)) + 
           geom_treemap()+ 
           labs(title = "") +  theme(panel.background = element_blank(),
                                     axis.title = element_blank(),
                                     axis.text = element_blank(),
                                     axis.line = element_blank(),
                                     axis.ticks= element_blank(),
           plot.title = element_text(size = 20, face = "bold")) +
           geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                             color = c("white"), place = "centre") +
           ggtitle("Shows the percentage of the Students that scored <= 5 marks in their G3,
                   grouped by Student Additional Educational Support from school status") +
           labs(fill="Student Additional Educational Support\nfrom school status")+ 
           scale_fill_manual(values = c("#7FFF00","#008B8B"), labels = c("1 - No", "2 - Yes"))
Q3A2V4


ggarrange(Q3A2V2, Q3A2V3, Q3A2V4, nrow = 3, ncol = 1)


Q3A2R5<-  dsap_data %>% group_by(schoolsup)%>% select(G1,G2,G3) %>%
          filter(G1<=5, schoolsup == "2")
View(Q3A2R5)  




#Analysis 3-3
#Finding the relationship between students' family educational support and their average marks.
Q3A3R1<- dsap_data %>% group_by(famsup,avgGradeRange) %>% summarise(counts = n())
Q3A3R1
Q3A3V1<- ggplot(Q3A3R1, aes(x=avgGradeRange, y=counts, fill=as.factor(famsup))) + 
         geom_bar(stat="identity",width = 0.5, color="white") +
         ggtitle("The number of Students with their average score grouped 
                 by their Family Educational Support status.")+
         labs(x="Average Student Marks Range", y = "Student Counts", 
              fill="Family Educational Support status")+ 
         theme(plot.title = element_text(size = 15, face = "bold")) +
         scale_fill_manual(values=c("#2F4F4F", "#008080"),
         labels = c("1 - No", "2 - Yes")) + coord_flip() +
         geom_text(aes(label=counts), position = position_stack(vjust = 0.5),
                   color = "white")
Q3A3V1


Q3A3R2 <- dsap_data %>% group_by(famsup, avgGradeRange) %>% 
          filter(avgGrade>avgMeanGrade) %>% summarise(counts = n())
Q3A3R2
Q3A3V2 <- ggplot(Q3A3R2, aes(x=avgGradeRange, y=counts, fill = as.factor(famsup))) +
          geom_bar(stat = "identity", position=position_dodge2(), width = 0.5, color="black") + 
          ggtitle("The number of Students with their Average Student Marks > average mean marks grouped 
                  by their Family Educational Suppport Status") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs(fill = "Family Educational Suppport Status", x="Average Student Marks Range", 
               y = "Student Counts")+
          facet_wrap(~famsup, labeller = as_labeller(c(`2`="Got Support", `1`="No Support"))) +
          geom_text(aes(label=counts), vjust=-0.3) +
          scale_fill_manual(values = c("#00FFFF", "#191970"),labels = c("1 - No", "2 - Yes"))
Q3A3V2

Q3A3R3 <- dsap_data %>% group_by(famsup, avgGrade) %>%
          filter(avgGrade>15) %>% summarise(counts = n())
Q3A3R3
Q3A3V3 <-  ggplot(Q3A3R3, aes(x=avgGrade, y=counts, fill = as.factor(famsup))) +
           geom_bar(stat = "identity", position=position_dodge2(), width = 0.5, color="black") + 
           ggtitle("The number of Students with their Average Student Marks > 15 marks 
                             grouped by their Family Educational Suppport Status") +
           theme(plot.title = element_text(size = 15, face = "bold")) + 
           labs(fill = "Family Educational Suppport Status", x="Average Student Marks Range", 
                 y = "Student Counts")+
           facet_wrap(~famsup, labeller = as_labeller(c(`2`="Got Support", `1`="No Support"))) +
           geom_text(aes(label=counts), vjust=-0.3) +
           scale_fill_manual(values = c("#00FFFF", "#191970"),labels = c("1 - No", "2 - Yes"))

Q3A3V3



#Analysis 3-4
#Finding the relationship between the students' travel time to the class and their average grade.
Q3A4R1 <- dsap_data %>% group_by(traveltime, avgGradeRange) %>% summarise(counts = n())
Q3A4R1
Q3A4V1 <-  ggplot(Q3A4R1, aes(x=avgGradeRange,y=counts, fill=as.factor(traveltime))) + 
           geom_bar(stat="identity",width = 0.5, color="black") +
           ggtitle("The number of Students with their average score grouped by their Travel Time.")+
           labs(x="Average Student Marks Range", y = "Student Counts", fill="Students Travel Time (Hours)")+ 
           theme(plot.title = element_text(size = 15, face = "bold")) +
           scale_fill_manual(values=c("#48D1CC", "#9370DB", "#000080","#800000"),
           labels = c("1", "2", "3", "4"))+
           geom_text(aes(label=counts), position = position_stack(vjust = 0.5), color="#F5F5F5")
Q3A4V1

  
Q3A4R2<- dsap_data %>% group_by(traveltime) %>% filter(avgGrade>avgMeanGrade) %>% 
         summarise(counts = n(),  percentage = n()/length(which(dsap_data$avgGrade>avgMeanGrade))*100)
Q3A4R2   
Q3A4V2 <-  ggplot(Q3A4R2, aes(x=percentage, y="", fill = as.factor(traveltime), area = percentage)) + geom_treemap()+ 
           theme(legend.justification="top",
                 panel.background = element_blank(),
                 axis.title = element_blank(),
                 axis.text = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks= element_blank(),
           plot.title = element_text(size = 20, face = "bold")) +
           ggtitle("Shows the percentage of the Students that scored\n> average mark grouped by their travel time.") +
           labs(fill="Student Travel Time (Hours)")+ scale_fill_manual(values = c("#008B8B", "#191970","#4B0082","#DA70D6"),
                                                                       labels = c("1", "2", "3", "4")) +
           geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                             color = c("white"), place = "left")
Q3A4V2 



Q3A4R3<- dsap_data %>% group_by(studytime)%>% filter(avgGrade>avgMeanGrade & traveltime == 1) %>% 
          summarise(counts = n(),  percentage = n()/length(which(dsap_data$avgGrade>avgMeanGrade & dsap_data$traveltime == 1))*100)
Q3A4R3
Q3A4V3 <- ggplot(Q3A4R3, aes(x=percentage, y="", fill = as.factor(studytime), area = percentage)) + geom_treemap()+ 
          theme(legend.justification="top",
                panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +
          ggtitle("Shows the percentage of the Students that scored\n> average mark and their Travel Time to school were 1 hour,
                  grouped by their study time.") +
          labs(fill="Student Study Time (Hours)")+ scale_fill_manual(values = c("#008B8B", "#191970","#4B0082","#DA70D6"), 
                                                                     labels = c("1", "2", "3", "4")) +
          geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), color = c("white"), place = "left")
Q3A4V3  
  
  
ggarrange(Q3A4V2, Q3A4V3, nrow=2, ncol=1)



  
#Analysis 3-5
#Finding the correlation between students' internet access and their average marks.
Q3A5R1 <- dsap_data %>% group_by(internet, avgGradeRange) %>% summarise(counts = n())
Q3A5R1

Q3A5V1 <-  ggplot(Q3A5R1, aes(x=avgGradeRange, y = counts, fill = as.factor(internet))) +
           geom_bar(stat = "identity", width = 1, color="black", position = position_dodge2()) +
           labs(fill="Student Internet access", x = "Average Student Marks Range", y="Student Counts") + 
           ggtitle("The number of Students with their average score\n grouped by Internet Access status.") +
           theme(plot.title = element_text(size = 15, face = "bold")) + 
           geom_text(aes(label=counts), position = position_dodge2(1), vjust=-0.5) +
           scale_fill_manual(values=c("#48D1CC", "#9370DB"),labels = c("1 - No", "2 - Yes"))
Q3A5V1       
        


Q3A5R2<- dsap_data %>% group_by(internet) %>% filter(avgGrade>15) %>% 
         summarise(counts = n(),  percentage = n()/length(which(dsap_data$avgGrade>15))*100)
Q3A5R2   
Q3A5V2 <- ggplot(Q3A5R2, aes(x=percentage, y="", fill = as.factor(internet), area = percentage)) +
          geom_treemap()+ 
          theme(legend.justification="top",
                panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +
          ggtitle("Shows the percentage of the Students that scored\n> 15 average mark grouped by the Internet Access status.") +
          labs(fill="Internet Access")+ scale_fill_manual(values = c("#008B8B", "#191970","#4B0082","#DA70D6"), 
                                                          labels = c("1 - No", "2 - Yes")) +
          geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                            color = c("white"), place = "left")
Q3A5V2 


Q3A5R3<- dsap_data %>% group_by(internet) %>% filter(avgGrade<=5) %>% 
        summarise(counts = n(),  percentage = n()/length(which(dsap_data$avgGrade<=5))*100)
Q3A5R3   
Q3A5V3 <- ggplot(Q3A5R3, aes(x=percentage, y="", fill = as.factor(internet), area = percentage)) + geom_treemap()+ 
          theme(legend.justification="top",
                panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +
          ggtitle("Shows the percentage of the Students that scored\n<= 5 average mark grouped by the Internet Access status.") +
          labs(fill="Internet Access")+ scale_fill_manual(values = c("#008B8B", "#191970","#4B0082","#DA70D6"), 
                                                          labels = c("1 - No", "2 - Yes")) +
          geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                            color = c("white"), place = "left")
Q3A5V3 


ggarrange(Q3A5V2, Q3A5V3, nrow=2, ncol=1)




#=======Question 4========#
#Question 4: How does students' family influence impact their marks?
#Analysis 4 - 1
#Finding the relationship between family size and students' average grades.
Q4A1R1 <- dsap_data %>% group_by(famsize, avgGradeRange) %>% summarise(counts = n())
Q4A1R1

Q4A1V1 <-  ggplot(Q4A1R1, aes(x=avgGradeRange, y = counts, fill=as.factor(famsize))) +
           geom_bar(stat="identity",width = 0.5, color="white") +
           ggtitle("The number of Students with their average score grouped\n by their Family Size.")+
           labs(x="Average Student Marks Range", y = "Student Counts", fill="Student Family Size")+ 
           theme(plot.title = element_text(size = 15, face = "bold")) +
           scale_fill_manual(values=c("#BC8F8F", "#483D8B"))+ coord_flip() +
           facet_wrap(~famsize, labeller = as_labeller(c(`GT3` = "GT3 - Greater Than 3",
                                                         `LE3`= "LE3 - Less Than 3"))) +
           geom_text(aes(label=counts),  position = position_stack(vjust = 0.5), color = "white")
Q4A1V1       


Q4A1R2<- dsap_data %>% group_by(famsize, avgGrade) %>%
          filter(avgGrade>15) %>% summarise(counts = n())
Q4A1R2  
Q4A1V2 <- ggplot(Q4A1R2, aes(x=avgGrade, y= counts, fill=as.factor(famsize))) + 
          geom_bar(stat="identity",width = 0.5, color="black") +
          ggtitle("The number of Students with their average score > 15 grouped by their Family Size.")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="Students Family Size")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          scale_fill_manual(values=c("#FF6347", "#FFA500"))+
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q4A1V2


Q4A1R3<- dsap_data %>% group_by(famsize, avgGrade) %>%
        filter(avgGrade<=5) %>% summarise(counts = n())
Q4A1R3   
Q4A1V3 <- ggplot(Q4A1R3, aes(x=avgGrade, y= counts, fill=as.factor(famsize))) + 
          geom_bar(stat="identity",width = 0.5, color="black") +
          ggtitle("The number of Students with their average score <= 5 grouped by their Family Size.")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="Students Family Size")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          scale_fill_manual(values=c("#FF6347", "#FFA500"))+
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q4A1V3          
  

ggarrange(Q4A1V2, Q4A1V3, nrow=2, ncol=1)




#Analysis 4 - 2
#Finding the correlation between parents' cohabitation status and students' average grades.
Q4A2R1 <- dsap_data %>% group_by(Pstatus, avgGradeRange) %>% summarise(counts = n())
Q4A2R1
Q4A2R1 <- ggplot(Q4A2R1, aes(x=avgGradeRange, y = counts, fill=as.factor(Pstatus))) + 
          geom_bar(stat="identity",width = 0.5, color="white") +
          ggtitle("The number of Students with their average score grouped by their Parents' Cohabitation Status")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="Student Parents' Cohabitation Status")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          scale_fill_manual(values=c("#4169E1", "#F4A460"))+
          facet_wrap(~Pstatus, labeller = as_labeller(c(`A`= "A - Living Apart",
                                                        `T`= "T - Living Together"))) +
          geom_text(aes(label=counts),  position = position_stack(vjust = 0.5), color = "black")
Q4A2R1




#Analysis 4 - 3
#Finding the relationship between a mother's education and a student's average mark.
Q4A3R1 <- dsap_data %>% group_by(Medu, avgGradeRange) %>% summarise(counts = n())
Q4A3R1
Q4A3V1 <-ggplot(Q4A3R1, aes(x=avgGradeRange, y = counts, fill=as.factor(Medu))) +
         geom_bar(stat="identity",width = 0.5, color="black") +
         ggtitle("The number of Students with their average score grouped by their Mother's Education Level")+
         labs(x="Average Student Marks Range", y = "Student Counts", fill="Mother's Education Level")+ 
         theme(plot.title = element_text(size = 15, face = "bold")) +
         scale_fill_manual(values=c("#483D8B", "#1E90FF","#2F4F4F", "#4B0082","#DA70D6"), 
                           labels=c(c("0 - None",
                                      "1 - Primary Education (4th grade)", 
                                      "2 - 5th to 9th grade", "3 - secondary education", 
                                      "4 - higher education")))+
         facet_wrap(~Medu, labeller = as_labeller(c(`0`= "0 - None", 
                                                    `1`= "1 - Primary Education (4th grade)", 
                                                    `2`= "2 - 5th to 9th grade",
                                                    `3`="3 - secondary education",
                                                    `4`=" 4 - higher education"))) +
         geom_text(aes(label=counts),  position = position_stack(vjust = 0.5), color = "white")

Q4A3V1



Q4A3R2<- dsap_data %>% group_by(Medu) %>% filter(avgGrade>15) %>% 
         summarise(counts = n(),  percentage = n()/length(which(dsap_data$avgGrade>15))*100)
Q4A3R2  
Q4A3V2<- ggplot(Q4A3R2, aes(x="", y =percentage, fill=as.factor(Medu))) + geom_col(color = "white") + 
         coord_polar("y", start = 0) +
         theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
                plot.title = element_text(size = 20, face = "bold")) + 
          geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                    color = c("white"), position = position_stack(vjust=0.5)) + 
          ggtitle("Shows the percentage of the Students that scored\n> 15 average marks and their Mother's Education Level") +
          labs(fill="Mother's Education Level")+ 
          scale_fill_manual(values = c("#483D8B", "#1E90FF","#2F4F4F", "#4B0082","#DA70D6"), 
                    labels = c("1 - Primary Education (4th grade)", 
                               "2 - 5th to 9th grade", 
                               "3 - Secondary education", 
                               "4 - Higher education"))
Q4A3V2





#Analysis 4 - 4
#Finding the relationship between father's education and students' average mark.
Q4A4R1 <- dsap_data %>% group_by(Fedu, avgGradeRange) %>% summarise(counts = n())
Q4A4R1
Q4A4V1 <-ggplot(Q4A4R1, aes(x=avgGradeRange, y = counts, fill=as.factor(Fedu))) + 
        geom_bar(stat="identity",width = 0.5, color="black") +
        ggtitle("The number of Students with their average score grouped by their Father's Education Level")+
        labs(x="Average Student Marks Range", y = "Student Counts", fill="Father's Education Level")+ 
        theme(plot.title = element_text(size = 15, face = "bold")) +
        scale_fill_manual(values=c("#483D8B", "#1E90FF","#2F4F4F", "#4B0082","#DA70D6"), 
                          labels=c(c("0 - None",
                                     "1 - Primary Education (4th grade)",
                                     "2 - 5th to 9th grade", 
                                     "3 - secondary education", 
                                     " 4 - higher education")))+
        facet_wrap(~Fedu, labeller = as_labeller(c(`0`= "0 - None", 
                                                   `1`= "1 - Primary Education (4th grade)", 
                                                   `2`= "2 - 5th to 9th grade", 
                                                   `3`="3 - secondary education",
                                                   `4`=" 4 - higher education"))) +
        geom_text(aes(label=counts),  position = position_stack(vjust = 0.5), color = "white")
Q4A4V1



Q4A4R2<- dsap_data %>% group_by(Fedu) %>% filter(avgGrade>15) %>% 
         summarise(counts = n(),  percentage = n()/length(which(dsap_data$avgGrade>15))*100)
Q4A4R2  
Q4A4V2 <-ggplot(Q4A4R2, aes(x="", y =percentage, fill=as.factor(Fedu))) + geom_col(color = "white") +
         coord_polar("y", start = 0) +
         theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
                plot.title = element_text(size = 20, face = "bold")) + 
         geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                    color = c("white"), position = position_stack(vjust=0.5)) + 
         ggtitle("Shows the percentage of the Students that scored\n> 15 average marks and their Father's Education Level") +
         labs(fill="Father's Education Level")+ 
         scale_fill_manual(values = c("#483D8B", "#1E90FF","#2F4F4F", "#4B0082","#DA70D6"), 
                    labels = c("1 - Primary Education (4th grade)", 
                               "2 - 5th to 9th grade", 
                               "3 - Secondary education",
                               "4 - Higher education"))
Q4A4V2




#Analysis 4 - 5
#Finding the relationship between the quality of the mother's education level, the father's education level, and the student's average marks.
Q4A5R1<- dsap_data %>%  group_by(Medu,Fedu,avgGradeRange) %>% summarise(counts = n())
Q4A4R1
Q4A5V1<- ggplot(Q4A5R1, aes(x=avgGradeRange, y=counts ,fill = as.factor(Medu))) + 
         geom_bar(stat = "identity", position = position_dodge2(),width = 0.5, color="black") + 
         ggtitle("The number of Students with their Average Student Marks grouped by\ntheir Mother's and Father's Education Level") +   
         theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs(fill = "Mothers's Education Level", x="Average Student Marks Range", y = "Student Counts")+
         facet_grid(Medu~Fedu, labeller =labeller(.rows=c(`0`= "Medu: 0 - None", 
                                                          `1`= "Medu: 1 - Primary Education (4th grade)",
                                                          `2`= "Medu: 2 - 5th to 9th grade", 
                                                          `3`= "Medu: 3 - Secondary Education", 
                                                          `4`= "Medu: 4 - Higher Education"), 
                                                  .cols=c(`0`= "Fedu: 0 - None", 
                                                          `1`= "Fedu: 1 - Primary Education (4th grade)",
                                                          `2`= "Fedu: 2 - 5th to 9th grade", 
                                                          `3`= "Fedu: 3 - Secondary Education", 
                                                          `4`= "Fedu: 4 - Higher Education"))) + 
         geom_text(aes(label=counts), vjust=-0.3) + ylim(0, 80) + 
         scale_fill_manual(values=c("#483D8B", "#1E90FF","#2F4F4F", "#4B0082","#DA70D6"),
                          labels = c("0 - None",
                                     "1 - Primary Education (4th grade)", 
                                     "2 - 5th to 9th grade", 
                                     "3 - Secondary education",
                                     "4 - Higher education"))
Q4A5V1        


Q4A5R2<- dsap_data %>% group_by(Fedu, Medu) %>% 
         filter(avgGrade> 15) %>% summarise(counts = n())
View(Q4A5R2)


#Analysis 4 - 6
#Finding the correlation between mother's job, father's job and students' average marks.
Q4A6R1<- dsap_data %>%  group_by(Mjob,Fjob,avgGradeRange) %>% summarise(counts = n())
Q4A6R1
Q4A6V1<- ggplot(Q4A6R1, aes(x=avgGradeRange, y=counts, fill = as.factor(Fjob))) + 
         geom_bar(stat = "identity", position = position_dodge2(),width = 0.5, color="black") + 
         ggtitle("The number of Students with their Average Student Marks grouped by\ntheir Mother's and Father's Job") +   
         theme(plot.title = element_text(size = 15, face = "bold")) + 
         labs(fill = "Father's Job", x="Average Student Marks Range", y = "Student Counts")+
         facet_grid(Fjob~Mjob, labeller = labeller(.rows= c(`teacher`  = "Fjob - Teacher", 
                                                            `health`  = "Fjob - Health - care related",
                                                            `services`= "Fjob - Civil Services (e.g. administrative or police)", 
                                                            `at_home` = "Fjob - At Home",
                                                            `other`   = "Fjob - Other"),
                                                   .cols = c(`teacher` = "Mjob - Teacher", 
                                                             `health`  = "Mjob - Health - care related",
                                                             `services`= "Mjob - Civil Services (e.g. administrative or police)", 
                                                             `at_home` = "Mjob - At Home",
                                                             `other`   = "Mjob - Other"))) +  
         geom_text(aes(label=counts), vjust=-0.3) + ylim(0, 80) + 
         scale_fill_manual(values=c("#483D8B", "#1E90FF","#2F4F4F", "#4B0082","#DA70D6"),
         labels = c("At Home",
                    "Health - care related",
                    "Other",
                    "Civil Services (e.g. administrative or police)", 
                    "Teacher"))
Q4A6V1        




#=======Question 5========#
#Question 5: How do students' personal lives impact their marks?
#Analysis 5 - 1
#Finding the relationship between students' workday alcohol consumption and their average marks.
Q5A1R1<- dsap_data %>% group_by(Dalc,avgGradeRange) %>% summarise(counts = n())
Q5A1R1
Q5A1V1<- ggplot(Q5A1R1, aes(x=avgGradeRange, y=counts, fill=as.factor(Dalc))) + 
         geom_bar(stat="identity",width = 0.5, color="black") +
         ggtitle("The number of Students with their average score grouped by Workday Alcohol Consumption.")+
         labs(x="Average Student Marks Range", y = "Student Counts", fill="Workday Alcohol Consumption")+ 
         theme(plot.title = element_text(size = 15, face = "bold")) + coord_flip()+
         scale_fill_manual(values=c("#8B008B", "#00CED1", "#1E90FF", "#7CFC00", "#CD853F"),
                            labels = c("1 - Very Low",
                                       "2 - Low",
                                       "3 - Medium", 
                                       "4 - High", 
                                       "5 - Very High"))+
         geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A1V1

Q5A1R2<- dsap_data %>% group_by(Dalc, avgGrade) %>% filter(avgGrade>15) %>% summarise(counts = n())
Q5A1R2
Q5A1V2<- ggplot(Q5A1R2, aes(x=avgGrade, y=counts, fill=as.factor(Dalc))) + 
         geom_bar(stat="identity",width = 0.5, color="black") +
         ggtitle("The number of Students with their average score > 15 grouped by Workday Alcohol Consumption.")+
         labs(x="Average Student Marks Range", y = "Student Counts", fill="Workday Alcohol Consumption")+ 
         theme(plot.title = element_text(size = 15, face = "bold")) +
         scale_fill_manual(values=c("#8B008B", "#00CED1", "#1E90FF", "#7CFC00", "#CD853F"),
                            labels = c("1 - Very Low", 
                                       "2 - Low", 
                                       "3 - Medium", 
                                       "4 - High",
                                       "5 - Very High"))+
         geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A1V2



#Analysis 5 - 2
#Finding the relationship between students' weekend alcohol consumption and their average marks.
Q5A2R1<- dsap_data %>%  group_by(Walc,avgGradeRange) %>% summarise(counts = n())
Q5A2R1
Q5A2V1<-  ggplot(Q5A2R1, aes(x=avgGradeRange, y=counts, fill=as.factor(Walc))) +
          geom_bar(stat="identity",width = 0.5, color="black") +
          ggtitle("The number of Students with their average score grouped by Weekend Alcohol Consumption.")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="Weekend Alcohol Consumption")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          scale_fill_manual(values=c("#8B008B", "#00CED1", "#1E90FF", "#7CFC00", "#CD853F"),
                            labels = c("1 - Very Low",
                                       "2 - Low",
                                       "3 - Medium",
                                       "4 - High",
                                       "5 - Very High"))+
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A2V1

Q5A2R2<- dsap_data %>% group_by(Walc, avgGrade) %>% filter(avgGrade>15) %>% summarise(counts = n())
Q5A2R2
Q5A2V2<- ggplot(Q5A2R2, aes(x=avgGrade, y=counts, fill=as.factor(Walc))) +
          geom_bar(stat="identity",width = 0.5, color="black") +
          ggtitle("The number of Students with their average score > 15 grouped by Weekend Alcohol Consumption.")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="Weekend Alcohol Consumption")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          scale_fill_manual(values=c("#8B008B", "#00CED1", "#1E90FF", "#7CFC00", "#CD853F"),
                            labels = c("1 - Very Low", 
                                       "2 - Low", 
                                       "3 - Medium", 
                                       "4 - High",
                                       "5 - Very High"))+
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A2V2



#Analysis 5 - 3
#Finding the correlation between students' decision to pursue higher studies and their average marks.
Q5A3R1<- dsap_data %>% group_by(higher, avgGradeRange) %>% summarise(counts = n())
Q5A3R1

Q5A3V1<-  ggplot(Q5A3R1, aes(avgGradeRange, y=counts, fill = as.factor(higher))) + 
          geom_bar(stat = "identity", position = position_dodge2(preserve = 'single'), width=0.9) + 
          ggtitle("The number of Students with their average score grouped by their decision of pursuing for Higher Studies.") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs(fill = "Students Higher Studies Decided Status", x="Average Students Marks Range", y = "Student Counts")+
          geom_text(aes(label=counts), position = position_dodge2(1), vjust=-0.5) +
          scale_fill_manual(values = c("#191970","#FF1493"),labels = c("1 - No", "2 - Yes"))
Q5A3V1



Q5A3R2<- dsap_data %>% group_by(higher) %>% filter(avgGrade>15) %>% 
         summarise(counts = n(),  percentage = n()/length(which(dsap_data$avgGrade>15))*100)
Q5A3R2   

Q5A3V2 <- ggplot(Q5A3R2, aes(x=percentage, y="", fill = as.factor(higher), area = percentage)) + geom_treemap()+ 
          theme(legend.justification="top",
                panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
          plot.title = element_text(size = 20, face = "bold")) +
          ggtitle("Shows the percentage of the Students that scored\n> 15 average mark grouped by their decision of pursuing for Higher Studies.") +
          labs(fill="Students Higher Studies Decided Status")+
          scale_fill_manual(values = c("#4B0082","#DA70D6"), labels = c("2 - Yes")) +
          geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                            color = c("white"), place = "left")
Q5A3V2 




#Analysis 5 - 4
#Finding the relationship between students' health status and their average marks
Q5A4R1<- dsap_data %>% group_by(health,avgGradeRange) %>% summarise(counts = n())
Q5A4R1
Q5A4V1<-  ggplot(Q5A4R1, aes(x=avgGradeRange, y=counts, fill=as.factor(health))) + 
          geom_bar(stat="identity",width = 0.5, color="black") +
          ggtitle("The number of Students with their average score grouped by Health Status.")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="Student Health Status")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          scale_fill_manual(values=c("#FFC0CB", "#FFE4C4","#87CEFA","#F0E68C","#00FF7F"),
                            labels = c("1 - Very Low",
                                       "2 - Low",
                                       "3 - Okay",
                                       "4 - Good",
                                       "5 - Very Good"))+
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A4V1


Q5A4R2<- dsap_data %>% group_by(health) %>% filter(avgGrade>avgMeanGrade) %>% 
          summarise(counts = n(),  percentage = n()/length(which(dsap_data$avgGrade>avgMeanGrade))*100)
Q5A4R2   
Q5A4V2 <- ggplot(Q5A4R2, aes(x=percentage, y="", fill = as.factor(health), area = percentage)) + geom_treemap()+ 
          theme(legend.justification="top",
                panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank()
                ,axis.line = element_blank(),
                axis.ticks= element_blank(),
                plot.title = element_text(size = 20, face = "bold")) +
          ggtitle("Shows the percentage of the Students that scored\n> average mean mark grouped by their Health status.") +
          labs(fill="Health Status")+ 
          scale_fill_manual(values = c("#FFC0CB", "#FFE4C4", "#87CEFA","#F0E68C","#00FF7F"),
                            labels = c("1 - Very Low",
                                       "2 - Low",
                                       "3 - Okay", 
                                       "4 - Good", 
                                       "5 - Very Good")) +
          geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")),
                            color = c("black"), place = "left")
Q5A4V2 


#Analysis 5 - 5
#Finding the relationship between students' nursery school attendance and their average grade.
Q5A5R1 <- dsap_data %>% group_by(nursery, avgGradeRange) %>% summarise(counts = n())
Q5A5R1

Q5A5V1 <-   ggplot(Q5A5R1, aes(x=avgGradeRange, y = counts, fill=as.factor(nursery))) + 
            geom_bar(stat="identity",width = 0.5, color="black") +
            ggtitle("The number of Students with their average score grouped by their status of attending Nursery School.")+
            labs(x="Average Student Marks Range", y = "Student Counts", fill="Nursery School")+ 
            theme(plot.title = element_text(size = 15, face = "bold")) +
            scale_fill_manual(values=c("#6495ED", "#D8BFD8"), labels = c("1 - No", "2 - Yes"))+ coord_flip() +
            facet_wrap(~nursery, labeller = as_labeller(c(`1` = "Not Attended", 
                                                          `2`= "2 - Attended"))) +
            geom_text(aes(label=counts),  position = position_stack(vjust = 0.5), color = "black")
Q5A5V1    



Q5A5R2<- dsap_data %>% group_by(nursery) %>% filter(avgGrade>avgMeanGrade) %>% 
        summarise(counts = n(),  percentage = n()/length(which(dsap_data$avgGrade>avgMeanGrade))*100)
Q5A5R2   
Q5A5V2 <-ggplot(Q5A5R2, aes(x="", y =percentage, fill=as.factor(nursery))) + geom_col(color = "black") + 
         coord_polar("y", start = 0) +
         theme(panel.background = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.line = element_blank(),
                axis.ticks= element_blank(),
                plot.title = element_text(size = 20, face = "bold")) + 
          geom_text(aes(x=1.2, label = paste0(round(percentage),"%",sep=" ","(",counts,")")), 
                    color = c("black"), position = position_stack(vjust=0.5)) + 
          ggtitle("Shows the percentage of the Students that scored\n> average mean mark grouped by their status of atteding Nursery School.") +
          labs(fill="Nursery School Status")+ 
          scale_fill_manual(values = c("#F0E68C","#00FF7F"), 
                           labels =  c("1 - No", "2 - Yes"))
          
Q5A5V2 



#Analysis 6 -Finding the relationship between students' number of school absences and their average grade.
Q5A6R1 <- dsap_data %>% group_by(absences, avgGradeRange) %>% summarise(counts = n())
Q5A6R1

Q5A6V1 <- ggplot(Q5A6R1, aes(absences, counts)) + geom_point(aes(color=as.factor(avgGradeRange))) +
          ggtitle("The number of Students absences grouped by their average grade range.") +
          theme(plot.title = element_text(size = 15, face = "bold")) + 
          labs( x="Number of Absences", y = "Student Counts") +
          xlim(0,80) +
          scale_color_manual(name = "Student Average Mark Range",  
                             values = c(`00-05` = "darkblue",
                                        `06-10` = "red", 
                                        `11-15` = "yellow", 
                                        `16-20`= "green"))
Q5A6V1


Q5A6R2<- dsap_data %>% group_by(absences) %>% filter(avgGrade>15) %>% 
          summarise(counts = n(),  percentage = n()/length(which(dsap_data$avgGrade>15))*100)
Q5A6R2

Q5A6V2 <-  ggplot(Q5A6R2, aes(x=percentage, y="", fill = as.factor(absences), area = percentage)) + geom_treemap()+ 
           theme(legend.justification="top",
                 panel.background = element_blank(),
                 axis.title = element_blank(),
                 axis.text = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks= element_blank(),
                  plot.title = element_text(size = 20, face = "bold")) +
           ggtitle("Shows the percentage of the Students that scored\n> 15 average marks grouped by the number of absences.") +
           labs(fill="Total Absences")+
           geom_treemap_text(aes(label = paste0(round(percentage),"%",sep=" ","(",counts,")")),
                             color = c("black"), place = "left")
Q5A6V2




#Analysis 7 - Finding the relationship between students' past class failures and their average mark.
Q5A7R1 <- dsap_data %>% group_by(failures, avgGradeRange) %>% summarise(counts = n())
Q5A7R1
Q5A7V1 <- ggplot(Q5A7R1, aes(x=avgGradeRange, y=counts, fill=as.factor(failures))) + 
          geom_bar(stat="identity",width = 0.5, color="black") +
          ggtitle("The number of Students with their average score grouped by Past Failures.")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="Student Total Past Failures")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          scale_fill_manual(values=c("#FFC0CB", "#FFE4C4", "#87CEFA", "#F0E68C"),
                            labels = c("1", "2", "3", "4"))+
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A7V1        


Q5A7R2<- dsap_data %>% group_by(failures, avgGrade) %>% filter(avgGrade>15) %>% summarise(counts = n())
Q5A7R2
Q5A7V2<- ggplot(Q5A7R2, aes(x=avgGrade, y=counts, fill=as.factor(failures))) + 
         geom_bar(stat="identity",width = 0.5, color="black") +
         ggtitle("The number of Students with their average score > 15 grouped by Past Failures.")+
         labs(x="Average Student Marks Range", y = "Student Counts", fill="Student Total Past Failures")+ 
         theme(plot.title = element_text(size = 15, face = "bold")) +
         scale_fill_manual(values=c("#FFC0CB", "#FFE4C4", "#87CEFA", "#F0E68C"),
                          labels = c("1", "2", "3", "4"))+
        geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A7V2





#Analysis 8 - Finding the relationship between students' school and their average mark.
Q5A8R1 <- dsap_data %>% group_by(school, avgGradeRange) %>% summarise(counts = n())
Q5A8R1
Q5A8V1 <- ggplot(Q5A8R1, aes(x=avgGradeRange, y=counts, fill=as.factor(school))) + 
          geom_bar(stat="identity",width = 0.5, color="black") +
          ggtitle("The number of Students with their average score grouped by their School.")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="School")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          facet_wrap(~school, labeller = as_labeller(c(`GP`="Gabriel Pereira", 
                                                       `MS`="Mousinho da Silveira"))) +
          scale_fill_manual(values=c("#87CEFA", "#F0E68C"),
                            labels = c("Gabriel Pereira", "Mousinho da Silveira"))+
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A8V1        




Q5A8R2<- dsap_data %>% group_by(school, avgGrade) %>% filter(avgGrade>15) %>% summarise(counts = n())
Q5A8R2
Q5A8V2<-ggplot(Q5A8R2, aes(x=avgGrade, y=counts, fill=as.factor(school))) + 
        geom_bar(stat="identity",width = 0.5, color="black") +
        ggtitle("The number of Students with their average score > 15 average mark grouped by Past Failures.")+
        labs(x="Average Student Marks Range", y = "Student Counts", fill="School")+ 
        theme(plot.title = element_text(size = 15, face = "bold")) +
        scale_fill_manual(values=c("#87CEFA", "#F0E68C"),
                          labels = c("Gabriel Pereira", "Mousinho da Silveira"))+
        geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A8V2





#Analysis 9 - Finding the relationship between students' sex and their average mark.
Q5A9R1 <- dsap_data %>% group_by(sex, avgGradeRange) %>% summarise(counts = n())
Q5A9R1
Q5A9V1 <- ggplot(Q5A9R1, aes(x=avgGradeRange, y=counts, fill=as.factor(sex))) + 
          geom_bar(stat="identity",width = 0.5, color="black") +
          ggtitle("The number of Students with their average score grouped by their Sex.")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="Sex")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          facet_wrap(~sex, labeller = as_labeller(c(`F`="Female", 
                                                    `M`="Male"))) +
          scale_fill_manual(values=c("#F0E68C","#00FF7F"),
                            labels = c("Female", "Male"))+
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A9V1        



Q5A9R2<- dsap_data %>% group_by(sex, avgGrade) %>% filter(avgGrade>15) %>% summarise(counts = n())
Q5A9R2
Q5A9V2<-ggplot(Q5A9R2, aes(x=avgGrade, y=counts, fill=as.factor(sex))) + 
        geom_bar(stat="identity",width = 0.5, color="black") +
        ggtitle("The number of Students with their average score > 15 average mark grouped by their Sex.")+
        labs(x="Average Student Marks Range", y = "Student Counts", fill="Sex")+ 
        theme(plot.title = element_text(size = 15, face = "bold")) +
        scale_fill_manual(values=c("#F0E68C","#00FF7F"),
                          labels = c("Female", "Male"))+
        geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A9V2




Q5A9R3<- dsap_data %>% group_by(sex, avgGrade) %>% filter(avgGrade<=5) %>% summarise(counts = n())
Q5A9R3
Q5A9V3<-ggplot(Q5A9R3, aes(x=avgGrade, y=counts, fill=as.factor(sex))) + 
        geom_bar(stat="identity",width = 0.5, color="black") +
        ggtitle("The number of Students with their average score <= 5 average mark grouped by their Sex.")+
        labs(x="Average Student Marks Range", y = "Student Counts", fill="Sex")+ 
        theme(plot.title = element_text(size = 15, face = "bold")) +
        scale_fill_manual(values=c("#F0E68C","#00FF7F"),
                          labels = c("Female", "Male"))+
        geom_text(aes(label=counts), position = position_stack(vjust = 0.5))
Q5A9V3



ggarrange(Q5A9V2, Q5A9V3, nrow=2, ncol=1)




#Analysis 10 - Finding the relationship between students' address type and their average mark.
Q5A10R1 <- dsap_data %>% group_by(address, avgGradeRange) %>% summarise(counts = n())
Q5A10R1
Q5A10V1 <-  ggplot(Q5A10R1, aes(x=avgGradeRange, y=counts, fill=as.factor(address))) + 
            geom_bar(stat="identity",width = 0.5, color="black") +
            ggtitle("The number of Students with their average score grouped by their Address.")+
            labs(x="Average Student Marks Range", y = "Student Counts", fill="Address")+ 
            theme(plot.title = element_text(size = 15, face = "bold")) +
            facet_wrap(~address, labeller = as_labeller(c(`U`="Urban", 
                                                          `R`="Rural"))) +
            scale_fill_manual(values=c("#191970","#FF1493"),
                              labels = c("Rural", "Urban"))+
            geom_text(aes(label=counts), position = position_stack(vjust = 0.5), color="white")
Q5A10V1        


Q5A10R2<- dsap_data %>% group_by(address, avgGrade) %>% filter(avgGrade>15) %>% summarise(counts = n())
Q5A10R2
Q5A10V2<- ggplot(Q5A10R2, aes(x=avgGrade, y=counts, fill=as.factor(address))) + 
          geom_bar(stat="identity",width = 0.5, color="white") +
          ggtitle("The number of Students with their average score > 15 average mark grouped by their Address.")+
          labs(x="Average Student Marks Range", y = "Student Counts", fill="Address")+ 
          theme(plot.title = element_text(size = 15, face = "bold")) +
          scale_fill_manual(values=c("#191970","#FF1493"),
                            labels = c("Rural", "Urban"))+
          geom_text(aes(label=counts), position = position_stack(vjust = 0.5), color="white")
Q5A10V2




