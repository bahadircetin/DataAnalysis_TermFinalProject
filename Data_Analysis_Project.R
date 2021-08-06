#Phase 2 Requirement 1- Clean Data Source without NA rows.



videogame.data <- read.csv("vgsales.csv", header = TRUE)

#Clear the NA values to get clean data. 
clean.videogame.data <- na.omit(videogame.data)
library(dplyr)

#Requirement-2 Descriptive Statistics
df1 <- clean.videogame.data %>% select(Genre, NA_Sales, Platform)
df1 %>% group_by(Genre, Platform)

df_new <- df1 %>% group_by(Genre, Platform) %>% 
  summarise(Sales=sum(NA_Sales))

US_Sales_of_all_genres <- df1 %>% group_by(Genre) %>%
  summarise(Platform = toString(unique(Platform)), Sales =sum(NA_Sales))

# Frequency Distribution Table of Genres 
freq.dist.genres <- df1 %>% count(Genre, sort = FALSE)

#Max Min Avg, Skw, Kur
#Sorting the dataframe by Genres
freq.dist <- freq.dist.genres[order(freq.dist.genres$Genre),]

df1 %>% group_by(Genre) %>% 
  summarise(Max=max(NA_Sales)) 

library(e1071) 
freq.dist.genres$Max <- df1 %>% group_by(Genre) %>% 
  summarise(Max=max(NA_Sales)) 

freq.dist.genres$Min <- df1 %>% group_by(Genre) %>% 
  summarise(Min=min(NA_Sales)) 

freq.dist.genres$Mean <- df1 %>% group_by(Genre) %>% 
  summarise(Mean=mean(NA_Sales)) 
#Getting quartiles of the data.. 
data.quartiles <- df1 %>% group_by(Genre) %>% 
  summarise(Quartiles=quantile(NA_Sales))

freq.dist.genres$SD <- df1 %>% group_by(Genre) %>% 
  summarise(Std=sd(NA_Sales)) 

freq.dist.genres$Skw <- df1 %>% group_by(Genre) %>% 
  summarise(Skw=skewness(NA_Sales))

freq.dist.genres$Kur <- df1 %>% group_by(Genre) %>% 
  summarise(Kur=kurtosis(NA_Sales))


#Create grouped frequency dist table.
range(df1$NA_Sales)
(41.49-0 +1) /5
breaks <- seq(0, 45, by=5)
breaks
freq.cuts <- cut(df1[df1$Genre=="Action",]$NA_Sales, breaks, right=FALSE)
freq.cuts
freq.freq =table(freq.cuts)
Frequency.dist.genres<- cbind(freq.freq)

#Convert table to the dataframe
ff <- as.data.frame.matrix(Frequency.dist.genres)

#Add all genres as columns into grouped frequency distribution dataframe.  
adventure.cuts <- cut(df1[df1$Genre=="Adventure",]$NA_Sales, breaks, right=FALSE)
adventure.table <- table(adventure.cuts)
adventure.table <- cbind(adventure.table)
ff$Adventure <- adventure.table[, 'adventure.table'] 

Fighting.cuts <- cut(df1[df1$Genre=="Fighting",]$NA_Sales, breaks, right=FALSE)
Fighting.table <- table(Fighting.cuts)
Fighting.table <- cbind(Fighting.table)
ff$Fighting <- Fighting.table[, 'Fighting.table'] 

Misc.cuts <- cut(df1[df1$Genre=="Misc",]$NA_Sales, breaks, right=FALSE)
Misc.table <- table(Misc.cuts)
Misc.table <- cbind(Misc.table)
ff$Misc <- Misc.table[, 'Misc.table'] 

Platform.cuts <- cut(df1[df1$Genre=="Platform",]$NA_Sales, breaks, right=FALSE)
Platform.table <- table(Platform.cuts)
Platform.table <- cbind(Platform.table)
ff$Platform <- Platform.table[, 'Platform.table'] 

Puzzle.cuts <- cut(df1[df1$Genre=="Puzzle",]$NA_Sales, breaks, right=FALSE)
Puzzle.table <- table(Puzzle.cuts)
Puzzle.table <- cbind(Puzzle.table)
ff$Puzzle <- Puzzle.table[, 'Puzzle.table'] 

Racing.cuts <- cut(df1[df1$Genre=="Racing",]$NA_Sales, breaks, right=FALSE)
Racing.table <- table(Racing.cuts)
Racing.table <- cbind(Racing.table)
ff$Racing <- Racing.table[, 'Racing.table'] 

Role.Playing.cuts <- cut(df1[df1$Genre=="Role-Playing",]$NA_Sales, breaks, right=FALSE)
Role.Playing.table <- table(Role.Playing.cuts)
Role.Playing.table <- cbind(Role.Playing.table)
ff$"Role-Playing" <- Role.Playing.table[, 'Role.Playing.table'] 

Shooter.cuts <- cut(df1[df1$Genre=="Shooter",]$NA_Sales, breaks, right=FALSE)
Shooter.table <- table(Shooter.cuts)
Shooter.table <- cbind(Shooter.table)
ff$Shooter <- Shooter.table[, 'Shooter.table'] 

Simulation.cuts <- cut(df1[df1$Genre=="Simulation",]$NA_Sales, breaks, right=FALSE)
Simulation.table <- table(Simulation.cuts)
Simulation.table <- cbind(Simulation.table)
ff$Simulation <- Simulation.table[, 'Simulation.table'] 

Sports.cuts <- cut(df1[df1$Genre=="Sports",]$NA_Sales, breaks, right=FALSE)
Sports.table <- table(Sports.cuts)
Sports.table <- cbind(Sports.table)
ff$Sports <- Sports.table[, 'Sports.table']

Strategy.cuts <- cut(df1[df1$Genre=="Strategy",]$NA_Sales, breaks, right=FALSE)
Strategy.table <- table(Strategy.cuts)
Strategy.table <- cbind(Strategy.table)
ff$Strategy <- Strategy.table[, 'Strategy.table']


#Finalizing the dataframe which includes the max, min, avg, etc.
General.DF <- freq.dist.genres %>% select("Genre", "n")
General.DF$Max <- freq.dist.genres$Max %>% pull(Max)
General.DF$Mean <- freq.dist.genres$Mean %>% pull(Mean)
General.DF$Min <- freq.dist.genres$Min %>% pull(Min)
General.DF$Std <- freq.dist.genres$SD %>% pull(Std)
General.DF$Skw <- freq.dist.genres$Skw %>% pull(Skw)
General.DF$Kur <- freq.dist.genres$Kur %>% pull(Kur)

# Getting an Average of columns and adding as a row into DF
newrow <-  list("Average", mean(General.DF$n), mean(General.DF$Max), mean(General.DF$Mean), mean(General.DF$Min), mean(General.DF$Std), mean(General.DF$Skw), mean(General.DF$Kur))
General.DF <-  rbind(General.DF,newrow)
#Formatting the dataframe.
General.DF$n <- format(round(General.DF$n, 2), nsmall = 2)
General.DF$Max <- format(round(General.DF$Max, 2), nsmall = 2)
General.DF$Mean <- format(round(General.DF$Mean, 2), nsmall = 2)
General.DF$Std <- format(round(General.DF$Std, 2), nsmall = 2)
General.DF$Skw <- format(round(General.DF$Skw, 2), nsmall = 2)
General.DF$Kur <- format(round(General.DF$Kur, 2), nsmall = 2)

#Controlling structure of our 2 main Dataframes.
str(General.DF)
str(ff)

#Getting an Average
ff$Average <- rowMeans(ff[, 1:12])
#Decimal Formatting grouped distribution dataframe 
ff$Average <- round(rowMeans(ff[, 1:12]), 2)
#Changing rows and columns 
final_df <- as.data.frame(t(ff))


#Controlling the final status of our 2 main Dataframes.
final_df
General.DF






#Graph 1 Genre number of games (lollipop graph)

General.DF %>% group_by(n) %>% top_n(10)
library(dplyr)
library(ggplot2)
ggplot(General.DF, aes(y = n, x = Genre)) + 
  geom_segment( aes(x = Genre, xend = Genre, y = n, yend = 0), size = 1, color="#fe424d") +
  geom_point(size = 6, color="#022d41") + 
  xlab("Game Genres")+
  ylab("Number of Games")+
  theme(legend.title = element_text(size=18), legend.position = "top", legend.text = element_text(size = 16))+
  theme(axis.text.x = element_text(face="bold", color="#457b9d", 
                                   size=10, angle=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#1aa6b7"))+
  theme(plot.title = element_text(hjust = 0.5))


 #Graph 2 line graph of game production by years


 line_years <- strsplit(videogame.data$Year, split = ", ")
 df_line <- data.frame(type = rep(videogame.data$Year, sapply(line_years, length)), line_list = unlist(line_years))
 df_line$line_list <- as.character(gsub(",","",df_line$line_list))
 
 line_gra <- df_line %>% group_by(line_list) %>% summarise(count = n()) %>%
   arrange(desc(count)) %>% top_n(15)


 line_gra %>% ggplot( aes(x=line_list, y=count, group =1 )) +
   geom_line(color = "orange")  +
   geom_point()+
   ylab("Game Production")+
   xlab("Years")+
   theme(plot.title = element_text(hjust = 0.5))+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
   theme(axis.text.x = element_text(face="bold", color="orange", 
                                    size=10, angle=30))
 
 
 
 
 #Graph 3 bar chart of total number of Video game sales of North America and Europe comparison
 #for every game genre.
 
data_NA <- videogame.data %>% select(Genre, NA_Sales) %>% group_by(Genre)
 
data_EU <- videogame.data %>% select(Genre, EU_Sales) %>% group_by(Genre)

 
 SumOfNA <- tapply(data_NA$NA_Sales, data_NA$Genre, FUN=sum)
typeof(SumOfNA)
 sumOfEU <- tapply(data_EU$EU_Sales, data_EU$Genre, FUN=sum)

names <- c("Action","Adventure","Fighting", "Misc","Platform","Puzzle","Racing","Role-Playing ","Shooter","Simulation",
           "Sports","Strategy")

dataframeTotalSum <- data.frame(names,SumOfNA, sumOfEU)


library(reshape2)
dataFrameTotalSum.long<-melt(dataframeTotalSum)
ggplot(dataFrameTotalSum.long,aes(names,value,fill=variable))+
  geom_bar(stat="identity",position="dodge") +
  xlab("Game Genres")+
  ylab("Video Game Sales")+
  scale_fill_discrete(name = "Areas", labels = c("North America", "Europe")) + 
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=10, angle=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.title = element_text(size=10), legend.position = "top", legend.text = element_text(size = 10))+
  theme(plot.title = element_text(hjust = 0.5))


#Graph-4 Scatter Plot of platfroms of the games and how many games in total launched for them.
platform_gra <- strsplit(videogame.data$Platform, split = ", ")
platformList <- data.frame(type = rep(videogame.data$Platform, sapply(platform_gra, length)), platform = unlist(platform_gra))
platfromTotalList <- platformList %>% group_by(platform) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% top_n(15)


ggplot(platfromTotalList, aes(x=platform, y=count, color = platform)) + 
  geom_point(size = 5)+
  xlab("Gaming Platform")+
  ylab("Number of Games")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.title = element_text(size=10), legend.position = "top", legend.text = element_text(size = 10))+
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=12, angle=30))

