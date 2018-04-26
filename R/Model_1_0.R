#---- NBA Draft Model 1.0 ----
#Last Updated: February 5, 2018
#Author: Alexander Powell
#Project Progress: #2 College Data (0%)
#
#
#----------------------------#

#Packages
library(plyr)
library(dplyr)
library(data.table)
library(randomForest)
library(e1071)

#Global Functions
multmerge <- function(path){
  filenames=list.files(path = path, full.names = TRUE)
  rbindlist(lapply(filenames, fread))
}
#---- NBA Data ----

#Input Data
SalaryCap <- read.csv("Documents/Analytics/NBA Draft Model 1_0/NBA_SalaryCap_85_to_18.csv")
Salaries <- read.csv("Documents/Analytics/NBA Draft Model 1_0/NBA_Salaries_90_to_18.csv")

#Data Cleaning
Per.Cap <- merge(Salaries, SalaryCap, by.y = "Year", by.x = "season_end", all.x = TRUE)
Per.Cap$Per.Cap <- Per.Cap$salary / Per.Cap$Salary.Cap

NBA <- Per.Cap %>%
  group_by(player) %>%
  #filter(Per.Cap == max(Per.Cap)) 
  dplyr::mutate( fifth = nth(season_end, 5, order_by = season_end, default = 0) ) %>%
  filter(season_end == fifth)

NBA <- NBA[!duplicated(NBA[,c(2,9)]),]
NBA <- NBA[,c(1:4, 7:9)]

#---- College Data ----
usage <- multmerge("Documents/Analytics/NBA Draft Model 1_0/usage")
eff <- multmerge("Documents/Analytics/NBA Draft Model 1_0/efficiency")
basic <- multmerge("Documents/Analytics/NBA Draft Model 1_0/basic")

College <- merge(usage, eff, by = c("Player", "Year", "Team"), all = TRUE)
College <- merge(College, basic, by = c("Player", "Year", "Team"), all = TRUE) %>%
  unique()

College$V1.y <- NULL
College$V1.x <- NULL
College$V1 <- NULL

College <- College[!duplicated(College[,1:3]),]
#detach("package:data.table")

College1 <- College %>%
  group_by(Player, Team) %>% 
  dplyr::mutate(n = n()) %>%
  top_n(1, Year) %>%
  unique()

College2 <- College1[!duplicated(College1[,1:3]),]

#Merge with NBA statistics
Past <- merge(NBA, College2, by.x = "player", by.y = "Player") %>%
  dplyr::filter(season_end > Year) #%>%
  #filter(fifth >= (Year+5) & fifth <= (Year+8))
Past <- Past[!duplicated(Past[,1:2]),]

#NBA Draft Data
draft <- multmerge("Documents/Analytics/NBA Draft Model 1_0/draft data")
PastDraft <- merge(Past, draft, by.x = "player", by.y = "Player") %>% filter(Year <= 2013)

nrow(PastDraft)

#tmp <- PastDraft[,c(1,9)]

#Removing errors
PastDraft <- PastDraft[!PastDraft$player == "Brandon Jennings", ]
PastDraft <- PastDraft[!PastDraft$player == "Brandon Knight", ]
PastDraft <- PastDraft[!PastDraft$player == "Marcus Thornton", ]
PastDraft <- PastDraft[!PastDraft$player == "Jrue Holiday", ]
PastDraft <- PastDraft[!PastDraft$player == "Dominic McGuire", ]

PastDraft$Pos2 <- ifelse(PastDraft$Pos == "PG/SG", "PG", ifelse(PastDraft$Pos == "SF/PF", "Wing",
                                                               ifelse(PastDraft$Pos == "SG", "Wing",
                                                                      ifelse(PastDraft$Pos == "SF", "Wing",
                                                                             ifelse(PastDraft$Pos == "SG/SF", "Wing", 
                                                                                    ifelse(PastDraft$Pos == "PG", "PG", "Big"))))))

PastDraft <- ddply(PastDraft, .(Pos), function(x) transform(x, percentile=ecdf(x$Per.Cap)(x$Per.Cap)))

columns <- c("player", "season_end", "Per.Cap", "Year", "Team.x", "GP.x", "Min.x", "Pos.G", "Pts.Pos", "Ast.Pos",
             "TO.Pos", "TS.", "FTA.FGA", "X3PA.FGA", "Ast.FGA", "Stl.x", "Blk.x", "PF.x", "X2FGA", "X2FG.", "X3FGA",
             "X3FG.", "FTA", "FT.", "Reb.Off", "Reb.Def", "n", "Age", "HT", "WT", "WS", "Pos2", "percentile", "Pts.x", "PER.x", "eFG.")

PastDraft2 <- PastDraft[,columns]

height <- function(hT){
  feet <- gsub("\\'.*", "", hT)
  inches <- gsub(".*'", "", hT)
  inches <- substr(inches, start = 1, stop = 2)
  inches <- gsub("\\\".*", "", inches)
  height.in <- as.numeric(feet)*12 + as.numeric(inches)
  return(height.in)
}

PastDraft2$HT <- height(PastDraft2$HT)
PastDraft2$WS <- height(PastDraft2$WS)
PastDraft2$X2FG. <- as.numeric(gsub("\\%.*", "", PastDraft2$X2FG.))
PastDraft2$X3FG. <- as.numeric(gsub("\\%.*", "", PastDraft2$X3FG.))
PastDraft2$FT. <- as.numeric(gsub("\\%.*", "", PastDraft2$FT.))

kenpom <- multmerge("Documents/Analytics/NBA Draft Model 1_0/kenpom")
kenpom <- kenpom[,c("Season", "Team", "AdjTempo", "AdjOE", "AdjDE")]
for(i in 1:nrow(kenpom)){
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "North Carolina", "UNC", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Saint Joseph's", "St Joseph's", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Saint Mary's", "St Mary's", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Oklahoma St.", "Oklahoma St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Arizona St.", "Arizona St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Florida St.", "Florida St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Michigan St.", "Michigan St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Ohio St.", "Ohio St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Colorado St.", "Colorado St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "North Carolina St.", "N.C. State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Eastern Washington", "Eastern Wash.", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Kansas St.", "Kansas St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Fresno St.", "Fresno St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Iowa St.", "Iowa St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Morehead St.", "Morehead St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "San Diego St.", "San Diego St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Washington St.", "Washington St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Murray St.", "Murray St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Norfolk St.", "Norfolk St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Weber St.", "Weber St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Long Beach St.", "LBSU", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Western Carolina", "WCU", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Western Kentucky", "WKU", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Missouri St.", "Missouri St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Oregon St.", "Oregon St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Georgia St.", "Georgia St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Boise St.", "Boise St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Montana St.", "Montana St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Fort Wayne", "IPFW", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Penn St.", "Penn St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Wichita St.", "Wichita State", kenpom$Team[i])
  
  
  
  
}
PastDraft2 <- merge(PastDraft2, kenpom, all.x=TRUE, by.x = c("season_end", "Team.x"), by.y = c("Season", "Team") )

columns2 <- c("Year", "GP.x", "Min.x", "Pos.G", "Pts.Pos", "Ast.Pos",
              "TO.Pos", "TS.", "FTA.FGA", "X3PA.FGA", "Ast.FGA", "Stl.x", "Blk.x", "PF.x", "X2FGA", "X2FG.", "X3FGA",
              "X3FG.", "FTA", "FT.", "Reb.Off", "Reb.Def", "n", "Age", "HT", "WS", "percentile", 
              "Pts.x", "PER.x", "eFG.", "AdjTempo", "AdjOE", "AdjDE")
PD <- PastDraft2[,columns2]

impute <- function(data, ht){
  df <- na.omit(data) %>% filter(HT == ht)
  return(median(df$WS))
}

for(i in 1:nrow(PD)){
  if(is.na(PD$WS[i])){
    PD$WS[i] <- impute(PD, PD$HT[i])
  }
}

#apply(PD, 2, function(x) any(is.na(x) | is.infinite(x)))
PD <- na.omit(PD)
#---- Modeling ----

#Training Model
train <- PD %>%
  filter(Year < 2013)
drops <- c("Year", "Reb.Off")
train <- train[ , !(names(train) %in% drops)] 
#train$Pos2 <- as.factor(train$Pos2)
set.seed(222)

train.rf <- randomForest(percentile ~ ., train, ntree = 500, importance = T)
print(train.rf)

plot(train.rf)

varImpPlot(train.rf,
           sort = T,
           main="Variable Importance",
           n.var=(ncol(train)-1))
test <- PD %>%
  filter(Year == 2013)
test$predict <- predict(train.rf, test)
test <- merge(test, PastDraft, all.x = TRUE, by = c("Year", "percentile", "GP.x"))

test <- test[,c("player", "Year", "percentile", "predict")]


sum((test$predict - test$percentile)^2) / nrow(test)

#Model with all obs.
All <- PD[ , !(names(PD) %in% drops)] 
draft.rf <- randomForest(percentile ~ ., All, ntree = 500, importance = T)
print(draft.rf)

plot(draft.rf)

varImpPlot(draft.rf,
           sort = T,
           main="Variable Importance",
           n.var=25)

All$predict <- predict(draft.rf, All)
sum((All$predict - All$percentile)^2) / nrow(All)
All2 <- merge(All, PastDraft2[,c("player", "Year", "percentile", "Pos2", "GP.x", "Age", "HT", "Reb.Off")], 
             all.x = TRUE, by = c("percentile", "GP.x", "Age", "HT"))
All.summary <- All2[,c("player", "Year", "Pos2", "percentile", "predict")]

#Predicting 2018 Draft----
columns3 <- c("Player","Team", "Year", "GP.x", "Min.x", "Pos/G", "Pts/Pos", "Ast/Pos",
              "TO/Pos", "TS%", "FTA/FGA", "3PA/FGA", "Ast/FGA", "Stl.x", "Blk.x", "PF.x", "2FGA", "2FG%", "3FGA",
              "3FG%", "FTA", "FT%", "Reb.Off", "Reb.Def", "n", "Pts.x", "PER", "eFG%")
Draft.2018 <- College2[,columns3] %>%
  filter(Year == 2018)

espn100 <- read.csv("Documents/Analytics/NBA Draft Model 1_0/espn.draft.csv")
for(i in 1:nrow(espn100)){
  if(is.na(espn100$WS[i])){
    espn100$WS[i] <- impute(PD, espn100$HT[i])
  }
}
Draft.2018 <- merge(Draft.2018, espn100, by = c("Player", "Team"))
colnames(Draft.2018)[27] <- "PER.x"
colnames(Draft.2018)[28] <- "eFG."
colnames(Draft.2018)[6] <- "Pos.G"
colnames(Draft.2018)[7] <- "Pts.Pos"
colnames(Draft.2018)[8] <- "Ast.Pos"
colnames(Draft.2018)[9] <- "TO.Pos"
colnames(Draft.2018)[10] <- "TS."
colnames(Draft.2018)[11] <- "FTA.FGA"
colnames(Draft.2018)[12] <- "X3PA.FGA"
colnames(Draft.2018)[13] <- "Ast.FGA"
colnames(Draft.2018)[17] <- "X2FGA"
colnames(Draft.2018)[18] <- "X2FG."
colnames(Draft.2018)[19] <- "X3FGA"
colnames(Draft.2018)[20] <- "X3FG."
colnames(Draft.2018)[22] <- "FT."
Draft.2018[15,17:24] <- c(11.7, 63.7, 1.1, 32.3, 5.3, 75.3, 3.0, 8.1) #Deandre Ayton
Draft.2018[53,c(4:9,27)] <- c(28, 28, 11.9, 1.22, 0.06, 0.12,25.3) #Moritz Wagner
Draft.2018$X2FG. <- as.numeric(gsub("\\%.*", "", Draft.2018$X2FG.))
Draft.2018$X3FG. <- as.numeric(gsub("\\%.*", "", Draft.2018$X3FG.))
Draft.2018$FT. <- as.numeric(gsub("\\%.*", "", Draft.2018$FT.))

Draft.2018$Pos2 <- ifelse(Draft.2018$Pos == "PG/SG", "PG", ifelse(Draft.2018$Pos == "SF/PF", "Wing",
                                                                ifelse(Draft.2018$Pos == "SG", "Wing",
                                                                       ifelse(Draft.2018$Pos == "SF", "Wing",
                                                                              ifelse(Draft.2018$Pos == "SG/SF", "Wing", 
                                                                                     ifelse(Draft.2018$Pos == "PG", "PG", "Big"))))))


Draft.2018 <- merge(Draft.2018, kenpom, by.x = c("Team", "Year"), by.y = c("Team", "Season"), all.x = TRUE)
drops2 <- c("Player", "Team", "Year", "Pos", "Pos2", "Numb")
Draft.predict <- Draft.2018[ , !(names(Draft.2018) %in% drops2)] 
Draft.predict$PREDICT <- predict(draft.rf, Draft.predict)

BigBoard <- merge(Draft.predict, Draft.2018, by = colnames(Draft.2018[,4:25, 29]))


All.error <- All2 %>%
  group_by(Pos2) %>%
  dplyr::mutate(sd = mean((predict - percentile)^2)) %>%
  dplyr::select(Pos2, sd) %>%
  unique()

BigBoard <- merge(BigBoard, All.error, by = "Pos2", all.x = TRUE)

Sim.BB <- BigBoard[,c("Player", "Team", "PREDICT", "sd", "Pos2", "Numb..y", "GP.x")]
sim <- 10000
for(i in 1:nrow(Sim.BB)) {
  df.est <- data.frame(ID=1:sim)
  for(j in 1:sim){
    df.est$est[j] <- rnorm(1, mean = Sim.BB$PREDICT[i], sd = sqrt(Sim.BB$sd[i]))
    df.est$est[j] <- ifelse(df.est$est[j] > 1, 1, ifelse(df.est$est[j] < 0, 0, df.est$est[j]))
  }
  Sim.BB$Mean[i] <- mean(df.est$est)
  Sim.BB$Max[i] <- quantile(df.est$est, probs = .95, na.rm = TRUE)
  Sim.BB$Floor[i] <- quantile(df.est$est, probs = .05, na.rm = TRUE)
}

write.csv(Sim.BB, "Desktop/Results_320.csv")

##linear regression modeling
library(MASS)

r2ww <- function(x){
  SSe <- sum((x$w*x$resid)^2) #the residual sum of squares is weighted
  observed <- x$resid+x$fitted
  SSt <- sum((x$w*observed-mean(x$w*observed))^2) #the total sum of squares is weighted      
  value <- 1-SSe/SSt
  return(value)
}

draft.rlm <- rlm(percentile ~ Age + Pos2 + Pos2*Ast.Pos + Ast.Pos + TS. + FTA.FGA + X3PA.FGA + Stl.x + Blk.x, data = All2)
surmod <- summary(draft.rlm)
dd <- data.frame(surmod$coefficients)
dd$p.value <- pt(dd$t.value, surmod$df[2])
dd$sign <- ifelse(dd$p.value <= .05, "***", ifelse(dd$p.value <= .1, "*", "-"))
dd
r2ww(draft.rlm)



colnames(All2[,2:31])
draft.lm <- lm(percentile ~ GP.x + Age + HT + Min.x + Pos.G +
                 Ast.Pos + TO.Pos + TS. + FTA.FGA + X3PA.FGA + Ast.FGA + Stl.x + 
                 Blk.x + PF.x + X2FGA + X2FG. + X3FGA + X3FG. + FTA + FT. + Reb.Def + Reb.Off +
                 WS + PER.x + eFG. + AdjOE + AdjDE, data = All2, weights = GP.x)
summary(draft.lm)







