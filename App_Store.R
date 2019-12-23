Data = read.csv("C:/Users/rajendra/Desktop/AppStore.csv",sep = ",", na.strings=c("","NA"))
onehot.encoded.Data = read.csv("C:/Users/rajendra/Desktop/AppStore1.csv",sep = ",")
D = as.Date(as.character(Data$Current.Version.Release.Date), format = "%d/%m/%Y")-
  as.Date(as.character(Data$Original.Release.Date), format = "%d/%m/%Y")
Years = D/365
Data$Years = Years
Data$Years = as.numeric(gsub("\\days", "", Data$Years))

colnames(Data)
Data
Summary(Data)
#######    Missin Value imputation  ########################
Missing.Data = Data[,-c(1,2,6,8,9,10,11,12,13)]
library(mice)
model <- mice(Missing.Data, m=3, seed=500)
FData <-complete(model, 1)
OData = cbind(Data[,c(1,2,6,8,9,10,11,12,13)], FData)
OData = na.omit(OData)
summary(OData)
dim(OData)

############################################################

# Q.1 Popularity Index
max_rated  = max(OData$User.Rating.Count)

Popularity = OData$Average.User.Rating + (OData$User.Rating.Count/max_rated)*5

OData$Popularity = Popularity

############################################################
##   Q.2 Replacement 


# finding the application which is unpaid

colnames(OData)
data = which(OData$Price==0)
CData = OData[data,]
Genere = as.factor(CData$Genres)
Paid_app = OData[data,]
Unpaid_app = OData[-data,c(1,6,13,10)]
colnames(Unpaid_app)
summary(Unpaid_app)
colnames(Unpaid_app)

######################## Function for Finding best match ############################

Dist= c()
fun<- function(Sdata, MData){
  for(i in 1:nrow(Unpaid_app)){
    dist = 0;
    for(j in 1:ncol(Sdata)){
      #pr
      if(as.character(MData[i,j]) == as.character(Sdata[1,j])){
        dist = dist + 1;
      }else{
        dist = dist + 0;
      }
    }
    Dist = append(Dist, dist)
  }
  MData$Dist = Dist;
  GD = MData[order(MData$Dist), ]
  if(GD[2766,]$Dist==3){
    print(GD[2766,])
  }else{
      gen = 0;
    for(k in 2764:2766){
        if(as.character(GD[k,1]$Primary.Genre)==as.character(Sdata[k,1]$Primary.Genre)){
          gen = gen + 1;
          t=k;
        }
    }
    if(gen>1){
      print(GD[t,])
    }
  }
}


TD = fun(Unpaid_app[9, ], Unpaid_app)
Unpaid_app[567, ]

##########################################################################################


# Q.3  App popularity for 18+ members

index = which(as.character(OData$Age.Rating)=='17+')
LData = OData[index,]
dim(LData)
M = LData[order(-LData$Popularity),]

#########################################################################################

# important Finding

plot(OData$Popularity, OData$Price)

# There are some applications which are veri Popular though there price is high.
colnames(OData)
PData = OData[,c(5,10,11,12,13,14,15)]
PData$Age.Rating = as.numeric(gsub("\\+", "", PData$Age.Rating))

library("PerformanceAnalytics")
chart.Correlation(PData, histogram=TRUE, pch=19)
