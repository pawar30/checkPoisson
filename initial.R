library(tidyverse)
library(yaml)
library(lubridate)


filemetadata <- read.csv("~/R_Scripts/ipl/filemetadata.csv")

csk.files <- filemetadata[which(grepl('*Chennai Super Kings*',filemetadata$Match)),]
options(stringsAsFactors=FALSE)

dhoni.df<- data.frame(matchDate=character(),City=character(),Bowler=character(),Player=character(),Delivery=character(),runsMade=character())

for(file in csk.files$Filename){
  df <- data.frame()
  df <- yaml.load_file(paste0("~/R_Scripts/ipl/",file,".yaml"))
  #parse YAML for match meta data
  city <- df$info$city
  matchDate <- df$info$dates
  #parse the yaml string and check if csk played in the first or second innings
  innings1.team <- df$innings[[1]][1]$`1st innings`$team
  innings2.team <- df$innings[[2]][1]$`2nd innings`$team
  
  #check which innings CSK played and assign the list to team.csk accordingly
  if(innings1.team=="Chennai Super Kings"){
    team.csk <- df$innings[[1]][1]$`1st innings`$deliveries
  }else{
    team.csk <- df$innings[[2]][1]$`2nd innings`$deliveries
  }
  
  deliveries <- length(team.csk)
  for(d in 1:deliveries){
    
    ball<-names(team.csk[d][[1]])
    batsman <- team.csk[d][[1]][[ball]]$batsman
    
    if(batsman=="MS Dhoni"){
      batsman.runs <- team.csk[d][[1]][[ball]]$runs$batsman
      bowler <- team.csk[d][[1]][[ball]]$bowler
      row <- c(matchDate,city,bowler,batsman,ball,batsman.runs)
      dhoni.df <- rbind(dhoni.df,row)
    }
  }
  names(dhoni.df) <- c("matchDate","City","Bowler","Player","Delivery","runsMade")
}

str(dhoni.df)
dhoni.df$runsMade <- as.numeric(dhoni.df$runsMade)
dhoni.df <- dhoni.df[!is.na(dhoni.df$runsMade),]
dhoni.df$matchDate <-as.Date(dhoni.df$matchDate)
dhoni.df[dhoni.df$City=="Bengaluru",]$City <- "Bangalore"
dhoni.df$monthYear <- paste0(month(dhoni.df$matchDate),"/",year(dhoni.df$matchDate))
#EDA


#runs made in each city
dhoni.df %>% group_by(City) %>%
  mutate(tot.run=sum(runsMade))%>%
  select(c("City","tot.run")) %>%
  group_by(City) %>%
  filter(row_number()==1)%>%
  ggplot(aes(x=City,y=tot.run))+
  geom_col(show.legend = FALSE)+
  ylab("Total Runs Made")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#runs made per month
dhoni.df %>% mutate(monthOfMatch = month(matchDate)) %>%
group_by(monthOfMatch) %>%
  mutate(tot.run=sum(runsMade))%>%
  select(c("monthOfMatch","tot.run")) %>%
  group_by(monthOfMatch) %>%
  filter(row_number()==1)%>%
  ggplot(aes(x=monthOfMatch,y=tot.run))+
  geom_col(show.legend = FALSE)+
  ylab("Total Runs Made")+
  xlab("Month")

#Total Runs made in each year
dhoni.df %>% mutate(yearOfMatch = year(matchDate)) %>%
  group_by(yearOfMatch) %>%
  mutate(tot.run=sum(runsMade))%>%
  select(c("yearOfMatch","tot.run")) %>%
  group_by(yearOfMatch) %>%
  filter(row_number()==1)%>%
  ggplot(aes(x=yearOfMatch,y=tot.run))+
  geom_col(show.legend = FALSE)+
  ylab("Total Runs Made")+
  xlab("Year")

#Top10 bowlers he performed best against. Overall.
dhoni.df %>%
  group_by(Bowler) %>%
  mutate(tot.run=sum(runsMade))%>%
  select(c("Bowler","tot.run")) %>%
  group_by(Bowler) %>%
  filter(row_number()==1)%>%
  arrange(desc(tot.run)) %>%
  head(10) %>%
  ggplot(aes(x=Bowler,y=tot.run))+
  geom_col(show.legend = FALSE)+
  ylab("Total Runs Made")+
  xlab("Bowler")

#city where he scored most number of sixes
dhoni.df %>%
  filter(runsMade==6) %>%
  group_by(City) %>%
  summarise(numberOfSixes=n())%>%
  select(c("City","numberOfSixes")) %>%
  group_by(City) %>%
  filter(row_number()==1)%>%
  ggplot(aes(x=City,y=numberOfSixes))+
  geom_col(show.legend = FALSE)+
  ylab("Number of Sixes")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#year wise break-down of sixes

dhoni.df %>%
  filter(runsMade==6) %>%
  mutate(yearOfMatch = year(matchDate))%>%
  group_by(yearOfMatch) %>%
  summarise(numberOfSixes=n())%>%
  select(c("yearOfMatch","numberOfSixes")) %>%
  group_by(yearOfMatch) %>%
  filter(row_number()==1)%>%
  ggplot(aes(x=yearOfMatch,y=numberOfSixes))+
  geom_col(show.legend = FALSE)+
  ylab("Number of Sixes")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#create the frequecy table
sixes.df <- dhoni.df %>% filter(runsMade==6) %>%
            group_by(matchDate) %>%
            summarise(Freq=n())
#check for matches where he hit no sixers
zero.sixes <- dhoni.df %>% filter(!matchDate %in% sixes.df$matchDate) 
              
                           
  

Sixes.Freq.df <- table(sixes.df$Freq)
Sixes.Freq.df <- data.frame(Sixes.Freq.df)
names(Sixes.Freq.df) <- c("Observed Counts","Freq")
Sixes.Freq.df$`Observed Counts` <- as.numeric(Sixes.Freq.df$`Observed Counts`)

Sixes.Freq.df <- rbind(Sixes.Freq.df, c(0,length(unique(zero.sixes$matchDate))))
Sixes.Freq.df <- Sixes.Freq.df[order(Sixes.Freq.df$`Observed Counts`),]

ggplot(data = Sixes.Freq.df,
       mapping = aes(sample = Freq)) + 
  stat_qq(distribution = stats::qpois,
          dparams = list(lambda = mean(Sixes.Freq.df$Freq))) +
  stat_qq_line(distribution = stats::qpois, dparams = list(lambda = mean(Sixes.Freq.df$Freq)))




#Chi-Squared Goodness of Fit
tot.freq <- sum(Sixes.Freq.df$Freq)
expected.sixes <- sum(Sixes.Freq.df$`Observed Counts`*Sixes.Freq.df$Freq)/tot.freq

Sixes.Freq.df$ActProb <- Sixes.Freq.df$Freq/tot.freq
Sixes.Freq.df$PoissonProb <- dpois(Sixes.Freq.df$`Observed Counts`,expected.sixes,FALSE)

Sixes.Freq.df$expectedFreq <- Sixes.Freq.df$PoissonProb*tot.freq

#check for rows where expected frequencies is < 5. Add them to the rows above/below
expected.Freq.below.5 <- Sixes.Freq.df[Sixes.Freq.df$expectedFreq<5,]

#Adding this to the row above
max.expected.freq <- max(expected.Freq.below.5$expectedFreq)
freq.to.add <- sum(expected.Freq.below.5$Freq)
expected.freq.to.add <- sum(expected.Freq.below.5$expectedFreq)
row.to.modify<- min(Sixes.Freq.df[Sixes.Freq.df$expectedFreq > max.expected.freq,]$expectedFreq)
Sixes.Freq.df[Sixes.Freq.df$expectedFreq==row.to.modify,]$expectedFreq <- Sixes.Freq.df[Sixes.Freq.df$expectedFreq==row.to.modify,]$expectedFreq+expected.freq.to.add
Sixes.Freq.df[Sixes.Freq.df$expectedFreq==row.to.modify,]$Freq <- Sixes.Freq.df[Sixes.Freq.df$expectedFreq==row.to.modify,]$Freq+freq.to.add

Sixes.Freq.df <- Sixes.Freq.df[Sixes.Freq.df$expectedFreq > max.expected.freq,]
if(sum(Sixes.Freq.df$PoissonProb)!=1){
  Sixes.Freq.df$PoissonProbMod <- Sixes.Freq.df$PoissonProb/sum(Sixes.Freq.df$PoissonProb)
}
Sixes.Freq.df$chisq <- (Sixes.Freq.df$`Observed Counts`- Sixes.Freq.df$expectedFreq)^2/Sixes.Freq.df$expectedFreq


result <- chisq.test(x=Sixes.Freq.df$Freq,p=Sixes.Freq.df$PoissonProbMod,simulate.p.value=TRUE)

