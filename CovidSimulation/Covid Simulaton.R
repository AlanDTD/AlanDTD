#author: Dung (Alan) Nguyen

#Clean list
rm(list = ls())

#packages
library("tidyverse")
library("ggplot2")
library("ggrepel")

#Adjustable Parameters
population.N <- 10000
initial.sick <- 2
meet <- 20
infection.rate <- .3
period.day <- 300
mortality.rate <- 0.002
Immunity.co <- .1

#Counter
day <- 1

#Build Data Frame to capture data, Status types = "0 - Non Immune", "1 - Sick:, "2 - Dead", "3 - Immune"
df <- data.frame("Person No"= seq(population.N),"Status" = rep(0,population.N),"Days" = rep(0,population.N))


#Select random people to be the starting sick people
day1.sick <- sample(population.N, initial.sick)

#Updating sick status
df$Days[day1.sick] <- 1
df$Status[day1.sick] <- 1
sick.N <- sum(df$Status == 1)


#Core loop
while (day < period.day) {


#Checking how many people are sick and how many people they meet
if (sick.N != 0 ){
  meet.N <- replicate(sick.N,sample(0:meet,1))
} else {
  print(c("No more sick people"))
  break
  }




#Array of people that sick people met
live.population <- df$Person.No[df$Status == 0 | df$Status == 1 | df$Status == 3]
meet.a <- sample((live.population),sum(meet.N),replace = T)

#Splitting into people met into immune and normal

#Immune group
meet.immune <- meet.a * as.numeric(df$Status[meet.a] == 3)
meet.immune <- meet.immune * (as.numeric(runif(meet.immune) < (infection.rate * Immunity.co)))
df$Status[meet.immune] <- 1


#Normal group
meet.normal <- meet.a * as.numeric(df$Status[meet.a] == 0)
meet.normal <- meet.normal * as.numeric(runif(meet.normal) < (infection.rate))
df$Status[meet.normal] <- 1

#Identifying newly infected
new.infected <- c(meet.immune,meet.normal)
new.infected <- new.infected[new.infected != 0]
new.infected <- new.infected[!duplicated(new.infected)]


#Death Event, checks for sick, then runs probability event
sick.test <- df$Person.No[df$Status == 1]
sick.test <- as.numeric(runif(length(sick.test)) < mortality.rate) * sick.test
sick.test <- sick.test[sick.test != 0 ]
df$Status[sick.test] <- 2
df$Days[sick.test] <- 0


#Count for the day
Healthy.N <- sum(df$Status == 0 | (df$Status == 3))
Non.Immune <- sum(df$Status == 0)
sick.N <- sum(df$Status == 1)
Death.N <- sum(df$Status == 2)
Immune.N <- sum(df$Status == 3)
New.infected.N <- length(new.infected)


#Creating Dataframe for output
day.summary <- c(day, Healthy.N, Non.Immune, Immune.N, New.infected.N, sick.N, Death.N)

if (exists("df.summary")&&is.data.frame(get("df.summary"))) {
  df.summary <- rbind(df.summary, day.summary)
} else {
  df.summary <- data.frame("Days" = day,"Healthy" = Healthy.N, "Non-Immune" = Non.Immune,"Immune" = Immune.N,"New Infected" = New.infected.N, "Sick" = sick.N, "Death" = Death.N)
}


#New Day Calculation 
df$Days[df$Status == 1] <- df$Days[df$Status == 1] + 1


#Making People Immune after 10 days
df$Status[df$Days == 10] <- 3
df$Days[df$Days == 10] <- 0

# Increase day counter
day <- day + 1
}



#Adjust coming status into a column for easy graphing
df.graph <- df.summary %>%
  select(Days, Healthy, Sick, Death) %>%
  gather(key = "Status", value = "Population", - Days)


#Adding label notes for the graph
infection.perc <- paste0(infection.rate * 100, "%")
mortality.perc <- paste0(mortality.rate * 100, "%")

if (Immunity.co == 1){
  immunity.perc <- "N/A"
} else {
immunity.perc <- paste0(Immunity.co * 100, "%")}

#Adding values to the last point
data_end <- df.graph %>% filter(Days == nrow(df.summary))

#Creating the graph
graph <-ggplot(df.graph, aes(x = Days, y = Population)) + 
  geom_line(aes(color = Status)) +
  labs(title = "Covid Simulation", 
       x = "Day",
       caption = "Healthy population includes non-immuine and immune",
       subtitle = paste("Population =",population.N, "\t","\t",
               "Days observed =",period.day, "\t","\t",
               "Allowed meetings =",meet, "\t","\t",
               "Initial Sick =", initial.sick, "\t","\t",
               "Infection Rate =", infection.perc, "\t","\t",
               "Mortality Rate =", mortality.perc, "\t","\t",
               "Immunity Coefficent =",immunity.perc)) +
  geom_text_repel(aes(label = Population), data = data_end)



#Summary
print(df.summary)
print(graph)
print("--Final numbers--")
print(paste("Days =", day,"  ", "Healthy =", Healthy.N,"  ","Sick =", sick.N, "  ","Death =", Death.N ))

print("--Parameters--")
print(paste("Days observed =",period.day, "  ","Allowed meetings =",meet, "  ", 
            "Initial Sick =", initial.sick,"  ", "Infection Rate =", infection.perc,"  ",
            "Mortality Rate =", mortality.perc, "Immunity Coefficent =",immunity.perc))


# Notes: 
# Changing Parameters as per previous test 1 python simulation:
#   
# 1) Increasing the duration of sickness led to a high death count
# 
# 2) Changing the parameter of encounters between 0-2 to reflect a possible lock down scenario has 
# the most significant impact of reducing overall sickness.
# 
# 3) Decreasing the infection had a positive impact with a decrease in mortality count
# 
# 4) Increasing the mortality rate to 3% shorten the simulator run due to population dying out or small surviving population count. 
# At 30% infection and 0-20 encounter rate. More people are getting sick faster than then dying. 
# Unless the mortality rate is significantly higher than the growth rate of infected people 
# 
# Code Limitations:
# - The code does not account for demographic diversity. For example the infection rate, duration of sickness, mortality rate between a child and a adult are different.
# - The simulation does not account for infected people would be hospitalise or bedridden. 
# - Does not account for outlier events such as super spreader events. 

# Additional Notes based on Covid simulation in R:
# 1) The addition of immunity parameter has dramatic impact on the overall numbers, decrease in the Sick population level and increase in healthy(immune) population.
# 2) Due to the speed of the infection rate majority of the population was infected within 10 days. Within 20 days population would have gone through Covid and came out immune. 
#    Because of this we see a dramatic reduction in newly infected.
# 3) In every trial simulation there would always be a dip within the 10-20 day mark and a sharp rise. This could be due a large amount of sick people dropping off after 10 days 
#    and the fact that there was a sharp rise in  sick cases in the initial 10 days. The 2nd sharp rise could be due to lag with significant amount of still people sick.
# 4) Previous code limitation in python was slow when increasing population has been fixed with vectorisation through R.  



