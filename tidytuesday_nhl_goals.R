library(tidyverse)
library(patchwork)

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

mean_goals <- season_goals %>% filter(total_goals > 500 & age<45& player !="Alex Ovechkin" )  %>% group_by(age) %>% summarize(mean_goals=mean(goals),sd_goals=sd(goals))

alex_goals <- season_goals %>% filter(player=="Alex Ovechkin") %>% group_by(age) %>% 
  summarize(alex_goals=mean(goals)) %>% mutate(alex_goals_cumulative = cumsum(alex_goals))

all_goals <- merge(mean_goals,alex_goals)
all_goals_z <- all_goals %>% mutate(z_score = (alex_goals-mean_goals)/sd_goals)
alex_mean_z <- mean(all_goals_z$z_score)
alex_predict <- mean_goals %>% filter(age >34) %>% mutate(alex_predict = mean_goals + sd_goals*alex_mean_z) %>% filter(age<41)

p1 <- season_goals %>% filter(total_goals > 500 & age<41 & player !="Alex Ovechkin")  %>% group_by(player,age) %>% summarize(goals=sum(goals)) %>% 
  ggplot(aes(x=age, y=goals)) + geom_point(alpha=0.5, col="grey80") + stat_summary(geom="line", fun.y=mean)+
  theme_minimal() + geom_point(data=season_goals %>% filter(player=="Alex Ovechkin"), aes(x=age, y=goals), size=3, col="orange") +
  geom_point(data=alex_predict, aes(x=age,y=alex_predict), col="orange", alpha=0.5, size=3) +
  labs(title="NHL goals by age scored, players with more than 500 career goals", 
       subtitle="Orange dots show total goals by Alex Ovechkin each year. Opaque dots are predicted values", x="Age", y="Total goals")

#Gretzky comparison
wayne_goals <- season_goals %>% filter(player=="Wayne Gretzky" & team !="TOT") %>% 
  group_by(age) %>% summarize(wayne_goals=mean(goals)) %>% mutate(wayne_goals_cumulative = cumsum(wayne_goals))

wayne_alex <- merge(wayne_goals,alex_goals) %>% gather(key=key,value=value, -age)

p2 <- ggplot(wayne_alex %>% filter(key %in% c("alex_goals_cumulative", "wayne_goals_cumulative")), 
                             aes(x=age,y=value,col=key)) + geom_line(size=2) + theme_minimal() +
  scale_color_brewer(palette="Dark2", labels=c("Alex", "Wayne")) +labs(x="Age", y="Goals (cumulative)", title="Alex Ovechkin vs Wayne Gretsky, cumulative goals by age", col="Player")

#Cumulative prediction by age 40
alex_pred2 <- alex_predict %>% mutate(alex_goals=alex_predict, predict=T)
alex_goals2 <-alex_goals %>% mutate(predict=F)
alex_goals_predict <- rbind(alex_goals2[,c(1:2,4)], alex_pred2[,c(1,5,6)])
alex_goals_predict <- alex_goals_predict %>% mutate(cumulative = cumsum(alex_goals))

p3 <- ggplot(alex_goals_predict, aes(x=age, y=cumulative, col=predict)) + geom_line(size=2) +
  scale_colour_manual(values=c("orange","darkorange3"), labels=c("Actual", "Predicted"))+ 
  theme_minimal() + labs(caption="Source: HockeyReference.com. Prediction based on age-based goal trajectories of other top goal scorers.", col="", title="Will Ovechkin make it to Gretzky's 895 by age 40?", subtitle="Alex Ovechkin - predicted cumulative goals to age 40", x="Age",
                         y="Goals (cumulative)") + geom_text(x=40.5, y=895, label="887", col="black")

p1/p2/p3 + plot_annotation(title="Will Ovechkin make it to 895 by age 40?", 
                             theme = theme(plot.title = element_text(size = 18)))
ggsave("ovechkin.png", height=10, width=15)
