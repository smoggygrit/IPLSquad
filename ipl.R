ross <- read.csv("iplroster.csv")

teamcols <- c( "#f4ee42","#cc2c1a" , "#b5a8a4", "#7e2ea0", "#2e6ca0", "#092ea5" , "#a50b09","#e57614")

totteam <- ross %>% group_by(Team) %>%
                    summarise(totplay = n()) %>%
                    arrange(Team)

aveage <- ross %>% group_by(Team) %>%
                    arrange(Team)

avegame <-  ross %>% group_by(Team) %>%
                    arrange(Team)

ggplot(totteam, aes(y = totplay, x = Team, fill = Team)) + geom_col() + coord_flip() + scale_y_discrete("Total Players") +
  theme(panel.background = element_blank(), legend.position = "none") + 
  scale_fill_manual(values = teamcols)

ggplot(aveage, aes(y = Age, x = Team, color = Team, size = 3)) + geom_point(alpha = 0.6, position = position_jitter(width = 0.2)) + coord_flip() + scale_y_discrete("Average Age") +
  theme(panel.background = element_blank(), legend.position = "none") + 
  scale_fill_manual(values = teamcols)

ggplot(avegame, aes(y = IPLGa, x = Team, color = Team, size = 3)) + geom_point(alpha = 0.6, position = position_jitter(width = 0.2)) + coord_flip() + scale_y_discrete("Average IPL Games") +
  theme(panel.background = element_blank(), legend.position = "none") + 
  scale_fill_manual(values = teamcols)

##### ring chart ###################################################################################
totbats <- ross %>% filter(Player_type == "Batsmen") %>%
                    group_by(Team) %>%
                    summarise(bats = n())
totAlRo <- ross %>% filter(Player_type == "All Rounder") %>%
                    group_by(Team) %>%
                    summarise(AllRou = n())
totFB <- ross %>% filter(Player_type == "Fast Bowler") %>%
                    group_by(Team) %>%
                    summarise(FastB = n())
totSB <- ross %>% filter(Player_type == "Spin Bowler") %>%
                  group_by(Team) %>%
                  summarise(SpinB = n())

tot1 <- full_join(totteam, totbats, by = "Team")
tot2 <- full_join(tot1, totAlRo, by = "Team")
tot3 <- full_join(tot2, totFB, by = "Team")
tot4 <- full_join(tot3, totSB, by = "Team")

perc1 <- mutate(tot4, PerBat = bats/ totplay * 100, perAllR = AllRou / totplay * 100, perFB = FastB / totplay * 100, perSB = SpinB /totplay *100)

perc2 <-  select(perc1, Team, contains("per"))

team <- filter(perc2, Team == "Chennai Super Kings")

team2 <- gather(team, "Cat", "per", 2:5)

team2$fraction = team2$per / sum(team2$per)
team2 = team2[order(team2$fraction), ]
team2$ymax = cumsum(team2$fraction)
team2$ymin = c(0, head(team2$ymax, n=-1))

allteam <- summarise(perc2, aveBa = mean(PerBat), avepAR = mean(perAllR), aveFB  = mean(perFB), aveSB = mean(perSB) )

########## treamap ################################################################################################

teamc <- filter(ross, Team == "Sunrisers Hyderabad")

ggplot(teamc, aes(area = Value, fill = Player_type, subgroup = Player_type, label = Name)) + geom_treemap() + geom_treemap_text(col= "white")


##### batting  ####################################################################################

teamc2 <- filter(ross, Team == "Sunrisers Hyderabad", Player_type %in% c("Batsmen", "Wicket Keeper"))

allteam2 <- ross %>% filter(Team != "Sunrisers Hyderabad",Player_type %in% c("Batsmen", "Wicket Keeper")) %>%
                     mutate(Team = "All Teams" )
batsmen2 <- bind_rows(teamc2, allteam2)

ggplot(batsmen2, aes(x = Twenty20Av, y= TwentyStrike, col = Team )) + geom_point(size = 3, alpha = 0.3) + geom_vline(xintercept = 30) + geom_hline(yintercept = 125) + scale_x_discrete("Twenty 20 Average") +
  scale_y_discrete("Twenty 20 Strike Rate")+
  theme(panel.background = element_blank()) + 
  annotate("text", x = 25, y = 100, label = "Low Run Rate, Short Innings") +
  annotate("text", x = 36, y = 100, label = "Low Run Rate, Long Innings" ) +
  annotate("text", x = 25, y = 160, label = "High Run Rate, Short Innings") +
  annotate("text", x = 36, y = 160, label = "High Run Rate, Long Innings")
  

####### All Rounders  ########################################################################################
team3 <- filter(ross, Team == "Sunrisers Hyderabad", Player_type == "All Rounder" )

allteam3 <- ross %>% filter(Team != "Sunrisers Hyderabad",Player_type == "All Rounder" ) %>%
                     mutate(Team = "All Teams" )
allround2 <- bind_rows(team3, allteam3)

allroun3 <- mutate(allround2, ecoRa = dense_rank(EcoRate) )



ggplot(allroun3, aes(x = Twenty20Av, y= BowlAV, size = ecoRa, col = Team)) + geom_point(alpha = 0.4) + xlab("Twenty 20 Average") + ylab("Twenty20 Bowling Average") +
  theme(panel.background = element_blank())


#################### Bowlers ###################################################################################

teamc3 <- filter(ross, Team == "Sunrisers Hyderabad", Player_type == "Fast Bowler" )

allteam3 <- ross %>% filter(Team != "Sunrisers Hyderabad" & Player_type %in% c("Spin Bowler", "Fast Bowler")) %>%
            mutate(Team = "All Teams" )
bowler2 <- bind_rows(teamc3, allteam3)

ggplot(bowler2, aes(x = EcoRate, y= BowlAV, col = Team)) + geom_point(size = 4, alpha = 0.4) + geom_vline(xintercept = 7.5) + geom_hline(yintercept = 22.5) +
xlab(" Twenty 20 Economy Rate") + ylab("Twenty20 Bowling Average") +
theme(panel.background = element_blank())+
annotate("text", x = 6.2, y = 15, label ="High Threat, Low Runs") +
annotate("text", x= 8, y = 15,label = "High Threat, High Runs") +
  annotate("text", x= 6.2, y = 35,label = "Low Runs, Low Threat") +
  annotate("text", x = 8, y = 35, label ="Low Threat, High Runs ")
  
#######+ geom_vline(xintercept = 30) + geom_hline(yintercept = 125)