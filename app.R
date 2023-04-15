library(shiny)
library(tidyverse)
library(elo)
library(MetBrewer)
library(DT)
library(bslib)

teams_files <- list.files(path = "matchups",
                    pattern= "Teams*",
                    full.names = TRUE)

teams <- lapply(teams_files, read.csv) %>% bind_rows(.id="Season") 

team_names <- teams %>%
  select(ID,Name,Manager,Rank) 

matchup_files <- list.files(path = "matchups",
                       pattern= "Matchup*",
                       full.names = TRUE)

matchups <- lapply(matchup_files,function(x) read.csv(x) %>%
                     select(Week,Team.1.ID,Team.1.Name,Team.2.ID,Team.2.Name, Complete, Playoff, Consolation, Team.1.Points, Team.2.Points)
                            
                            )
                   
                     



matchups <- lapply(matchups,left_join,team_names,by = c("Team.1.ID" = "ID","Team.1.Name"="Name")) %>%
  map(~rename(.,Team.1.Manager=Manager,Team.1.Playoff.Rank=Rank))
matchups <- lapply(matchups,left_join,team_names,by = c("Team.2.ID" = "ID","Team.2.Name"="Name")) %>%
  map(~rename(.,Team.2.Manager=Manager,Team.2.Playoff.Rank=Rank)) %>% 
  map(~mutate(.,max_week= max(Week)))


regular_season <- matchups %>%
  map(~filter(., !(Playoff==TRUE & Week==max_week & (Team.1.Playoff.Rank>4 | Team.2.Playoff.Rank>4) | #finals and third place
                     Playoff==TRUE & Week==(max_week-1) & (Team.1.Playoff.Rank>4 | Team.2.Playoff.Rank>4)  | #semifinals
                     Playoff==TRUE & Week==(max_week-1) & (Team.1.Playoff.Rank>8 | Team.2.Playoff.Rank>8) |  #first roune
                     Consolation==TRUE
                     ))) %>% 
  bind_rows(.id = "Season") %>%
  filter(Consolation==FALSE)


e <- elo.run(score(Team.1.Points,Team.2.Points)~Team.1.Manager + Team.2.Manager,#+ regress(Season,1500,0.1,regress.unused = TRUE),
             k=15,
             data=regular_season)


elo <- as.data.frame(e) %>%
  mutate(p.B = 1-p.A,
         wins.B = 1-wins.A
  ) %>%
  bind_cols(regular_season %>% select(Season,Week)) %>% 
  mutate(team.A = (str_to_sentence(team.A) %>% str_split(pattern = " ",simplify = TRUE))[,1],
         team.B = (str_to_sentence(team.B) %>% str_split(pattern = " ",simplify = TRUE))[,1])

team_a <- elo %>%
  select(contains(".A"),Week,Season)
names(team_a) <- str_remove_all(names(team_a),".A")

team_b <- elo %>%
  select(contains(".B"),Week,Season)
names(team_b) <- str_remove_all(names(team_b),".B")

team_rankings <- bind_rows(team_a,team_b) %>%
  left_join(teams %>% select(Season,Manager,Rank) %>% mutate(team = (str_to_sentence(Manager) %>% str_split(pattern = " ",simplify = TRUE))[,1]),
            by = c("Season","team")) %>% 
  arrange(team,Season,Week) %>%
  group_by(team) %>%
  mutate(match_week = row_number(),
         year = case_when(Season==1~"2014/15",
                          Season==2~"2015/16",
                          Season==3~"2016/17",
                          Season==4~"2017/18",
                          Season==5~"2018/19",
                          Season==6~"2019/20",
                          Season==7~"2020/21",
                          Season==8~"2021/22",
                          Season==9~"2022/23")
  ) %>%
  mutate(label = (ifelse(match_week == max(match_week),team %>% as.character(),NA_character_))) %>% 
  group_by(Season,team) %>% 
  mutate(championship = ifelse(Week==max(Week) & Rank==1 & Season != 6,elo,NA_real_)) %>% 
  group_by(team) %>% 
  mutate(peak_elo = ifelse(elo==max(elo),elo,NA_real_))


season_matchweeks <- matchups %>% 
  bind_rows(.id="Season") %>% 
  group_by(Season,Week) %>% 
  tally(name = "N Matchups") %>% 
  ungroup() %>% 
  mutate(rolling_match_week = row_number()) %>% 
  select(-`N Matchups`)

team_rankings <- team_rankings %>% 
  left_join(season_matchweeks)

overall <- team_rankings %>% 
  group_by(team) %>% 
  filter(match_week==max(match_week)) %>% 
  select(team,elo) %>% 
  arrange(desc(elo)) %>% 
  ungroup() %>% 
  mutate(Rank = row_number())

peak_elo <-  team_rankings %>% 
  group_by(team) %>% 
  summarise(`Peak Elo`= round(max(elo)),
            `Avg Elo` = round(mean(elo)))

rank <- team_rankings %>% 
  group_by(team) %>% 
  summarise(Wins = sum(as.character(wins)=="1"),
            Losses = sum(as.character(wins)=="0"),
            Ties = sum(as.character(wins)=="0.5"),
            `Win Pct.` =  round(sum(wins) / n()*100,1)
  ) 

overall <- overall %>% 
  left_join(rank) %>% 
  left_join(peak_elo) %>% 
  rename(Team = team,ELO = elo) %>% 
  select(Rank,Team,contains("Elo"),everything())

# team_rankings %>% 
#   filter(Season==1&team=="Alex") %>% 
#   summarise(n =  n(),wins=sum(wins))
#   View()


playoffs <- teams %>% 
  mutate(Manager = (str_to_sentence(Manager) %>% str_split(pattern = " ",simplify = TRUE))[,1]) %>% 
  group_by(Manager) %>% 
  summarise(
    # `Presidents Trophy` = sum(Season != "6" & Playoff.Seed ==1 | Season==6 & Rank ==1,na.rm=TRUE),
            `Playoff Appearance` = sum(Season==1 & Rank<=6) + sum(Season>1 & Rank<=8),
            `Third` = sum(Season != "6" & Rank == 3),
            `Second` = sum(Season != "6" & Rank == 2),
            `Championship` = sum(Season != "6" & Rank== 1)
  ) %>% 
  mutate(Total =  `Third` + `Second` + `Championship`) %>% 
  arrange(desc(Championship),desc(`Second`),desc(`Third`), desc(`Playoff Appearance`))

playoffs$Manager <- factor(playoffs$Manager,levels = playoffs$Manager)

# teams %>% 
#   select(-contains("Image")) %>% 
#   filter(Playoff.Seed<=6&Season==1 | Season>1 & !is.na(Playoff.Seed)) %>%
#   mutate(Rank = ifelse(Rank>4,8,Rank)) %>% 
#   mutate(expectation_score = (Playoff.Seed-Rank)/Playoff.Seed,
#          Category = case_when(expectation_score==0~"Fulfilled Potential",
#                               expectation_score<0~"Choked",
#                               expectation_score>0~"Overachiever")
#            ) %>% 
#   ggplot(aes(x = Playoff.Seed, y = expectation_score, label = Manager,color=Category)) +
#   geom_hline(yintercept = 0,linetype="dashed") +
#   geom_point()+
#   geom_function(fun = function(x) (x-1)/x,linetype="dashed",colour="grey")+
#   geom_function(fun = function(x) (x-8)/x,linetype="dashed",colour="grey")
#   
#   





ui <- fluidPage(
  
  titlePanel("The Complete History of The League"),
  h5(paste0("Every General Managers skill calculated after every matchweek from 2014 to ", 2014 + max(as.integer(team_rankings$Season))," (" ,max(team_rankings$Season)," seasons)")),
  theme = bs_theme(version = 5, bootswatch = "litera"),
  

  
  tags$div(
    tags$p("We use the Elo method to measure the performance of fantasy general managers' (GMs) teams based on head-to-head results. 
           Elo ratings start at 1500, and a team's rating is adjusted based on whether they win or lose a head-to-head matchup. 
           Wins increase the rating, while losses decrease it. 
           The Elo method also gives more credit to teams that win against higher-ranked opponents and less credit to teams that win against lower-ranked opponents." ),
  ),
    tags$div(
    tags$p("The chart below is the complete history of our league starting in the 2014/15 season."),
    tags$ul(
    tags$li("A star (★) represents a championship."),
    tags$li(paste0("A square (\u25A0) represents a GMs peak Elo rating."))
    ),
    tags$p("Use the box below to pick specific GMs trendlines to isolate:")
  ),
  br(),
  selectInput("team", 
              label = "Filter General Manager", 
              choices = team_rankings$team %>% unique %>% sort,
              multiple = TRUE
  ),
  

  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Historical ELO Ratings",plotOutput("plot1",width = "150%")),
                tabPanel("Playoff Success",plotOutput("plot2")),
                tabPanel("Summary Statistics",DT::dataTableOutput("static",width = "75%"))
                
    )
    
    
  ),
  sliderInput("xrange", "Zoom in to matchweeks:", min = 1, 
              max = max(team_rankings$rolling_match_week), 
              value = c(1, max(team_rankings$rolling_match_week)), 
              step = 1,
              width = "100%")
  
)


  
  
  # fluidRow(
  #   plotOutput("plot1")
  # ),
  # 
  # titlePanel("Summary Stats"),
  # 
  # 
  # fluidRow(
  #   DT::dataTableOutput("static",width = "75%")
  # ),
  # 
  # titlePanel("Playoffs"),
  # 
  # 
  # fluidRow(
  #   plotOutput("")
  # ),
  
  # titlePanel("Current Playoff Rankings"),
  # 
  # fluidRow(
  #   tableOutput("static2")
  # ),
  
  # titlePanel("Choke Shows and Heroes"),
  # 
  # 
  # fluidRow(
  #   plotOutput("plot3")
  # ),
  


server <- function(input, output, session) {
  
  
  output$plot1 <- renderPlot({
    
    d <- team_rankings %>% 
      { if (length(input$team) > 0) filter(., team %in% input$team) else . } 
    
    cols <- d$team %>% unique() %>% length()
    
    season_week_max <- d %>% group_by(year,Season) %>% 
      summarise(max_match_week = max(rolling_match_week))
    
    current_season <- max(season_week_max$Season)
    current_year <- season_week_max %>% filter(Season==current_season) %>% pull(year)
    max_week_current_season <- season_week_max %>% filter(Season==current_season) %>% pull(max_match_week)
    
    end_of_season <- d %>% group_by(year) %>% 
      summarise(start_of_year  = ifelse(min(rolling_match_week)==1,0,min(rolling_match_week)),
        end_of_year = max(rolling_match_week)) %>% 
      mutate(
        midpoint = round((start_of_year+end_of_year)/2),
        label_placement = start_of_year,
        
             y = max(d$elo)+10) %>% 
      ungroup()
    
  
   breaks_x <-  c(end_of_season$start_of_year,end_of_season$midpoint) %>% sort()
    
    # 
    # d <- d %>% ungroup() %>% add_row(year = current_year,Season = current_season, 
    #                                  rolling_match_week = c(max_week_current_season + seq(1:10))
    #                                  
    #                                  )

    tail(d)
  
    d  %>%
      group_by(team) %>% 
      mutate(max_season = max(Season)) %>% 
      ungroup() %>% 
      ggplot(aes(x=rolling_match_week ,y=elo)) +
      geom_text(data = end_of_season, aes(x = label_placement, y = y, label = year,fontface="bold"), color = "grey50", hjust=-0.1) +
      geom_hline(yintercept = 1500,linetype="dashed")+
      geom_vline(xintercept = end_of_season %>% pull(start_of_year),colour="grey50") +
      geom_line(linewidth=1,linetype="solid",aes( alpha = max_season != current_season,colour=team,path=year)) +
      scale_color_manual(guide = "none",values = met.brewer(name="Juarez",n=cols,type="continuous")) +
      scale_x_continuous(breaks = breaks_x)+
      ggrepel::geom_label_repel(aes(alpha = max_season != current_season,colour=team,label=label),show.legend = FALSE,nudge_x = 3,segment.size=0.25,segment.color = "grey") +
      # ggthemes::theme_tufte()+
      cowplot::theme_minimal_vgrid() +
      labs(y="ELO",x="Matchweek") +
      # facet_grid(.~year,scales = "free_x",space = "free_x") + 
      # theme(panel.spacing = unit(0.5, "mm"))+
      geom_point(aes(y=peak_elo,alpha = max_season != current_season,colour=team),size=3,shape=15)+
      scale_alpha_manual(guide = "none",values = c(1,0.3)) +
      # geom_point(aes(y=championship),size=6,shape=21,fill="gold")+
      geom_text(aes(y = championship,colour=team),label="★", size=7,family = "HiraKakuPro-W3")+

      theme(axis.text.x = element_text(angle = 45,hjust =1),
            )    +
      coord_cartesian(xlim = input$xrange)

      
    # annotate("text", x = 11, y = ann_y, size=5,label = "2014/15")+
    # annotate("text", x = 33, y = ann_y, size=5,label = "2015/16")+
    # annotate("text", x = 54, y = ann_y, size=5,label = "2016/17")+
    # annotate("text", x = 74, y = ann_y, size=5,label = "2017/18")+
    # annotate("text", x = 94, y = ann_y, size=5,label = "2018/19")+
    # annotate("text", x = 115, y = ann_y, size=5,label = "2019/20")+
    # annotate("text", x = 132, y = ann_y, size=5,label = "2020/21")
    
    
  })
  
  
  output$static <- DT::renderDataTable(
    
    overall %>% 
      left_join(playoffs,by = c("Team"="Manager")) %>% 
      mutate(ELO = round(ELO,0)) %>% 
      rename(`Current Rank`=Rank,`Current Elo`=ELO) %>% 
      { if (length(input$team) > 0) filter(., Team %in% input$team) else . }
    
    
    
    
  )
  
  
  output$plot2 <- renderPlot({
    
  playoffs %>% 
    select(-Total,-`Playoff Appearance`) %>% 
      { if (length(input$team) > 0) filter(., Manager %in% input$team) else . } %>% 
    pivot_longer(-Manager,names_to = "Trophies") %>% 
    mutate(Trophies = factor(Trophies, levels = c("Championship","Second","Third","Presidents Trophy"))) %>% 
    ggplot(aes(x=Manager,y=value,fill=Trophies)) +
    geom_col() + 
    theme_classic()+
    scale_fill_manual(values = c("#D4AF37","#b5b5bd", "#9c5221","black"))+
    labs(y="Total")+
    theme(legend.position = "top",axis.text.x = element_text(angle = 45,hjust =1)
          )
    
  }
  )
  # 
  # output$static2 <- renderTable(
  #   
  #   playoffs %>% 
  #     mutate(Rank = row_number()) %>% 
  #     select(Rank,Manager,Total,Championship,Second,Third,`Presidents Trophy`) %>% 
  #     rename(`Total Trophies`=Total)
  #     
  #   
  #   
  #   
  # )
  # 
  
  # output$plot3 <- renderPlot({
  #   
  #   
  #   overall %>% select(Team,Rank) %>% 
  #     left_join(playoffs %>% mutate(Rank = row_number()),by = c("Team"="Manager")) %>% 
  #     mutate(resid = (Rank.y-  Rank.x) / length(Rank.y),
  #            Rank.x.center = mean(Rank.x)-Rank.x,
  #            Rank.y.center = mean(Rank.y)-Rank.y,
  #            
  #            ) %>% 
  #     filter(`Playoff Appearance`>1) %>% 
  #     mutate(Category = case_when(Rank.x<5 & resid>=0.15 ~ "Chokeshow",
  #                            Rank.x>=5 & resid >=0.15 ~ "Chokeshow mini",
  #                            Rank.x>=5 & resid <=0.15  & resid >=-0.15~ "Not great anyways",
  #                            Rank.x<5 & resid <=0.15 ~ "Powerhouse",
  #                            Rank.x>=5 & resid <=-0.15 ~ "Built for playoffs",
  #                            
  #                            TRUE~"Irrelevant")) %>% 
  #     ggplot( aes(x=fct_reorder(Team,resid), y=resid,colour=Category)) + 
  #     geom_hline(yintercept = 0,linetype="dashed")+ 
  #     geom_point(aes(size=`Playoff Appearance`))  +
  #     geom_segment(aes(y = 0, 
  #                      x = Team, 
  #                      yend = resid, 
  #                      xend = Team), 
  #                  color = "black") +
  #     labs(y="Choke Factor",
  #          x="Manager")+
  #     coord_flip() +
  #     theme_bw() +
  #     scale_color_manual(values = met.brewer("Juarez",n=5,) %>% rev())
  #   
  #   
  #     
  #   
  # })
  
  
  
  
  
}

shinyApp(ui, server)
