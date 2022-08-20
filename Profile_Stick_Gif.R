#Packages Used
library(tidyverse)
library(gganimate)
library(scales)
library(ggthemes)

setwd("~/Desktop/R_Directory")

#Function to crete circles
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
#Create our circles for the head
circle <- circleFun(center= c(11,61), diameter = 10, npoints=100)

#Manually create the stick body
stick_man <- data.frame(X = c(seq(0, 11, by=.25), #Left leg 
                              seq(11, 22, by=.25), #Right leg
                              rep(11, 69), #Body
                              seq(3, 11, by=.25), #Left Arm
                              seq(11, 19, by=.25), #Right Arm
                              circle$x,
                              c(13, 9), #Eyes,
                              
                              seq(0, 11, by=.25), #Left leg 
                              seq(11, 22, by=.25), #Right leg
                              rep(11, 69), #Body
                              seq(3, 11, by=.25), #Left Arm
                              seq(11, 19, by=.25), #Right Arm
                              circle$x,
                              c(13, 9)), 
                        
                        Y= c(seq(0, 22, by=.5), #Left leg
                             seq(22, 0, by=-.5), #Right Leg
                             seq(22, 56, by=.5), #Body
                             seq(30, 46, by=.5), #Left arm
                             seq(46, 30, by=-.5), #Right aram 
                             circle$y, #Head
                             c(63, 63),
                             
                             
                             seq(0, 22, by=.5), #Left leg
                             seq(22, 0, by=-.5), #Right Leg
                             seq(22, 56, by=.5), #Body
                             seq(30, 46, by=.5), #Left arm
                             seq(46, 62, by=.5), #Right Arm
                             circle$y,
                             c(63, 63)
                        ),
                        
                        year = c(rep("2008", 327),
                                  rep("2009", 327))
                        
    )


#Duplicate our stick man rows and combine it all together
stick_man$class <- "Stick"
stick_man_dupe <- stick_man
stick_man_dupe$year <- ifelse(stick_man$year == "2008", 
                         "2010", "2011")

stick_man_dupe2 <- stick_man_dupe
stick_man_dupe2$year <- ifelse(stick_man_dupe2$year == "2010", 
                              "2012", "2013")


stick_man <- rbind(stick_man, stick_man_dupe, stick_man_dupe2)
stick_man <- stick_man %>%
  filter(year != "2013")

#Shift our x values
stick_man$X <- stick_man$X + 25000

#Read in any gappminder dataset
gapminder <- read_csv('/Users/jackogozaly/Desktop/R_Directory/gapminder-FiveYearData.csv')

#Filter so we have less values until we get to the stickman
gapminder <- gapminder %>% 
  select(gdpPercap, lifeExp, year, continent) %>%
  filter(year > 1980)

#Change column names
colnames(gapminder) <- c("X", "Y", "year", "class")

#Combine our stickman dataset with gapminder
gapminder_stick <- rbind(gapminder, stick_man)

#Make the funny graph
p <- gapminder_stick %>%
  ggplot(aes(x= X, y= Y)) + geom_point(color ="#F3FE08") + theme_fivethirtyeight() +
  theme(panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "top",
        axis.text = element_text(),
        axis.title = element_text(face="bold")) +
  xlab("\nGDP Per Capita") + ylab("Life Expectancy\n") + ggtitle("Data Viz is My Passion...") +
  scale_x_continuous(labels = label_dollar())

#Create animation
anim <- p + 
  transition_states(year,
                    transition_length = 3,
                    state_length = 0, 
                    wrap= FALSE) + view_follow() + 
                    enter_fade() + exit_fade()
#Render Our Gif
anim <- animate(
  plot = anim,
  render = gifski_renderer(),
  nframes= 300,
  duration = 15,
  fps = 20,
  height = 400, width =600)

#View our gif
anim

#Save our gif
anim_save("Profile_Stickman.gif", animation = last_animation())




