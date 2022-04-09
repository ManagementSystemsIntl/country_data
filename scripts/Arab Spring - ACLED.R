# Arab Spring
# ACLED

dat <- read_csv("data/ACLED/ACLEDOffline.csv")

head(dat[,24:32])
names(dat)
frq(dat$Country)

dat2 <- dat %>%
  filter(Event_Year>2005,
         Event_Year<2021,
         Country=="Egypt" | 
           Country=="Iraq" |
           Country=="Libya" |
           Country=="Tunisia" |
           Country=="Yemen" |
           Country=="Jordan" |
           Country=="Algeria" |
           Country=="Morocco" | 
           Country=="Lebanon")

frq(dat$Event_Type)

YrTyp <- dat2 %>%
  group_by(country=Country, year=Event_Year, type=Event_Type) %>%
  summarize(events=n()) %>%
  as.data.frame()

YrTyp
str(YrTyp)

ggplot(YrTyp, aes(year, events, group=country, color=country)) +
  geom_point() + 
  geom_line() + 
  scale_color_viridis_d() + 
  facet_wrap(~type) +
  facet_style()


dat3 <- dat %>%
  filter(Event_Year>2007,
         Event_Year<2021,
         Country=="Egypt" | 
           #Country=="Iraq" |
           Country=="Libya" |
           Country=="Tunisia" |
          #Country=="Yemen" |
           #Country=="Jordan" |
           #Country=="Algeria" |
           #Country=="Lebanon" |
           Country=="Morocco",
         Event_Type=="Protests")

YrTyp <- dat3 %>%
  group_by(country=Country, year=Event_Year, type=Event_Type) %>%
  summarize(events=n()) %>%
  as.data.frame()


ggplot(YrTyp, aes(year, events, color=country)) +
  geom_vline(xintercept=2010, color="grey50", size=1, alpha=.8) +
  #geom_point() + 
  geom_line() + 
  #stat_smooth(se=F) +
  scale_color_viridis_d() + 
  facet_wrap(~country) +
  facet_style() + 
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2008,2020,2),
                     labels=c("", "Arab\nSpring","","2014","","2018","")) + 
  scale_y_continuous(labels=comma) +
  theme(axis.title.y=element_text(angle=0, vjust=.5),
        legend.position="none") +
  labs(x="",
       y="",
       title="Protests in Arab Spring countries")


ggsave("viz/ACLED/ACLED protests facet.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


YrTyp

protest_ends <- YrTyp %>%
  group_by(country) %>%
  summarize(protests = last(events)) %>%
  mutate(protests2 = c(60, 170, 1322,1511))

protest_ends

ggplot(YrTyp, aes(year, events, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  #geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(0,1550),
                     labels=comma,
                     breaks=seq(0,1500,250),
                     sec.axis=sec_axis(~.,
                                       breaks=protest_ends$protests2,
                                       labels=protest_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right = element_text(color=c("#440154FF", "#31688EFF", "#35B779FF","#FDE725FF"))) +
  labs(x="",
       y="",
       title="Protests in Arab Spring countries")


ggsave("viz/ACLED/ACLED protests.png",
       device="png",
       type="cairo",
       height=4.5,
       width=7)


viridis(4)

# violent events and fatalities

frq(dat$Event_Type)
describe(dat$Fatalities)

dat3 <- dat %>%
  filter(Event_Year>2007,
         Event_Year<2021,
         Country=="Egypt" | 
           #Country=="Iraq" |
           Country=="Libya" |
           Country=="Tunisia" |
           #Country=="Yemen" |
           #Country=="Jordan" |
           #Country=="Algeria" |
           #Country=="Lebanon" |
           Country=="Morocco",
         Event_Type=="Battles" |
           Event_Type=="Explosions/Remote violence" |
           Event_Type=="Violence against civilians")

YrTyp <- dat3 %>%
  group_by(country=Country, year=Event_Year, type=Event_Type) %>%
  summarize(events=n(),
            fatalities=sum(Fatalities)) %>%
  as.data.frame()

YrTyp

ggplot(YrTyp, aes(year)) +
  geom_vline(xintercept=2010, color="grey50", size=1, alpha=.8) +
  #geom_point() + 
  #geom_line(aes(y=events), color="dodgerblue") + 
  stat_smooth(aes(y=events), color="dodgerblue", se=F) +
  stat_smooth(aes(y=fatalities), color="firebrick2", se=F) +
  #scale_color_viridis_d() + 
  facet_wrap(~country) +
  facet_style() + 
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2008,2020,2),
                     labels=c("", "Arab\nSpring","","2014","","2018","")) + 
  scale_y_continuous(labels=comma) +
  theme(axis.title.y=element_text(angle=0, vjust=.5),
        legend.position="none") +
  labs(x="",
       y="",
       title="Protests in Arab Spring countries")



YrFat <- dat3 %>%
  group_by(country=Country, year=Event_Year) %>%
  summarize(events=n(),
            fatalities=sum(Fatalities)) %>%
  filter(country!="Morocco") %>%
  as.data.frame()

YrFat


fatal_ends <- YrFat %>%
  group_by(country) %>%
  summarize(fatalities = last(fatalities)) 
%>%
  mutate(protests2 = c(60, 170, 1322,1511))

fatal_ends

ggplot(YrFat, aes(year, color=country, group=country)) +
  geom_vline(xintercept=2010, color="grey50", size=1, alpha=.8) +
  #geom_point(aes(y=fatalities)) + 
  geom_line(aes(y=fatalities), size=1) + 
  #stat_smooth(aes(y=events), se=F) +
  #stat_smooth(aes(y=fatalities), se=F, span=.465) +
  scale_color_viridis_d() + 
  #facet_wrap(~country) +
  #facet_style() + 
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring","2012","2014","2016","2018","2020")) + 
  scale_y_continuous(limits=c(0,5000),
                     labels=comma,
                     sec.axis=sec_axis(~.,
                                       breaks=fatal_ends$fatalities,
                                       labels=fatal_ends$country)) +
  theme(axis.text.y.right=element_text(color=viridis(3)),
        legend.position="none") +
  labs(x="",
       y="",
       title="Conflict fatalities in Arab Spring countries")


ggsave("viz/ACLED/ACLED conflict fatalities.png",
       device="png",
       type="cairo",
       height=4,
       width=7)



YrFat <- dat3 %>%
  group_by(country=Country, year=Event_Year) %>%
  summarize(events=n(),
            fatalities=sum(Fatalities)) %>%
  #filter(country!="Morocco") %>%
  as.data.frame()

YrFat


fatal_ends <- YrFat %>%
  group_by(country) %>%
  summarize(fatalities = last(fatalities)) %>%
  mutate(fatalities2 = c(800, 1600, -50,255))

fatal_ends

ggplot(YrFat, aes(year, color=country, group=country)) +
  geom_vline(xintercept=2010, color="grey50", size=1, alpha=.8) +
  #geom_point(aes(y=fatalities)) + 
  geom_line(aes(y=fatalities), size=1) + 
  #stat_smooth(aes(y=events), se=F) +
  #stat_smooth(aes(y=fatalities), se=F, span=.465) +
  scale_color_viridis_d() + 
  #facet_wrap(~country) +
  #facet_style() + 
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring","2012","2014","2016","2018","2020")) + 
  scale_y_continuous(limits=c(0,5000),
                     labels=comma,
                     sec.axis=sec_axis(~.,
                                       breaks=fatal_ends$fatalities2,
                                       labels=fatal_ends$country)) +
  theme(axis.text.y.right=element_text(color=viridis(4)),
        legend.position="none") +
  labs(x="",
       y="",
       title="Conflict fatalities in Arab Spring countries")


ggsave("viz/ACLED/ACLED conflict fatalities four countries.png",
       device="png",
       type="cairo",
       height=4,
       width=7)





## all GISR countries ----

dat4 <- dat %>%
  filter(Event_Year>2007,
         Event_Year<2021,
         Country=="Egypt" | 
           #Country=="Iraq" |
           Country=="Libya" |
           Country=="Tunisia" |
           Country=="Yemen" |
           Country=="Jordan" |
           #Country=="Syria" |
           #Country=="Lebanon" |
           Country=="Morocco",
         Event_Type=="Protests")


YrTyp4 <- dat4 %>%
  group_by(country=Country, year=Event_Year, type=Event_Type) %>%
  summarize(events=n()) %>%
  as.data.frame()


ggplot(YrTyp4, aes(year, events, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  #geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(0,1550),
                     labels=comma,
                     breaks=seq(0,1500,250)) +
  facet_wrap(~country) +
  faceted +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right = element_text(color=c("#440154FF", "#31688EFF", "#35B779FF","#FDE725FF"))) +
  labs(x="",
       y="",
       title="Protests in Arab Spring countries")


ggsave("viz/ACLED/ACLED protests.png",
       device="png",
       type="cairo",
       height=4.5,
       width=7)




