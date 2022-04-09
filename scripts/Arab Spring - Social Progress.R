# Arab Spring
# Social Progress Index

# import data ---- 

dat <- read_excel("data/Social Progress Index/2011-2020-Social-Progress-Index.xlsx", sheet=2)

gisr <- dat %>%
  filter(Country=="Lebanon" |
           Country=="Iraq" |
           Country=="Morocco" |
           Country=="Libya" |
           Country=="Syria" |
           Country=="Jordan" |
           Country=="Yemen" |
           Country=="Egypt" |
           Country=="Tunisia") %>%
  select(1:9) %>%
  rename(rank=1,
         code=3,
         year=4,
         spi=`Social Progress Index`,
         needs=7,
         well=8,
         opp=9)

names(gisr)

eg <- gisr %>%
  filter(Country=="Egypt")

tun <- gisr %>%
  filter(Country=="Tunisia")

# gisr spi score line ---- 

score_ends <- gisr %>%
  group_by(Country) %>%
  summarize(score = first(spi)) %>%
  mutate(color=viridis(6))

score_ends


ggplot(gisr, aes(year, spi, color=Country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  #geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2010,2020,1),
                     labels=c("Arab\nSpring", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")) + 
  scale_y_continuous(limits=c(55,76),
                     breaks=seq(55,75,5),
                     sec.axis=sec_axis(~.,
                                       breaks=score_ends$score,
                                       labels=score_ends$Country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=score_ends$color)) +
  labs(x="",
       y="",
       title="Social Progress Index",
       caption="Overall score, 0-100")


ggsave("viz/Social Progress/spi gisr line.png",
       device="png",
       type="cairo",
       height=5,
       width=7)


# gisr spi rank line

rank_ends <- gisr %>%
  group_by(Country) %>%
  summarize(rank = first(rank)) %>%
  mutate(color=viridis(6))

rank_ends


ggplot(gisr, aes(year, rank, color=Country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  #geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2010,2020,2),
                     labels=c("Arab\nSpring", 
                              #"2011",
                              "2012",
                              #"2013",
                              "2014",
                              #"2015",
                              "2016",
                              #"2017",
                              "2018",
                              #"2019",
                              "2020")) + 
  scale_y_reverse(   #limits=c(0,176),
                     #breaks=seq(0,175,25),
                     sec.axis=sec_axis(~.,
                                       breaks=rank_ends$rank,
                                       labels=rank_ends$Country)) +
  #scale_y_reverse() +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Rank",
       title="Social Progress Index")


ggsave("viz/Social Progress/spi gisr rank line.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


# Egypt dimensions line ---- 

score_ends <- data.frame(dimension=c("Basic needs","Wellbeing","Opportunity"),
                         value=c(79.4, 51.5, 49),
                         color=viridis(3))

score_ends

colors <- viridis(3)

ggplot(eg, aes(year)) + 
  geom_line(aes(y=needs), color=colors[1], size=1) + 
  geom_line(aes(y=well), color=colors[2], size=1) + 
  geom_line(aes(y=opp), color=colors[3], size=1)  +
  scale_x_continuous(breaks=seq(2010,2020,2),
                     labels=c("Arab\nSpring", 
                              #"2011",
                              "2012",
                              #"2013",
                              "2014",
                              #"2015",
                              "2016",
                              #"2017",
                              "2018",
                              #"2019",
                              "2020")) + 
  scale_y_continuous(limits=c(40,80),
                     breaks=seq(40,80,5),
                     sec.axis=sec_axis(~.,
                                       breaks=score_ends$value,
                                       labels=score_ends$dimension)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=score_ends$color)) +
  labs(x="",
       y="",
       title="Social Progress Index, Egypt",
       caption="Overall score, 0-100")


ggsave("viz/Social Progress/spi gisr line.png",
       device="png",
       type="cairo",
       height=5,
       width=7)



