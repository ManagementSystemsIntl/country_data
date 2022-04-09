# Arab Spring
# V-Dem

library(vdem)
library(vdemdata)

source("scripts/00 Arab Spring - prep.r")

vdem  
  
vdem()

?VDem_plus

vd <- VDem_plus

vd <- vdem

frq(vd$year)

names(vd)[1:80]

reg <- vd %>%
  select(country=vdem_country_name,
         year,
         )

vd_cb <- vdem_codebook


# selected countries, electoral democracy ---------------------------------

reg <- vd %>%
  select(country=vdem_country_name,
         year,
         v2x_polyarchy,
         v2x_libdem,
         v2x_partipdem,
         v2x_delibdem,
         v2x_egaldem) %>%
  filter(country=="Egypt" | 
           country=="Tunisia" |
           country=="Libya",
         year>2007)

reg
frq(reg$country)
describe(reg)

electoral_ends <- reg %>%
  group_by(country) %>%
  summarize(electoral = last(v2x_polyarchy))

electoral_ends


ggplot(reg, aes(year, v2x_polyarchy, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(0,1),
    #breaks=0:16,
    sec.axis=sec_axis(~.,
                      breaks=electoral_ends$electoral,
                      labels=electoral_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Index\nscore",
       title="Varieties of Democracy\nElectoral Democracy Index")


ggsave("viz/V-Dem/vd electoral Eg Tun Lib.png",
       device="png",
       type="cairo",
       height=4,
       width=7)



score_ends <- reg %>%
  group_by(country) %>%
  summarize(electoral = last(v2x_polyarchy),
            liberal = last(v2x_libdem),
            partip = last(v2x_partipdem),
            delib = last(v2x_delibdem),
            egal = last(v2x_egaldem)) %>%
  mutate(liberal=ifelse(country=="Tunisia", liberal,
                        ifelse(country=="Egypt", liberal-.04, liberal+.04)),
         partip=ifelse(country=="Tunisia", partip,
                        ifelse(country=="Egypt", partip-.02, partip+.02))) # jitter

score_ends

ggplot(reg, aes(year, v2x_libdem, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(0,1),
                     #breaks=0:16,
                     sec.axis=sec_axis(~.,
                                       breaks=score_ends$liberal,
                                       labels=score_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Index\nscore",
       title="Varieties of Democracy\nLiberal Democracy Index")


ggsave("viz/V-Dem/vd liberal Eg Tun Lib.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


  # participatory

ggplot(reg, aes(year, v2x_partipdem, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(0,1),
                     #breaks=0:16,
                     sec.axis=sec_axis(~.,
                                       breaks=score_ends$partip,
                                       labels=score_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Index\nscore",
       title="Varieties of Democracy\nParticipatory Democracy Index")


ggsave("viz/V-Dem/vd participatory Eg Tun Lib.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


# deliberative

ggplot(reg, aes(year, v2x_delibdem, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(0,1),
                     #breaks=0:16,
                     sec.axis=sec_axis(~.,
                                       breaks=score_ends$delib,
                                       labels=score_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Index\nscore",
       title="Varieties of Democracy\nDeliberative Democracy Index")


ggsave("viz/V-Dem/vd deliberative Eg Tun Lib.png",
       device="png",
       type="cairo",
       height=4,
       width=7)



# egalitarian

ggplot(reg, aes(year, v2x_egaldem, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(0,1),
                     #breaks=0:16,
                     sec.axis=sec_axis(~.,
                                       breaks=score_ends$egal,
                                       labels=score_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Index\nscore",
       title="Varieties of Democracy\nEgalitarian Democracy Index")


ggsave("viz/V-Dem/vd egalitarian Eg Tun Lib.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# nine countries-----

reg1 <- vd %>%
  select(country=vdem_country_name,
         year,
         v2x_polyarchy,
         v2x_libdem,
         v2x_partipdem,
         v2x_delibdem,
         v2x_egaldem) %>%
  filter(country=="Egypt" | 
           country=="Tunisia" |
           country=="Libya" |
           country=="Iraq" |
           country=="Syria" |
           country=="Jordan" |
           country=="Lebanon" |
           country=="Morocco" |
           country=="Yemen",
         year>2007)



### electoral
ggplot(reg1, aes(year, v2x_polyarchy, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  scale_color_viridis_d() + 
  facet_wrap(~country) +
  faceted +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(0,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Varieties of Democracy\nElectoral Democracy Index")


ggsave("viz/V-Dem/ vd electoral democracy facet.png",
       device="png",
       type="cairo",
       height=5,
       width=7)

### electoral line

reg_name <- reg1 %>%
  group_by(country) %>%
  summarise(value = last(v2x_polyarchy)) %>%
  mutate(color=viridis(9))

reg_name

ggplot(reg1, aes(year, v2x_polyarchy, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  scale_color_viridis_d() + 
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) +
  scale_y_continuous(limits=c(0,.8),
                     breaks=seq(0,.8,.2),
                     sec.axis=sec_axis(~.,
                                       breaks=reg_name$value,
                                       labels=reg_name$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=reg_name$color)) +
  labs(x="",
       y="",
       title="Varieties of Democracy\nElectoral Democracy Index")

ggsave("viz/V-Dem/vd electoral democracy.png",
       device="png",
       type="cairo",
       height=5,
       width=7)




### liberal
ggplot(reg1, aes(year, v2x_libdem, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  scale_color_viridis_d() + 
  facet_wrap(~country) +
  faceted +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(0,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Index\nscore",
       title="Varieties of Democracy\nLiberal Democracy Index")



### participatory

ggplot(reg1, aes(year, v2x_partipdem, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  scale_color_viridis_d() + 
  facet_wrap(~country) +
  faceted +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(0,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Index\nscore",
       title="Varieties of Democracy\nParticipatory Democracy Index")


### deliberative

ggplot(reg1, aes(year, v2x_delibdem, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  scale_color_viridis_d() + 
  facet_wrap(~country) +
  faceted +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(0,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Index\nscore",
       title="Varieties of Democracy\nDeliberative Democracy Index")


# egalitarian

ggplot(reg1, aes(year, v2x_egaldem, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  scale_color_viridis_d() + 
  facet_wrap(~country) +
  faceted +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(0,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Index\nscore",
       title="Varieties of Democracy\nEgalitarian Democracy Index")
