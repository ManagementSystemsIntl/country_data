# Arab Spring
# Youth employment

ye <- read_xlsx("data/Youth employment/API_SL.UEM.1524.ZS_DS2_en_csv_v2_2253710.xlsx", sheet = "Cleaned_data")

ye1 <- ye %>%
  select(Country_name, `2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`) %>%
  pivot_longer(!Country_name, names_to = "Year", values_to = "Unemployment") %>%
  filter(Country_name == "Egypt"|
         Country_name == "Libya"|
         Country_name == "Tunisia") %>%
  mutate(Unemployment = round(Unemployment/100,2))

ye_name <- ye1 %>%
  group_by(Country_name) %>%
  summarise(value = last(Unemployment))

ggplot(ye1, aes(as.double(Year), Unemployment, color=Country_name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2019),
                     breaks=seq(2008,2018,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                     limits = c(0.2,0.5,0.1),
                     sec.axis=sec_axis(~.,
                                       breaks=ye_name$value,
                                       labels=ye_name$Country_name)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Youth Unemployment, Arab Spring countries")

# "2008", "2009", "Arab\nSpring", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")) +
  
ggsave("viz/Youth employment/Youth unemployment eg lib tun.png",
         device="png",
         type="cairo",
         height=4,
         width=7)

#nine countries----

ye2 <- ye %>%
  select(Country_name, `2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`) %>%
  pivot_longer(!Country_name, names_to = "Year", values_to = "Unemployment") %>%
  mutate(Unemployment = round(Unemployment/100,2))

ggplot(ye2, aes(as.double(Year), Unemployment, color=Country_name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  #geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  scale_color_viridis_d() +
  facet_wrap(~Country_name) +
  faceted +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018"))  +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2),
                     limits = c(0.1,0.5,0.1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Youth Unemployment, Arab Spring countries")

ggsave("viz/Youth employment/youth emp gisr.png",
       device="png",
       type="cairo",
       height=6,
       width=7)

