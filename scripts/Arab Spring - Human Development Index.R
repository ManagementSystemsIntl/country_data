# Arab Spring
# Human Development Index

hdi <- read_xlsx("data/Human Development Index/Human development trends.xlsx", sheet = "Data")

hdi1 <- hdi %>%
  pivot_longer(!country, names_to = "year", values_to = "HDI")

hdi_name <- hdi1 %>%
  group_by(country) %>%
  summarise(value = mean(HDI)) %>%
  mutate(value2 = c(.75, .68, .72))

hdi_name

ggplot(hdi1, aes(as.double(year), HDI, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(breaks=seq(2010,2018,2),
                     labels=c("Arab Spring", "2012", "2014", "2016", "2018")) +
  scale_y_continuous(limits=c(0.6,0.8),
                     breaks=seq(0.6,0.8,0.05),
                     sec.axis=sec_axis(~.,
                                       breaks=hdi_name$value,
                                       labels=hdi_name$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Human Development Index")

ggsave("viz/Human Development Index/Humean Development Index.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

hdi_whole <- read_xlsx("data/Human Development Index/Human development trends.xlsx", sheet = "Whole Data")

hdi2 <- hdi_whole %>%
  pivot_longer(!country, names_to = "year", values_to = "HDI") 

ggplot(hdi2, aes(as.double(year), HDI, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(1990,2020),
                     breaks=seq(1990,2020,10),
                     labels=c("1990","2000","Arab Spring", "2020")) +
  scale_y_continuous(limits=c(0.5,0.8),
                     breaks=seq(0.5,0.8,0.05),
                     sec.axis=sec_axis(~.,
                                       breaks=hdi_name$value,
                                       labels=hdi_name$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Human Development Index")

ggsave("viz/Human Development Index/Humean Development Index with year before 2010.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

hdi3 <- hdi2 %>%
  filter(year>1990)

ggplot(hdi3, aes(as.double(year), HDI, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(2000,2020),
                     breaks=seq(2000,2020,5),
                     labels=c("2000","2005", "Arab\nSpring", "2015", "2020")) +
  scale_y_continuous(limits=c(0.6,0.8),
                     breaks=seq(0.6,0.8,0.05),
                     sec.axis=sec_axis(~.,
                                       breaks=hdi_name$value2,
                                       labels=hdi_name$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Human Development Index",
       caption = "2000, 2010, 2013, 2015, 2016, 2017, 2018")

ggsave("viz/Human Development Index/Human Development Index 2000-2018.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


