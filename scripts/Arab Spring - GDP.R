# Arab Spring
# GDP

gdp <- read_xlsx("data/GDP/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_2252098.xlsx", sheet = "Cleaned_data")

gdp1 <- gdp %>%
  select(`Country Name`, `2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`) %>%
  # filter(`Country Name` == "Egypt") %>%
  pivot_longer(!`Country Name`, names_to = "Year", values_to = "GDP")

gdp_name <- gdp1 %>%
  group_by(`Country Name`) %>%
  summarise(value = last(GDP)) %>%
  mutate(value2 = c(303092255125, 6e10, 3e10))

gdp_name

# billion <- function (x) { number_format(accuracy = 1,
#                                    scale = 1/1000000000,
#                                    suffix = "B")(x) }
# ks <- function (x) { number_format(accuracy = 1,
#                                    scale = 1/1000,
#                                    suffix = "k",
#                                    big.mark = ",")(x) }
# ks(2322)
# billion(303092255125)

ggplot(gdp1, aes(as.double(Year), GDP, color=`Country Name`)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  #geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2008,2019,1),
                     labels=c("2008", "2009", "Arab\nSpring", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")) +
  scale_x_continuous(limits=c(2008,2019),
                     breaks=seq(2008,2018,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018")) +
  scale_y_continuous(labels = scales::dollar_format(suffix  = "B", scale = 1e-9),
                     # breaks = 10^9 * ylab,
                     sec.axis=sec_axis(~.,
                                       breaks=gdp_name$value2,
                                       labels=gdp_name$`Country Name`)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Egypt, Tunisia, Libya GDP")

ggsave("viz/GDP/GDP eg lib tun.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

labels=c("2008", "2009", "Arab\nSpring", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")) +
  
# Yemen, Morocco, Algeria, Iraq
gdp2 <- read_xlsx("data/GDP/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_2252098.xlsx", sheet = "Cleaned_data2")

gdp3 <- gdp2 %>%
  select(`Country Name`, `2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`) %>%
  filter(`Country Name` == "Iraq" | `Country Name` == "Algeria" | `Country Name` == "Morocco" | `Country Name` == "Yemen") %>%
  pivot_longer(!`Country Name`, names_to = "Year", values_to = "GDP")

gdp_name2 <- gdp3 %>%
  group_by(`Country Name`) %>%
  summarise(value = last(GDP))

ggplot(gdp3, aes(as.double(Year), GDP, color=`Country Name`)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  #geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  geom_point(size=3) + 
  scale_color_viridis_d() +
  # scale_x_continuous(limits=c(2008,2020),
  #                    breaks=seq(2008,2019,1),
  #                    labels=c("2008", "2009", "Arab\nSpring", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")) +
  scale_x_continuous(limits=c(2008,2019),
                     breaks=seq(2008,2018,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018")) +
  scale_y_continuous(labels = scales::dollar_format(suffix  = "B", scale = 1e-9),
                     sec.axis=sec_axis(~.,
                                       breaks=gdp_name2$value,
                                       labels=gdp_name2$`Country Name`)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="GDP, Arab Spring countries")

ggsave("viz/GDP/GDP iq alg mor yem.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# nine countries--------

gdp4 <- gdp2 %>%
  select(`Country Name`, `2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`) %>%
  filter(`Country Name` != "Algeria", `Country Name` != "Syria") %>%
  pivot_longer(!`Country Name`, names_to = "Year", values_to = "GDP")

gdp_nine_coun <- rbind(gdp1, gdp4)

ggplot(gdp_nine_coun, aes(as.double(Year), GDP, color=`Country Name`)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) +
  scale_color_viridis_d() +
  facet_wrap(~`Country Name`, scales = "free_y") +
  faceted +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1,suffix  = "B", scale = 1e-9)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="GDP, Arab Spring countries")


ggsave("viz/GDP/GDP gisr.png",
       device="png",
       type="cairo",
       height=5,
       width=7)


gdp_nine_coun

gdp_ends <- gdp_nine_coun %>%
  group_by(country=`Country Name`) %>%
  summarize(value = last(GDP)) %>%
  mutate(color=viridis(8))

gdp_ends


ggplot(gdp_nine_coun, aes(as.double(Year), GDP, color=`Country Name`)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) +
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2019),
                     breaks=seq(2008,2018,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018")) +
  #scale_y_continuous(labels = scales::dollar_format(accuracy = 1,suffix  = "B", scale = 1e-9)) +
  scale_y_log10(labels = scales::dollar_format(accuracy = 1,suffix  = "B", scale = 1e-9),
                sec.axis=sec_axis(~.,
                                  breaks=gdp_ends$value,
                                  labels=gdp_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=gdp_ends$color)) +
  labs(x="",
       y="",
       title="GDP, Arab Spring countries")


ggsave("viz/GDP/GDP gisr line.png",
       device="png",
       type="cairo",
       height=5,
       width=7)



