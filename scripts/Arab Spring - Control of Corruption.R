# Arab Spring
# Control of Corruption

cc <- read_excel("data/Control of Corruption/cleaned.xlsx", sheet = "data")

cc1 <- cc %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

cc_name <- cc1 %>%
  group_by(Country_Name) %>%
  summarise(value = mean(Estimate))
  
ggplot(cc1, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab Spring", "2012", "2014", "2016", "2018", "2020")) +
  scale_y_continuous(limits=c(-2.0,0.5),
                     breaks=seq(-2.0,1,0.5),
                     sec.axis=sec_axis(~.,
                                       breaks=cc_name$value,
                                       labels=cc_name$Country_Name)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Control of corruption",
       caption = "Control of corruption ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/Control of Corruption/Control of Corruption.png",
       device="png",
       type="cairo",
       height=4,
       width=7)
