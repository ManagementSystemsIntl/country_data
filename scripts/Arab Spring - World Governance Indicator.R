# Arab Spring
# World Governance Indicator


# Control of corruption ---------------------------------------------------

cc <- read_excel("data/World Governance Indicator/cleaned.xlsx", sheet = "ControlofCorruption")

cc1 <- cc %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)
,
         Country_Name == "Egypt"|
         Country_Name == "Tunisia"|
         Country_Name == "Libya")

cc_name <- cc1 %>%
  group_by(Country_Name) %>%
  summarise(value = last(Estimate)) %>%
  mutate(color=viridis(9))
  
ggplot(cc1, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  #geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) +
  scale_y_continuous(limits=c(-2.0,0.5),
                     breaks=seq(-2.0,1,0.5),
                     sec.axis=sec_axis(~.,
                                       breaks=cc_name$value,
                                       labels=cc_name$Country_Name)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=cc_name$color)) +
  labs(x="",
       y="",
       title="Control of corruption",
       caption = "Control of corruption ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Control of Corruption.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

## Control of corruption, GISR countries ----

cc2 <- cc %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

ggplot(cc2, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  #geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  # geom_point(size=3) + 
  scale_color_viridis_d() +
  facet_wrap(~Country_Name) +
  faceted +
  scale_x_continuous(limits=c(2008,2019),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(-2.0,1),
                     breaks=seq(-2.0,1,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Control of corruption",
       caption = "Control of corruption ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Control of Corruption gisr countries.png",
       device="png",
       type="cairo",
       height=6,
       width=7)

# Voice and Accountability ---- 

va <- read_excel("data/World Governance Indicator/cleaned.xlsx", sheet = "VoiceandAccountability")

va1 <- va %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007,
         Country_Name == "Egypt"|
           Country_Name == "Tunisia"|
           Country_Name == "Libya")

va_name <- va1 %>%
  group_by(Country_Name) %>%
  summarise(value1 = last(Estimate),
            value2 = nth(Estimate,11)) %>%
  mutate(value3 = c(-1.25, -1.55, .281),
         color=viridis(3))

va_name

ggplot(va1, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  #geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2019),
                     breaks=seq(2008,2018,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018")) +
  scale_y_continuous(limits=c(-2.0,0.5),
                     breaks=seq(-2.0,1,0.5),
                     sec.axis=sec_axis(~.,
                                       breaks=va_name$value3,
                                       labels=va_name$Country_Name)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=va_name$color)) +
  labs(x="",
       y="",
       title="Voice and Accountability",
       caption = "Voice and Accountability ranges from -2.5 (weak) to 2.5 (strong)")


ggsave("viz/World Governance Indicator/Voice and Accountability eg tun lib.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

## Voice and accountability, gisr countries ----

va2 <- va %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

ggplot(va2, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  #geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  # geom_point(size=3) + 
  scale_color_viridis_d() +
  facet_wrap(~Country_Name) +
  faceted +
  scale_x_continuous(limits=c(2008,2019),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(-2.0,1),
                     breaks=seq(-2.0,1,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Voice and Accountability",
       caption = "Voice and Accountability ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Voice and Accountability gisr countries.png",
       device="png",
       type="cairo",
       height=6,
       width=7)


# Political Stability ---- 

ps <- read_excel("data/World Governance Indicator/cleaned.xlsx", sheet = "PoliticalStabilityNoViolence")

ps1 <- ps %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

ps1

ps_name <- ps1 %>%
  group_by(Country_Name) %>%
  summarise(value = last(Estimate)) %>%
  mutate(color=viridis(9))

ggplot(ps1, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  #geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) +
  scale_y_continuous(limits=c(-3,1),
                     breaks=seq(-3,1,0.5),
                     sec.axis=sec_axis(~.,
                                       breaks=ps_name$value,
                                       labels=ps_name$Country_Name)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=ps_name$color)) +
  labs(x="",
       y="",
       title="Political Stability",
       caption = "Political Stability ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Political Stability.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


## Political Stability, GISR countries

ps2 <- ps %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

ggplot(ps2, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  #Egeom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  # geom_point(size=3) + 
  scale_color_viridis_d() +
  facet_wrap(~Country_Name) +
  faceted +
  scale_x_continuous(limits=c(2008,2019),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(-3,1),
                     breaks=seq(-3,1,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Political stability",
       caption = "Ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Political stability gisr countries.png",
       device="png",
       type="cairo",
       height=6,
       width=7)


# Government Effectiveness ----

ge <- read_excel("data/World Governance Indicator/cleaned.xlsx", sheet = "GovernmentEffectiveness")

ge1 <- ge %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

ge_name <- ge1 %>%
  group_by(Country_Name) %>%
  summarise(value = last(Estimate)) %>%
  mutate(color=viridis(9))

ggplot(ge1, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  #geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) +
  scale_y_continuous(limits=c(-2.6,0.5),
                     breaks=seq(-2.5,1,0.5),
                     sec.axis=sec_axis(~.,
                                       breaks=ge_name$value,
                                       labels=ge_name$Country_Name)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=ge_name$color)) +
  labs(x="",
       y="",
       title="Government Effectiveness",
       caption = "Government Effectiveness ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Government Effectiveness.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

## Government effectiveness, GISR countries

ge2 <- ge %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

ggplot(ge2, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  #Egeom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  # geom_point(size=3) + 
  scale_color_viridis_d() +
  facet_wrap(~Country_Name) +
  faceted +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(-3,1),
                     breaks=seq(-3,1,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Government effectiveness",
       caption = "Ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Government effectiveness gisr countries.png",
       device="png",
       type="cairo",
       height=6,
       width=7)



# Regulatory Quality ----

rq <- read_excel("data/World Governance Indicator/cleaned.xlsx", sheet = "RegulatoryQuality")

rq1 <- rq %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

rq_name <- rq1 %>%
  group_by(Country_Name) %>%
  summarise(value = last(Estimate)) %>%
  mutate(color=viridis(9))

ggplot(rq1, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  #geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) +
  scale_y_continuous(limits=c(-2.6,0.5),
                     breaks=seq(-2.5,1,0.5),
                     sec.axis=sec_axis(~.,
                                       breaks=rq_name$value,
                                       labels=rq_name$Country_Name)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=rq_name$color)) +
  labs(x="",
       y="",
       title="Regulatory Quality",
       caption = "Regulatory Quality ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Regulatory Quality.png",
       device="png",
       type="cairo",
       height=5,
       width=7)

## Regulatory quality, GISR countries

rq2 <- rq %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

ggplot(rq2, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  #Egeom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  # geom_point(size=3) + 
  scale_color_viridis_d() +
  facet_wrap(~Country_Name) +
  faceted +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(-3,1),
                     breaks=seq(-3,1,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Regulatory quality",
       caption = "Ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Regulatory quality gisr countries.png",
       device="png",
       type="cairo",
       height=6,
       width=7)


# Rule of Law ----

rl <- read_excel("data/World Governance Indicator/cleaned.xlsx", sheet = "RuleofLaw")

rl1 <- rl %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

rl_name <- rl1 %>%
  group_by(Country_Name) %>%
  summarise(value = last(Estimate)) %>%
  mutate(color=viridis(9))

ggplot(rl1, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  #geom_point(size=3) + 
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2019),
                     breaks=seq(2008,2018,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018")) +
  scale_y_continuous(limits=c(-2.6,0.5),
                     breaks=seq(-2.5,1,0.5),
                     sec.axis=sec_axis(~.,
                                       breaks=rl_name$value,
                                       labels=rl_name$Country_Name)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=rl_name$color)) +
  labs(x="",
       y="",
       title="Rule of Law",
       caption = "Rule of Law ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Rule of Law.png",
       device="png",
       type="cairo",
       height=5,
       width=7)

## Rule of law, GISR countries

rl2 <- rl %>%
  filter(Subindicator_Type == "Estimate") %>%
  pivot_longer(!Country_Name&!Indicator_Id&!Indicator&!Subindicator_Type, names_to = "Year", values_to = "Estimate") %>%
  select(1,5,6) %>%
  filter(Year > 2007)

ggplot(rl2, aes(as.double(Year), Estimate, color=Country_Name)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  #Egeom_hline(yintercept = 0, size=1.2, color="darkgrey", alpha=0.99) +
  geom_line(size=1) +
  # geom_point(size=3) + 
  scale_color_viridis_d() +
  facet_wrap(~Country_Name) +
  faceted +
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) +
  scale_y_continuous(limits=c(-3,1),
                     breaks=seq(-3,1,1)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Rule of law",
       caption = "Ranges from -2.5 (weak) to 2.5 (strong)")

ggsave("viz/World Governance Indicator/Rule of law gisr countries.png",
       device="png",
       type="cairo",
       height=6,
       width=7)


