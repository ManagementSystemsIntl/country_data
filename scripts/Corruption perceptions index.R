# SSPA
# Corruption Perceptions Index (CPI)

dat <- read_excel("data/Corruption Perceptions/CPI2021_GlobalResults&Trends.xlsx",
                  sheet="export")

cpi <- dat %>%
  select(contains(c("CPI", "error")))

head(cpi)

cpi_scr <- dat %>%
  select(1:3, 
         contains("CPI"))

cpi_se <- dat %>%
  select(1:3, 
         contains("error"))

head(cpi_scr)

cpi_scr_lng <- cpi_scr %>%
  pivot_longer(4:13,
               names_to="year2",
               values_to="score") %>%
  mutate(year=as.numeric(str_sub(year2, 11, 14))) %>%
  select(-year2)


cpi_se_lng <- cpi_se %>%
  pivot_longer(4:13,
               names_to="year2",
               values_to="se") %>%
  mutate(year=as.numeric(str_sub(year2, 16, 19))) %>%
  select(-year2)

cpi <- cpi_scr_lng %>%
  left_join(cpi_se_lng) %>%
  mutate(lower=score-1.96*se,
         upper=score+1.96*se) %>%
  relocate(year, .after=Region)

describe(cpi$score)
str(cpi)

ug <- cpi %>%
  filter(Country=="Uganda") %>%
  mutate(x = case_when(year==2021 ~ 2021.2,
                       year==2012 ~ 2011.8,
                       TRUE ~ year))
    
    x=c(rep(2021.2,5), rep(2011.8,5)))

ug

ggplot(ug, aes(year, group=1)) +
  geom_hline(yintercept=c(11,32.63, 88), color="darkgoldenrod2", size=1.2, alpha=.4) +
  geom_point(aes(y=score), color="dodgerblue", size=3, alpha=.5) +
#  stat_smooth(aes(y=score)) +
  stat_smooth(aes(x=x, y=upper), linetype="dotdash", size=1, color="dodgerblue", se=F, span=.2) +
  stat_smooth(aes(x=x, y=lower), se=F, linetype="dotdash", size=1, color="dodgerblue", span=.2) +
#  geom_line(aes(x=x, y=lower), se=F, linetype="dotdash", size=1, color="dodgerblue") +
#  geom_line(aes(x=x, y=upper), se=F, linetype="dotdash", size=1, color="dodgerblue") +
  geom_label(aes(y=score, label=score), size=4, color="dodgerblue") + 
  geom_ribbon(aes(x=x, ymin=lower, ymax=upper), fill="lightgrey", alpha=.4) + 
  scale_x_continuous(limits=c(2011.8, 2021.2),
                     breaks=2012:2021) +
  scale_y_continuous(limits=c(0,91),
                     breaks=seq(0,90,10)) +
  annotate("text", x=2018, y=14, label="South Sudan 2021 score", color="darkgoldenrod") +
  annotate("text", x=2018, y=36, label="Sub-Saharan Africa regional average, 2021", color="darkgoldenrod") +
  annotate("text", x=2018, y=91, label="Denmark 2021 score", color="darkgoldenrod") + 
  labs(x="",
       y="")

ggsave("viz/Corruption Perceptions Index/Uganda cpi.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

















