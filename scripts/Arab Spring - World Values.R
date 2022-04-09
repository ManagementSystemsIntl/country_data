
pew <- read_spss("../../World's Muslims/2012-Pew-Religion-Worlds-Muslims_dataset.sav")

names(pew)
frq(pew$COUNTRY)


wv <- read_rds("../../World Values Survey/1981-2020/WVS_TimeSeries_R_v1_6.rds")


wv <- read_dta("../../World Values Survey/1981-2020/WVS_TimeSeries_stata_v1_6.dta")


wv <- read_dta("data/World Values/1981-2020/WVS_TimeSeries_stata_v1_6.dta")

wvNames <- data.frame(names(wv))

frq(wv$S003) # country
frq(wv$X051) # ethnicity
frq(wv$A008) # happy
frq(wv$A013) # lonely
frq(wv$A168A)
frq(wv$F178) # West pos attributes
frq(wv$E274)

# covariates
frq(wv$X047)
describe(wv$X047)


wvdat <- wv  %>%
  select(wave = S002,
         country=S003,
         year=S020,
         ethnicity=X051,
         religion=F025,
         age=X003,
         educ=X025,
         educ2=X025CSWVS,
         literate=X025LIT,
         employed=X028,
         chief_earner_emplyd=X041,
         savings=X044,
         social_class_subj=X045,
         #hhsize=X047CS,
         income_ladder=X047,
         town_size=X049,
         habitat=X050,
         settlement_type=X050B,
         locality=X050C,
         A008,
         A013,
         take_advntg=A168A,
         E274, # bribe
         West_pos = F178,
         conf_NatGov = E069_11,
         free_choice = A173,
         bright_future = B017,
         interest_politics = E023,
         sign_petition=E025,
         boycott = E026,
         demonstrate=E027,
         strike=E028,
         occupy=E029,
         neighbor_diff_religion=A124_12,
         neighbor_same_religion=A124_13,
         imprtnt_veil = D067,
         imprtnt_educ = D071,
         imprtnt_indpndnt = D072,
         imprtnt_work = D073) %>%
  filter(country==818 | 
           country==887 |
           country==788 | # tunisia
           country==434 | 
           country==792 | # turkey
           country==682 | # saudi arabia
           country==504 | # morocco
           country==400 | # jordan
           country==368 | # iraq
           country==364 | # iran
           country==360 | # indonesia
           country==12 )  # algeria

frq(wvdat$ethnicity)
frq(wvdat$take_advntg)
frq(wvdat$West_pos)
frq(wvdat$E274)
frq(wvdat$conf_NatGov)
frq(wvdat$free_choice)
frq(wvdat$bright_future)
frq(wvdat$interest_politics)
frq(wvdat$sign_petition)
frq(wvdat$boycott)
frq(wvdat$neighbor_diff_religion)
frq(wvdat$neighbor_same_religion)
frq(wvdat$religion)
frq(wvdat$imprtnt_veil)
frq(wvdat$imprtnt_educ)
frq(wvdat$imprtnt_indpndnt)
frq(wvdat$imprtnt_work)

wvdat <- wvdat %>%
  mutate(serial = seq(1, nrow(.), 1),
         year=drop_labels(year),
         country=drop_labels(country),
         country_name = as_character(country),
         treat = ifelse(year>2010, 1,0),
         ethnicity=drop_labels(ethnicity),
         religion=drop_labels(religion),
         inc_grp = case_when(income_ladder < 4 ~ 1,
                             income_ladder > 3 & income_ladder < 7 ~ 2,
                             income_ladder > 6 ~ 3,
                             TRUE ~ NA_real_),
         happy = ifelse(A008<3, 1,0),
         fairness = ifelse(take_advntg>5, 1,0),
         conf_NatGov_bin = ifelse(conf_NatGov<3, 1,0),
         free_choice_bin = ifelse(free_choice>5, 1,0),
         interest_politics_bin = case_when(interest_politics == 1 ~ 1,
                                       interest_politics == 2 ~ 1,
                                       interest_politics >2 ~ 0,
                                       TRUE ~ NA_real_),
         sign_petition_bin = case_when(sign_petition == 1 ~ 1,
                                       sign_petition == 2 ~ 1,
                                       sign_petition == 3 ~ 0,
                                       TRUE ~ NA_real_),
         boycott_bin = case_when(boycott == 1 ~ 1,
                                       boycott == 2 ~ 1,
                                       boycott == 3 ~ 0,
                                       TRUE ~ NA_real_),
         demonstrate_bin = case_when(demonstrate == 1 ~ 1,
                                 demonstrate == 2 ~ 1,
                                 demonstrate == 3 ~ 0,
                                 TRUE ~ NA_real_),
         strike_bin = case_when(strike == 1 ~ 1,
                                 strike == 2 ~ 1,
                                 strike == 3 ~ 0,
                                 TRUE ~ NA_real_),
         occupy_bin = case_when(occupy == 1 ~ 1,
                                 occupy == 2 ~ 1,
                                 occupy == 3 ~ 0,
                                 TRUE ~ NA_real_),
         imprtnt_educ_rec = 6 - imprtnt_educ,
         imprtnt_indpndnt_rec = 6 - imprtnt_indpndnt,
         imprtnt_work_rec = 6 - imprtnt_work,
         imprtnt_veil_rec = drop_labels(imprtnt_veil))

frq(wvdat$happy)
frq(wvdat$conf_NatGov_bin)
frq(wvdat$free_choice_bin)
frq(wvdat$interest_politics_bin)
frq(wvdat$sign_petition_bin)
frq(wvdat$boycott_bin)
frq(wvdat$demonstrate_bin)
frq(wvdat$strike_bin)
frq(wvdat$occupy_bin)
frq(wvdat$imprtnt_veil)
frq(wvdat$imprtnt_veil_rec)
frq(wvdat$imprtnt_educ_rec)

names(wvdat)

act <- wvdat %>%
  select(sign_petition_bin, 
         boycott_bin,
         demonstrate_bin)

head(act)

?pca

fa.parallel(act, cor="tet")

act_pca <- pca(act, nfactors=1)
act_pca

wvdat <- wvdat %>%
  mutate(politically_active = act_pca$scores,
         politically_active_bin = ifelse(politically_active>0,1,0))

describe(wvdat$politically_active)
frq(wvdat$politically_active_bin)

ggplot(wvdat, aes(politically_active)) + 
  geom_density()

frq(wvdat$wave)
frq(wvdat$happy)
frq(wvdat$year)
frq(wvdat$take_advntg)

ggplot(wvdat, aes(year, happy, group=year)) + 
  geom_point() + 
  geom_line()


wom <- wvdat %>%
  select(serial, imprtnt_educ_rec:imprtnt_work_rec) %>%
  na.omit()

head(wom)

fa.parallel(wom[,2:4], cor="poly")

wom_fac <- fa(wom[,2:4], 
              cor="poly",
              fm="ml",
              scores="tenBerge")
wom_fac

wom <- wom %>%
  mutate(WomEmp = wom_fac$scores,
         WomEmp_bin = ifelse(WomEmp>0,1,0),
         WomEmp_bin2 = ifelse(WomEmp>1, 1,0))

ggplot(wom, aes(WomEmp)) + 
  geom_density()

describe(wom)
head(wom)

wvdat <- wvdat %>%
  left_join(wom[,c(1,5:7)])


write_rds(wvdat, "data/World Values/1981-2020/wv prepared.rds")
write_csv(wvdat, "data/World Values/1981-2020/wv prepared.csv")


wvyr <- wvdat %>%
  group_by(year) %>%
  summarize(happy=mean(happy, na.rm=T),
            take_advntg=mean(take_advntg, na.rm=T),
            fair_se = std.error(fairness),
            fairness=mean(fairness, na.rm=T),
            conf_NatGov_bin = mean(conf_NatGov_bin, na.rm=T),
            free_choice_bin = mean(free_choice_bin, na.rm=T)) 

wvyr

ggplot(wvyr, aes(year, happy)) + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_line(color="dodgerblue2", size=1) + 
  stat_smooth(method="lm", se=F)


wvYrEth <- wvdat %>%
  group_by(year, ethnicity) %>%
  summarize(happy=mean(happy, na.rm=T),
            take_advntg=mean(take_advntg, na.rm=T)) %>%
  filter(!is.na(ethnicity))

wvYrEth



ggplot(wvyr, aes(year, happy)) + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_line(color="dodgerblue2", size=1) + 
  stat_smooth(method="lm", se=F) +
  facet_grid(~ethnicity)
  #stat_smooth()

warnings()

?stat_smooth

# take advantage

ggplot(wvyr, aes(year, take_advntg)) + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_line(color="dodgerblue2", size=1) + 
  stat_smooth(method="lm", se=F) + 
  geom_vline(xintercept=2010, size=1, color="darkgrey", alpha=.6) +
  scale_x_continuous(limits=c(2007.5, 2013.5),
                     breaks=c(2008, 2013)) +
  scale_y_continuous(limits=c(2,8))



# fairness ----

ggplot(wvyr, aes(year, fairness)) +
  geom_vline(xintercept=2010, size=1.1, color="darkgoldenrod", alpha=.6) +
  geom_label_repel(aes(label=paste(round(fairness*100,0), "%")), color="dodgerblue2", box.padding=.35) + 
  geom_errorbar(aes(ymin=fairness - 2.2*fair_se, ymax=fairness + 2.2*fair_se), color="dodgerblue2", size=1) +
  geom_point(color="dodgerblue2", size=3.2) + 
  geom_line(color="dodgerblue2", size=1.1) + 
  stat_smooth(method="lm", se=F) + 
  scale_x_continuous(limits=c(2007.5, 2013.5),
                     breaks=c(2008, 2010, 2013),
                     labels=c("2008", "Arab Spring", "2013")) +
  scale_y_continuous(limits=c(.25,.75),
                     labels=percent_format(accuracy=1),
                     sec.axis = dup_axis()) +
  labs(x="", 
       y="",
       #y="Proportion reporting\nthat people are fair",
       title="Belief that people treat each other fairly") + 
  theme(axis.title.y=element_text(angle=0, vjust=.5)) +
  facet_wrap(~Cou)


ggsave("Egypt fairness.png",
       type="cairo",
       device="png",
       height=4,
       width=4.7)




# confidence --------------------------------------------------------------

GovYr <- wvdat %>%
  group_by(year) %>%
  summarize(conf_NatGov=mean(conf_NatGov_bin, na.rm=T))

GovYr



# free choice -------------------------------------------------------------

FreeCntryYr <- wvdat %>%
  filter(country_name=="Egypt" |
           country_name=="Iraq" |
           country_name=="Jordan" |
           country_name=="Turkey") %>%
  group_by(country_name, year, treat) %>%
  summarize(free_bin_se = std.error(free_choice_bin, na.rm=T),
            free_choice_bin=mean(free_choice_bin, na.rm=T)) %>%
  mutate(free_bin_lower = free_choice_bin - 1.96*free_bin_se,
         free_bin_upper = free_choice_bin + 1.96*free_bin_se,
         n=n(),
         year=drop_labels(year)) %>%
  na.omit()

frq(FreeCntryYr$year)

FreeCntryYr

ggplot(FreeCntryYr, aes(x=year, y=free_choice_bin, color=as.factor(treat))) +
  stat_smooth(method="lm", se=F) +
  geom_point() + 
  facet_wrap(~country_name) +
  faceted +
  scale_color_manual(values=c("dodgerblue", "firebrick2"))
  
  geom_vline(xintercept=2010, size=1.2, color="darkgoldenrod3", alpha=.4) +
  #geom_abline(intercept=)
  geom_segment(aes(x=2008, xend=2019, y=.562, yend=.622), 
               linetype="dashed", color="firebrick3", alpha=.4, size=2) +
  geom_line(size=1, color="dodgerblue2") + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_label(aes(label=paste(round(free_choice_bin, 3)*100, "%", sep="")), size=4, color="dodgerblue2") +
  scale_x_continuous(limits=c(2000,2020),
                     breaks=seq(2000,2020,2), labels=c("2000", "2002","2004", "2006","2008","Arab\nSpring","2012", "2014","2016", "2018","2020")) +
  scale_y_continuous(limits=c(.5,.7),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Perceived freedom of choice and control over one's life",
       caption="How much freedom of choice and control do you\nfeel you have over the way your life turns out?\n\nWorld Values Survey, Egypt\n2003, 2008, 2013, 2018")

ggsave("viz/World Values/wv Egypt perceived freedom of choice and control 2.png",
       device="png",
       type="cairo",
       height=4,
       width=7)  
  
  annotate(text, label="Arab\nSpring", x=2010, y=0)

3.8/7
.038/7 # .00543
.562+(10*.00543) # .616
.562+(11*.00543) # .622


FreeCntryYr <- wvdat %>%
  group_by(year, country_name) %>%
  summarize(free_bin_se = std.error(free_choice_bin, na.rm=T),
            free_choice_bin=mean(free_choice_bin, na.rm=T)) %>%
  mutate(free_bin_lower = free_choice_bin - 1.96*free_bin_se,
         free_bin_upper = free_choice_bin + 1.96*free_bin_se,
         n=n()) %>%
  arrange(country_name) %>%
  filter(country_name=="Algeria" | 
           country_name=="Egypt" | 
           country_name=="Indonesia" |
           country_name=="Iran" |
           country_name=="Iraq" |
           country_name=="Jordan" |
           country_name=="Morocco" |
           country_name=="Turkey") %>%
  na.omit()

FreeCntryYr


ggplot(FreeCntryYr, aes(x=year, y=free_choice_bin)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgoldenrod3", alpha=.4) +
  #geom_abline(intercept=)
  #geom_segment(aes(x=2008, xend=2019, y=.562, yend=.622), 
  #             linetype="dashed", color="firebrick3", alpha=.4, size=2) +
  geom_line(size=1, color="dodgerblue2") + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_label(aes(label=paste(round(free_choice_bin, 3)*100, "%", sep="")), size=4, color="dodgerblue2") +
  scale_x_continuous(limits=c(2000,2020),
                     breaks=seq(2002,2018,4), labels=c("2002","2006","Arab\nSpring","2014","2018")) +
  scale_y_continuous(limits=c(.2,.9),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Perceived freedom of choice and control over one's life") +
#,
#       caption="How much freedom of choice and control do you\nfeel you have over the way your life turns out?\n\nWorld Values Survey, Egypt\n2003, 2008, 2013, 2018") +
  facet_wrap(~country_name) +
  facet_style()

ggsave("viz/World Values/wv Egypt perceived freedom of choice and control 2.png",
       device="png",
       type="cairo",
       height=4,
       width=7)  




frq(wvdat$free_choice_bin)
frq(wvdat$educ)
frq(wvdat$literate)
frq(wvdat$income_ladder)
frq(wvdat$inc_grp)
describe(wvdat$age)

names(wvdat)

out <- wvdat %>%
  group_by(year, treat) %>%
  summarize(n=n())

out

l1 <- lm(free_choice_bin ~ treat,
         data=wvdat)

summary(l1)


l2 <- lm(free_choice_bin ~ treat + as.factor(year),
         data=wvdat)

summary(l2)

?predict

test <- data.frame(year=2001:2020,
                   treat=rep(0:1, each=10))

test

a <- predict(l2, newdata=test)

data.frame(year=rep(2000:2018, 2),
                                    treat=rep(0:1, 19)))

a
describe(a)

test <- test %>%
  mutate(predicted=a)

l3 <- lm(free_choice_bin ~ treat + age + income_ladder,
         data=wvdat)

summary(l3)


l4 <- lm(free_choice_bin ~ treat + as.factor(year) + age + income_ladder + employed,
         data=wvdat)

summary(l4)


l5 <- lm(free_choice_bin ~ treat*as.factor(income_ladder) + as.factor(year) + age + employed,
         data=wvdat)

summary(l5)

frq(wvdat$income_ladder)
describe(wvdat$savings)
frq(wvdat$savings)

lme1 <- lmer(free_choice_bin ~ treat + income_ladder + as.factor(year) + age + employed,
           data=wvdat)

summary(lme1)
ranef(lme1)

lme2 <- lmer(free_choice_bin ~ treat + (treat|inc_grp) + as.factor(year) + age + employed,
             data=wvdat)

summary(lme2)
ranef(lme2)

?stan_glm


sg1 <- stan_glmer(free_choice_bin ~ treat + (treat|inc_grp) + as.factor(year) + age + employed,
             data=wvdat)

summary(lme2)
ranef(lme2)

?brm

b1 <- brm(free_choice_bin ~ treat + (treat|inc_grp) + as.factor(year) + age + employed,
                  data=wvdat)



sg1 <- stan_glmer(free_choice_bin ~ treat + (treat|inc_grp) + as.factor(year) + age + employed,
                  data=wvdat)

summary(lme2)
ranef(lme2)

frq(wvdat$ethnicity)



?quantreg

library(quantreg)
?rq

q1 <- rq(free_choice_bin ~ income_ladder,
         tau=c(.25,.5,.75,1),
         data=wvdat)

summary(q1)


# bright future ---- 

FreeYr <- wvdat %>%
  group_by(year) %>%
  summarize(free_bin_se = std.error(free_choice_bin, na.rm=T),
            free_choice_bin=mean(free_choice_bin, na.rm=T)) %>%
  mutate(free_bin_lower = free_choice_bin - 1.96*free_bin_se,
         free_bin_upper = free_choice_bin + 1.96*free_bin_se)

FreeYr

ggplot(FreeYr, aes(x=year, y=free_choice_bin)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgoldenrod3", alpha=.4) +
  #geom_abline(intercept=)
  geom_segment(aes(x=2008, xend=2018, y=.562, yend=.616), 
               linetype="dotdash", color="darkgrey", alpha=.4, size=1) +
  geom_line(size=1.2, color="dodgerblue2") + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_label(aes(label=paste(round(free_choice_bin, 3)*100, "%", sep="")), size=4, color="dodgerblue2") +
  scale_x_continuous(limits=c(2000,2018),
                     breaks=seq(2000,2018,2), labels=c("2000", "2002","2004", "2006","2008","Arab\nSpring","2012", "2014","2016", "2018")) +
  scale_y_continuous(limits=c(.5,.7),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Perceived freedom of choice and control over one's life",
       caption="How much freedom of choice and control do you\nfeel you have over the way your life turns out?\n\nWorld Values Survey, Egypt\n2003, 2008, 2013, 2018")

ggsave("viz/World Values/wv Egypt perceived freedom of choice and control.png",
       device="png",
       type="cairo",
       height=4,
       width=7)  

names(wvdat)
frq(wvdat$free_choice)

l1 <- lm(free_choice ~ treat,
   wvdat)

summary(l1)

l2 <- lm(free_choice_bin ~ treat,
         wvdat)

summary(l2)

l3 <- lm(free_choice_bin ~ treat + year,
         wvdat)

summary(l3)

l4 <- lm(free_choice_bin ~ treat + year + treat:year,
         wvdat)

summary(l4)


l5 <- lm(free_choice_bin ~ treat + year + age + educ + income_ladder,
         wvdat)

summary(l5)


l6 <- lm(free_choice_bin ~ treat + year + age + educ + income_ladder + treat*income_ladder,
         wvdat)

summary(l6)


frq(wvdat$locality)
frq(wvdat$treat)
table(wvdat$treat, wvdat$year)


# sign petition ----

#boycott demonstrate strike occupy

sign_pet <- wvdat %>%
  group_by(year) %>%
  summarize(#sign_petition_se = std.error(sign_petition_bin, na.rm=T),
            #interest_politics = mean(interest_politics_bin, na.rm=T),
            sign_petition=mean(sign_petition_bin, na.rm=T),
            boycott=mean(boycott_bin, na.rm=T),
            demonstrate=mean(demonstrate_bin, na.rm=T)) %>%
  as.data.frame() %>%
  pivot_longer(.,
               cols=2:4,
               names_to="type",
               values_to="value")

sign_pet
str(sign_pet)

ggplot(sign_pet, aes(x=year, y=value, color=type, group=type)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgoldenrod3", alpha=.4) +
  #geom_abline(intercept=)
  #geom_segment(aes(x=2008, xend=2019, y=.562, yend=.622), 
  #             linetype="dashed", color="firebrick3", alpha=.4, size=2) +
  geom_line(size=1) + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_label(aes(label=paste(round(value, 3)*100, "%", sep="")), size=4) +
  scale_x_continuous(limits=c(2000,2020),
                     breaks=seq(2000,2020,2), labels=c("2000", "2002","2004", "2006","2008","Arab\nSpring","2012", "2014","2016", "2018","2020")) +
  scale_y_continuous(limits=c(0,.7),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Perceived freedom of choice and control over one's life",
       caption="How much freedom of choice and control do you\nfeel you have over the way your life turns out?\n\nWorld Values Survey, Egypt\n2003, 2008, 2013, 2018") +
  scale_color_viridis_d()

ggsave("viz/World Values/wv Egypt perceived freedom of choice and control 2.png",
       device="png",
       type="cairo",
       height=4,
       width=7) 


# politically active ----


politically_active <- wvdat %>%
  group_by(year) %>%
  summarize(politically_active_se = std.error(politically_active, na.rm=T),
            politically_active=mean(politically_active, na.rm=T),
            politically_active_bin = mean(politically_active_bin, na.rm=T),
            interest_politics = mean(interest_politics_bin, na.rm=T)) 


politically_active

ggplot(politically_active, aes(x=year, y=politically_active_bin)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgoldenrod3", alpha=.4) +
  #geom_segment(aes(x=2008, xend=2019, y=.562, yend=.622), 
  #             linetype="dotdash", color="darkgrey", alpha=.4, size=1) +
  geom_line(size=1, color="dodgerblue2") + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_label(aes(label=paste(round(politically_active_bin, 3)*100, "%", sep="")), size=4, color="dodgerblue2") +
  scale_x_continuous(limits=c(2000,2018),
                     breaks=seq(2000,2018,2), labels=c("2000", "2002","2004", "2006","2008","Arab\nSpring","2012", "2014","2016", "2018")) +
  scale_y_continuous(limits=c(0,.8),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Willingness to engage in political activism",
       caption="Political acts: sign petition, demonstrate, boycott\n\nWorld Values Survey, Egypt\n2003, 2008, 2013, 2018")

ggsave("viz/World Values/wv Egypt political activism.png",
       device="png",
       type="cairo",
       height=4,
       width=7) 


ggplot(politically_active, aes(x=year, y=interest_politics)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgoldenrod3", alpha=.4) +
  geom_segment(aes(x=2008, xend=2018, y=.344, yend=.224), 
               color="darkgrey", alpha=.4, size=1, linetype="dotdash") +
  geom_line(size=1.2, color="dodgerblue2") + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_label(aes(label=paste(round(interest_politics, 3)*100, "%", sep="")), size=4, color="dodgerblue2") +
  scale_x_continuous(limits=c(2000,2018),
                     breaks=seq(2000,2018,2), labels=c("2000", "2002","2004", "2006","2008","Arab\nSpring","2012", "2014","2016", "2018")) +
  scale_y_continuous(limits=c(0,.8),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Interest in politics",
       caption="World Values Survey, Egypt\n2003, 2008, 2013, 2018")

ggsave("viz/World Values/wv Egypt interest in politics.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


l1 <- lm()


# women's empowerment ----

EmpCntryYr <- wvdat %>%
  filter(!is.na(WomEmp)) %>%
  group_by(year, country_name) %>%
  summarize(WomEmp_bin_se = std.error(WomEmp_bin, na.rm=T),
            WomEmp_bin=mean(WomEmp_bin, na.rm=T)) %>%
  #mutate(free_bin_lower = free_choice_bin - 1.96*free_bin_se,
  #       free_bin_upper = free_choice_bin + 1.96*free_bin_se,
  #       n=n()) %>%
  arrange(country_name) 
%>%
  filter(country_name=="Algeria" | 
           country_name=="Egypt" | 
           country_name=="Indonesia" |
           country_name=="Iran" |
           country_name=="Iraq" |
           country_name=="Jordan" |
           country_name=="Morocco" |
           country_name=="Turkey") %>%
  na.omit()

FreeCntryYr


ggplot(FreeCntryYr, aes(x=year, y=free_choice_bin)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgoldenrod3", alpha=.4) +
  #geom_abline(intercept=)
  #geom_segment(aes(x=2008, xend=2019, y=.562, yend=.622), 
  #             linetype="dashed", color="firebrick3", alpha=.4, size=2) +
  geom_line(size=1, color="dodgerblue2") + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_label(aes(label=paste(round(free_choice_bin, 3)*100, "%", sep="")), size=4, color="dodgerblue2") +
  scale_x_continuous(limits=c(2000,2020),
                     breaks=seq(2002,2018,4), labels=c("2002","2006","Arab\nSpring","2014","2018")) +
  scale_y_continuous(limits=c(.2,.9),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Perceived freedom of choice and control over one's life") +
  #,
  #       caption="How much freedom of choice and control do you\nfeel you have over the way your life turns out?\n\nWorld Values Survey, Egypt\n2003, 2008, 2013, 2018") +
  facet_wrap(~country_name) +
  facet_style()

ggsave("viz/World Values/wv Egypt perceived freedom of choice and control 2.png",
       device="png",
       type="cairo",
       height=4,
       width=7)  



# age heaping -------------------------------------------------------------

source("https://data.princeton.edu/eco572/R/myers.txt")

names(wvdat)

pop <- data.frame(frq(wvdat$age)) %>% 
  mutate(pop=frq,
         percent=frq/sum(frq),
         age=val) %>%
  filter(age>0) %>%
  select(age, pop, percent)

pop

myers(pop, age, pop, c(20,99))

# Egypt
popYr <- wvdat %>%
  group_by(year) %>%
  count(age) %>%
  na.omit() %>%
  mutate(pop=n,
         percent=n/sum(n)) %>%
  as.data.frame()

head(popYr)
str(popYr)

myers(popYr, age, pop, c(20,99))

lapply(popYr, function(x) myers(x, age, pop, c(20,99)))

pop2001 <- popYr %>%
  filter(year==2001) 

myers(pop2001, age, pop, c(20,79))

pop2008 <- popYr %>%
  filter(year==2008) 

myers(pop2008, age, pop, c(20,79))

pop2013 <- popYr %>%
  filter(year==2013) 

myers(pop2013, age, pop, c(20,79))

pop2018 <- popYr %>%
  filter(year==2018) 

myers(pop2018, age, pop, c(20,79))

myers_wv_egypt <- data.frame(year=c(2001, 2008, 2013, 2018),
                             myers=c(5.78,17.5,20,15.9),
                             country="Egypt",
                             source="World Values")

myers_wv_egypt

ggplot(myers_wv_egypt, aes(year)) +
  geom_point(aes(y = myers), size = 3) +
  geom_line(aes(y = myers), size = 1) +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(2001,2020),
                     breaks=seq(2001,2020,6)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Myers Index for Egypt")

# Libya

wv_libya <- wv  %>%
  select(wave = S002,
         country=S003,
         year=S020,
         ethnicity=X051,
         age=X003,
         educ=X025,
         literate=X025LIT,
         chief_earner_emplyd=X041,
         savings=X044,
         social_class_subj=X045,
         #hhsize=X047CS,
         income_ladder=X047,
         town_size=X049,
         habitat=X050,
         settlement_type=X050B,
         locality=X050C,
         A008,
         A013,
         take_advntg=A168A,
         E274, # bribe
         West_pos = F178,
         conf_NatGov = E069_11,
         free_choice = A173) %>%
  filter(country==434) 
  
pop_lib <- wv_libya %>%
  group_by(year) %>%
  count(age) %>%
  na.omit() %>%
  mutate(pop=n,
         percent=n/sum(n)) %>%
  as.data.frame()

myers(pop_lib, age, pop, c(18,77))

myers_wv_libya <- data.frame(year=2014,
           myers= 4.57,
           country="Egypt",
           source="World Values")

ggplot(myers_wv_libya, aes(year)) +
  geom_point(aes(y = myers), size = 3) +
  geom_line(aes(y = myers), size = 1) +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(2010,2018),
                     breaks=seq(2010,2018,4)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Myers Index for Libya",
       caption = "Sorce from World Values")

# Tunisia
wv_tunisia <- wv  %>%
  select(wave = S002,
         country=S003,
         year=S020,
         ethnicity=X051,
         age=X003,
         educ=X025,
         literate=X025LIT,
         chief_earner_emplyd=X041,
         savings=X044,
         social_class_subj=X045,
         #hhsize=X047CS,
         income_ladder=X047,
         town_size=X049,
         habitat=X050,
         settlement_type=X050B,
         locality=X050C,
         A008,
         A013,
         take_advntg=A168A,
         E274, # bribe
         West_pos = F178,
         conf_NatGov = E069_11,
         free_choice = A173) %>%
  filter(country==788) 

pop_tuni <- wv_tunisia %>%
  group_by(year) %>%
  count(age) %>%
  na.omit() %>%
  mutate(pop=n,
         percent=n/sum(n)) %>%
  as.data.frame()

year2013 <- pop_tuni %>%
  filter(year == 2013)

myers(year2013, age, pop, c(18,87))

year2019 <- pop_tuni %>%
  filter(year == 2019)

myers(year2019, age, pop, c(18,87))

myers_wv_tunisia <- data.frame(year=c(2013, 2019),
                             myers=c(6.83, 7.34),
                             country="Tunisia",
                             source="World Values")

myers_wv_tunisia

ggplot(myers_wv_tunisia, aes(year)) +
  geom_point(aes(y = myers), size = 3) +
  geom_line(aes(y = myers), size = 1) +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(2013,2019),
                     breaks=seq(2013,2019,6)) +
  scale_y_continuous(limits = c(6.8,7.4)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Myers Index for Tunisia",
       caption = "Sorce from World Values")


yrOut <- split(popYr, year)

yrOut <- popYr %>%
  group_split(year)

myers(data=yrOut, age, pop, c(20,99))

?split  

summarise(freq=tally())

?tally

<- data.frame(frq(wvdat$age)) %>% 
  mutate(pop=frq,
         percent=frq/sum(frq),
         age=val) %>%
  filter(age>0) %>%
  select(age, pop, percent)

pop

myers(pop, age, pop, c(20,99))



?range
x <- 20:99
range(x)

wvdat <- wvdat %>%
  mutate(country_nm = drop_labels(country),
         ethnic = drop_labels(ethnicity),
         last.digit=age%%10)


frq(wvdat$country_nm)
frq(wvdat$ethnicity)       
frq(wvdat$ethnic)
frq(wvdat$last.digit)


> ph <- mutate(ph, last.digit = age %% 10)

> total <- sum(ph$pop)

out <- wvdat %>%
  group_by(last.digit) %>% 
  summarize(freq = sum(pop), percent = 100*freq/total)
Source: local data frame [10 x 3]

out

frq(ph$age)
ph <- read.table("https://data.princeton.edu/eco572/datasets/phpop1990.dat") %>%
  mutate(percent=V2/sum(V2)) %>%
  rename(age=V1,
         pop=V2)

head(ph)  

myers(ph, age, pop, c(0,99))


# regression, egypt ---- 

library(rddtools)

names(wvdat)
frq(wvdat$country_name)

eg <- wvdat %>%
  filter(country_name=="Egypt",
         !is.na(year)) %>%
  mutate(year = drop_labels(year),
         free_choice_sc = scale(free_choice))

names(eg)  

# check covariates
frq(eg$social_class_subj)
frq(eg$income_ladder)
frq(eg$locality)

frq(eg$free_choice_bin)
frq(eg$year)
describe(eg$free_choice_sc)

ggplot(eg, aes(x=free_choice_sc)) + 
  geom_density()


l1 <- lm(free_choice_bin ~ treat + year,
         data=eg)

summary(l1)

?rdd_reg_lm

eg_rdd <- rdd_data(y=free_choice_bin,
                   x=year,
                   cutpoint=2010,
                   data=eg)

rd1 <- rdd_reg_lm(rdd_object = eg_rdd,
                  slope="same")

summary(rd1)

rd2 <- rdd_reg_lm(rdd_object = eg_rdd,
                  slope="separate")

summary(rd2)




frq(eg$treat)
frq(eg$year)

?geom_jitter

ggplot(eg, aes(x=year, y=free_choice, color=as.factor(treat))) +
  geom_jitter(width=.3, alpha=.4, size=.4) +
  stat_smooth(method="lm") + 
  scale_color_manual(values=c("dodgerblue","firebrick2"))


ggplot(eg, aes(x=year, y=free_choice_bin, color=as.factor(treat))) +
  geom_jitter(width=.3, alpha=.4, size=.4) +
  stat_smooth(method="lm") + 
  scale_color_manual(values=c("dodgerblue","firebrick2"))

frq(eg$free_choice_bin)


FreeYrEg <- eg %>%
  group_by(year, treat) %>%
  summarize(free_bin_se = std.error(free_choice_bin, na.rm=T),
            free_choice_bin=mean(free_choice_bin, na.rm=T)) %>%
  mutate(free_bin_lower = free_choice_bin - 1.96*free_bin_se,
         free_bin_upper = free_choice_bin + 1.96*free_bin_se,
         n=n())

FreeYrEg

ggplot(FreeYrEg, aes(x=year, y=free_choice_bin)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgoldenrod3", alpha=.4) +
  geom_errorbar(aes(ymin=free_bin_lower, ymax=free_bin_upper), color="dodgerblue2", width=0, size=1) +
  geom_segment(aes(x=2008, xend=2018, y=.562, yend=.616), 
               linetype="dotdash", color="darkgrey", alpha=.4, size=1) +
  geom_line(size=1.2, color="dodgerblue2") + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_label(aes(label=paste(round(free_choice_bin, 3)*100, "%", sep="")), size=4, color="dodgerblue2") +
  scale_x_continuous(limits=c(2000,2018),
                     breaks=c(2001,2008,2010,2013,2018),
                     labels=c("2001","2008","Arab\nSpring","2013","2018")) +
#                     breaks=seq(2000,2018,2), labels=c("2000", "2002","2004", "2006","2008","Arab\nSpring","2012"#, "2014","2016", "2018")) +
  scale_y_continuous(limits=c(.5,.7),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="")

,
       title="Perceived freedom of choice and control over one's life",
       caption="Treatment effect estimates\nDifference in differences: 7.5%\nRegression discontinuity: 8.9%")
         
#         "How much freedom of choice and control do you\nfeel you have over the way your life turns out?\n\nWorld #Values Survey, Egypt\n2003, 2008, 2013, 2018")

ggsave("viz/World Values/wv Egypt perceived freedom of choice and control error.png",
       device="png",
       type="cairo",
       height=4,
       width=7)  




ggplot(FreeYrEg, aes(x=year, y=free_choice_bin, color=as.factor(treat))) + 
  geom_vline(xintercept=2010, size=1.2, color="darkgoldenrod3", alpha=.4) +
  geom_point() +
  stat_smooth(method="lm", size=1.2) +
  scale_color_manual(values=c("dodgerblue","firebrick2"))
  
  
geom_segment(aes(x=2008, xend=2019, y=.562, yend=.622), 
               linetype="dashed", color="firebrick3", alpha=.4, size=2) +
  geom_line(size=1, color="dodgerblue2") + 
  geom_point(color="dodgerblue2", size=3) + 
  geom_label(aes(label=paste(round(free_choice_bin, 3)*100, "%", sep="")), size=4, color="dodgerblue2") +
  scale_x_continuous(limits=c(2000,2020),
                     breaks=seq(2000,2020,2), labels=c("2000", "2002","2004", "2006","2008","Arab\nSpring","2012", "2014","2016", "2018","2020")) +
  scale_y_continuous(limits=c(.5,.7),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Perceived freedom of choice and control over one's life",
       caption="How much freedom of choice and control do you\nfeel you have over the way your life turns out?\n\nWorld Values Survey, Egypt\n2003, 2008, 2013, 2018")

ggsave("viz/World Values/wv Egypt perceived freedom of choice and control 2.png",
       device="png",
       type="cairo",
       height=4,
       width=7)  

## multi-country, free choice ----

free_multi <- wvdat %>%
  filter(country_name=="Egypt" |
           country_name=="Iraq" |
           country_name=="Jordan" |
           country_name=="Turkey")


free_multi1 <- lm(free_choice_bin ~ treat + year,
         data=free_multi)

summary(free_multi1)

?rdd_reg_lm

free_multi_rdd <- rdd_data(y=free_choice_bin,
                   x=year,
                   cutpoint=2010,
                   data=free_multi)

free_multi_rd1 <- rdd_reg_lm(rdd_object = free_multi_rdd,
                  slope="same")

summary(free_multi_rd1)


free_multi_rd2 <- rdd_reg_lm(rdd_object = free_multi_rdd,
                  slope="separate")

summary(free_multi_rd2)

## Turkey ---- 

tk <- free_multi %>%
  filter(country_name=="Turkey")

tk_lm <- lm(free_choice_bin ~ treat + year,
            data=tk)

summary(tk_lm)

tk_rdd <- rdd_data(y=free_choice_bin,
                           x=year,
                           cutpoint=2010,
                           data=tk)

tk_rd1 <- rdd_reg_lm(rdd_object = tk_rdd,
                             slope="same")

summary(tk_rd1)


tk_rd2 <- rdd_reg_lm(rdd_object = tk_rdd,
                             slope="separate")

summary(tk_rd2)


## Iraq ---- 

iq <- free_multi %>%
  filter(country_name=="Iraq")

iq_lm <- lm(free_choice_bin ~ treat + year,
            data=iq)

summary(iq_lm)

iq_rdd <- rdd_data(y=free_choice_bin,
                   x=year,
                   cutpoint=2010,
                   data=iq)

iq_rd1 <- rdd_reg_lm(rdd_object = iq_rdd,
                     slope="same")

summary(iq_rd1)


iq_rd2 <- rdd_reg_lm(rdd_object = iq_rdd,
                     slope="separate")

summary(iq_rd2)


## Jordan ---- 

jd <- free_multi %>%
  filter(country_name=="Jordan")

jd_lm <- lm(free_choice_bin ~ treat + year,
            data=jd)

summary(jd_lm)

jd_rdd <- rdd_data(y=free_choice_bin,
                   x=year,
                   cutpoint=2010,
                   data=jd)

jd_rd1 <- rdd_reg_lm(rdd_object = jd_rdd,
                     slope="same")

summary(jd_rd1)


jd_rd2 <- rdd_reg_lm(rdd_object = jd_rdd,
                     slope="separate")

summary(jd_rd2)



