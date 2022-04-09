# Arab Spring
# Arab Barometer

# prep ---- 

abi <- read_dta("data/Arab Barometer/Wave I/ABI_English.dta")

abii <- read_dta("data/Arab Barometer/Wave II/ABII_English.dta")

abiii <- read_dta("data/Arab Barometer/Wave III/ABIII_English.dta")

abiv <- read_dta("data/Arab Barometer/Wave IV/ABIV_English.dta")

abv <- read_dta("data/Arab Barometer/Wave V/ABV_Release_Data.dta") %>%
  mutate()

names(abv)

frq(abv$country)
frq(abv$Q201A_1)
frq(abv$Q103)

# country names to harmonize
v_cntry <- frq(abv$country) %>%
  as.data.frame()

v_cntry

iv_cntry <- frq(abiv$country) %>%
  as.data.frame()

iv_cntry


iii_cntry <- frq(abiii$country) %>%
  as.data.frame()

iii_cntry


ii_cntry <- frq(abii$country) %>%
  as.data.frame()

ii_cntry


i_cntry <- frq(abi$country) %>%
  as.data.frame()

i_cntry


# 
# country_key <- data.frame(country=a$val,
#                           name=a$label) %>%
#   .[1:15,]
# 
# country_key
# 
# frq(i_out$country)
# frq(i_out$country_name)
# 
# frq(ii_out$country)
# frq(ii_out$country_name)



Q412A4

# USG opinion
frq(abv$Q700A_1)
frq(abv$Q701C_1)
frq(abv$Q706A)
frq(abv$Q706B)
frq(abv$Q707)

ggplot(abv, aes(country, Q707))

v_cntry

v_out <- abv %>%
  select(country, 
         #date,
         wave,
         #right_direction=Q100,
         #govt_satis=Q513,
         #donate=Q266,
         #wom_quota=Q601A,
         #democratic=Q511,
         #dem_suit=Q512,
         #soc_media_hrs=Q424,
         #soc_media_facebook=Q412A3,
         #soc_media_Twitter=Q412A4,
         #soc_media_Instagram=Q412A6,
         #free_opine=Q521_1,
         #free_protest=Q521_4,
         #free_join=Q521_5,
         #free_live=Q854,
         #wom_PM=Q601_1,
         #wom_wrs=Q601_3,
         wom_educ_less=Q601_4,
         #wom_trvl=Q601_7,
         #wom_dvrc=Q601_14,
         #wom_say=Q601_18,
         #wom_inh=Q601_9,
         #wom_inh2=Q601_9A,
         trustworthy=Q103,
         trust_NatGov = Q201A_1,
         Q700A_1,
         Q701C_1,
         Q706A,
         Q706B,
         am_good=Q707) %>%
  mutate(country_name=as_character(country),
         country=as.numeric(country),
         name=country_name,
         year = 2018,
         wom_educ_less = ifelse(wom_educ_less==0, NA,
                                ifelse(wom_educ_less>4, NA, wom_educ_less)),
         wom_educ_less_rec = 5 - wom_educ_less,
         wom_educ_less_bin = case_when(wom_educ_less>2 ~ 1,
                                       TRUE ~ 0),
         trustworthy = case_when(trustworthy == 1 ~ 1,
                                 trustworthy == 2 ~ 0,
                                 trustworthy > 2 ~ NA_real_),
         trust_NatGov = ifelse(trust_NatGov>4, NA, trust_NatGov),
         trust_NatGov_bin = case_when(trust_NatGov < 3 ~ 1,
                                      trust_NatGov == 3 ~ 0,
                                      trust_NatGov == 4 ~ 0,
                                      trust_NatGov > 4 ~ NA_real_,
                                      TRUE ~ NA_real_),
         am_good_ord = case_when(am_good==1 ~ 1,
                                 am_good==2 ~ 2,
                                 am_good==3 ~ 4,
                                 am_good==4 ~ 5,
                                 am_good>4 ~ 3,
                                 is.na(am_good) ~ NA_real_),
         am_good_bin = case_when(am_good_ord<3 ~ 1,
                                 am_good_ord>2 ~ 0,
                                 is.na(am_good_ord) ~ NA_real_)) %>%
  select(country, year, wave, everything()) %>%
  drop_labels(country) %>%
  as.data.frame()

frq(v_out$trust_NatGov_bin)
frq(v_out$country)
frq(v_out$country_name)

frq(v_out$am_good)
frq(v_out$am_good_ord)
frq(v_out$am_good_bin)

am_good <- v_out %>%
  group_by(country_name) %>%
  summarize(am_good = mean(am_good_bin, na.rm=T),
            se = std.error(am_good_bin)) %>%
  na.omit()

am_good

ggplot(am_good, aes(reorder(country_name, am_good), am_good, country_name)) + 
  geom_chicklet(width=.4,
           fill="cadetblue2",
           alpha=.6,
           color="dodgerblue") +
  coord_flip() +
  scale_y_continuous(limits=c(0,1),
                     labels=percent) +
  labs(x="",
       y="",
       title="Positive perception of United States people",
       caption="Agreement with the statement:\n\"Irrespective of US foreign policies, most\nordinary Americans are good people.\"
       
       Arab Barometer, Wave 5 (2019)")

ggsave("viz/Arab Barometer/Positive perception of American people chick, Arab Barometer W5.png",
       device="png",
       type="cairo",
       height=6,
       width=7)

?drop_labels

names(abiv)
frq(abiv$q2011)
frq(abiv$q103)
frq(abiv$q707)
frq(abiv$year)

iv_cntry

iv_out <- abiv %>%
  select(country,
         #date,
         #wave,
         #wom_PM=q6011,
         #wom_wrk_outside=q6012,
         #wom_wrs=q6013,
         wom_educ_less=q6014,
         #wom_inh=q6018,
         #wom_say=q60118,
         trustworthy = q103,
         trust_NatGov = q2011,
         am_good=q707) %>%
  mutate(country_name=as_character(country),
         country=as.numeric(country),
         name=country_name,
         wave=4,
         year=2016,
         wom_educ_less = ifelse(wom_educ_less==0, NA,
                                ifelse(wom_educ_less>4, NA, wom_educ_less)),
         wom_educ_less_rec = 5 - wom_educ_less,
         wom_educ_less_bin = case_when(wom_educ_less>2 ~ 1,
                                       TRUE ~ 0),
         trustworthy = case_when(trustworthy == 1 ~ 1,
                                 trustworthy == 2 ~ 0,
                                 trustworthy > 2 ~ NA_real_),
         trust_NatGov = ifelse(trust_NatGov>4, NA, trust_NatGov),
         trust_NatGov_bin = case_when(trust_NatGov < 3 ~ 1,
                                      trust_NatGov == 3 ~ 0,
                                      trust_NatGov == 4 ~ 0,
                                      trust_NatGov > 4 ~ NA_real_,
                                      TRUE ~ NA_real_),
         am_good_bin = case_when(am_good==1 ~ 1,
                                 TRUE ~ 0)) %>%
  select(country, year, wave, everything()) %>%
  as.data.frame()


frq(v_out$trust_NatGov_bin)
frq(iv_out$country)
frq(iv_out$am_good_bin)


frq(abiii$q103)
frq(abiii$q2011)

names(abiii)

iii_cntry

iii_out <- abiii %>%
  select(country, 
         #wave,
         #wom_wrk_outside=q6012,
         #wom_wrs=q6013,
         wom_educ_less=q6014,
         trustworthy=q103,
         trust_NatGov = q2011,
         am_good=q707) %>%
  mutate(country_name=as_character(country),
         country=as.numeric(country),
         name=country_name,
         wave=3,
         year=2012,
         wom_educ_less = ifelse(wom_educ_less==0, NA,
                                ifelse(wom_educ_less>4, NA, wom_educ_less)),
         wom_educ_less_rec = 5 - wom_educ_less,
         wom_educ_less_bin = case_when(wom_educ_less>2 ~ 1,
                                       TRUE ~ 0),
         trustworthy = case_when(trustworthy == 0 ~ NA_real_,
                                 trustworthy == 1 ~ 1,
                                 trustworthy == 2 ~ 0,
                                 trustworthy > 2 ~ NA_real_),
         trust_NatGov = ifelse(trust_NatGov>4, NA, trust_NatGov),
         trust_NatGov_bin = case_when(trust_NatGov < 3 ~ 1,
                                      trust_NatGov == 3 ~ 0,
                                      trust_NatGov == 4 ~ 0,
                                      trust_NatGov > 4 ~ NA_real_,
                                      TRUE ~ NA_real_),
         am_good_bin = case_when(am_good==1 ~ 1,
                                 TRUE ~ 0)) %>%
  select(country, year, wave, everything()) %>%
  as.data.frame()

# note change of wording for q2011

frq(v_out$trust_NatGov_bin)
frq(iii_out$am_good)
frq(iii_out$am_good_bin)

names(abii)
frq(abii$q103)
frq(abii$q2011)
frq(abii$q60109)
frq(abii$q707)

ii_cntry

ii_out <- abii %>%
  select(country, 
         #wave,
         #wom_PM=q60101,
         #wom_wrk_outside=q60102,
         #wom_wrs=q60103,
         wom_educ_less=q60104,
         #wom_inh=q60109,
         #wom_say=q60118,
         trustworthy=q103,
         trust_NatGov = q2011,
         am_good=q707) %>%
  mutate(country_name=as_character(country),
         country=as.numeric(country),
         name=case_when(country==1 ~ "Algeria",
                        country==2 ~ "Bahrain",
                        country==3 ~ "Comoros",
                        country==4 ~ "Djibouti",
                        country==5 ~ "Egypt",
                        country==6 ~ "United Arab Emirates",
                        country==7 ~ "Iraq",
                        country==8 ~ "Jordan",
                        country==9 ~ "Kuwait",
                        country==10 ~ "Lebanon",
                        country==11 ~ "Libya",
                        country==12 ~ "Mauritania",
                        country==13 ~ "Morocco",
                        country==14 ~ "Oman",
                        country==15 ~ "Palestine",
                        country==16 ~ "Qatar",
                        country==17 ~ "Saudi Arabia",
                        country==18 ~ "Somalia",
                        country==19 ~ "Sudan",
                        country==20 ~ "Syria",
                        country==21 ~ "Tunisia",
                        country==22 ~ "Yemen",
                        TRUE ~ country_name),
         wave=2,
         year=2010,
         wom_educ_less = ifelse(wom_educ_less==0, NA,
                                ifelse(wom_educ_less>4, NA, wom_educ_less)),
         wom_educ_less_rec = 5 - wom_educ_less,
         wom_educ_less_bin = case_when(wom_educ_less>2 ~ 1,
                                       TRUE ~ 0),
         trustworthy = case_when(trustworthy == 0 ~ NA_real_,
                                 trustworthy == 1 ~ 1,
                                 trustworthy == 2 ~ 0,
                                 trustworthy > 2 ~ NA_real_),
         trust_NatGov = ifelse(trust_NatGov>4, NA, trust_NatGov),
         trust_NatGov_bin = case_when(trust_NatGov < 3 ~ 1,
                                      trust_NatGov == 3 ~ 0,
                                      trust_NatGov == 4 ~ 0,
                                      trust_NatGov > 4 ~ NA_real_,
                                      TRUE ~ NA_real_),
         am_good_bin = case_when(am_good==1 ~ 1,
                                 TRUE ~ 0)) %>%
  select(country, year, wave, everything()) %>%
  as.data.frame()

# note change of wording for q2011

frq(v_out$trust_NatGov_bin)
frq(ii_out$name)
frq(ii_out$am_good_bin)

names(abi)

frq(abi$q707)

i_cntry

i_out <- abi %>%
  select(country, 
         #wave,
         #wom_PM=q5051,
         #wom_wrk_outside=q5052,
         #wom_wrs=q5053,
         wom_educ_less=q5054,
         #wom_inh=q6018,
         #wom_say=q60118,
         trustworthy=q103,
         trust_NatGov = q2011) %>%
  mutate(country_name=as_character(country),
         country=as.numeric(country),
         wave=1,
         year=2006,
         wom_educ_less = ifelse(wom_educ_less==0, NA,
                                ifelse(wom_educ_less>4, NA, wom_educ_less)),
         #wom_educ_less = case_when(wom_educ_less == 0 ~ NA_real_,
         #                          wom_educ_less>4 ~ NA_real_),
         wom_educ_less_rec = 5 - wom_educ_less,
         wom_educ_less_bin = case_when(wom_educ_less>2 ~ 1,
                                       TRUE ~ 0),
         trustworthy = case_when(trustworthy == 0 ~ NA_real_,
                                 trustworthy == 1 ~ 1,
                                 trustworthy == 2 ~ 0,
                                 trustworthy > 2 ~ NA_real_),
         trust_NatGov = ifelse(trust_NatGov>4, NA, trust_NatGov),
         trust_NatGov_bin = case_when(trust_NatGov < 3 ~ 1,
                                      trust_NatGov == 3 ~ 0,
                                      trust_NatGov == 4 ~ 0,
                                      trust_NatGov > 4 ~ NA_real_,
                                      TRUE ~ NA_real_),
         name=case_when(country==1 ~ "Jordan",
                        country==2 ~ "Palestine",
                        country==3 ~ "Algeria",
                        country==4 ~ "Morocco",
                        country==6 ~ "Lebanon",
                        country==7 ~ "Yemen",
                        country==8 ~ "Bahrain",
                        TRUE ~ country_name)) %>%
  select(country, year, wave, everything()) %>%
  as.data.frame()

# note change of wording for q2011

frq(i_out$name)
frq(v_out$trust_NatGov_bin)

str(i_out)

write_rds(i_out, "data/Arab Barometer/combined/i_out.rds")
write_rds(ii_out, "data/Arab Barometer/combined/ii_out.rds")
write_rds(iii_out, "data/Arab Barometer/combined/iii_out.rds")
write_rds(iv_out, "data/Arab Barometer/combined/iv_out.rds")
write_rds(v_out, "data/Arab Barometer/combined/v_out.rds")

i_out <- read_rds("data/Arab Barometer/combined/i_out.rds")
ii_out <- read_rds("data/Arab Barometer/combined/ii_out.rds")
iii_out <- read_rds("data/Arab Barometer/combined/iii_out.rds")
iv_out <- read_rds("data/Arab Barometer/combined/iv_out.rds")
v_out <- read_rds("data/Arab Barometer/combined/v_out.rds")


names(i_out)

out <- do.call(rbind, list(i_out, ii_out, iii_out, iv_out, v_out)) %>%
  left_join(country_key)

head(out)

?bind_rows

out <- bind_rows(list(i_out, ii_out, iii_out, iv_out, v_out)) 

%>%
  left_join(country_key)

frq(i_out$wom_educ_less)

frq
frq(out$year)

country_key
frq(out$country) # scrambled!
frq(out$country_name) # duplicates
frq(out$name) # should be ok

# Perception of US ---- 

amYr <- out %>%
  group_by(name, year) %>%
  summarize(am_good_se = std.error(am_good_bin),
            am_good=mean(am_good_bin, na.rm=T),
            n=n()) %>%
  na.omit() %>%
  mutate(lower=am_good-1.96*am_good_se,
         upper=am_good+1.96*am_good_se) %>% 
  filter(name!="Kuwait",
         name!="Saudi Arabia") # only single data points

amYr
frq(amYr$year)

ggplot(amYr, aes(year, am_good, group=name, color=name)) + 
  geom_hline(yintercept=.5, color="grey60", size=1, alpha=.6) +
  geom_point(size=3) + 
  geom_line(size=1) +
  scale_color_viridis_d() +
  facet_wrap(~name,
             ncol=3) +
  faceted +
  theme(legend.position="none") +
  scale_x_continuous(breaks=c(2012,2016,2018)) +
  scale_y_continuous(limits=c(.2,.8),
                     breaks=seq(.2,.8,.2),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Positive perception of United States",
       caption="Agreement with the statement:\n\"Irrespective of US foreign policies, most\nordinary Americans are good people.\"
       
       Arab Barometer, Waves 1-5")


ggsave("viz/Arab Barometer/Positive perception of American people line, Arab Barometer W1-5.png",
       device="png",
       type="cairo",
       height=8,
       width=10)


ggplot(amYr, aes(year, am_good, group=name, color=name)) + 
  geom_hline(yintercept=.5, color="grey60", size=1, alpha=.6) +
  geom_line(size=1) +
  geom_label(aes(label=paste(round(am_good*100,1), "%", sep="")),
             size=4) +
  scale_color_viridis_d() +
  facet_wrap(~name,
             ncol=3) +
  faceted +
  theme(legend.position="none") +
  scale_x_continuous(breaks=c(2012,2016,2018)) +
  scale_y_continuous(limits=c(.2,.8),
                     breaks=seq(.2,.8,.2),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Positive perception of United States",
       caption="Agreement with the statement:\n\"Irrespective of US foreign policies, most\nordinary Americans are good people.\"
       
       Arab Barometer, Waves 1-5")


ggsave("viz/Arab Barometer/Positive perception of American people line label, Arab Barometer W1-5.png",
       device="png",
       type="cairo",
       height=8,
       width=10)




# countries with 2010 or earlier
byYr <- out %>%
  group_by(name, year) %>%
  summarize(trustworthy=mean(trustworthy, na.rm=T),
            trust_NatGov = mean(trust_NatGov_bin, na.rm=T)) %>%
  filter(name=="Algeria" |
           name=="Egypt" |
           name=="Iraq" |
           name=="Jordan" |
           #name=="Kingdom of Saudi Arabia" |
           #name=="Kuwait" |
           name=="Lebanon" |
           #name=="Libya" |
           #name=="Morocco" | 
           name=="Palestine" |
           name=="Sudan" |
           name=="Tunisia" |
           name=="Yemen")

byYr

a

country_key

ggplot(byYr, aes(year, trustworthy, color=name)) + 
  geom_vline(xintercept=2010, color="darkgrey", size=1.2, alpha=.8) + 
  geom_point(size=3) + 
  geom_line(size=1) + 
  scale_color_viridis_d() +
  facet_wrap(~name) +
  faceted + 
  theme(legend.position="none") +
  scale_x_continuous(limits=c(2006,2018),
                     breaks=seq(2006, 2018, 4),
                     labels=c("", "Arab\nSpring","2014","2018")) +
  scale_y_continuous(limits=c(0,.6),
                     breaks=seq(.2,.6,.2),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="People are generally trustworthy")

ggsave("viz/Arab Barometer/ab trustworthy facet.png",
       device="png",
       type="cairo",
       height=5,
       width=8)


ggplot(byYr, aes(year, trust_NatGov, color=name)) + 
  geom_vline(xintercept=2010, color="darkgrey", size=1.2, alpha=.8) + 
  geom_point(size=3) + 
  geom_line(size=1) + 
  scale_color_viridis_d() +
  facet_wrap(~name) +
  faceted + 
  theme(legend.position="none") +
  scale_x_continuous(limits=c(2006,2018),
                     breaks=seq(2006, 2018, 4),
                     labels=c("", "Arab\nSpring","2014","2018")) +
  scale_y_continuous(limits=c(0,.9),
                     breaks=seq(.2,.8,.2),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="Trust in government")

ggsave("viz/Arab Barometer/ab trust in gov facet.png",
       device="png",
       type="cairo",
       height=5,
       width=8)




# countries with 2006
byYr_2006 <- out %>%
  group_by(name, year) %>%
  summarize(trustworthy=mean(trustworthy, na.rm=T),
            trust_NatGov = mean(trust_NatGov_bin, na.rm=T),
            wom_emp = mean(wom_educ_less_bin)) %>%
  filter(name=="Algeria" |
           #name=="Egypt" |
           name=="Iraq" |
           name=="Jordan")
           #name=="Kingdom of Saudi Arabia" |
           #name=="Kuwait" |
           #name=="Lebanon" |
           #name=="Libya" |
           #name=="Morocco" | 
           #name=="Palestine" |
           #name=="Sudan" |
           #name=="Tunisia" |
           #name=="Yemen")

byYr_2006

a

country_key

ggplot(filter(byYr_2006, name!="Jordan"), aes(year, trustworthy, color=name)) + 
  geom_vline(xintercept=2010, color="darkgrey", size=1.2, alpha=.8) + 
  geom_point(size=3.5) + 
  geom_line(size=1) + 
  #geom_label(aes(label=paste(round(trustworthy, 2)*100, "%", sep=""))) +
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2006,2018),
                     breaks=seq(2006,2018,2),
                     labels=c("2006", "2008", "Arab\nSpring", "2012","2014", "2016","2018")) +
                     #labels=c("2006", "Arab\nSpring","2014","2018")) +
  scale_y_continuous(limits=c(0,.6),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
       title="People are generally trustworthy") +
  theme(legend.position="bottom",
        legend.title=element_blank())

ggsave("viz/Arab Barometer/ab trustworthy.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


ggplot(byYr_2006, aes(year, trustworthy, color=name)) + 
  geom_vline(xintercept=2010, color="darkgrey", size=1.2, alpha=.8) + 
  geom_point(size=3) + 
  geom_line(size=1) + 
  scale_color_viridis_d() +
  facet_wrap(~name) +
  faceted +
  scale_x_continuous(limits=c(2006,2018),
                     breaks=seq(2006,2018,4),
                     labels=c("", "Arab\nSpring","2014","2018")) +
  scale_y_continuous(limits=c(0,.6)) +
  labs(x="",
       y="")


ggplot(byYr_2006, aes(year, trust_NatGov, color=name)) + 
  geom_vline(xintercept=2010, color="darkgrey", size=1.2, alpha=.8) + 
  geom_line(size=1) + 
  geom_point(size=3.5) +
  geom_label(aes(label=paste(round(trust_NatGov,2)*100, "%", sep="")), size=3.4) +
  scale_x_continuous(limits=c(2006,2018),
                     breaks=seq(2006,2018,2),
                     labels=c("2006", "2008", "Arab\nSpring", "2012","2014", "2016","2018")) +
  #labels=c("2006", "Arab\nSpring","2014","2018")) +
  scale_y_continuous(limits=c(0,.8),
                     labels=percent_format(accuracy=1),
                     sec.axis=dup_axis(breaks=c(.42, .3, .18),
                                       labels=c("Jordan","Algeria","Iraq"))) +
  scale_color_viridis_d() +
  labs(x="",
       y="",
       title="Trust in government") +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.key=element_rect()) 
+
  guide_legend(label=F)

?guide_legend
?dup_axis

ggsave("viz/Arab Barometer/ab trust in gov.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


# Women's empowerment ----

ggplot(byYr_2006, aes(year, wom_emp, color=name)) + 
  geom_vline(xintercept=2010, color="darkgrey", size=1.2, alpha=.8) + 
  geom_line(size=1) + 
  geom_point(size=3.5) +
  geom_label(aes(label=paste(round(trust_NatGov,2)*100, "%", sep="")), size=3.4) +
  scale_x_continuous(limits=c(2006,2018),
                     breaks=seq(2006,2018,2),
                     labels=c("2006", "2008", "Arab\nSpring", "2012","2014", "2016","2018")) +
  #labels=c("2006", "Arab\nSpring","2014","2018")) +
  scale_y_continuous(limits=c(.4,1),
                     labels=percent_format(accuracy=1),
                     sec.axis=dup_axis(breaks=c(.42, .3, .18),
                                       labels=c("Jordan","Algeria","Iraq"))) +
  scale_color_viridis_d() +
  labs(x="",
       y="",
       title="Trust in government") +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.key=element_rect()) 


ggplot(byYr_2006, aes(year, wom_emp, color=name)) + 
  geom_vline(xintercept=2010, color="darkgrey", size=1.2, alpha=.8) + 
  geom_point(size=3) + 
  geom_line(size=1) + 
  scale_color_viridis_d() +
  facet_wrap(~name) +
  faceted +
  scale_x_continuous(limits=c(2006,2018),
                     breaks=seq(2006,2018,4),
                     labels=c("", "Arab\nSpring","2014","2018")) +
  scale_y_continuous(limits=c(.4,1),
                     labels=percent) +
  labs(x="",
       y="",
       title="Women's empowerment, Arab Spring countries",
       caption="Disagreement with statement that university\neducation is more important for men than women\n\nArab Barometer\n2006, 2010, 2012, 2016, 2018")  +
  theme(legend.position="none")

ggsave("viz/Arab Barometer/wom emp.png",
       device="png",
       type="cairo",
       height=4,
       width=8.5)

frq(byYr_2006$year)

# Age heaping ---- 
# wave ii egypt

abii_egy <- abii %>% 
  filter(country == 5)

abii_egy_pop <- data.frame(frq(abii_egy$q1001)) %>%
  select(val, frq) %>%
  mutate(percent = frq/sum(frq)) %>%
  filter(val < 86) %>%
  rename(age = val, pop = frq) 

myers(abii_egy_pop, age, pop, c(21,80))

# wave iii egypt

abiii_egy <- abiii %>%
  filter(country == 5) %>%
  select(age = q1001, date) %>%
  mutate(date = year(date)) %>%
  group_by(date) %>%
  count(age) %>%
  mutate(percent = n/sum(n)) %>%
  as.data.frame()

myers(abiii_egy, age, n, c(18,77))

# wave iV egypt

abiv_egy <- abiv %>%
  filter(country == 5) %>%
  select(age = q1001) %>%
  count(age) %>%
  mutate(percent = n/sum(n)) %>%
  as.data.frame()

myers(abiv_egy, age, n , c(18,67))

# wave v egpyt

abv_egy <- abv %>%
  filter(country == 5) %>%
  select(age = Q1001) %>%
  count(age) %>%
  mutate(percent = n/sum(n)) %>%
  as.data.frame() %>%
  na.omit()

myers(abv_egy, age, n, c(18,67))

myers_ab_egypt <- data.frame(year=c(2010, 2013, 2016, 2019),
                             myers=c(15.3, 12.6, 9.54, 9.92),
                             country="Egypt",
                             source="Arab Barometer")
myers_ab_egypt

ggplot(myers_ab_egypt, aes(year)) +
  geom_point(aes(y = myers), size = 3) +
  geom_line(aes(y = myers), size = 1) +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(2010,2019),
                     breaks=seq(2010,2019,3)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Myers Index for Egypt")

# wave iii libya
abiii_lib <- abiii %>%
  filter(country == 11) %>%
  select(age = q1001, date) %>%
  mutate(date = year(date)) %>%
  group_by(date) %>%
  count(age) %>%
  mutate(percent = n/sum(n)) %>%
  as.data.frame()

myers(abiii_lib, age, n , c(18,77))


# wave v libya

abv_lib <- abv %>%
  filter(country == 11) %>%
  select(age = Q1001, date) %>%
  mutate(date = year(date)) %>%
  group_by(date) %>%
  count(age) %>%
  mutate(percent = n/sum(n)) %>%
  as.data.frame() %>%
  na.omit()

myers(abv_lib, age, n , c(18,77))

myers_ab_libya <- data.frame(year=c(2014, 2019),
                             myers=c(11.4, 5.98),
                             country="Libya",
                             source="Arab Barometer")
myers_ab_libya

ggplot(myers_ab_libya, aes(year)) +
  geom_point(aes(y = myers), size = 3) +
  geom_line(aes(y = myers), size = 1) +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(2014,2019),
                     breaks=seq(2014,2019,5)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Myers Index for Libya",
       caption = "Sorce from Arab Barometer")

# wave ii tunisia

abii_tuni <- abii %>% 
  filter(country == 21)

abii_tuni_pop <- data.frame(frq(abii_tuni$q1001)) %>%
  select(val, frq) %>%
  mutate(percent = frq/sum(frq)) %>%
  filter(val < 86) %>%
  rename(age = val, pop = frq) 

myers(abii_tuni_pop, age, pop, c(21,80))

# wave iii tunisia

abiii_tuni <- abiii %>%
  filter(country == 21) %>%
  select(age = q1001, date) %>%
  mutate(date = year(date)) %>%
  group_by(date) %>%
  count(age) %>%
  mutate(percent = n/sum(n)) %>%
  as.data.frame()

myers(abiii_tuni, age, n, c(18,87))

# wave iV tunisia

abiv_tuni <- abiv %>%
  filter(country == 21) %>%
  select(age = q1001) %>%
  count(age) %>%
  mutate(percent = n/sum(n)) %>%
  as.data.frame()

myers(abiv_tuni, age, n , c(18,87))

# wave v tunisia

abv_tuni <- abv %>%
  filter(country == 21) %>%
  select(age = Q1001) %>%
  count(age) %>%
  mutate(percent = n/sum(n)) %>%
  as.data.frame() %>%
  na.omit()

myers(abv_tuni, age, n, c(18,87))

myers_ab_tunisia <- data.frame(year=c(2010, 2013, 2016, 2019),
                             myers=c(7.84, 8.1, 7.98, 3.4),
                             country="Tunisia",
                             source="Arab Barometer")
myers_ab_tunisia

ggplot(myers_ab_tunisia, aes(year)) +
  geom_point(aes(y = myers), size = 3) +
  geom_line(aes(y = myers), size = 1) +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(2010,2019),
                     breaks=seq(2010,2019,3)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Myers Index for Tunisia",
       caption = "Sorce from Arab Barometer")
