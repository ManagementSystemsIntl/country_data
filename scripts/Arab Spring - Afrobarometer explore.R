# Afro Barometer
# prep / explore

# prep ---- 

source("scripts/00 Arab Spring - prep.r")

# Round 8 ---- 

r8 <- read_spss("data/Afro Barometer/2021 (r8)/uga_r8.data_.new_.final_.wtd_release.31mar21.sav")

r8Names <- data.frame(names(r8))

frq(r8$DATEINTR)
frq(r8$Q43A)
frq(r8$Q44I)

r8 <- r8 %>%
  mutate(corruption_inc_ord = case_when(Q43A==1 ~ 2,
                                        Q43A==2 ~ 1,
                                        Q43A==3 ~ 0,
                                        Q43A==4 ~ -1,
                                        Q43A==5 ~ -2,
                                        Q43A>5 ~ 0),
         corruption_inc_bin = ifelse(corruption_inc_ord>0,1,0),
         corruption_dec_bin = ifelse(corruption_inc_ord<0, 1,0),
         
         schl_contact = ifelse(Q44A==1, 1,0),
         schl_bribe = case_when(Q44C == -1 | Q44C > 6 ~ NA_real_,
                                TRUE ~ Q44C),
         schl_bribe_bin = ifelse(schl_bribe>0,1,0),
         hosp_bribe = ifelse(Q44F==-1 | Q44F>3, NA, Q44F),
         hosp_bribe_bin = ifelse(hosp_bribe>0,1,0),
         id_bribe = ifelse(Q44I==-1 | Q44I>3, NA, Q44I),
         id_bribe_bin = ifelse(id_bribe>0,1,0),
         #service_bribe = ifelse(Q==-1 | Q55H>3, NA, Q55H),
         #service_bribe_bin = ifelse(service_bribe>0,1,0),
         police_bribe = ifelse(Q44L==-1 | Q44L>3, NA, Q44L),
         police_bribe_bin = ifelse(police_bribe>0,1,0)) %>%
  rowwise() %>%
  mutate(bribe_exposure = sum(c_across(c(schl_bribe_bin, hosp_bribe_bin, id_bribe_bin, police_bribe_bin)), na.rm=T)) %>%
  ungroup() %>%
  mutate(anyservice_bribe=ifelse(bribe_exposure>0,1,0))

frq(r8$bribe_exposure)
frq(r8$anyservice_bribe)


frq(r8$corruption_inc_ord)




frq(r8$DATEINTR)
frq(r8$Q44A)
frq(r8$Q44B)
frq(r8$Q44C)

frq(r8$schl_bribe)
frq(r8$schl_bribe_bin)

# Round 7 ---- 

r7m <- read_spss("data/Afro Barometer/2019 (r7)/r7_merged_data_34ctry.release.sav")

r7mNames <- data.frame(names(r7_m))

frq(r7m$URBRUR_COND)

r7 <- read_spss("data/Afro Barometer/2019 (r7)/uga_r7_data.sav")


names(r7)
r7Names <- data.frame(names(r7))

frq(r7$DATEINTR)

svydat <- svydesign(data = dat,
                    ids= ~LOCATION.LEVEL.1,
                    #strata = ,
                    weights = ~Combinwt)

svyrdat <- dat %>%
  as_survey_design(ids= c(PSU,A9),
                   strata=strata,
                   weights=Wgt1)

frq(r7$Q49R)

r7 <- r7 %>%
#  filter(COUNTRY==32) %>%
  mutate(corruption_inc_ord = case_when(Q45==1 ~ 2,
                                        Q45==2 ~ 1,
                                        Q45==3 ~ 0,
                                        Q45==4 ~ -1,
                                        Q45==5 ~ -2,
                                        Q45>5 ~ 0),
         corruption_inc_bin = ifelse(corruption_inc_ord>0,1,0),
         corruption_dec_bin = ifelse(corruption_inc_ord<0, 1,0),
         
         schl_contact = ifelse(Q49A==1, 1,0),
         schl_bribe = case_when(Q49C == -1 | Q49C >6 ~ NA_real_,
                                TRUE ~ Q49C),
         schl_bribe_bin = ifelse(schl_bribe>0,1,0),
         hosp_bribe = ifelse(Q49G==-1 | Q49G>3, NA, Q49G),
         hosp_bribe_bin = ifelse(hosp_bribe>0,1,0),
         id_bribe = ifelse(Q49K==-1 | Q49K>3, NA, Q49K),
         id_bribe_bin = ifelse(id_bribe>0,1,0),
         #service_bribe = ifelse(Q49N==-1 | Q49N>3, NA, Q49N),
         #service_bribe_bin = ifelse(service_bribe>0,1,0),
         police_bribe = ifelse(Q49R==-1 | Q49R>3, NA, Q49R),
         police_bribe_bin = ifelse(police_bribe>0,1,0)) %>%
  rowwise() %>%
  mutate(bribe_exposure = sum(c_across(c(schl_bribe_bin, hosp_bribe_bin, id_bribe_bin, police_bribe_bin)), na.rm=T)) %>%
  ungroup() %>%
  mutate(anyservice_bribe=ifelse(bribe_exposure>0,1,0))

frq(r7$bribe_exposure)
frq(r7$anyservice_bribe)


frq(dat$DATEINTR)
frq(r7m$COUNTRY)
frq(dat$REGION)
frq(dat$URBRUR)
frq(dat$LOCATION.LEVEL.1)

frq(r7$Q44A) # levels of corruption
frq(r7$Q45)

r7m <- r7m %>%
  filter(COUNTRY==32) %>%
  mutate(corruption_inc_ord = case_when(Q45==1 ~ 2,
                                    Q45==2 ~ 1,
                                    Q45==3 ~ 0,
                                    Q45==4 ~ -1,
                                    Q45==5 ~ -2,
                                    Q45>5 ~ 0),
         corruption_inc_bin = ifelse(corruption_inc_ord>0,1,0),
         corruption_dec_bin = ifelse(corruption_inc_ord<0, 1,0),
         
         schl_contact = ifelse(Q49A==1, 1,0),
         schl_bribe = case_when(Q49C == -1 | Q49C >6 ~ NA_real_,
                                TRUE ~ Q49C),
         schl_bribe_bin = ifelse(schl_bribe>0,1,0))

frq(r7$corruption_inc_ord)


# ug <- dat %>%
#   filter(COUNTRY==32)

svyug <- svydesign(data = ug,
                    ids= ~LOCATION.LEVEL.1,
                    #strata = ,
                    weights = ~withinwt)

svyrug <- ug %>%
  as_survey_design(ids= LOCATION.LEVEL.1,
                   #strata=strata,
                   weights=withinwt)



# corruption perception ---- 

frq(r8$Q43A)

# bribery in schools ---- 


frq(r7$Q49A)
frq(r7$Q49B)
frq(r7$Q49C)

frq(r7$schl_contact)
frq(r7$schl_bribe)
frq(r7$schl_bribe_bin)

frq(dat$Q49A)

frq(ug$Q49A)

frq(ug$schl_bribe)
frq(ug$schl_bribe_bin)

svyrug %>%
  group_by(schl_bribe_bin) %>%
  summarise(prop=survey_mean())

svyciprop(~schl_bribe_bin, svyug)

svymean(~schl_bribe_bin,
        na.rm=T,
        design=svyug)




b1 <- svyrdat %>%
  group_by(B1) %>%
  summarise(proportion = survey_mean(),
            total = survey_total()) %>%
  as.data.frame() %>%
  left_join(yes,
            by=c("B1" = "yes_no_code")) %>%
  mutate(Percent=proportion,
         `Margin of error` = 1.96*proportion_se,
         Sample=round(total,0)) %>%
  select(Response, 
         Percent,
         `Margin of error`,
         Sample) %>%
  gt() %>%
  fmt_percent(vars(Percent, `Margin of error`),
              decimals=1) %>%
  tab_header("B1. Aware of USAID")

b1


# Round 6 ---- 

?read_spss

r6 <- haven::read_sav("data/Afro Barometer/2016 (r6)/uga_r6_data.sav")

r6Names <- data.frame(names(r6))

r6m <- haven::read_dta("data/Afro Barometer/2016 (r6)/merged_r6_data_2016_36countries2.dta") 

r6mNames <- data.frame(names(r6m))

frq(r6$COUNTRY)
frq(r6$LOCATION_LEVEL_1)

frq(r6$Q54)
frq(r6$Q55A) # Q55A is really Q55B? Or a labeling issue. assume it's labeling, or use the no contact info
frq(r6$Q55J)

r6 <- r6 %>%
  #filter(COUNTRY==34) %>%
  mutate(corruption_inc_ord = case_when(Q54==1 ~ 2,
                                        Q54==2 ~ 1,
                                        Q54==3 ~ 0,
                                        Q54==4 ~ -1,
                                        Q54==5 ~ -2,
                                        Q54>5 ~ 0),
         corruption_inc_bin = ifelse(corruption_inc_ord>0,1,0),
         corruption_dec_bin = ifelse(corruption_inc_ord<0, 1,0),
         
         schl_contact = ifelse(Q55A==-1 | Q55A>4, 0,1),
         schl_bribe = ifelse(Q55B==-1 | Q55B>3, NA, Q55B),
         #schl_bribe = case_when(Q55B == -1 | Q55B >3 ~ NA_real_,
         #                        TRUE ~ Q55B),
         schl_bribe_bin = ifelse(schl_bribe>0,1,0),
         hosp_bribe = ifelse(Q55D==-1 | Q55D>3, NA, Q55D),
         hosp_bribe_bin = ifelse(hosp_bribe>0,1,0),
         id_bribe = ifelse(Q55F==-1 | Q55F>3, NA, Q55F),
         id_bribe_bin = ifelse(id_bribe>0,1,0),
         #service_bribe = ifelse(Q55H==-1 | Q55H>3, NA, Q55H),
         #service_bribe_bin = ifelse(service_bribe>0,1,0),
         police_bribe = ifelse(Q55J==-1 | Q55J>3, NA, Q55J),
         police_bribe_bin = ifelse(police_bribe>0,1,0)) %>%
  rowwise() %>%
  mutate(bribe_exposure = sum(c_across(c(schl_bribe_bin, hosp_bribe_bin, id_bribe_bin, police_bribe_bin)), na.rm=T)) %>%
  ungroup() %>%
  mutate(anyservice_bribe=ifelse(bribe_exposure>0,1,0))

frq(r6$anyservice_bribe)
frq(r6$bribe_exposure)


r6m <- r6m %>%
  filter(COUNTRY==34) %>%
  mutate(corruption_inc_ord = case_when(Q54==1 ~ 2,
                                        Q54==2 ~ 1,
                                        Q54==3 ~ 0,
                                        Q54==4 ~ -1,
                                        Q54==5 ~ -2,
                                        Q54>5 ~ 0),
         corruption_inc_bin = ifelse(corruption_inc_ord>0,1,0),
         corruption_dec_bin = ifelse(corruption_inc_ord<0, 1,0),
         
         schl_contact = ifelse(Q55A==-1 | Q55A>4, 0,1),
         schl_bribe = ifelse(Q55B==-1 | Q55B>3, NA, Q55B),
         #schl_bribe = case_when(Q55B == -1 | Q55B >3 ~ NA_real_,
        #                        TRUE ~ Q55B),
         schl_bribe_bin = ifelse(schl_bribe>0,1,0))

frq(r6m$schl_contact)
frq(r6m$schl_bribe)


# Round 5 ---- 

r5 <- read_spss("data/Afro Barometer/2013 (r5)/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav")

frq(r5$COUNTRY)

r5_ug <- r5 %>%
  filter(COUNTRY==18)

frq(r5_ug$Q49)

# Uganda merged ---- 

r6m_temp <- r6m %>%
  select(id=1,
         date=42,
         locality=5,
         region=6,
         ea=7,
         wt=withinwt,
         corruption_inc_ord,
         corruption_inc_bin,
         corruption_dec_bin,
         schl_contact,
         schl_bribe,
         schl_bribe_bin) %>%
  mutate(year=2015)

frq(r8temp$date)

r7m_temp <- r7m %>%
  select(id=1,
         date=45,
         locality=7,
         region=8,
         ea=9,
         wt=withinwt,
         corruption_inc_ord,
         corruption_inc_bin,
         corruption_dec_bin,
         schl_contact,
         schl_bribe,
         schl_bribe_bin) %>%
  mutate(year=2017)

r8temp <- r8 %>%
  select(id=1,
         date=37,
         ea_wt=withinwt_ea,
         wt=withinwt_hh,
         locality=2,
         region=3,
         corruption_inc_ord,
         corruption_inc_bin,
         corruption_dec_bin,
         schl_contact,
         schl_bribe,
         schl_bribe_bin) %>%
  mutate(year=2019)

names(r8temp)

out <- do.call(rbind, list(r6m_temp, r7m_temp))

names(out)

frq(out$corruption_dec_bin)

tbl_summary(out[,3])
frq(out$locality)
tbl_summary(out$locality)

tab_xtab(out$corruption_dec_bin, out$year)

frq(out$year)
frq(out$corruption_dec_bin)

out %>%
  group_by(year) %>%
  summarize(corruption_dec_bin=mean(corruption_dec_bin))

svy_out <- svydesign(data=out,
                     ids=~ea,
                     weights=~wt)

svyr_out <- out %>%
  as_survey_design(ids=ea,
                   weights=wt)

svyr_out %>%
  group_by(corruption_dec_bin) %>%
  summarise(prop=survey_mean())

cor_yr_svy <- svyr_out %>%
  group_by(year) %>%
  summarise(prop=survey_mean(corruption_dec_bin)) %>%
  mutate(lower = prop - 1.96*prop_se,
         upper = prop + 1.96*prop_se)

cor_yr_svy

cor_yr2 <- svyr_out %>%
  group_by(year) %>%
  summarise(prop=survey_mean(corruption_dec_bin, vartype="ci"))

cor_yr2
str(cor_yr2)

?survey_mean

ggplot(cor_yr2, aes(year, prop)) + 
  geom_point(size=4, color="dodgerblue") + 
  geom_line(size=1, color="dodgerblue") +
  geom_linerange(aes(ymin=prop_low, ymax=prop_upp), size=1, color="dodgerblue") +
  scale_x_continuous(limits=c(2014.9,2017.1),
                     breaks=c(2015,2017)) +
  scale_y_continuous(limits=c(0,.5),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="")

?geom_errorbar

# Uganda merged, no survey setting ---- 


r6_temp <- r6 %>%
  select(id=1,
         date=35,
         locality=2,
         region=3,
         #subregion=4,
         #ea=7,
         wt=withinwt,
         corruption_inc_ord,
         corruption_inc_bin,
         corruption_dec_bin,
         schl_contact,
         schl_bribe,
         schl_bribe_bin,
         hosp_bribe,
         hosp_bribe_bin,
         id_bribe,
         id_bribe_bin,
         police_bribe,
         police_bribe_bin,
         anyservice_bribe,
         bribe_exposure) %>%
  mutate(id=paste(id, 2015),
         year=2015,
         locality=as_character(locality),
         region=as_character(region),
         urban=ifelse(locality=="Urban", 1,0)) %>%
  as.data.frame() %>%
  remove_attributes(c("label", "labels", "format.spss", "display_width"))

frq(r6$REGION)
frq(r6$URBRUR)
frq(r6_temp$region)
str(r6_temp)
frq(r6_temp$locality)

r7_temp <- r7 %>%
  select(id=1,
         date=35,
         locality=2,
         region=3,
         #ea=9,
         wt=withinwt,
         corruption_inc_ord,
         corruption_inc_bin,
         corruption_dec_bin,
         schl_contact,
         schl_bribe,
         schl_bribe_bin,
         hosp_bribe,
         hosp_bribe_bin,
         id_bribe,
         id_bribe_bin,
         police_bribe,
         police_bribe_bin,
         bribe_exposure,
         anyservice_bribe) %>%
  mutate(id=paste(id, 2017),
         year=2017,
         locality=as_character(locality),
         region=as_character(region),
         urban=ifelse(locality=="Urban", 1,0)) %>%
  as.data.frame() %>%
  remove_attributes(c("label", "labels", "format.spss", "display_width"))

r8_temp <- r8 %>%
  select(id=1,
         date=37,
         #ea_wt=withinwt_ea,
         locality=2,
         region=3,
         wt=withinwt_hh,
         corruption_inc_ord,
         corruption_inc_bin,
         corruption_dec_bin,
         schl_contact,
         schl_bribe,
         schl_bribe_bin,
         hosp_bribe,
         hosp_bribe_bin,
         id_bribe,
         id_bribe_bin,
         police_bribe,
         police_bribe_bin,
         bribe_exposure,
         anyservice_bribe) %>%
  mutate(id=paste(id, 2019),
         year=2019,
         locality=as_character(locality),
         region=as_character(region),
         urban=ifelse(locality=="Urban", 1,0)) %>%
  as.data.frame() %>%
  remove_attributes(c("label", "labels", "format.spss", "display_width"))

str(r8_temp)
names(r8 temp)

sum(names(r6m_temp2) != names(r8temp))

str(r7m_temp2)

lapply(r8temp, frq)

out2 <- do.call(rbind, list(r6_temp, r7_temp, r8_temp))
names(out2)

frq(out2$year)
frq(out2, anyservice_bribe, weights=wt)
frq(out2, bribe_exposure, weights=wt)
weighted.mean(out2$schl_bribe_bin, out2$wt, na.rm=T)

cor_yr <- out2 %>%
  group_by(year) %>%
  summarize(corruption_dec=weighted.mean(corruption_dec_bin, wt),
            se=std.error(corruption_dec_bin)) %>%
  as.data.frame() %>%
  mutate(se_svy=se_comp$svy_se,
         lower=corruption_dec-1.96*se_svy,
         upper=corruption_dec+1.96*se_svy,
         x=round(c(2014.7, 2017, 2019.3),1))

cor_yr
str(cor_yr)


se_comp <- data.frame(se=cor_yr[,3],
                      svy_se=c(.0287, .0162, .0097387*2)) %>%
  mutate(multiple=svy_se/se)

se_comp

cor_yr_svy
cor_yr

ggplot(cor_yr, aes(year, corruption_dec)) + 
  #  geom_point(size=4, color="dodgerblue") + 
  geom_line(size=1, color="dodgerblue") +
  geom_label(aes(label=paste(round(corruption_dec*100,1),"%", sep="")),
             size=4, color="dodgerblue") +
#  geom_linerange(aes(ymin=corruption_dec-1.96*se_svy, ymax=corruption_dec+1.96*se_svy), 
#                 size=1, color="dodgerblue") +
  scale_x_continuous(limits=c(2014.7,2019.3)) +
  #                     breaks=c(2015,2017, 2019)) +
  scale_y_continuous(limits=c(0,.3),
                     labels=percent_format(accuracy=1)) +
  labs(caption="Corruption in Uganda has decreased",
    x="",
    y="") + 
  geom_ribbon(aes(x=x,ymin=corruption_dec-1.96*se_svy, ymax=corruption_dec+1.96*se_svy),
              fill="lightgrey", alpha=.4) +
  geom_line(aes(x=x, y=upper), linetype="dotdash", color="dodgerblue", size=1) +
  geom_line(aes(x=x,y=lower), linetype="dotdash", color="dodgerblue", size=1)



ggplot(cor_yr, aes(year, corruption_dec)) + 
  geom_point(size=4, color="dodgerblue") + 
  geom_line(size=1, color="dodgerblue") +
#  geom_label(aes(label=paste(round(corruption_dec*100,1),"%", sep="")),
#                 size=5, color="dodgerblue") +
  #geom_linerange(aes(ymin=corruption_dec-1.96*se_svy, ymax=corruption_dec+1.96*se_svy), 
  #               size=1, color="dodgerblue") +
  scale_x_continuous(limits=c(2014.9,2019.1)) +
#                     breaks=c(2015,2017, 2019)) +
  scale_y_continuous(limits=c(0,.3),
                     labels=percent_format(accuracy=1)) +
  labs(#caption="Level of corruption in Uganda has decreased in past 12 months\nAfrobarometer",
       x="",
       y="") + 
  geom_ribbon(aes(ymin=lower, ymax=upper),
              fill="lightgrey", alpha=.4) +
  geom_line(aes(y=upper), linetype="dotdash", color="dodgerblue", size=1) +
  geom_line(aes(y=lower), linetype="dotdash", color="dodgerblue", size=1)


ggsave("viz/Afro Barometer/Corruption decreased label fill.png",
       device="png",
       type="cairo",
       height=3,
       width=5.5)


?weighted.mean

frq(r7m_temp2$locality)
frq(r8temp$urban)

frq(r6m_temp2$region)

out_list <- list(r6_temp, r7_temp, r8_temp)

lapply(out_list, str)




# bribe for any service ---- 

bribe_yr <- out2 %>%
  group_by(year) %>%
  summarize(bribe=weighted.mean(anyservice_bribe, wt, na.rm=T),
            se=std.error(anyservice_bribe)) %>%
  as.data.frame() %>%
  mutate(se_svy=se*2,
         lower=bribe-1.96*se_svy,
         upper=bribe+1.96*se_svy,
         x=round(c(2014.7, 2017, 2019.3),1))

bribe_yr

str(bribe_yr)

ggplot(bribe_yr, aes(year, bribe)) + 
  geom_line(size=1, color="dodgerblue") +
  geom_label(aes(label=paste(round(bribe*100,1),"%", sep="")),
             size=4, color="dodgerblue") +
  scale_x_continuous(limits=c(2014.7,2019.3)) +
  #                     breaks=c(2015,2017, 2019)) +
  scale_y_continuous(limits=c(.2,.6),
                     labels=percent_format(accuracy=1)) +
  labs(caption="Paid a bribe for any of four public services",
    x="",
    y="") + 
  geom_ribbon(aes(x=x,ymin=lower, ymax=upper),
              fill="lightgrey", alpha=.4) +
  geom_line(aes(x=x, y=upper), linetype="dotdash", color="dodgerblue", size=1) +
  geom_line(aes(x=x,y=lower), linetype="dotdash", color="dodgerblue", size=1)

ggsave("viz/Afro Barometer/bribe for any service.png",
       device="png",
       type="cairo",
       height=3,
       width=5.5)






