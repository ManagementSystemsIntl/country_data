# Arab Spring
# Freedom House

library(democracyData)

# misc ----

fh <- download_fh(verbose=F)

fh

eg <- fh %>%
  filter(fh_country=="Egypt",
        #fh_country=="Tunisia",
         year>2003)

eg

egYr <- eg %>%
  group_b


ggplot(eg, aes(year, fh_total)) + 
  geom_point() + 
  #geom_line() + 
  stat_smooth(se=F, span=.55)


reg <- fh %>%
  filter(fh_country=="Tunisia" |
         fh_country=="Egypt",
         year>2008)

reg

ggplot(reg, aes(year, fh_total_reversed, color=fh_country)) +
  geom_point() + 
  geom_line()


score_ends <- reg %>%
  group_by(fh_country) %>%
  summarize(fh_total_reversed = last(fh_total_reversed))

score_ends

reg

#  tail(reg$fh_total_reversed,1) %>%
  pull(fh_total_reversed)

score_ends

?top_n


ggplot(reg, aes(year, fh_total_reversed, color=fh_country)) +
  geom_vline(2010.8)
  geom_point() + 
  geom_line() + 
  theme(legend.position = "none") +
  scale_y_continuous(sec.axis=sec_axis(~.,
                                       breaks=score_ends$fh_total_reversed,
                                       labels=score_ends$fh_country)) +
  scale_color_manual(values=c("firebrick3","dodgerblue2"))
  
scale_color_viridis_d()





sec.axis=sec_axis(~., 
                  breaks=case_ends,
                  labels=lab[1:6]))



# manual import -----------------------------------------------------------

type_key <- data.frame(type=LETTERS[1:7],
                       type_name = c("Electoral Process","Political Pluralism and Participation","Functioning of Government","Freedom of Expresion and Belief","Associational and\nOrganizational Rights","Rule of Law","Personal Autonomy and Individual Rights"),
                       type_name2 = c("Electoral\nProcess","Political Pluralism\nand Participation","Functioning\nof Government","Freedom of\nExpression and Belief","Associational and\nOrganizational Rights","Rule\nof Law","Personal Autonomy\nand Individual Rights"))

type_key

dat <- read_excel("data/Freedom House/Aggregate_Category_and_Subcategory_Scores_FIW_2003-2021.xlsx",
                  sheet="export")

names(dat)

eg <- dat %>%
  filter(country=="Egypt") %>%
  select(1,2, 4,8:10, 14:17)

names(eg)

egL <- eg %>%
  pivot_longer(cols=4:10,
               names_to="type",
               values_to="score") %>%
  filter(year>2008) %>%
  left_join(type_key)

egL

ggplot(egL, aes(year, score, color=type_name)) + 
  geom_line() + 
  geom_point() + 
  scale_color_viridis_d() + 
  scale_y_continuous(breaks=0:10)

ggplot(egL, aes(year, score, color=type_name2)) + 
  geom_vline(xintercept=2010.9, color="darkgoldenrod3", size=1, alpha=.6) +
  geom_line() + 
  geom_point() + 
  scale_color_viridis_d() + 
  facet_wrap(~type_name2) +
  faceted +
  theme(legend.position="none")




# overall measure ---- 
## overall, selected countries ---------------------------------------------

reg <- dat %>%
  filter(country=="Egypt" |
           country=="Tunisia" |
           country=="Libya",
         year>2008) %>%
  mutate(year= year-1) %>%
  select(1,2, 4, 5, 19) %>%
  mutate(status_lab = ifelse(status=="F", "Free",
                             ifelse(status=="PF", "Partly\nFree", "Not\nFree")))

names(reg)
reg

score_ends <- reg %>%
  group_by(country) %>%
  summarize(Total = first(Total))

score_ends



ggplot(reg, aes(year, Total, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  geom_point(size=3) + 
  geom_label(aes(label=status_lab)) +
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(0,100),
                     breaks=seq(0,100,10),
                     sec.axis=sec_axis(~.,
                                                           breaks=score_ends$Total,
                                                           labels=score_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="Total\nscore",
       title="Freedom House Index")


ggsave("viz/Freedom House/fh total score Eg Tun Lib.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

ggplot(egL, aes(year, score, color=type_name2)) + 
  geom_vline(xintercept=2010.9, color="darkgoldenrod3", size=1, alpha=.6) +
  geom_line() + 
  geom_point() + 
  scale_color_viridis_d() + 
  facet_wrap(~type_name2) +
  faceted +
  theme(legend.position="none")


## overall, all GISR countries facet ---- 

gisr <- dat %>%
  filter(country=="Egypt" |
           country=="Tunisia" |
           country=="Libya" | 
           country=="Iraq" |
           country=="Syria" |
           country=="Morocco" |
           country=="Yemen" | 
           country=="Jordan" |
           country=="Lebanon",
         year>2008) %>%
  mutate(year= year-1) %>%
  select(1,2, 4, 5, 19) %>%
  mutate(status_lab = ifelse(status=="F", "Free",
                             ifelse(status=="PF", "Partly\nFree", "Not\nFree")),
         Total = ifelse(Total<0, 0, Total))


ggplot(gisr, aes(year, Total, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  #geom_point(size=3) + 
  #geom_label(aes(label=status_lab)) +
  facet_wrap(~country) +
  faceted +
  scale_color_viridis_d() + 
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) + 
  scale_y_continuous(limits=c(0,100),
                     breaks=c(0, 25,50,75, 100)) +
                     #breaks=seq(0,100,25),
                     #labels=c("", "25","50","75","")) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       #y="Total\nscore",
       title="Freedom House Index",
       caption="Total score, 0-100")


ggsave("viz/Freedom House/fh total score gisr.png",
       device="png",
       type="cairo",
       height=6,
       width=6.5)

## Overall, gisr line ----

gisr_ends <- gisr %>%
  group_by(country) %>%
  summarize(Total = first(Total)) %>%
  arrange(desc(Total)) %>%
  mutate(Total2 = c(71,43, 38, 33, 28, 18, 12, 8, 1),
         color=viridis(9)) %>%
  arrange(country)

gisr_ends

ggplot(gisr, aes(year, Total, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  #geom_point(size=3) + 
  #geom_label(aes(label=status_lab)) +
  #scale_color_viridis_d() + 
  scale_color_manual(values=gisr_ends$color) +
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(0,80),
                     breaks=seq(0,80,10),
                     sec.axis=sec_axis(~.,
                                       breaks=gisr_ends$Total2,
                                       labels=gisr_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=gisr_ends$color)) +
  labs(x="",
       y="Total\nscore",
       title="Freedom House Index")


ggsave("viz/Freedom House/fh total score gisr together.png",
       device="png",
       type="cairo",
       height=5,
       width=7)


# sub-index B ---- 

## sub-index B, selected countries -----------------------------------------

b <- dat %>%
  filter(country=="Egypt" |
           country=="Tunisia" |
           country=="Libya",
         year>2008) %>%
  mutate(year= year-1) %>%
  select(1,2, 4,9)

names(b)
b
frq(b$B)

score_ends <- b %>%
  group_by(country) %>%
  summarize(B = first(B)) %>%
  mutate(B2 = c(2.1,.9,14))

score_ends

frq(dat$G)
12+16+12+16+12+16+16

ggplot(b, aes(year, B, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(#limits=c(0,100),
                     breaks=0:16,
                     sec.axis=sec_axis(~.,
                                       breaks=score_ends$B2,
                                       labels=score_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="sub-Index\nscore",
       title="Freedom House Index\nPolitical Pluralism and Participation")


ggsave("viz/Freedom House/fh B Eg Tun Lib.png",
       device="png",
       type="cairo",
       height=5,
       width=7)


## sub-index B, GISR countries facet ---- 



gisr_b <- dat %>%
  filter(country=="Egypt" |
           country=="Tunisia" |
           country=="Libya" | 
           country=="Iraq" |
           country=="Syria" |
           country=="Morocco" |
           country=="Yemen" | 
           country=="Jordan" |
           country=="Lebanon",
         year>2008) %>%
  mutate(year= year-1) %>%
  select(1,2, 4, 5, 9) %>%
  mutate(status_lab = ifelse(status=="F", "Free",
                             ifelse(status=="PF", "Partly\nFree", "Not\nFree")))
         
,
         B = ifelse(Total<0, 0, Total))


ggplot(gisr_b, aes(year, B, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  #geom_point(size=3) + 
  #geom_label(aes(label=status_lab)) +
  facet_wrap(~country) +
  faceted +
  scale_color_viridis_d() + 
  scale_x_continuous(limits=c(2008,2020),
                     breaks=seq(2010,2018,4),
                     labels=c("Arab\nSpring", "2014", "2018")) + 
  scale_y_continuous(limits=c(0,17),
                     breaks=seq(0,16,4)) +
  #breaks=seq(0,100,25),
  #labels=c("", "25","50","75","")) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       #y="Total\nscore",
       title="Freedom House Index",
       caption="Inclusion sub-index, 0-16")


ggsave("viz/Freedom House/fh b gisr.png",
       device="png",
       type="cairo",
       height=6,
       width=6.5)


## sub-index B, GISR line

gisr_b_ends <- gisr_b %>%
  group_by(country) %>%
  summarize(Total = first(B)) %>%
  arrange(desc(Total)) %>%
  mutate(Total2 = c(71,43, 38, 33, 28, 18, 12, 8, 1),
         color=viridis(9)) %>%
  arrange(country)

gisr_b_ends

ggplot(gisr_b, aes(year, B, color=country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  #geom_point(size=3) + 
  #geom_label(aes(label=status_lab)) +
  #scale_color_viridis_d() + 
  scale_color_manual(values=gisr_ends$color) +
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(0,16),
                     breaks=0:16,
                     sec.axis=sec_axis(~.,
                                       breaks=gisr_b_ends$Total,
                                       labels=gisr_b_ends$country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.text.y.right=element_text(color=gisr_b_ends$color)) +
  labs(x="",
       y="sub-Index\nscore",
       title="Freedom House Index",
       caption="Participation and Inclusion")


ggsave("viz/Freedom House/fh b gisr together.png",
       device="png",
       type="cairo",
       height=5,
       width=7)

