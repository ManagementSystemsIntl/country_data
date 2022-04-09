# Arab Spring
# Voice and Accountability

?download_wgi_voice_and_accountability

va <- download_wgi_voice_and_accountability()

names(va)
va

reg <- va %>%
  filter(wb_country == "Egypt, Arab Rep." | 
           wb_country == "Tunisia" | 
           wb_country == "Libya",
         year>2007) %>%
  mutate(wb_country = ifelse(wb_country=="Egypt, Arab Rep.", "Egypt", wb_country))
         
reg
frq(va$wb_country)

score_ends <- reg %>%
  group_by(wb_country) %>%
  summarize(Estimate = last(Estimate)) %>%
  mutate(Estimate = ifelse(wb_country=="Tunisia", Estimate,
                    ifelse(wb_country=="Egypt", Estimate+.12, Estimate-.12)))


score_ends


ggplot(reg, aes(year, Estimate, color=wb_country)) +
  geom_vline(xintercept=2010, size=1.2, color="darkgrey",alpha=.99) +
  geom_line(size=1) + 
  geom_point(size=3) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=seq(2008,2020,2),
                     labels=c("2008", "Arab\nSpring", "2012", "2014", "2016", "2018", "2020")) + 
  scale_y_continuous(limits=c(-2,1),
    #breaks=0:16,
    sec.axis=sec_axis(~.,
                      breaks=score_ends$Estimate,
                      labels=score_ends$wb_country)) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Voice and Accountability Index")


ggsave("viz/Voice and Accountability/va Eg Tun Lib.png",
       device="png",
       type="cairo",
       height=4,
       width=7)
