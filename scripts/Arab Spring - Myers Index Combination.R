# Egypt
myers_wv_egypt <- data.frame(year=c(2001, 2008, 2013, 2018),
                             myers=c(5.78,17.5,20,15.9),
                             country="Egypt",
                             source="World Values")

myers_wv_egypt

myers_ab_egypt <- data.frame(year=c(2010, 2013, 2016, 2019),
                             myers=c(15.3, 12.6, 9.54, 9.92),
                             country="Egypt",
                             source="Arab Barometer")
myers_ab_egypt

myers_ec_egypt <- data.frame(year=c(1996, 2006),
                             myers=c(20.5, 17.6),
                             country="Egypt",
                             source="Egypt Census")
myers_ec_egypt

myers_egypt <- rbind(myers_wv_egypt, myers_ab_egypt, myers_ec_egypt)
myers_egypt_name <- myers_egypt %>%
  group_by(source) %>%
  summarise(value = first(myers))


ggplot(myers_egypt, aes(year,  color=source)) +
  geom_point(aes(y = myers), size = 3) +
  geom_line(aes(y = myers), size = 1) +
  scale_color_viridis_d() +
  scale_x_continuous(limits = c(1995,2020),
                     breaks=seq(1995,2020,5)) +
  scale_y_continuous(limits=c(5,25),
                     breaks=seq(5,25,5)) +
  theme(legend.position = "right",
        axis.title.y=element_text(angle=0, vjust=.5)) +
  labs(x="",
       y="",
       title="Myers Index for Egypt",
       color = "Source")


,
sec.axis=sec_axis(~.,
                  breaks=myers_egypt_name$value,
                  labels=myers_egypt_name$source)
