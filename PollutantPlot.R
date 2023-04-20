# load the packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

# must have the standardised group column named as: q5a_std_mc in the
# tblcompldf datafile

plotthepollutants<-function(tblabstdf, tblcompldf, tblrefdf, outputfolder){

# pollutant data
polldata<-readxl::read_xlsx(tblabstdf)
### just select those that are in ###

# standardised groups
polldata_com<-readxl::read_xlsx(tblcompldf) %>%
  select(rec_no, q5a_std_mc)
polldata<-merge(polldata,polldata_com, by=("rec_no"))

# author name and year
refdata<-readxl::read_xlsx(tblrefdf)
refdata<-refdata %>% select(rec_no, surnamefirst, pubyear)
polldata<-merge(polldata, refdata, by="rec_no")
polldata$citation<-paste0(polldata$surnamefirst, " (", polldata$pubyear,")")

#clean up
polldata <- polldata %>% select(citation, q3, q5a_std_mc)
polldata<-polldata[!is.na(polldata$q5a_std_mc),]

polldata2 <- polldata %>% separate(q3, c("p1", "p2",
                                         "p3", "p4",
                                         "p5"), ";")

# Reshape the data to long format
PM_long <- polldata2 %>%
  pivot_longer(cols = starts_with("p"), names_to = "variable", values_to = "value") %>%
  filter(value != "")

# so we have the most important up top
PM_long$value <- factor(PM_long$value, levels = c("NO2", "PM2.5", "PM10", "O3", "SO2"))
PM_long$value2 <- ifelse(PM_long$value !="", 1,0)

# to get total studies - redundant
PM_long_total <- PM_long %>%
  group_by(variable) %>%
  summarize(total_freq = sum(value2))
PM_long<-merge(PM_long, PM_long_total, by=c("variable"))
PM_long<-PM_long %>% arrange(value)


# Create the plot
out<-
  ggplot(PM_long, 
         aes(x = citation, y = value, color = value)) +
  geom_point(size=5) +
  labs(x = "", y = "", color = "Pollutant")+
  theme(
    axis.text = element_text(size = 12),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    strip.text.x = element_text(size = 20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 16),
    panel.grid.major.x = element_line(color="#F1F1F1"),
    panel.grid.major.y = element_line(color="#F1F1F1"),
    panel.background = element_blank())+
  coord_flip() +
  facet_wrap(~q5a_std_mc, scales = "free_y")

# output the plot
ggsave(paste0(outputfolder,"pollutantplot.png"),
       height = 25,
       width=40,
       units = c("cm"))
}

