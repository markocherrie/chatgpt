#PM<-read.csv("C:/Users/mark.cherrie/Desktop/pollutantdata.csv")
#dput(PM)

#####
PM<-structure(list(p1 = c("NO2", "NO2", "NO2", "NO2", "NO2", "NO2", 
                          "", "", "", "", "", "", "", "", "NO2", "NO2", "", "", "", "NO2", 
                          "", "NO2", "NO2", "", "NO2", "NO2", "", "", "", "NO2", "", "NO2", 
                          "", "NO2", "NO2", "", "NO2", "", "", "", "NO2", "NO2", "NO2", 
                          "NO2", "NO2", "NO2", "NO2", "NO2", "NO2", "NO2", "NO2", "NO2", 
                          "NO2", "", "NO2", "", "NO2", "NO2", "NO2", ""), p2 = c("O3", 
                                                                                 "O3", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                                                                                 "O3", "", "O3", "", "O3", "O3", "", "", "O3", "", "", "O3", "", 
                                                                                 "", "", "", "", "", "", "", "O3", "", "O3", "", "", "", "", "", 
                                                                                 "", "", "", "O3", "", "", "", "O3", "", "", "O3", "", "", "", 
                                                                                 "", ""), p3 = c("", "", "", "", "PM10", "", "", "", "", "", "PM10", 
                                                                                                 "PM10", "PM10", "", "", "", "PM10", "", "", "", "", "PM10", "", 
                                                                                                 "", "PM10", "", "PM10", "", "", "", "", "", "PM10", "", "", "", 
                                                                                                 "PM10", "", "", "", "PM10", "PM10", "", "", "", "", "PM10", "PM10", 
                                                                                                 "", "PM10", "PM10", "", "", "PM10", "", "PM10", "", "PM10", "PM10", 
                                                                                                 ""), p4 = c("PM2.5", "PM2.5", "PM2.5", "", "", "", "PM2.5", "PM2.5", 
                                                                                                             "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "", "", 
                                                                                                             "PM2.5", "PM2.5", "", "PM2.5", "", "PM2.5", "", "PM2.5", "", 
                                                                                                             "PM2.5", "PM2.5", "PM2.5", "PM2.5", "", "PM2.5", "PM2.5", "PM2.5", 
                                                                                                             "", "PM2.5", "PM2.5", "", "PM2.5", "", "PM2.5", "PM2.5", "", 
                                                                                                             "", "", "", "", "PM2.5", "PM2.5", "", "", "", "", "", "PM2.5", 
                                                                                                             "", "PM2.5", "", "", "PM2.5", ""), p5 = c("SO2", "", "", "", 
                                                                                                                                                       "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
                                                                                                                                                       "", "SO2", "", "", "SO2", "", "", "", "", "", "", "", "", "", 
                                                                                                                                                       "", "", "", "", "", "", "", "", "", "", "", "", "", "SO2", "", 
                                                                                                                                                       "", "SO2", "", "", "", "", "", "", "SO2", "", "")), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                               -60L))



####
library(tidyverse)

# Reshape the data to long format
PM_long <- PM %>%
  mutate(row = row_number()) %>%
  pivot_longer(cols = starts_with("p"), names_to = "variable", values_to = "value") %>%
  filter(value != "")



PM_long$value <- factor(PM_long$value, levels = c("NO2", "PM2.5", "PM10", "O3", "SO2"))

PM_long$value2 <- ifelse(PM_long$value !="", 1,0)


PM_long_total <- PM_long %>%
  group_by(variable) %>%
  summarize(total_freq = sum(value2))

PM_long<-merge(PM_long, PM_long_total, by=c("variable"))

#PM_long$total_freq<- paste0("n = ", PM_long$total_freq)

# Create a scatterplot of row vs value
out<-
  ggplot(PM_long, 
         aes(x = row, y = value, color = variable)) +
  geom_point(size=3) +
  #geom_text(aes(x=65, label = total_freq),
  #          vjust = 0, size = 4, color = "black")+
  labs(x = "Study Number", y = "", color = "Variable")+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12))

ggsave("C:/Users/mark.cherrie/Desktop/pollutantgraph.png",
       height = 4,
       width=20,
       units = c("cm"))