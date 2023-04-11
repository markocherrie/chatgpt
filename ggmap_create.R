library(readxl)


#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
#system("unzip data/world_shape_file.zip")


world<-sf::read_sf("data/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp")

world<-world[world$NAME!="Antarctica",]

df<-read.csv("data/geodata_defra.csv")
dput(df)

world<-merge(world, df, by.x="NAME", by.y="Country_clean", all.x=T)

library(fastDummies)
world_dummies <- dummy_cols(world, select_columns = "Group", remove_first_dummy = FALSE)



world<-cbind(world, world_dummies[,16:24])


replace_zeros_with_nas <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

library(dplyr)

world<-world %>% 
  mutate_at(vars(contains('Group_')), funs(replace_zeros_with_nas))%>%
              mutate_at(vars(contains('Group_')), funs(as.factor))

library(sf)

# repeat for each group
# "Group_Children"          "Group_Commuters"         "Group_Ethnic.Minorities"
# "Group_Low.SES"           "Group_Physical.activity" "Group_Pregnant.Women"   
# "Group_Workers"           
library(ggplot2)

for(i in c("Group_Children","Group_Commuters","Group_Ethnic.Minorities",
   "Group_Low.SES","Group_Physical.activity","Group_Pregnant.Women",   
   "Group_Workers")){
out<-ggplot(world) + 
  geom_sf(aes_string(fill=i))+ 
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values = c("#D3D3D3", "#8ec045"))+
  ggtitle(gsub("(\\_|Group|\\.)", " ",i))

ggsave( paste0("output/", gsub("\\_", " ",i),".png"), out, dpi = 500)
}


###
# Load necessary libraries
library(ggplot2)
library(sp)
library(crosstalk)

# Load data
data(nc)

# Create a ggplot object
p <- ggplot(nc, aes(fill = SID74, map_id = NAME)) + 
  geom_map(map = nc_map) +
  expand_limits(x = nc_map$range$longitude, y = nc_map$range$latitude) +
  scale_fill_gradient(low = "white", high = "red", na.value = "grey50",
                      guide = "colorbar", name = "SID74") +
  labs(title = "North Carolina Counties, 1974")

# Create a SharedData object
sd <- SharedData$new(nc)

# Create a filter_select widget
fs <- filter_select("county", "Select county", sd, ~NAME, multiple = FALSE)

# Create a bscols layout with the ggplot and filter_select widget
bscols(p, fs)






