library(readxl)

mapdf<-readxl::read_xlsx("data/tbl_completed_std.xlsx")

library(dplyr)
mapdf<-mapdf %>% select(study_ID, q5a_std, geo)