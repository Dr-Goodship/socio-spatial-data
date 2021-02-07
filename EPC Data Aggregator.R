library(plyr)
library(zip)
library(dplyr)
library(readr)
library(tidyverse)
library(data.table)

#zipF<- "E:/Transfer/EPC Data/all-domestic-certificates.zip"
#outDir<-"E:/Transfer/EPC Data/allDomestic_ouput"
#unzip(zipF,exdir=outDir)

setwd("/Users/paulgoodship/Documents/Data Projects/Housing Stock Model/EPC/all-display-certificates")

filenames <- list.files(pattern ="certificates.csv", recursive = T)

All <- lapply(filenames,function(i){
  read.csv(i, header=FALSE, skip=4)
})

df <- do.call(rbind.data.frame, All)

write.csv(df,'all_postcodes.csv', row.names=FALSE)

getwd()
#join all data
epc_MergedCert <-
  do.call(rbind,
          lapply(list.files(pattern="certificates.csv", recursive = T), read.csv))

epc_MergedRecom <-
  do.call(rbind,
          lapply(list.files(pattern="recommendations.csv", recursive = T), read.csv))

postcode_Merge <- do.call(rbind,
                             lapply(list.files(pattern="postcodes.csv", recursive = T), read.csv))


#merge all data
epcPostcodeData <- merge(epc_MergedCert, postcode_Merge, by.x = "POSTCODE", by.y = "Postcode", all.x = TRUE)
epcPostcodeData$LODGEMENT_DATE <- as.Date(epcPostcodeData$LODGEMENT_DATE)
epcPostcodeData$LODGEMENT_DATETIME <- as.Date(epcPostcodeDate$LODGEMENT_DATETIME)

#check data
str(epcPostcodeData)

#filter to latest data
epcDateFiltered <- epcPostcodeData %>% 
  group_by(ADDRESS1, ADDRESS2, ADDRESS3, POSTCODE) %>%
  filter(LODGEMENT_DATE == max(LODGEMENT_DATE))

#tenure data
epcTenure <- epcDateFiltered %>%
  group_by(TENURE, LSOA.Code) %>% 
  filter(TENURE == 'rental (social)' | TENURE == 'rental (private)') %>%
  summarise(total = n()) %>%
  group_by(LSOA.Code) %>%
  mutate(Percentage = total/sum(total))

tenureWide <- dcast(setDT(epcTenure), LSOA.Code ~ TENURE, value.var = c("total","Percentage"))
names(tenureWide) <- gsub("total_", "Tenure Total: ", names(tenureWide))
names(tenureWide) <- gsub("Percentage_", "Tenure Percentage: ", names(tenureWide))

write.csv(tenureWide, "tenureWide.csv")



############## 2020.10.05 update ##############

#property type
epcPropertyType_GM <- epcDateFiltered_DATA %>%
  group_by(PROPERTY_TYPE, LSOA.Code) %>% 
  #filter(TENURE == 'rental (social)' | TENURE == 'rental (private)') %>%
  summarise(total = n()) %>%
  group_by(LSOA.Code) %>%
  mutate(Percentage = total/sum(total))

propertyTypeGM_Wide <- dcast(setDT(epcPropertyType_GM), LSOA.Code ~ PROPERTY_TYPE, value.var = c("total","Percentage"))
names(propertyTypeGM_Wide) <- gsub("total_", "Property Type Total: ", names(propertyTypeGM_Wide))
names(propertyTypeGM_Wide) <- gsub("Percentage_", "Property Type Percentage: ", names(propertyTypeGM_Wide))

write.csv(propertyTypeGM_Wide, "propertyTypeGM_Wide.csv")


#built form
epcBuiltForm_GM <- epcDateFiltered_DATA %>%
  group_by(BUILT_FORM, LSOA.Code) %>%
  filter(BUILT_FORM != 'NO DATA!') %>%
  summarise(total = n()) %>%
  group_by(LSOA.Code) %>%
  mutate(Percentage = total/sum(total))

builtFormGM_Wide <- dcast(setDT(epcBuiltForm_GM), LSOA.Code ~ BUILT_FORM, value.var = c("total","Percentage"))
names(builtFormGM_Wide) <- gsub("total_", "Built Form Total: ", names(builtFormGM_Wide))
names(builtFormGM_Wide) <- gsub("Percentage_", "Built Form Percentage: ", names(builtFormGM_Wide))

write.csv(builtFormGM_Wide, "builtFormGM_Wide.csv")


#floor area
epcFloor_Area_GM <- epcDateFiltered_DATA %>%
  group_by(TENURE, LSOA.Code) %>% 
  filter(TENURE == 'rental (social)' | TENURE == 'rental (private)') %>%
  summarise(Average = mean(TOTAL_FLOOR_AREA))

floorArea_Wide <- dcast(setDT(epcFloor_Area_GM), LSOA.Code ~ TENURE, value.var = "Average")
names(floorArea_Wide) <- gsub("rental (social)", "Average Floor Area: rental (social)", names(floorArea_Wide))
names(floorArea_Wide) <- gsub("rental", "Average Floor Area: ", names(floorArea_Wide))

write.csv(floorArea_Wide, "floorArea_Wide.csv")

str(floorArea_Wide)


#floor level
epcfloorLevel_GM <- epcDateFiltered_DATA %>%
  group_by(FLOOR_LEVEL, LSOA.Code) %>% 
  summarise(total = n()) %>%
  group_by(LSOA.Code) %>%
  #filter out no data
  mutate(Percentage = total/sum(total))

floorLevelGM_Wide <- dcast(setDT(epcfloorLevel_GM), LSOA.Code ~ FLOOR_LEVEL, value.var = c("total","Percentage"))
names(floorLevelGM_Wide) <- gsub("total_", "Floor Level Total: ", names(floorLevelGM_Wide))
names(floorLevelGM_Wide) <- gsub("Percentage_", "Floor Level Percentage: ", names(floorLevelGM_Wide))

write.csv(floorLevelGM_Wide, "floorLevelGM_Wide.csv")


#flat storey count
epcFlatStoreyCount_GM <- epcDateFiltered_DATA %>%
  group_by(FLAT_STOREY_COUNT, LSOA.Code) %>% 
  summarise(total = n()) %>%
  group_by(LSOA.Code) %>%
  #filter out no data
  mutate(Percentage = total/sum(total))

flatStoreyCountGM_Wide <- dcast(setDT(epcFlatStoreyCount_GM), LSOA.Code ~ FLAT_STOREY_COUNT, value.var = c("total","Percentage"))
names(flatStoreyCountGM_Wide) <- gsub("total_", "Flat Storey Count Total: ", names(flatStoreyCountGM_Wide))
names(flatStoreyCountGM_Wide) <- gsub("Percentage_", "Flat Storey Count Percentage: ", names(flatStoreyCountGM_Wide))

write.csv(bflatStoreyCountGM_Wide, "flatStoreyCountGM_Wide.csv")


#habitable room no
epcHabitableRMs_GM <- epcDateFiltered_DATA %>%
  group_by(TENURE, LSOA.Code) %>% 
  filter(TENURE == 'rental (social)' | TENURE == 'rental (private)') %>%
  summarise(total = n(), Average = mean(NUMBER_HABITABLE_ROOMS))

habitableRooms_Wide <- dcast(setDT(epcHabitableRMs_GM), LSOA.Code ~ TENURE, value.var = c("total", "Average"))
names(habitableRooms_Wide) <- gsub("Average_", "Average Number of Habitable Rooms: ", names(habitableRooms_Wide))
names(habitableRooms_Wide) <- gsub("total_", "Average Number of Habitable Rooms: Total ", names(habitableRooms_Wide))

write.csv(habitableRooms_Wide, "habitableRooms_Wide.csv")


#heated rom count
epcHeatedRMs_GM <- epcDateFiltered_DATA %>%
  group_by(TENURE, LSOA.Code) %>% 
  filter(TENURE == 'rental (social)' | TENURE == 'rental (private)') %>%
  summarise(total = n(), Average = mean(NUMBER_HEATED_ROOMS))

heatedRooms_Wide <- dcast(setDT(epcHeatedRMs_GM), LSOA.Code ~ TENURE, value.var = c("total", "Average"))
names(heatedRooms_Wide) <- gsub("Average_", "Average Number of Heated Rooms: ", names(heatedRooms_Wide))
names(heatedRooms_Wide) <- gsub("total_", "Average Number of Heated Rooms: Total ", names(heatedRooms_Wide))

write.csv(heatedRooms_Wide, "heatedRooms_Wide.csv")


#building age
epcBuildingAge <- epcDateFiltered_DATA %>%
  group_by(CONSTRUCTION_AGE_BAND, LSOA.Code) %>% 
  filter(CONSTRUCTION_AGE_BAND != 'NO DATA!' & CONSTRUCTION_AGE_BAND != 'INVALID!') %>%
  summarise(total = n()) %>%
  group_by(LSOA.Code) %>%
  #filter out no data
  mutate(Percentage = total/sum(total))

buildingAge_Wide <- dcast(setDT(epcBuildingAge), LSOA.Code ~ CONSTRUCTION_AGE_BAND, value.var = c("total","Percentage"))
names(buildingAge_Wide) <- gsub("total_", "Building Age Total: ", names(buildingAge_Wide))
names(buildingAge_Wide) <- gsub("Percentage_", "Building Age Percentage: ", names(buildingAge_Wide))

write.csv(bflatStoreyCountGM_Wide, "flatStoreyCountGM_Wide.csv")


#average distance from station
epcAverageDistanceStation_GM <- epcDateFiltered_DATA %>%
  group_by(TENURE, LSOA.Code) %>% 
  filter(TENURE == 'rental (social)' | TENURE == 'rental (private)') %>%
  summarise(total = n(), Average = mean(Distance.to.station))%>%
  group_by(LSOA.Code) %>%
  mutate(Percentage = total/sum(total))

distanceToStation_Wide <- dcast(setDT(epcAverageDistanceStation_GM), LSOA.Code ~ TENURE, value.var = c("total", "Average"))
names(distanceToStation_Wide) <- gsub("Average_", "Average Distance to Station: ", names(distanceToStation_Wide))
names(distanceToStation_Wide) <- gsub("total_", "Average Distance to Station: Total ", names(distanceToStation_Wide))

write.csv(distanceToStation_Wide, "distanceToStation_Wide.csv")


# merge all
data_list <- list(tenureGM_Wide, propertyTypeGM_Wide, builtFormGM_Wide, floorArea_Wide, habitableRooms_Wide, heatedRooms_Wide, 
                  buildingAge_Wide, distanceToStation_Wide)

mergeGM_EPC <- data_list %>% 
  reduce(inner_join, by = "LSOA.Code")  

mergeGM_EPC <- gsub("NA", "0", mergeGM_EPC)
merge

write.csv(mergeGM_EPC, "mergeGM_EPC.csv")


