library(scales)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
register_google(key="AIzaSyBazeoZ-Ielur29d_4REgdQRd3WGnA5DJo")
DrugData <- read.csv("/Users/jiwonyun/Desktop/DNSC 4211/slp/20_AccidentalDrugRelatedDeaths_CT.csv")


#Years and drug use overtime
summary(DrugData)
DrugData$Date <- as.Date(DrugData$Date, format = "%m/%d/%y")
DrugData$Year <- format(DrugData$Date, "%Y")


#Seprating by years
counts_by_year <- DrugData %>% group_by(Year) %>% summarise(Count = n())
counts_by_year <- na.omit(counts_by_year)
counts_by_year$Percentage <- counts_by_year$Count / sum(counts_by_year$Count) * 100


# Create a pie chart
pie_chart <- ggplot(counts_by_year, aes(x = "", y = Count, fill = as.factor(Year))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  ggtitle("Pie Chart of Counts by Year")
# Print the pie chart
print(pie_chart)




#Drug Combination Trends
drug_combinations <- DrugData %>%
  group_by(Year) %>%
  summarize(
    Heroin = sum(Heroin == "Y", na.rm = TRUE),
    Cocaine = sum(Cocaine == "Y", na.rm = TRUE),
    Fentanyl = sum(Fentanyl == "Y", na.rm = TRUE),
    Oxycodone = sum(Oxycodone == "Y", na.rm = TRUE),
    Oxymorphone = sum(Oxymorphone == "Y", na.rm = TRUE),
    EtOH = sum(EtOH == "Y", na.rm = TRUE),
    Hydrocodone = sum(Hydrocodone == "Y", na.rm = TRUE),
    Benzodiazepine = sum(Benzodiazepine == "Y", na.rm = TRUE),
    Methadone = sum(Methadone == "Y", na.rm = TRUE),
    Amphet = sum(Amphet == "Y", na.rm = TRUE),
    Tramad = sum(Tramad == "Y", na.rm = TRUE),
    Morphine = sum(Morphine..not.heroin. == "Y", na.rm = TRUE),
    Other = sum(Other == "Y", na.rm = TRUE),
    AnyOpioid = sum(Any.Opioid == "Y", na.rm = TRUE)
  )
drug_combinations$Year <- as.numeric(drug_combinations$Year)
# Reshape data for better visualization
reshaped_data <- drug_combinations %>%
  pivot_longer(cols = -Year, names_to = "Drug", values_to = "Count")
# Create a ggplot using geom_line
ggplot(reshaped_data, aes(x = Year, y = Count, color = Drug)) +
  geom_line(size = 1) +
  labs(title = "Trends in Drug Combinations/Lacing Over Time",
       x = "Year",
       y = "Count") +
  scale_color_manual(values = c("Heroin" = "blue", "Cocaine" = "red", "Fentanyl" = "green", "Oxycodone" = "black","Oxymorphone" ="white","EtOH" = "purple","Hydrocodone" = "pink","Benzodiazepine" = "skyblue","Methadone" = "navy","Amphet" = "orange","Tramad" = "yellow","Morphine" = "brown","Other" = "violet","AnyOpioid" = "hotpink"))



#Form of Opioids
heroin_count <- sum(DrugData$Heroin == "Y")
fentanyl_count <- sum(DrugData$Fentanyl == "Y")
oxycodone_count <- sum(DrugData$Oxycodone == "Y")
oxymorphone_count <- sum(DrugData$Oxymorphone == "Y")
hydrocodone_count <- sum(DrugData$Hydrocodone == "Y")
methadone_count <- sum(DrugData$Methadone == "Y")
tramadol_count <- sum(DrugData$Tramad == "Y")
OpioidDeaths <- data.frame(
  OpioidForm = c("Heroin", "Fentanyl", "Oxycodone", "Oxymorphone","Hydrocodone","Methadone", "Tramadol"),
  AccidentalDeaths = c(heroin_count, fentanyl_count, oxycodone_count, oxymorphone_count, hydrocodone_count, methadone_count, tramadol_count )
)


# Create a bar chart
ggplot(OpioidDeaths, aes(x = OpioidForm, y = AccidentalDeaths, fill = OpioidForm)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Forms of Opioids Leading to Accidental Deaths",
    x = "Opioid Form",
    y = "Accidental Deaths"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#Getting Lat and Long from Address#
DrugData$addy <- ifelse(DrugData$Residence.State == "", 
                        paste(DrugData$Residence.City, DrugData$Residence.County, ", CT", sep = ", "),
                        paste(DrugData$Residence.City, DrugData$Residence.County, DrugData$Residence.State, sep = ", "))
locationData = table(DrugData$addy)
locationData <- as.data.frame(locationData)
colnames(locationData) <- c('Addy', 'Count')
locationData <- locationData[-1, ]

locationData$Addy <- as.character(locationData$Addy)
geocoded_data <- geocode(locationData$Addy)
locationData <- cbind(locationData, geocoded_data)

locationData <- locationData[order(locationData$Count, decreasing = TRUE), ]

model <- lm(Count ~ lon + lat, data = locationData)
summary(model)





ggplot(data = locationData, aes(x = lon, y = lat)) +
  stat_density_2d(aes(fill = ..level.., alpha = ..level..), geom = "polygon") +
  scale_fill_viridis_c() +
  geom_point(aes(size = Count), color = "red", alpha = 0.5) + 
  scale_alpha(range = c(0.3, 0.7), guide = FALSE) +
  labs(title = "Address Frequency Heatmap", x = "Longitude", y = "Latitude")