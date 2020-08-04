library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)

data_file <- "repdata_data_StormData.csv.bz2"

data <- read.csv(data_file)

data_deaths <- data %>%
                group_by(event = EVTYPE) %>%
                summarize(deaths = sum(FATALITIES)) %>%
                arrange(desc(deaths)) %>%
                top_n(10, wt = deaths)

data_deaths

data_injuries <- data %>%
        group_by(event = EVTYPE) %>%
        summarize(injuries = sum(INJURIES)) %>%
        arrange(desc(injuries)) %>%
        top_n(10, wt = injuries)

data_injuries[1,]

death_plot <- ggplot(data_deaths, aes(event, deaths)) +
        geom_bar(stat = "identity", aes(fill = event)) +
        ylab("Total Deaths") +
        xlab("Event Type") +
        theme(axis.text.x = element_text(angle = 90, size = 6),
                legend.position = "none",
                axis.title.x = element_text(color = "blue", 
                        size = 12),
                axis.title.y = element_text(color = "blue", 
                        size = 12))
        
injuries_plot <- ggplot(data_injuries, aes(event, injuries)) +
        geom_bar(stat = "identity", aes(fill = event)) +
        ylab("Total Injuries") +
        xlab("Event Type") +
        theme(axis.text.x = element_text(angle = 90, size =6),
                legend.position = "none",
        axis.title.x = element_text(color = "blue", 
                size = 12),
        axis.title.y = element_text(color = "blue", 
                size = 12))

ggarrange(death_plot, injuries_plot, ncol = 2, 
        labels = c("Total Deaths per Event Type", 
                "Total Injuries per Event Type"),
        font.label = list(color = "blue"))

data_damage <- data %>%
        group_by(event = EVTYPE) %>%
        summarize(property_damage = sum(PROPDMG)/1000,
                crop_damage = sum(CROPDMG)/1000,
                total_damage = (property_damage + crop_damage)) %>%
        arrange(desc(total_damage)) %>%
        top_n(10, wt = total_damage)

data_damage

ggplot(data_damage, aes(event, total_damage)) +
        geom_bar(stat = "identity", aes(fill = event)) +
        ylab("Total Damage (,000s)") +
        xlab("Event Type") +
        ggtitle("Total Damage per Event Type") +
        theme(axis.text.x = element_text(angle = 90, size =6),
                legend.position = "none", 
                plot.title = element_text(hjust = 0.5, color = "blue", 
                        size = 16, face = "bold"),
                axis.title.x = element_text(color = "blue", 
                        size = 12, face ="bold"),
                axis.title.y = element_text(color = "blue", 
                        size = 12, face ="bold"))

