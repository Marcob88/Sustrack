library(tidyr)
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(readxl)
library(shinythemes)
library(rsconnect)
library(readr)
library(DT)
library(shinyBS)
library(shinydashboard)
library(leaflet)
library(rworldmap)
library(sf)
library(plotly)
library(eurostat)

# Import the database
EU_bioeconomy_dashboard <- read_excel("C:/Users/109490/OneDrive - Fundacion Tecnalia Research & Innovation/Escritorio/Paper SUSTRACK/SUSTRACK/EU bioeconomy_dashboard.xlsx", sheet = 1)

# Filter rows with years greater than or equal to 2010
EU_bioeconomy_dashboard <- EU_bioeconomy_dashboard %>%
  filter(time >= 2015)

# Filter rows with European countries and remove columns "type label" and "indicator_id"
# Define the list of geo_code values
geo_code_names <- c(
  "AD" = "Andorra", "AL" = "Albania", "AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria",
  "CH" = "Switzerland", "CY" = "Cyprus", "CZ" = "Czech Republic", "DE" = "Germany", "DK" = "Denmark",
  "EE" = "Estonia", "EL" = "Greece", "ES" = "Spain", "EU27_2020" = "EU27 (2020)", "EU28" = "EU28",
  "FI" = "Finland", "FR" = "France", "HR" = "Croatia", "HU" = "Hungary", "IE" = "Ireland",
  "IS" = "Iceland", "IT" = "Italy", "LT" = "Lithuania", "LU" = "Luxembourg", "LV" = "Latvia",
  "MD" = "Moldova", "ME" = "Montenegro", "MT" = "Malta", "NL" = "Netherlands", "NO" = "Norway",
  "PL" = "Poland", "PT" = "Portugal", "RO" = "Romania", "RS" = "Serbia", "SE" = "Sweden",
  "SI" = "Slovenia", "SK" = "Slovakia", "UK" = "United Kingdom"
)

# Assuming your dataframe is named 'df'
EU_bioeconomy_dashboard <- EU_bioeconomy_dashboard %>%
  filter(geo_code %in% names(geo_code_names)) %>%
  select(-c(5, 6, 8, 10,11,12,13,15,17,19,20,21,22,23,24,25,26:33)) %>%
  mutate(Country = geo_code_names[geo_code])%>%
  relocate(Country, .after = geo_code)

# Reshape the dataframe from long to wide format, considering both indicator_name and type
panel_data_wide <- pivot_wider(EU_bioeconomy_dashboard, 
                               names_from = c(indicator_name, type), 
                               values_from = value)

#get population
population_data <- get_eurostat("demo_r_pjangrp3", time_format = "num", stringsAsFactors = TRUE, sinceTimePeriod=2015)%>%
  filter(age == "TOTAL")%>%
  filter(sex == "T")%>%
  select(-1:-4) %>% 
  rename(
    geo_code = "geo",
    time = "TIME_PERIOD",
    population = "values"
  )

merged_panel_data_wide <- merge(panel_data_wide, population_data, by = c("geo_code", "time" ), all.x = TRUE)

colnames(merged_panel_data_wide)

#Re-name columns (1 to 111)
new_names <- c(
  "Geo_code",
  "Year",
  "Country",
  #"objective_id",                                                                                                                                          
  "objective_name",                                                                                                                                        
  #"norm_crit_id",                                                                                                                                          
  #"norm_crit_name",                                                                                                                                        
  #"key_comp_id",                                                                                                                                           
  #"key_comp_name",                                                                                                                                         
  "sdg_ids",                                                                                                                                               
  #"green_deal_names",                                                                                                                                      
  "description",                                                                                                                                           
  #"references",                                                                                                                                            
  "unit",                                                                                                                                                  
  # "directionality",                                                                                                                                        
  # "first_published_kcb",                                                                                                                                   
  # "source",                                                                                                                                                
  # "link",                                                                                                                                                  
  # "val_chain_step",                                                                                                                                        
  # "primary_production_sector",                                                                                                                             
  # "geo_coverage",                                                                                                                                          
  # "geog_comparable",                                                                                                                                       
  # "frequency",                                                                                                                                             
  # "timeliness",                                                                                                                                            
  # "time_series_length",                                                                                                                                    
  # "comparable_over_time",                                                                                                                                  
  # "used_elsewhere",                                                                                                                                        
  # "link_used_elsewhere",                                                                                                                                   
  # "last_date_data_generated", 
  "Agricultural factor income per annual work unit (AWU)",
  "Total biomass supply for food purposes including inputs (kilotonnes dry matter)",
  "Biomass directly consumed by EU citizens as food (kilotonnes dry matter)",
  "Prevalence of moderate or severe food insecurity in the total population (percentage)",  
  "Food purchasing power (food and non-alcoholic beverages)",
  "Daily calorie supply per capita by animal source",
  "Daily calorie supply per capita by animal source (share)",
  "Total daily calorie supply per capita",
  "Daily calorie supply per capita by vegetal source",
  "Daily calorie supply per capita by vegetal source (share)",
  "Government support to agricultural research and development (by sector) euro per capita",
  "Biochemical oxygen demand in rivers (mg o2/l)",
  "Phosphate in rivers (mg PO4/l)",
  "Nitrate in groundwater (NO3/l)",
  "Share of organic farming in utilised agricultural area (percentage)",
  "Livestock density index (unit per ha)",
  "Forest growing stock (1000 m3)",
  "Bird and butterfly indices EU aggregate (common farmland bird Index, common forest bird index, grassland butterfly index)",
  "Bird and butterfly indices EU aggregate (common farmland bird Index, common forest bird index, grassland butterfly index)",
  "Surface of terrestrial sites designated under NATURA 2000 (km2)",
  "Surface of terrestrial sites designated under NATURA 2000 (percentage)",
  "Surface of marine sites designated under NATURA 2000 (km2)",
  "Ratio of annual fellings (m3/ha/year) to net annual increment (m3/ha/year)",
  "Ratio of annual fellings (m3/ha/year) to net annual increment (m3/ha/year)_trend_fellings",
  "Intensification of farming (share of high input farms in UAA)",
  "Intensification of farming (share of low input farms in UAA)",
  "Intensification of farming (share of medium input farms in UAA)",
  "Intensification of farming (share of high, medium and low input farms in UAA)trend_LOW_INP_trend_LOW_INP_",
  "Biomass production from agriculture (tonnes dry matter)",
  "Biomass production from fisheries (tonnes dry matter)",
  "Biomass production from forestry (tonnes dry matter)",
  "Roundwood removals (m3 overbark roundwood removals)",
  "Roundwood removals (m3 underbark roundwood removals)",
  "Domestic Material Consumption (biomass share)",
  "Material footprint (Biomass)(kg per dollar of GDP in USD)",
  "Energy productivity (EUR per KG of oil equivalent)",
  "Share of renewable energy in gross final energy consumption",
  "Cascade use of wood resources (rest of biomass)",
  "Cascade use of wood resources (secondary woody biomass used for material industry)(1000 m3 solid wood equivalent)",
  "Cascade use of wood resources (secondary woody biomass used for energy)(1000 m3 solid wood equivalent)",
  "Cascade use of wood resources (share of secondary woody biomass for energy)",
  "Cascade use of wood resources (share of secondary woody biomass for material industry)",
  "Cascade use of wood resources (total uses of woody biomass)(1000 m3 solid wood equivalent)",
  "Circular material rate (percentage)",
  "Recycling rate of municipal waste",
  "Biowaste generated by household (kilotonnes dry)",
  "Biowaste generated by industry and agriculture (kilotonnes dry)",
  "Total generated biowaste (kilotonnes dry)",
  "Household - Paper and cardboard wastes (W072)",
  "Industrial and agricultural paper and cardboard wastes (W072)",
  "Industrial rubber wastes (W073)",
  "Household - Untreated wood (W075)",
  "Industrial wood wastes (W075)",
  "Industrial textile wastes (W076)",
  "Household food waste (W091)",
  "Industrial animal and mixed food waste (W091)",
  "Household green waste (W092)",
  "Industrial vegetal waste (W092)",
  "Industrial animal feces, urine and manure waste (W093)",
  "Households textiles, leather and rubber waste (W7376)",
  "Household composites, human hygiene waste (W9999)",
  "Total disposed biowaste",
  "Recovered household biowaste (kilotonnes dry)",
  "Recovered industrial and agricultural biowaste (kilotonnes dry)",
  "Total recovered biowaste (kilotonnes dry)",
  "RCV Household paper and cardboard wastes (W072)",
  "RCV Industrial and agricultural paper and cardboard wastes (W072)",
  "RCV Industrial rubber wastes (W073)",
  "RCV Household untreated wood (W075)",
  "RCV Industrial wood wastes (W075)",
  "RCV Industrial textile wastes (W076)",
  "RCV Household food waste (W091)",
  "RCV Industrial animal and mixed food waste (W091)",
  "RCV Household green waste (W092)",
  "RCV Industrial vegetal wastes (W092)",
  "RCV Industrial animal feces, urine and manure (W093)",
  "RCV Household textiles, leather and rubber (W7376)",
  "RCV Household composites, human hygiene waste (W9999)",
  "Total food waste generation along supply chain (tonnes)",
  "Food services consumption (Food waste) (tonnes)",
  "Household consumption (Food waste) (tonnes)",
  "Primary production (Food waste) (tonnes)",
  "Processing and manufacturing (Food waste) (tonnes)",
  "Retail and distribution (Food waste) (tonnes)",
  "Total food waste by food category",
  "Cereals (Food waste) (tonnes)",
  "Cocoa and coffee (Food waste) (tonnes)",
  "Dairy (Food waste) (tonnes)",
  "Eggs (Food waste) (tonnes)",
  "Fish (Food waste) (tonnes)",
  "Fruits and nuts (Food waste) (tonnes)",
  "Meat (Food waste) (tonnes)",
  "Oilseeds (Food waste) (tonnes)",
  "Potatoes (Food waste) (tonnes)",
  "Sugar beets (Food waste) (tonnes)",
  "Vegetables (Food waste) (tonnes)",
  "Total biomass consumed for energy (kilotonnes dry matter)",
  "Total biomass consumed for materials (kilotonnes dry matter)",
  "Share of woody biomass used for energy (percent)",
  "Share of woody biomass used for materials (percent)",
  "Share of renewables for transport, electricity and heating & cooling (percent)",
  "Share of renewables for electricity (percentage)",
  "Share of renewables for heating & cooling (percentage)",
  "Share of renewables for transport (percentage)"
)

# Rename 1 to 111 columns
colnames(merged_panel_data_wide)[1:111] <- new_names

# Write the CSV file and upload (manually) to GitHub. 
#Pay attention to keep the same name as the one used in the app the upload the dataset, this is datasetmerged.csv.
#as a good practice save the old version with another name before substituting it with the new version.


write.csv(merged_panel_data_wide, "datasetmerged.csv", row.names = FALSE)

