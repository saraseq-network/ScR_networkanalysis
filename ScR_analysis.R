
#####################################  Scoping Review   ###############################################          

library(tidyverse)
library(usmap)
library(epiDisplay)
library(gridExtra)


database <- read.csv("~/OneDrive - The Ohio State University/Desktop/All OSU/Thesis/1. Scoping Review/Data/database_final.csv", stringsAsFactors = FALSE)

#seems like there are 673 but there are only 204 filled columns

database <- database[c(1:203),]

summary(database)




#####################################   KEY                      ###############################################          
#####################################     MILESTONES             ###############################################          
#####################################           AND              ###############################################          
#####################################             DEVELOPMENTS   ###############################################          


    ### Number of studies per year

#cumulative plot
# Aggregate number of studies per year

studies_per_year <- database %>% 
  arrange(year_published) %>%
  group_by(year_published) %>%
  summarise(count = n())  

mean(studies_per_year$count)
sd(studies_per_year$count)

studies_per_year_cum <- studies_per_year %>% 
  arrange(year_published) %>%
  mutate(cumulative_count = cumsum(count))

library(patchwork)

# Plot: Number of studies published per year
plot_a <- ggplot(studies_per_year_cum, aes(x = year_published, y = count)) +
  geom_bar(stat = "identity", fill = "#A9A9A9", color = "#696969", alpha = 0.9) +
  geom_text(aes(label = count), vjust = -0.5, size = 4.5, color = "#444444") +
  labs(
    title = "(A)",
    x = "Year Published",
    y = "Number of Studies"
  ) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = -0.12, color = "#333333"),
    axis.title.x = element_text(size = 18, margin = margin(t = 10), color = "#333333"),
    axis.title.y = element_text(size = 18, margin = margin(r = 10), color = "#333333"),
    axis.text.x = element_text(size = 14, color = "#333333"),
    axis.text.y = element_text(size = 14, color = "#333333")
  )


# Plot: Cumulative number of studies by year
plot_b <- ggplot(studies_per_year_cum, aes(x = year_published, y = cumulative_count)) +
  geom_line(color = "#696969", size = 1.5) +
  geom_point(color = "#696969", size = 4) +
  geom_hline(yintercept = c(50, 100, 150, 200), linetype = "dashed", color = "#B0B0B0", alpha = 0.7) +
  labs(
    title = "(B)",
    x = "Year Published",
    y = "Cumulative Number of Studies"
  ) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = -0.12, color = "#333333"),
    axis.title.x = element_text(size = 18, margin = margin(t = 10), color = "#333333"),
    axis.title.y = element_text(size = 18, margin = margin(r = 10), color = "#333333"),
    axis.text.x = element_text(size = 14, color = "#333333"),
    axis.text.y = element_text(size = 14, color = "#333333")
  )


combined_plot <- plot_a + plot_b


    ### No. Journals, top 5

journal_n <- database %>%
  group_by(journal) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))



n_distinct(journal_n$journal) - 5 #1 conference paper and other thesis


    ### Number of studies per country

country_study <- database %>%
  group_by(country) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))


# Find rows with multiple countries
multiple_countries <- country_study %>%
  filter(grepl(";", country))


# Count how many studies have more than one country
sum(multiple_countries$Count) # + United nations one with 193 nations = 7 + 1 = 8


# Clean the Country column to separate any joint countries
country_study <- country_study %>%
  separate_rows(country, sep = "; ") %>%
  mutate(country = str_trim(country)) %>%
  group_by(country) %>%
  summarise(Count = sum(Count), .groups = 'drop')

# Replace "PICTs (FJI" with "FJI"
country_study$country <- str_replace(country_study$country, "PICTs \\(FJI", "FJI")

# Replace "SLB\\)" with "SLB"
country_study$country <- str_replace(country_study$country, "SLB\\)", "SLB")

    # Independent" regions/ countries that are part of unions

# Replace "GB-WLS", "GB-SCT" and "GB-ENG" with "GBR"
country_study$country <- str_replace_all(country_study$country, "GB-WLS|GB-SCT|GB-ENG", "GBR")

# confirm there are no duplicates
anyDuplicated(country_study$country)

country_study <- country_study %>%
  group_by(country) %>%
  summarise(count = sum(Count), .groups = 'drop')

n_distinct(country_study$country) - 1

country_study <- country_study %>%
  filter(country != "193 United Nations Member States")

  ## Create a map
library(sf)
library(rnaturalearth)
library("maps")

##################################
# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Perform a left join to include all countries, filling non-matching countries with count = 0
map_data <- world %>%
  dplyr::select(iso_a3_eh, geometry) %>%
  left_join(country_study, by = c("iso_a3_eh" = "country")) %>%
  mutate(count = ifelse(is.na(count), 0, count))  # Replace NA with 0

map_data <- map_data %>%
  filter(iso_a3_eh != "-99")

# Add Mayotte as a separate region
mayotte_data <- data.frame(
  iso_a3_eh = "MYT",  # ISO code for Mayotte
  count = 1,
  geometry = st_sfc(st_point(c(45.1662, -12.8275)), crs = 4326)  # Coordinates for Mayotte
)
map_data <- bind_rows(map_data, st_as_sf(mayotte_data))

label_data <- map_data %>%
  mutate(
    centroid = st_point_on_surface(geometry)) %>%  # Use a point within the polygon
  mutate(lon = st_coordinates(centroid)[, 1], lat = st_coordinates(centroid)[, 2])

label_data <- label_data %>%
  dplyr::select(iso_a3_eh, count, lon, lat)

label_data <- label_data %>%
  filter(iso_a3_eh != "ATA")


library(ggspatial)
library(tmap)

ggplot(label_data) +
  geom_sf(fill = "white", color = "gray80") +  # World map with gray borders
  geom_point(aes(x = lon, y = lat, size = count), 
             color = "#2473b4ff", alpha = 0.3, shape= 19, stroke = 1.5) +  # Add bubbles
  geom_text(data = subset(label_data, count > 4), 
            aes(x = lon, y = lat, label = iso_a3_eh), 
            size = 3.5, color = "black", fontface = "italic",
            check_overlap = TRUE) +   # Labels only for countries with studies
  scale_size_continuous(range = c(1, 14), name = "Number of Studies", limits = c(1, 26), breaks = seq(2 , 26, by = 6)) +  # Adjust bubble size range
  annotation_north_arrow(
    location = "custom",
    pad_x = unit(24, "cm"),  # Center horizontally (adjust as needed)
    pad_y = unit(10, "cm"),  # Position below the legend
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(location = "br", width_hint = 0.1, 
                   pad_x = unit(1.5, "cm"), pad_y = unit(0.2, "cm")) +
 theme_void() +  # Remove background and axis
  theme(
    legend.position = "bottom",  # Position legend at the bottom
    legend.title = element_text(size = 16),
    legend.text = element_text(size =13)
  )


library(WDI)

gni_data <- WDI(
  country = country_study$country,
  indicator = "NY.GNP.PCAP.CD",
  start = 2023, end = 2023,
  extra = FALSE
)

gni_data <- gni_data %>%
  dplyr::select(iso3c, gni_per_capita = NY.GNP.PCAP.CD) %>%
  rename(country = iso3c)

country_study <- country_study %>%
  left_join(gni_data, by = "country") %>%
  mutate(income_group = case_when(
    gni_per_capita <= 1145 ~ "Low income",
    gni_per_capita <= 4515 ~ "Lower-middle income",
    gni_per_capita <= 14005 ~ "Upper-middle income",
    gni_per_capita > 14005  ~ "High income",
    TRUE ~ "Unclassified"
  ))

  
summary_income <- country_study %>%
  group_by(income_group) %>%
  summarise(total_studies = sum(count), .groups = "drop") %>%
  arrange(desc(total_studies))

summary_combo <- country_study %>%
  group_by(continent, income_group) %>%
  summarise(total_studies = sum(count), .groups = "drop") %>%
  arrange(desc(total_studies))





### Number of studies per animal species

animal_pop <- database %>%
  group_by(animal_pop) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Find rows with multiple species
multiple_animal_pop <- animal_pop %>%
  filter(grepl(";", animal_pop))

multiple_animal_pop <- multiple_animal_pop[c(2:4,6:18),] # remove the ones that are same species

# Count how many studies have more than one species
sum(multiple_animal_pop$count) # 20

      
# which year were those?
      multiple_animal_pop <-  database %>%
            filter(str_detect(animal_pop, ";")) %>%
        arrange(desc(year_published))# Filter for multiple populations
      
      tab1(desc(multiple_animal_pop$year_published))
      
 

# Clean the Country column to separate any joint countries
animal_pop <- animal_pop %>%
  separate_rows(animal_pop, sep = "; ") %>%
  mutate(animal_pop = str_trim(animal_pop)) %>%
  group_by(animal_pop) %>%
  summarise(count = sum(count), .groups = 'drop')


# Replace "pigs" and "wild pigs" with "pigs"
animal_pop$animal_pop <- str_replace_all(animal_pop$animal_pop, "pigs|wild pigs", "swine")
                                             # wild pigs vs pigs, different species (?)

# Replace "beef cattle", "calves" and "dairy cattle" with "cattle"
animal_pop$animal_pop <- str_replace_all(animal_pop$animal_pop, "calves|dairy cattle|beef cattle", "cattle")

# Replace "hatching eggs", "ducks" and "fattening ducks", "backyard chickens", "broilers" with "poultry"
animal_pop$animal_pop <- str_replace_all(animal_pop$animal_pop, "hatching eggs|fattening ducks|ducks|backyard chickens|broilers", "poultry")

# Replace "sheep" and "goats" with "small ruminants"
animal_pop$animal_pop <- str_replace_all(animal_pop$animal_pop, "sheep|goats", "small ruminants")


animal_pop <- animal_pop %>%
  group_by(animal_pop) %>%
  summarise(count = sum(count))

animal_pop <- animal_pop %>%
  mutate(percentage = count/sum(count) * 100)
  



  ### species over time

# Clean the Country column to separate any joint countries

animal_pop <- database %>%
  separate_rows(animal_pop, sep = "; ") %>%
  mutate(animal_pop = str_trim(animal_pop)) %>%
  group_by(animal_pop, year_published) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


# Recategorize species into broader categories (if necessary)
species_overtime <- animal_pop %>%
  mutate(
    species_group = case_when(
      str_detect(animal_pop, "cattle|calves|dairy cattle|beef cattle") ~ "cattle",
      str_detect(animal_pop, "swine|pigs|wild pigs") ~ "swine",
      str_detect(animal_pop, "poultry|hatching eggs|fattening ducks|ducks|backyard chickens|broilers") ~ "poultry",
      str_detect(animal_pop, "small ruminants|sheep|goats") ~ "small ruminants",
      TRUE ~ "other"
    )
  )

# Summarize the number of studies per year for each species group
species_overtime <- species_overtime %>%
  group_by(year_published, species_group) %>%
  summarize(total_studies = sum(count), .groups = 'drop')

# Ensure species_group is a factor with the desired order
species_overtime$species_group <- factor(species_overtime$species_group, 
                                         levels = c("cattle", "swine", "poultry", "small ruminants", "other"))




ggplot(species_overtime, aes(x = year_published, y = total_studies, color = species_group, linetype = species_group)) +
  geom_line(size = 1) +  # Plot lines with different patterns for each species group
  geom_point(size = 2) +  # Optional: Add points to emphasize data points
  labs(
    x = "Year Published",
    y = "Number of Studies",
    color = "Species Group",
    linetype = "Species Group"  # This ensures the line type is displayed under the same legend
  ) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +  
  scale_y_continuous(breaks = seq(0, 13, by = 1)) +
  scale_color_manual(values = c(
    "cattle" = "#2D2D2D",         # Dark grey for cattle
    "swine" = "#595959",          # Grey for swine
    "poultry" = "#C9A66B",        # Light brown for poultry
    "small ruminants" = "#6D9ED9", # Blue for small ruminants
    "other" = "grey"              # Light grey for other
  ),
  labels = c(
    "cattle" = "Cattle",
    "swine" = "Swine",
    "poultry" = "Poultry",
    "small ruminants" = "Small ruminants",
    "other" = "Other"
  )) +
  scale_linetype_manual(values = c(
    "cattle" = "solid",          # Solid line for cattle
    "swine" = "dashed",         # Dashed line for swine
    "poultry" = "dotted",       # Dotted line for poultry
    "small ruminants" = "dashed", # Dot-dash line for small ruminants
    "other" = "twodash"         # Two-dash line for other
  )) +
  theme_minimal() +  # Remove background grid for a cleaner presentation
  theme(
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),  # Add margin to move x-axis title downward
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),  # Add margin to move y-axis title leftward
    axis.text = element_text(size = 14),                                # Larger axis text
    legend.title = element_text(size = 16, face = "bold"),                             # Larger legend title
    legend.text = element_text(size = 15),                              # Larger legend text
    panel.grid = element_blank(),  # Remove grid lines for a clean look
    axis.line = element_line(size = 0.5),  # Add axis lines for definition
    legend.position = "bottom"  # Position legend at the bottom
  ) +
  guides(
    color = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dotted", "dashed", "twodash"))),  # Combine color and linetype in the same legend
    linetype = "none"  # Remove the separate linetype legend
  )

    ### species and countries over time

# Clean the Country column to separate any joint countries


# Clean the animal_pop and country columns to separate any joint animals and countries
animal_country_pop <- database %>%
  separate_rows(animal_pop, sep = "; ") %>%
  separate_rows(country, sep = "; ") %>%
  mutate(
    animal_pop = str_trim(animal_pop),
    country = str_trim(country)
  ) %>%
  group_by(country, animal_pop, year_published) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

# Re categorize species into broader categories (if necessary)
animal_country_pop <- animal_country_pop %>%
  mutate(
    species_group = case_when(
      str_detect(animal_pop, "cattle|calves|dairy cattle|beef cattle") ~ "cattle",
      str_detect(animal_pop, "swine|pigs|wild pigs") ~ "swine",
      str_detect(animal_pop, "poultry|hatching eggs|fattening ducks|ducks|backyard chickens|broilers") ~ "poultry",
      str_detect(animal_pop, "small ruminants|sheep|goats") ~ "small ruminants",
      TRUE ~ "other"
    )
  )

animal_country_pop <- animal_country_pop %>%
  mutate(
    country_group = case_when(
      str_detect(country, "PICTs \\(FJI") ~ "FJI",
      str_detect(country, "SLB\\)") ~ "SLB",
      str_detect(country, "GB-WLS|GB-SCT|GB-ENG") ~ "GBR",
      TRUE ~ country  # Keep original value if no match
    )
  )


# Summarize the number of studies per year for each species group
animal_country_overtime <- animal_country_pop %>%
  group_by(year_published, country_group, species_group) %>%
  summarize(total_studies = sum(count), .groups = 'drop')



### species and countries - no. studies
species_countries <- animal_country_pop %>%
  group_by(animal_pop, country) %>%
  summarize(total_studies = sum(count), .groups = 'drop') %>%
  arrange(desc(count))


# Summarize studies by species and country
species_countries <- animal_country_pop %>%
  group_by(country_group, species_group) %>%  # Group by country and species
  summarize(total_studies = sum(count), .groups = 'drop') %>%  # Sum studies for each species-country pair
  left_join(label_data %>% group_by(country_group) %>% summarize(total_country_studies = sum(count)), 
            by = "country_group") %>%  # Add total studies for each country
  mutate(percentage = (total_studies / total_country_studies) * 100) %>%  # Calculate the percentage per species
  arrange(desc(total_country_studies), country_group, desc(total_studies))  # Sort by total studies and species counts

# Format the n (%) column
species_countries <- species_countries %>%
  mutate(`n (%)` = paste0(total_studies, " (", round(percentage, 1), "%)")) %>%
  select(country_group, species_group, `n (%)`)  # Keep only relevant columns

# Print the resulting table
species_countries





library(plotly)

species_colors <- c("swine" = "pink", "cattle" = "blue", "small ruminants" = "green", "other" = "orange")

# Create the animated map without per-frame captions
map_overtime <- plot_ly(
  data = animal_country_overtime,
  type = 'scattergeo',
  mode = 'markers',
  locations = ~country_group,   # ISO-3 country codes
  marker = list(
    size = ~total_studies * 6,  # Adjust marker size
    sizemode = "area",
    sizeref = 2                 # Standardize scaling for consistent marker sizes
  ),
  color = ~species_group, 
  colors = species_colors,      # Color based on species group
  frame = ~year_published       # Animation over years
)

# Add a static title and layout settings to avoid resizing due to captions
map_overtime <- map_overtime %>% layout(
  title = list(text = "Animal Studies by Country and Species Over Time", y = 0.95), # Set title at top
  annotations = list(
    x = 0.5, y = -0.15,  # Place caption below the plot
    text = "Data source: Scoping Review Analysis",
    showarrow = FALSE,
    xref = 'paper',
    yref = 'paper',
    xanchor = 'center',
    yanchor = 'top',
    font = list(size = 12, color = "grey")
  ),
  geo = list(
    projection = list(type = "natural earth"),
    showland = TRUE,
    landcolor = 'lightgrey',    # Set land color to light grey
    oceancolor = 'lightblue',   # Optional: Set ocean color
    scope = "world"             # Ensures the entire world is shown consistently
  )
)

map_overtime







################### DISEASE OUTCOMES
tab1(database$disease_outcome)

database$disease_outcome_note
tab1(database$disease_outcome_note)

database %>%
  filter(grepl("simulat", disease_outcome_note)) %>%
  summarise(count = n())

database %>%
  filter(grepl("simulat.*not|not.*simulat", disease_outcome_note)) %>%
  summarise(count = n())


# Count the occurrences of FMD-related studies per year
fmd_counts_per_year <- database %>%
  filter(grepl("FMD", disease_outcome_note, ignore.case = TRUE)) %>%
  group_by(year_published) %>%
  summarise(count = n()) %>%
  arrange(year_published)

view(fmd_counts_per_year)


# Count the occurrences of ASF-related studies per year
asf_counts_per_year <- database %>%
  filter(grepl("ASF", disease_outcome_note, ignore.case = TRUE)) %>%
  group_by(year_published) %>%
  summarise(count = n()) %>%
  arrange(year_published)

view(asf_counts_per_year)




# Circular plot
inner_data <- tibble(
  `Diseases/Pathogens explored` = c("Bovine Tuberculosis", "Foot and Mouth Disease", "Avian Influenza Viruses", 
                                    "African Swine Fever", "Other Diseases", "Not specific"),
  count = c(16, 14, 13, 10, 38, 37)
)

# Calculate percentages based on a total of 128
inner_data <- inner_data %>%
  mutate(percentage = count / 128 * 100,
         label = paste0(round(percentage, 1), "%"))  # Format the label with one decimal

# Define colors
inner_colors <- c("#6D4C41", "#D7B49E", "#C9A66B", "#A3B18A", "darkgrey", "lightgrey")

# Create the plot
ggplot(inner_data, aes(x = 1, y = percentage, fill = `Diseases/Pathogens explored`)) +
  geom_bar(stat = "identity", color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6, color = "black", fontface = "bold") +  # White, bold text for percentages
  scale_fill_manual(values = inner_colors) +
  theme_void() +
  theme(
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 17)
  )









################### TEMPORALITY ACCOUNTED
tab1(database$temporality)

################### TEMPORALITY ACCOUNTED
tab1(database$temporality)

software <- database %>%
  separate_rows(software, sep = "; ") %>%
  mutate(software = str_trim(software)) %>%
  group_by(software, year_published) %>%
  summarize(count = n())  


software <- database %>%
  separate_rows(software, sep = "; ") %>%
  group_by(software, year_published) %>%
  summarize(count = n(), .groups = "drop_last") %>%  # Retain grouping by 'year_published'
  group_by(year_published) %>%  # Group by 'year_published' again for the slice_max
  slice_max(order_by = count, n = 4, with_ties = FALSE)  # No 'by' needed


software_totals <- software %>%
  group_by(software) %>%
  summarize(count = n())






#####################################   SIMILARITIES             ###############################################          
#####################################           AND              ###############################################          
#####################################             DIFFERENCES    ###############################################  

################### Data extraction sources


extraction_species <- database %>%
  dplyr::select(country,animal_pop, data_extraction, exceptions_any, exceptions_any_note) %>%
  group_by(country, data_extraction) %>%
  summarize(count = n())  

tab1(extraction_species$data_extraction)

tab1(database$exceptions_any)

filter_exceptions <- database %>%
  dplyr::select(country,animal_pop, data_extraction, exceptions_any, exceptions_any_note) %>%
  filter((grepl("national database", data_extraction) | data_extraction == "animal movement permits"))

# View the results of exceptions
tab1(filter_exceptions$exceptions_any)

filter_exceptions_Y <- filter_exceptions %>%
  filter(exceptions_any == "Y") %>%
  group_by(country, animal_pop) %>%
  summarize(count = n())  # Count the number of occurrences for each country and animal_pop combination

# View the results
filter_exceptions_Y







################### Data length of study
library(lubridate)

# handle date typos
database$data_end <- gsub("Nobember", "Nov", database$data_end)
database$data_end <- gsub("Dec-17", "Dec 2017", database$data_end)
database$data_end <- gsub("Dec-08", "Dec 2008", database$data_end)

database$data_start <- gsub("Sept", "Sep", database$data_start)
database$data_start <- gsub("Sepember", "Sep", database$data_start)
database$data_start <- gsub("Jan-14", "Jan 2014", database$data_start)
database$data_start <- gsub("Jan-06", "Jan 2006", database$data_start)


# Correcting data_start to add 'Jan' if it only contains a year // same for data_end with 'Dec'
database$data_start <- ifelse(nchar(database$data_start) == 4, paste0("Jan ", database$data_start), database$data_start)
database$data_end <- ifelse(nchar(database$data_end) == 4, paste0("31 Dec ", database$data_end), database$data_end)

# Correcting data_start to add '1' if it only contains month and year
database$data_start <- ifelse(grepl("^[A-Za-z]+ [0-9]{4}$", database$data_start), paste0("1 ", database$data_start), database$data_start)

# Correcting data_end to add last day of the month if it only contains month and year
database$data_end <- ifelse(grepl("^[A-Za-z]+ [0-9]{4}$", database$data_end), paste0(day(as.Date(paste0("1 ", database$data_end), 
                                                                                                 format = "%d %B %Y") + months(1) - days(1)), " ", database$data_end), database$data_end)

# lubridate: parse_date_time to convert data_start and data_end into a standardized format
formats <- c("%Y", "%B %Y", "%b %Y", "%Y-%m", "%Y-%m-%d", "%d %B %Y", "%d %b %Y")
database$data_start_clean <- parse_date_time(database$data_start, orders = formats)
database$data_end_clean <- parse_date_time(database$data_end, orders = formats)


inspect <- database %>%
  dplyr::select(data_start, data_start_clean, data_end, data_end_clean)


# Calculate the length of each study in days
database$study_length_days <- as.numeric(difftime(database$data_end_clean, database$data_start_clean, units = "days"))

categorize_study_length <- function(days) {
  if (is.na(days)) {
    return("Unknown")
  } else if (days < 180) {
    return("< 6 months")
  } else if (days >= 180 & days < 360) {
    return(">= 6 months - 1 year")
  } else if (days >= 360 & days < 730) {
    return("1 year")
  } else {
    return(paste0(round(days / 365), " years"))
  }
}

# Apply categorization to study length
database$study_length_category <- sapply(database$study_length_days, categorize_study_length)

# Reorder study length categories by years in proper numeric order
ordered_levels <- c("< 6 months", ">= 6 months - 1 year", "1 year", paste0(2:20, " years"), "Unknown")
database$study_length_category <- factor(database$study_length_category, levels = ordered_levels, ordered = TRUE)

tab1(database$study_length_category)

# Visualization for presenting study lengths
# Bar plot of study length categories
ggplot(database, aes(x = study_length_category)) +
  geom_bar(fill = "#C9A66B") +
  theme_minimal() +
  labs(title = "Distribution of Study Lengths", 
       x = "Study Length Category", 
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



        ## now ignoring the surveys and snowball sampling (which is very uncertain)

# Filter the dataset to exclude studies that are either 'snowball sampling' or 'survey'
filtered_data <- database[!database$data_extraction %in% c("snowball sampling", "survey"), ]
nrow(filtered_data)

# Visualization for presenting study lengths
# Bar plot of study length categories
ggplot(filtered_data, aes(x = study_length_category)) +
  geom_bar(fill = "#C9A66B") +
  theme_minimal() +
  labs(title = "Distribution of Study Lengths", 
       x = "Study Length Category", 
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





################### Descriptive variables



# Summary of usage for each descriptive variable

database %>%
  dplyr::select(country, animal_pop, descrip_age, descrip_sex, descrip_breed, descrip_premisetype) %>%
  summarise(
    descrip_age_count = sum(descrip_age == "Y", na.rm = TRUE),
    descrip_age_perc = (sum(descrip_age == "Y", na.rm = TRUE) / 203) * 100,
    
    descrip_sex_count = sum(descrip_sex == "Y", na.rm = TRUE),
    descrip_sex_perc = (sum(descrip_sex == "Y", na.rm = TRUE) / 203) * 100,
    
    descrip_breed_count = sum(descrip_breed == "Y", na.rm = TRUE),
    descrip_breed_perc = (sum(descrip_breed == "Y", na.rm = TRUE) / 203) * 100,
    
    descrip_premisetype_count = sum(descrip_premisetype == "Y", na.rm = TRUE),
    descrip_premisetype_perc = (sum(descrip_premisetype == "Y", na.rm = TRUE) / 203) * 100
  )

descrip_other <- database %>% 
  dplyr::select(country, animal_pop, descrip_other)


descrip_other %>% filter(grepl("ruck", descrip_other) | grepl("ransport", descrip_other))

descrip_other %>% filter(grepl("purpose", descrip_other) | grepl("reason", descrip_other) )

descrip_other %>% filter(grepl("herd", descrip_other))







################### DISEASES 
database %>%
  count(disease_outcome, name = "Count") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

database %>%
  count(disease_outcome_note, name = "Count") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Analyze 'disease_outcome_note' to see different diseases and account for simulated ones
database <- database %>%
  separate_rows(disease_outcome_note, sep = "[[:space:]]*;[[:space:]]*") %>%
  mutate(disease_outcome_note = str_trim(disease_outcome_note)) %>%
  mutate(disease_outcome_note = case_when(
    grepl("^simulated ", disease_outcome_note) ~ gsub("^simulated ", "", disease_outcome_note),
    TRUE ~ disease_outcome_note
  ))

# Summarize 'disease_outcome_note' to see different diseases
disease_outcome_note_summary <- database %>%
mutate(disease_outcome_note = case_when(
  disease_outcome_note == "PRRS" ~ "PRRSV",
  disease_outcome_note %in% c( "AIV (H5N1)", "AIV (H5N2)", "AIV (H5N6)", "AIV (H5N8)", "AIV (H7N9)") ~ "AIV",
  TRUE ~ disease_outcome_note )) %>%
  count(disease_outcome_note, name = "Count") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Display the summaries
print(disease_outcome_note_summary)







################### NODES

#	What type of nodes were most common? How many usually? Min-Max?

tab1(database$nodes)

# Split the 'nodes' column by ';' and expand it into multiple rows to handle multiple types of nodes per study
node_type <- database %>%
  separate_rows(nodes, sep = "[[:space:]]*;[[:space:]]*") %>%
  mutate(nodes = case_when(
    nodes == "LBMs" ~ "markets",
    nodes == "cattle markets" ~ "markets",
    nodes == "backyard chicken owners - ego" ~ "animal holding",
    nodes == "county" ~ "counties",
    nodes == "premises" ~ "animal holding",
    nodes == "traders - alters" ~ "traders",
    TRUE ~ nodes
  ))


# Identify the studies with more than one type of node
database %>%
  filter(grepl(';', nodes)) %>%
  nrow()

# Count the number of occurrences of each node type, including studies with multiple nodes
node_type_summary <- node_type %>%
  count(nodes, name = "Count") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)



################### EDGES


# Modify 'edges_weighted' to convert 'Y and N' to 'Y', keeping 'N' as 'N'
database <- database %>%
  mutate(edges_weighted = ifelse(edges_weighted == "Y and N", "Y", edges_weighted))

tab1(database$edges_weighted)

tab1(database$edges_weighted_note)


# Replace specific terms with 'average number of animals'
database <- database %>%
  mutate(edges_weighted_note = gsub("average monthly animal movements|number of weekly traded animals", "average number of animals", edges_weighted_note))

# Modify the 'edges_weighted_note' to separate rows and filter 'Y'
weighted_edge_type <- database %>%
  filter(edges_weighted == "Y") %>%
  separate_rows(edges_weighted_note, sep = "[[:space:]]*;[[:space:]]*") %>%
  count(edges_weighted_note, name = "Count") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
  



################### NO. OF NETWORKS


database <- database %>%
  mutate(n_networks = gsub("21000 iterations", "21000", n_networks))

# Convert 'n_networks' to numeric (if it's not already)
database$n_networks <- as.numeric(as.character(database$n_networks))


# Analyze the 'n_networks' column to see min, max, and distribution
networks_summary <- database %>%
  summarize(
    Min = min(n_networks, na.rm = TRUE),
    Max = max(n_networks, na.rm = TRUE),
    Mean = mean(n_networks, na.rm = TRUE),
    Median = median(n_networks, na.rm = TRUE),
    SD = sd(n_networks, na.rm = TRUE),
    IQR_value = IQR(n_networks, na.rm = TRUE))




################### EXCLUSIONS

database$exclusions



################### TEMPORALITY

database %>%
  count(temporality, name = "Count") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)


# Modify the 'temporality_note' to replace specific terms
# Ensure the changes are applied to the correct data type and handle trailing spaces
temporality_note_summary <- database %>%
  separate_rows(temporality_note, sep = "[[:space:]]*;[[:space:]]*") %>%
  filter(temporality == "Y") %>%
  mutate(temporality_note = case_when(
    temporality_note == "annualy" ~ "annually",
    temporality_note == "annual" ~ "annually",
    temporality_note == "biannually" ~ "bi-annually",
    temporality_note == "biannualy" ~ "bi-annually",
    temporality_note == "daily simulations" ~ "daily",
    temporality_note == "quaterly" ~ "quarterly",
    temporality_note %in% c("seasonal (4 seasons)", "seasonal trends", "seasonally") ~ "seasonal (4)",
    TRUE ~ temporality_note )) %>%
  count(temporality_note, name = "Count") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)
















