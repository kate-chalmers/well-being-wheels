# Load the relevant packages to build the wheel
library(tidyverse)
library(countrycode)
library(ggpattern)
library(rsdmx)

# Set the indicators needed to make the wheel based on measure name 
indicator_dictionary <- tibble(
  measure = c("1_1", "1_2", "1_3", 
              "2_1", "2_2", "2_7", 
              "3_1", "3_2", 
              "4_1", "4_3", 
              "5_1", "5_3", 
              "6_2", "6_2_DEP",
              "7_1_DEP", "7_2", 
              "8_1_DEP", "8_2",
              "9_1", "9_2", 
              "10_1", "10_2",
              "11_1", "11_2"),
  # Assign labels to indicators
  var_label = c("Household income", "S80/S20 income share ratio", "Household net wealth", 
                "Employment rate", "Gender wage gap", "Long paid working hours", 
                "Overcrowding rate", "Housing affordability", 
                "Time off", "Gender gap in hours worked", 
                "Life expectancy", "Deaths of despair", 
                "Student skills (maths)", "Students with low skills", 
                "Lack of social support", "Social interactions", 
                "Having no say in government", "Voter turnout", 
                "Access to green space", "Exposure to outdoor air pollution",
                "Homicides", "Gender gap in feeling safe",
                "Life satisfaction", "Negative affect balance")) %>%
  # Assign indicators where lower values are wanted
  mutate(direction = ifelse(measure %in% c("2_7", "3_1", "5_3", "9_2", 
                                           "9_3", "10_1", "11_2", 
                                           "7_1_DEP", "8_1_DEP", "6_2a"), "negative", "positive"))

# Set list of OECD countries to compare for normalization
country_list <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", 
                  "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", 
                  "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", 
                  "USA", "CRI")

# Collapse measure names into a format recognized by OECD SDMX API
measures_wanted <- indicator_dictionary$measure %>% paste0(., collapse = "+")

# Build API url
api_url <- paste0("https://sdmx.oecd.org/public/rest/data/OECD.WISE.WDP,DSD_HSL@DF_HSL_CWB/.", measures_wanted, ".._T.F+M+_T._T.?dimensionAtObservation=AllDimensions")

# Pull data and tidy
full_df <- rsdmx::readSDMX(api_url) %>% as.data.frame() 
colnames(full_df) <- tolower(colnames(full_df))
full_df <- full_df %>% rename(obs_value = obsvalue)

# Calculate gap in feelings of safety 
indic_10_2 <- full_df %>%
  filter(measure %in% c("10_2"), sex %in% c("M", "F")) %>%
  select(-obs_status) %>%
  pivot_wider(names_from = "sex", values_from = "obs_value") %>%
  mutate(obs_value = `F` - `M`) %>%
  select(-`F`, -`M`) %>%
  drop_na(obs_value)

# Pull out latest value for each country and indicator
wheel_tidy <- full_df %>% 
  filter(sex == "_T", !measure == "10_2") %>% 
  select(-sex, -obs_status) %>%
  # Append safety data
  rbind(indic_10_2) %>%
  select(measure, unit_measure, time_period, ref_area, obs_value) %>%
  mutate(time_period = as.numeric(time_period)) %>%
  # Find max year by indicator and country
  group_by(measure, ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup()


# Outlier treatment -------------------------------------------------------

# Find third largest value for each indicator
max_third <- wheel_tidy %>%
  arrange(measure, -obs_value) %>%
  group_by(measure) %>%
  slice_head(n = 3) %>%
  slice_tail(n=1) %>%
  select(measure, max_third_val = obs_value) %>%
  ungroup()

# Find third smallest value for each indicator
min_third <- wheel_tidy %>%
  arrange(measure, obs_value) %>%
  group_by(measure) %>%
  slice_head(n = 3) %>%
  slice_tail(n=1) %>%
  select(measure, min_third_val = obs_value) %>%
  ungroup()

# Create min and max columns with above values
wheel_tidy <- wheel_tidy %>%
  left_join(., max_third, by = c("measure")) %>%
  left_join(., min_third, by = c("measure")) %>%
  # Replace any values smaller or larger than values decided above and
  mutate(obs_value = ifelse(obs_value > max_third_val, max_third_val,
                        ifelse(obs_value < min_third_val, min_third_val, obs_value))) %>%
  left_join(., indicator_dictionary, by = "measure")

# Normalization -----------------------------------------------------------

# Normalize all values using min-max normalization
wheel_normalized <- wheel_tidy %>%
  mutate(
    normalized = ifelse(direction == "positive",
                        ((obs_value - min_third_val) / (max_third_val - min_third_val)) * 10,
                        # Reverse calculation when a lower value is wanted
                        10 - (((obs_value - min_third_val) / (max_third_val - min_third_val)) * 10)),
    normalized = normalized * 10,
    # Replace too small values with 5 so data is still visible on chart
    normalized = ifelse(normalized < 5, 5, normalized)) %>%
  select(-max_third_val, -min_third_val) %>%
  # Show all indicators for countries even when no data is available
  group_by(measure) %>%
  complete(ref_area = country_list,
           var_label = var_label,
           direction = direction) %>%
  ungroup() %>%
  distinct()

rm(max_third, min_third, wheel_tidy, country_list, api_url, indic_10_2, measures_wanted)

# Final cleaning
wheel_final <- wheel_normalized %>%
  # Great categories so NA values can be grouped
  mutate(measure2 = measure) %>%
  separate(measure2, into=c("cat", "subcat"), sep = "_") %>%
  # Set indicator type
  mutate(indicator_type = case_when(
    measure %in% c("1_2", "3_1", "2_2", 
                   "2_7", "5_3", "6_2_DEP",
                   "9_2","9_3", "11_2", "10_2", 
                   "4_3", "7_1_DEP", "8_1_DEP") ~ "inequality",
    TRUE ~ "average"),
    # Set order of indicators
    measure = factor(measure, levels = c("0_0",
                                         "1_1", "1_3", "1_2",
                                         "3_2", "3_1",
                                         "2_1", "2_2", "2_7",
                                         "5_1", "5_3",
                                         "6_2", "6_2_DEP",
                                         "9_1", "9_2",
                                         "11_1", "11_2",
                                         "10_1", "10_2",
                                         "4_1", "4_3",
                                         "7_2", "7_1_DEP",
                                         "8_2", "8_1_DEP"))) %>%
  filter(!is.na(measure)) %>%
  mutate(
    # Mark indicators that have reverse scoring
    var_label = ifelse(direction == "negative", paste0(var_label, "*"), var_label),
    # Create no data category (show up in dark red)
    cat = ifelse(is.na(time_period), "0", cat)
  ) %>%
  select( -direction, -obs_value)

# Build a function to plot the wheels
wheelPlotter <- function(df, country_name) {
  
  # Set specific coordinates for colored segments
  ymin_val_annotate <- c(0.55, 3.55, 5.55, 8.55, 10.55, 12.55, 14.55, 16.55, 18.55, 20.55, 22.55)
  ymax_val_annotate <- c(3.44, 5.44, 8.44, 10.44, 12.44, 14.44, 16.44, 18.44, 20.44, 22.44, 24.44)
  rect_colors <- c("#2CA3E0", "#3DA594", "#237FBD", "#7C3A73", "#7EA943", "#30A457", "#E26237",
                   "#606060", "#962828", "#CE485D", "#DCA922")
  
  # Build function to create segments
  add_colored_segments <- function(ymin_val, ymax_val, color_val) {
    annotate(
      "segment",
      x = c(rep(110,1), 110,rep(110,9)),
      xend =  c(rep(110,1), 110,rep(110,9)),
      y = ymin_val,
      yend = ymax_val,
      linetype = "solid",
      size = 5,
      colour = color_val,
      alpha = 0.85
    )
  }
  
  # Set color categories
  bar.col <- c("1" = "#2CA3E0",
               "3" = "#3DA594",
               "2" = "#237FBD",
               "5" = "#7C3A73",
               "6" = "#7EA943",
               "9" = "#30A457",
               "11" = "#E26237",
               "10" = "#606060",
               "4" = "#962828",
               "7" = "#CE485D",
               "8" = "#DCA922",
               "0" = "transparent")
  
  # Filter country wanted
  wheel_short <- df %>%
    filter(ref_area == countrycode::countrycode(country_name, "country.name", "iso3c")) %>%
    arrange(measure) 
  
  wheel_short %>%
    ggplot(aes(y=measure, fill=cat, pattern=indicator_type)) +
    # Build background columns 
    geom_col(aes(x=100),
             color="grey80",
             width=0.7,
             fill="grey70",
             position = "dodge2",
             alpha = ifelse(wheel_short$cat == "0", 0, 0.4)) +
    # Build colored segments at end of bars
    add_colored_segments(ymin_val_annotate, ymax_val_annotate, rect_colors) +
    # Build filled bars with normalized values
    ggpattern::geom_bar_pattern(
      aes(x=normalized, pattern_fill=cat, color=cat),
      stat = "identity",
      position = "dodge2",
      show.legend = T,
      width = 0.65,
      pattern_colour = "white",
      pattern_angle = 45,
      pattern_density = 0.025,
      pattern_spacing = 0.01,
      inherit.aes = T
    ) +
    # Add labels to indicators
    geom_label(aes(label=str_wrap(var_label, 10), x = 138), #c(rep(140, 9), 145, rep(140, 14))
               size=2.35,
               color = ifelse(wheel_short$cat == "0", "darkred", "black"),
               fontface = ifelse(wheel_short$cat == "0", "bold", "plain"),
               fill=NA,
               alpha=0.4,
               label.size=NA) +
    # Add in colors and legends
    scale_color_manual(values=bar.col) +
    ggpattern::scale_pattern_manual(values = c(inequality = "stripe", average = "none"),
                                    labels=c("Average", "Inequality")) +
    guides(
      pattern = guide_legend(nrow=1,
                             color="pink",
                             title=element_blank(),
                             byrow=T,
                             override.aes = list(fill="#2CA3E0", color="#2CA3E0",
                                                 linewidth=0.8,
                                                 pattern_density = 0.025,
                                                 pattern_spacing = 0.025,
                                                 pattern_colour = "white",
                                                 pattern_angle = 45)),
      color = "none",
      fill = "none",
      pattern_fill = "none"
    ) +
    labs(title = paste0("How's Life? in ", country_name),
         subtitle = "Current well-being in the latest year") +
    scale_fill_manual(values = bar.col) +
    # Polar coordinates to make circular
    coord_polar(theta="y", clip="off") +
    xlim(-25, 140) +
    # Add themes and styling
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          text = element_text(color = "grey20"),
          panel.grid.major.y = element_blank(),
          plot.background = element_rect(color="grey98", fill="grey98"),
          panel.background = element_rect(color="grey98", fill="grey98"),
          panel.grid.major.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(color = "#1d4f60",
                                    size = 20,
                                    hjust = 0.5,
                                    margin = margin(25, 5, 5, 5),
                                    face = "bold"),
          plot.subtitle = element_text(color = "#1d4f60",
                                       hjust=0.5),
          plot.margin = margin(5, 5, 15, 5),
          legend.key.width = unit(1.1, "cm"),
          legend.key.height = unit(0.5, "cm"),
          legend.title = element_blank(),
          legend.text = element_text(size=9, face="bold"),
          legend.margin=margin(c(5,5,5,5)),
          legend.position = c(0.55, 0.05))
  
}

# Run the function and replace with whatever country you want to see :)
wheelPlotter(wheel_final, "France")



