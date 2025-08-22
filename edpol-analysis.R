# ==============================================================================
# Replication Code: Tracking Educational Polarization in the Electorate
# Author: Zachary L. Hertz
# Data: Cumulative CES Common Content (2008, 2012, 2016, 2020. 2024)
# ==============================================================================

# Load Required Libraries --------------------------------------------------
library(dataverse)
library(tidyverse)
library(survey)

# Data Loading and Processing ===============================================

# Load CES Data ------------------------------------------------------------
dat <- get_dataframe_by_name(
  filename = "cumulative_2006-2024.dta",
  dataset = "10.7910/DVN/II2DB6",
  original = TRUE,
  .f = haven::read_dta,
  server = "dataverse.harvard.edu"
)

# Data cleaning and variable creation
dat <- dat |>
  filter(year %in% c(2008, 2012, 2016, 2020, 2024)) |>
  mutate(
    # Education categories
    college = case_when(
      educ < 3 ~ "No college",
      educ > 3 ~ "College"
    ),
    
    # Race categories
    race2 = case_when(
      race == 1 ~ "White Americans",
      race == 2 ~ "Black Americans", 
      race == 3 ~ "Hispanic Americans",
      race == 4 ~ "Asian Americans"
    ),
    
    # Clean party ID with labels
    party_id = case_when(
      pid3_leaner == 1 ~ "Democrat",
      pid3_leaner == 2 ~ "Republican", 
      pid3_leaner == 3 ~ "Independent/Non-affiliated",
      pid3_leaner == 8 ~ "Independent/Non-affiliated"
    ),
    
    # Party identification indicators (keep for education breakdowns)
    democrat = if_else(pid3_leaner == 1, 1, 0),
    republican = if_else(pid3_leaner == 2, 1, 0),
    independent = if_else(pid3_leaner == 3 | pid3_leaner == 8, 1, 0),
    
    # Vote intention and behavior (1 = Democratic, 0 = Republican)
    intent_dem = if_else(intent_pres_party == 1, 1, 0),
    voted_dem = if_else(voted_pres_party == 1, 1, 0),
    
    # Convert year to factor for plotting
    year = factor(year)
  ) |>
  filter(!is.na(party_id))  # Remove missing and "Not Sure" responses

# Data analysis ===============================================================

# Create svy design object -----------------------------------------------------
survey_design <- svydesign(ids = ~0, data = dat, weights = ~weight)

# Create weighted estimates ----------------------------------------------------

# Overall party ID trends (all races combined)
overall_trend_raw <- svyby(
  ~factor(party_id), 
  ~year, 
  survey_design, 
  svymean, 
  na.rm = TRUE
) |>
  as.data.frame()

# Reshape overall trend data for plotting
# I know this is ugly. Sorry! Email me if you know a good solve.
overall_trend_data <- overall_trend_raw |>
  pivot_longer(
    cols = starts_with("factor(party_id)"),
    names_to = "party_raw",
    values_to = "party_share"
  ) |>
  mutate(
    party = str_remove(party_raw, "factor\\(party_id\\)"),
    se_party = case_when(
      party == "Democrat" ~ `se.factor(party_id)Democrat`,
      party == "Republican" ~ `se.factor(party_id)Republican`,
      party == "Independent/Non-affiliated" ~ `se.factor(party_id)Independent/Non-affiliated`
    )
  ) |>
  select(year, party, party_share, se_party)

# Overall party ID by race (no education breakdown)
overall_party_raw <- svyby(
  ~factor(party_id), 
  ~year + race2, 
  survey_design, 
  svymean, 
  na.rm = TRUE
) |>
  as.data.frame()

# Reshape party by race data for plotting
# I know this is ugly. Sorry!
overall_party_data <- overall_party_raw |>
  pivot_longer(
    cols = starts_with("factor(party_id)"),
    names_to = "party_raw",
    values_to = "party_share"
  ) |>
  mutate(
    party = str_remove(party_raw, "factor\\(party_id\\)"),
    se_party = case_when(
      party == "Democrat" ~ `se.factor(party_id)Democrat`,
      party == "Republican" ~ `se.factor(party_id)Republican`,
      party == "Independent/Non-affiliated" ~ `se.factor(party_id)Independent/Non-affiliated`
    )
  ) |>
  select(year, race2, party, party_share, se_party)

# Vote intention by race and education
intent_estimates <- svyby(
  ~intent_dem, 
  ~year + race2 + college, 
  survey_design, 
  svymean, 
  na.rm = TRUE
) |>
  as.data.frame()

# Actual vote by race and education  
voted_estimates <- svyby(
  ~voted_dem, 
  ~year + race2 + college, 
  survey_design, 
  svymean, 
  na.rm = TRUE
) |>
  as.data.frame()

# Vote by party ID and education
pid_vote_estimates <- svyby(
  ~voted_dem, 
  ~year + pid3_leaner + college, 
  survey_design, 
  svymean, 
  na.rm = TRUE
) |>
  as.data.frame() |>
  filter(pid3_leaner < 4)

# Party identification by race and education
dem_id_estimates <- svyby(
  ~democrat, 
  ~year + race2 + college, 
  survey_design, 
  svymean, 
  na.rm = TRUE
) |>
  as.data.frame()

rep_id_estimates <- svyby(
  ~republican, 
  ~year + race2 + college, 
  survey_design, 
  svymean, 
  na.rm = TRUE
) |>
  as.data.frame()

ind_id_estimates <- svyby(
  ~independent, 
  ~year + race2 + college, 
  survey_design, 
  svymean, 
  na.rm = TRUE
) |>
  as.data.frame()

# Create Visualizations ======================================================

# Setup ------------------------------------------------------------------
# Common theme for all plots
plot_theme <- theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 1, face = "italic")
  )

# Function to create error bars (84% confidence intervals)
create_error_bars <- function(data, y_var, se_var) {
  geom_errorbar(
    aes(
      ymin = .data[[y_var]] - 1.41 * .data[[se_var]],
      ymax = .data[[y_var]] + 1.41 * .data[[se_var]]
    ),
    width = 0.2,
    position = position_dodge(0.05)
  )
}

# Overall Party Identification Trends
p1 <- ggplot(overall_trend_data, aes(x = year, y = party_share, 
                                     group = party, color = party)) +
  geom_line(size = 1.2) + 
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = party_share - 1.41 * se_party, 
        ymax = party_share + 1.41 * se_party),
    width = 0.2,
    position = position_dodge(0.05)
  ) +
  scale_color_manual(values = c("Democrat" = "#4392F1", 
                                "Republican" = "#D33F49", 
                                "Independent/Non-affiliated" = "#EAC435")) +
  labs(
    y = "Share Identifying with Party (leaners included)",
    x = "Year",
    caption = "Plot: Zachary L. Hertz\nBars indicate 84 percent confidence intervals.\nData: Cumulative CES Common Content"
  ) +
  plot_theme

# Overall Party Identification by Race
p2 <- ggplot(overall_party_data, aes(x = year, y = party_share, 
                                     group = party, color = party)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(
    aes(ymin = party_share - 1.41 * se_party, 
        ymax = party_share + 1.41 * se_party),
    width = 0.2,
    position = position_dodge(0.05)
  ) +
  scale_color_manual(values = c("Democrat" = "#4392F1", 
                                "Republican" = "#D33F49", 
                                "Independent/Non-affiliated" = "#EAC435")) +
  labs(
    y = "Share Identifying with Party (leaners included)",
    x = "Year",
    caption = "Plot: Zachary L. Hertz\nBars indicate 84 percent confidence intervals.\nData: Cumulative CES Common Content"
  ) +
  plot_theme +
  facet_wrap(~race2)

# Vote Intention by Race and Education
p3 <- ggplot(intent_estimates, aes(x = year, y = intent_dem,
                                   group = college, color = college)) +
  geom_line() + 
  geom_point() +
  create_error_bars(intent_estimates, "intent_dem", "se") +
  scale_color_manual(values = c("#AFB7D5", "#C0D8CC")) +
  labs(
    y = "Democratic Share of Vote Intention",
    x = "Year",
    caption = "Plot: Zachary L. Hertz\nData: Cumulative CES Common Content"
  ) +
  plot_theme +
  facet_wrap(~race2)

# Actual Vote by Race and Education  
p4 <- ggplot(voted_estimates, aes(x = year, y = voted_dem,
                                  group = college, color = college)) +
  geom_line() + 
  geom_point() +
  create_error_bars(voted_estimates, "voted_dem", "se") +
  scale_color_manual(values = c("#3D315B", "#3C896D")) +
  labs(
    y = "Democratic Share of Validated Vote",
    x = "Year",
    caption = "Plot: Zachary L. Hertz\nBars indicate 84 percent confidence intervals.\nData: Cumulative CES Common Content"
  ) +
  plot_theme +
  facet_wrap(~race2)

# Democratic Party Identification by Race and Education
p5 <- ggplot(dem_id_estimates, aes(x = year, y = democrat,
                                   group = college, color = college)) +
  geom_line() + 
  geom_point() +
  create_error_bars(dem_id_estimates, "democrat", "se") +
  scale_color_manual(values = c("#03045E", "#4392F1")) +
  labs(
    y = "Share Identifying as Democrats (leaners included)",
    x = "Year",
    caption = "Plot: Zachary L. Hertz\nBars indicate 84 percent confidence intervals.\nData: Cumulative CES Common Content"
  ) +
  plot_theme +
  facet_wrap(~race2)

# Republican Party Identification by Race and Education
p6 <- ggplot(rep_id_estimates, aes(x = year, y = republican,
                                   group = college, color = college)) +
  geom_line() + 
  geom_point() +
  create_error_bars(rep_id_estimates, "republican", "se") +
  scale_color_manual(values = c("#660000", "#D33F49")) +
  labs(
    y = "Share Identifying as Republicans (leaners included)",
    x = "Year", 
    caption = "Plot: Zachary L. Hertz\nBars indicate 84 percent confidence intervals.\nData: Cumulative CES Common Content"
  ) +
  plot_theme +
  facet_wrap(~race2)

# Independent Party Identification by Race and Education
p7 <- ggplot(ind_id_estimates, aes(x = year, y = independent,
                                   group = college, color = college)) +
  geom_line() + 
  geom_point() +
  create_error_bars(ind_id_estimates, "independent", "se") +
  scale_color_manual(values = c("#F79F79", "#F7D08A")) +
  labs(
    y = "Share Identifying as Independents (leaners included)",
    x = "Year",
    caption = "Plot: Zachary L. Hertz\nBars indicate 84 percent confidence intervals.\nData: Cumulative CES Common Content"
  ) +
  plot_theme +
  facet_wrap(~race2)

# Save Visualizations ==========================================================

# Save all plots with specified dimensions
ggsave("images/pid-overall-trend.png", p1, width = 6, height = 6, units = "in", dpi = 700)
ggsave("images/pid-overall-race.png", p2, width = 6, height = 6, units = "in", dpi = 700)
ggsave("images/intent-race.png", p3, width = 6, height = 6, units = "in", dpi = 700)
ggsave("images/voted-race.png", p4, width = 6, height = 6, units = "in", dpi = 700)
ggsave("images/pid-dem-race.png", p5, width = 6, height = 6, units = "in", dpi = 700)
ggsave("images/pid-rep-race.png", p6, width = 6, height = 6, units = "in", dpi = 700)
ggsave("images/pid-ind-race.png", p7, width = 6, height = 6, units = "in", dpi = 700)