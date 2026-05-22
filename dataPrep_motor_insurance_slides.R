## =========================================================================
## This has the data preparation for the slide deck for motor insurance
## =========================================================================

## 1. Load libraries and dataset & rid missing values
## -----------------------------------------------------
pacman::p_load(tidyverse, patchwork, ggrepel, ggmisc, ggiraph, DT, 
               gifski, plotly, shiny, gganimate, ggpp, ggtext, ggdist, 
               ggridges, colorspace, geomtextpath, nord, nortest, seriation, 
               dendextend, heatmaply, ggthemes, hrbrthemes, janitor, crosstalk, plotly) 

data <- read_delim("data/motor_ins/Dataset.csv", 
                   delim = ";",
                   na = c("", ".", "NA")) %>% 
                    clean_names() %>% 
                    na.omit()

## 2. Create a dictionary like list system to make chart labels reader-friendly
## -----------------------------------------------------------------------------
# Policy & Insured characteristics
policy_status_labels <- c(
  "A" = "Active",
  "C" = "Cancelled"
)

business_type_labels <- c(
  "NB" = "New Business",
  "P"  = "Portfolio (Renewal)"
)

payment_frequency_labels <- c(
  "A" = "Annual",
  "S" = "Semi-Annual",
  "Q" = "Quarterly"
)

bonus_score_labels <- c(
  "G" = "Good (Favourable History)",
  "N" = "Neutral",
  "B" = "Bad (Poor History)"
)

policy_type_labels <- c(
  "TP"     = "Third-Party Liability",
  "TPG"    = "Third-Party + Glass",
  "CC"     = "Third-Party + Combined",
  "COMP_E" = "Comprehensive (With Excess)",
  "COMP_N" = "Comprehensive (No Excess)"
)

# Driver & Vehicle characteristics
fuel_type_labels <- c(
  "D" = "Diesel",
  "G" = "Gasoline"
)

municipality_type_labels <- c(
  "I"  = "Inland",
  "C"  = "Coastal",
  "IS" = "Islands"
)

circulation_area_labels <- c(
  "U" = "Urban",
  "R" = "Rural"
)

## 3. Charts for '3 Overview of Motor Insurance policy portfolios/policy profile'
## -------------------------------------------------------------------------------------

policy_profile <- data %>%
  select(policy_type, policy_status, business_type, payment_frequency, bonus_score) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(label = case_when(
    variable == "policy_status"     ~ recode(value, !!!policy_status_labels),
    variable == "business_type"     ~ recode(value, !!!business_type_labels),
    variable == "payment_frequency" ~ recode(value, !!!payment_frequency_labels),
    variable == "bonus_score"       ~ recode(value, !!!bonus_score_labels),
    variable == "policy_type"       ~ recode(value, !!!policy_type_labels),
    TRUE ~ value  # fallback: keep original if no match
  )) %>%
  ggplot(aes(x = label, fill = label)) +
  geom_bar(width = 0.4) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(title = "Policy Profile Overview", x = NULL, y = "Count")

## 4. Charts for '4 Polcyholder Characteristics and Claims Behaviour'
## --------------------------------------------------------------------------

## data warngling
## -----------------
data_fe <- data %>%
  mutate(
    # Driving experience proxy
    driving_exp_proxy = driver_age - age_driving_licence,
    
    # Age bands
    age_band = cut(driver_age,
                   breaks = c(0, 25, 35, 45, 55, 65, Inf),
                   labels = c("Under 25", "25-34", "35-44", "45-54", "55-64", "65+"),
                   right  = FALSE),
    
    # Vehicle age bands
    vehicle_age_band = cut(vehicle_age,
                           breaks = c(0, 3, 7, 12, 20, Inf),
                           labels = c("New (0-2)", "Young (3-6)", "Mid (7-11)", 
                                      "Older (12-19)", "Old (20+)"),
                           right  = FALSE),
    
    # Driving experience bands
    exp_band = cut(driving_exp_proxy,
                   breaks = c(0, 2, 5, 10, 20, Inf),
                   labels = c("0-2 yrs", "3-5 yrs", "6-10 yrs", 
                              "11-20 yrs", "20+ yrs"),
                   right  = FALSE),
    
    # Derived KPIs
    loss_ratio      = total_incurred / total_premium,
    claim_frequency = total_claims / total_exposure,
    claim_severity  = ifelse(total_claims > 0, total_incurred / total_claims, 0)
  ) %>%
  # Remove bad records only (negative experience = data quality issue)
  filter(driving_exp_proxy >= 0)

## (a) bonus score
## ----------------
bonus_score <- data_fe %>%
  mutate(bonus_label = recode(bonus_score, !!!bonus_score_labels)) %>%
  count(age_band, bonus_label) %>%
  group_by(age_band) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = age_band, y = pct, fill = bonus_label)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlGn") +
  theme_minimal() +
  labs(title    = "Claims History by Driver Age Band",
       subtitle = "Are younger drivers more likely to have poor claims history?",
       x = "Age Band", y = "Proportion", fill = "Bonus Score")

## (b) policy type
## ----------------
policy_type <- data_fe %>%
  mutate(policy_label = recode(policy_type, !!!policy_type_labels)) %>%
  count(age_band, policy_label) %>%
  group_by(age_band) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = age_band, y = pct, fill = policy_label)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title    = "Policy Type by Driver Age Band",
       subtitle = "Do older drivers choose more comprehensive coverage?",
       x = "Age Band", y = "Proportion", fill = "Policy Type")

## (c) claim frequency
## --------------------
claim_frequency <- data_fe %>%
  mutate(area_label = recode(circulation_area, !!!circulation_area_labels)) %>%
  group_by(age_band, area_label) %>%
  summarise(claim_freq = sum(total_claims) / sum(total_exposure), 
            .groups = "drop") %>%
  ggplot(aes(x = age_band, y = claim_freq, fill = area_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title    = "Claim Frequency by Age Band and Circulation Area",
       subtitle = "Is urban risk consistently higher across all age groups?",
       x = "Age Band", y = "Claims per Exposure Year", fill = "Area")

## (d) claim severity
## -------------------
data_fe %>%
  filter(total_incurred > 0) %>%
  mutate(fuel_label = recode(fuel_type, !!!fuel_type_labels)) %>%
  group_by(vehicle_age_band, fuel_label) %>%
  summarise(avg_loss = mean(total_incurred), .groups = "drop") %>%
  ggplot(aes(x = vehicle_age_band, y = avg_loss, fill = fuel_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title    = "Average Claim Severity by Vehicle Age and Fuel Type",
       subtitle = "Do older vehicles cost more per claim?",
       x = "Vehicle Age Band", y = "Average Incurred", fill = "Fuel Type")

## (e) loss-ratio
## ---------------
loss_ratio <- data_fe %>%
  mutate(policy_label = recode(policy_type, !!!policy_type_labels)) %>%
  group_by(year, policy_label) %>%
  summarise(loss_ratio = sum(total_incurred) / sum(total_premium), 
            .groups = "drop") %>%
  ggplot(aes(x = year, y = loss_ratio, color = policy_label, group = policy_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) + ## force breaks at whole years
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title    = "Loss Ratio Trend by Policy Type",
       subtitle = "Which policy types are becoming more/less profitable?",
       x = "Year", y = "Loss Ratio", color = "Policy Type")

## (f) driving experience
## -----------------------
driving_experience1 <- data_fe %>%
  group_by(age_band, exp_band) %>%
  summarise(claim_freq = sum(total_claims) / sum(total_exposure), 
            .groups = "drop") %>%
  ggplot(aes(x = exp_band, y = claim_freq, fill = age_band)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title    = "Claim Frequency by Driving Experience and Age Band",
       subtitle = "Does experience reduce claims independently of age?",
       x = "Driving Experience", y = "Claims per Exposure Year", 
       fill = "Age Band")

## 5. Charts for Risk-Return Analysis
## --------------------------------------
## (a) Frequency-Severity Bubble Chart
## --------------------------------------
#| label: bubble2-plot

# 1. DATA PREP
bubble_data2_yr <- data_fe %>%
  mutate(policy_label = recode(policy_type, !!!policy_type_labels)) %>%
  group_by(year, age_band, policy_label) %>%
  summarise(
    avg_claim_freq     = ifelse(sum(total_exposure) > 0,
                                sum(total_claims) / sum(total_exposure), NA),
    avg_claim_severity = ifelse(sum(total_claims) > 0,
                                sum(total_incurred) / sum(total_claims), NA),
    avg_profit_margin  = ifelse(sum(total_premium) > 0,
                                1 - sum(total_incurred) / sum(total_premium), NA),
    n_policies         = n(),
    .groups            = "drop"
  ) %>%
  filter(
    !is.na(avg_claim_freq),
    !is.na(avg_claim_severity),
    !is.na(avg_profit_margin),
    is.finite(avg_profit_margin),
    avg_claim_severity > 0,
    avg_claim_freq     > 0
  ) %>%
  as.data.frame()

# 2. TEST DATA
test_data <- bubble_data2_yr %>%
  select(year, avg_claim_freq, avg_claim_severity,
         n_policies, policy_label, avg_profit_margin,
         age_band) %>%
  as.data.frame()

# 3. AXIS RANGE
x2_line <- median(test_data$avg_claim_freq)
y2_line <- median(test_data$avg_claim_severity)

x2_range <- max(abs(log10(test_data$avg_claim_freq)     - log10(x2_line)))
y2_range <- max(abs(log10(test_data$avg_claim_severity) - log10(y2_line)))

x2_min <- 10^(log10(x2_line) - x2_range * 1.2)
x2_max <- 10^(log10(x2_line) + x2_range * 1.2)
y2_min <- 10^(log10(y2_line) - y2_range * 1.2)
y2_max <- 10^(log10(y2_line) + y2_range * 1.2)

x2_ticks <- 10^seq(ceiling(log10(x2_min)), floor(log10(x2_max)), by = 1)
y2_ticks <- 10^seq(ceiling(log10(y2_min)), floor(log10(y2_max)), by = 1)

# 4. SETUP
years2    <- sort(unique(test_data$year))
policies2 <- sort(unique(test_data$policy_label))
n_pol2    <- length(policies2)
n_yr2     <- length(years2)


# ------- color changes section -- to ensure correctly based on profit margin ---
test_data <- test_data %>%
  mutate(profit_category = case_when(
    avg_profit_margin < 0    ~ "Loss",
    avg_profit_margin < 0.3  ~ "Low Profit",
    avg_profit_margin < 0.6  ~ "Medium Profit",
    TRUE                     ~ "High Profit"
  ),
  profit_color = case_when(
    avg_profit_margin < 0    ~ "#F44336",   # red
    avg_profit_margin < 0.3  ~ "#FF9800",   # orange
    avg_profit_margin < 0.6  ~ "#FFC107",   # yellow
    TRUE                     ~ "#4CAF50"    # green
  ))

## -------------end color changes section-----------


# 5. BUILD TRACES
traces <- list()
idx <- 1

for (i in seq_along(years2)) {
  yr    <- years2[i]
  df_yr <- test_data[test_data$year == yr, ]
  
  for (j in seq_along(policies2)) {
    pol    <- policies2[j]
    df_pol <- df_yr[df_yr$policy_label == pol, ]
    
    traces[[idx]] <- list(
      x           = df_pol$avg_claim_freq,
      y           = df_pol$avg_claim_severity,
      type        = "scatter",
      mode        = "markers",
      name        = df_pol$profit_category[1],        # profit category as legend
      legendgroup = df_pol$profit_category[1],        # group by profit category
      showlegend  = FALSE,  ## turn off legends
      visible     = (i == 1),
      
      ### ----------------------to use discrete color from section 4.1 ----
      marker = list(
        size     = log(df_pol$n_policies) * 3,   # log transform for balanced sizing
        sizemode = "diameter",
        opacity  = 0.7,
        color    = df_pol$profit_color ,   # direct hex color, no colorscale needed
        line     = list(width = 0)        # removes outline
      ),
      
      ## ---------------------end discrete color section--------------------
      
      ## ----------hover section---------------------------------
      text = paste0(
        "<b>", df_pol$age_band, " - ", df_pol$policy_label, "</b><br>",
        "Claim Frequency: ",  round(df_pol$avg_claim_freq, 3),                     "<br>",
        "Claim Severity: ",   scales::comma(round(df_pol$avg_claim_severity, 0)),  "<br>",
        "Profit: ",           df_pol$profit_category,                              "<br>",
        "Profit Margin: ",    scales::percent(round(df_pol$avg_profit_margin, 3)), "<br>",
        "No. Policies: ",     scales::comma(df_pol$n_policies)
      ),
      hoverinfo = "text"
    )
    
    ## --------------end hover section-----------
    idx <- idx + 1
  }
}

#cat("Traces built:", length(traces), "\n")

# 6. BUTTONS
buttons2 <- lapply(seq_along(years2), function(i) {
  visible <- rep(FALSE, n_yr2 * n_pol2)
  visible[((i - 1) * n_pol2 + 1):(i * n_pol2)] <- TRUE
  
  list(
    method = "update",
    label  = as.character(years2[i]),
    args   = list(
      list(visible = visible),
      list(
        xaxis = list(
          title      = "Claim Frequency (Claims per Exposure Year)",
          type       = "log",
          range      = c(log10(x2_min), log10(x2_max)),
          fixedrange = TRUE,
          tickvals   = log10(x2_ticks),
          ticktext   = as.character(round(x2_ticks, 4))
        ),
        yaxis = list(
          title      = "Claim Severity (Average Cost per Claim)",
          type       = "log",
          range      = c(log10(y2_min), log10(y2_max)),
          fixedrange = TRUE,
          tickvals   = log10(y2_ticks),
          ticktext   = scales::comma(y2_ticks)
        )
      )
    )
  )
})

# 7. BUILD PLOT
p2 <- plot_ly()

for (trace in traces) {
  p2 <- p2 %>% add_trace(
    x           = trace$x,
    y           = trace$y,
    type        = trace$type,
    mode        = trace$mode,
    name        = trace$name,
    legendgroup = trace$legendgroup,
    showlegend  = trace$showlegend,
    visible     = trace$visible,
    marker      = trace$marker,
    text        = trace$text,
    hoverinfo   = trace$hoverinfo
  )
}

# 8. LAYOUT
frequency_severity_bubble_chart <- p2 %>%
  layout(
    showLegend = FALSE, ## turn off legend
    title = list(
      text = "Claim Frequency vs Severity by Age Band and Policy Type",
      font = list(size = 16)
    ),
    xaxis = list(
      title      = "Claim Frequency (Claims per Exposure Year), log scale",
      type       = "log",
      range      = c(log10(x2_min), log10(x2_max)),
      fixedrange = TRUE,
      tickvals   = log10(x2_ticks),
      ticktext   = as.character(round(x2_ticks, 4))
    ),
    yaxis = list(
      title      = "Claim Severity (Average Cost per Claim), log scale",
      type       = "log",
      range      = c(log10(y2_min), log10(y2_max)),
      fixedrange = TRUE,
      tickvals   = log10(y2_ticks),
      ticktext   = scales::comma(y2_ticks)
    ),
    shapes = list(
      list(type = "line",
           x0   = x2_line, x1 = x2_line,
           y0   = y2_min,  y1 = y2_max,
           line = list(dash = "dash", color = "grey")),
      list(type = "line",
           x0   = x2_min,  x1 = x2_max,
           y0   = y2_line, y1 = y2_line,
           line = list(dash = "dash", color = "grey"))
    ),
    annotations = list(
      list(x = log10(x2_max), y = log10(y2_max),
           text      = "High Freq + High Severity",
           showarrow = FALSE, xanchor = "right",
           font      = list(color = "#F44336", size = 11)),
      list(x = log10(x2_min), y = log10(y2_max),
           text      = "Low Freq + High Severity",
           showarrow = FALSE, xanchor = "left",
           font      = list(color = "#FF9800", size = 11)),
      list(x = log10(x2_max), y = log10(y2_min),
           text      = "High Freq + Low Severity",
           showarrow = FALSE, xanchor = "right",
           font      = list(color = "#2196F3", size = 11)),
      list(x = log10(x2_min), y = log10(y2_min),
           text      = "Low Freq + Low Severity",
           showarrow = FALSE, xanchor = "left",
           font      = list(color = "#4CAF50", size = 11)),
      list(x         = 1.15,
           y         = 0.8,
           xref      = "paper",
           yref      = "paper",
           text      = "<b>Select Year</b>",
           showarrow = FALSE,
           xanchor   = "left",
           font      = list(size = 12))
    ),
    updatemenus = list(
      list(
        type        = "buttons",
        direction   = "down",
        active      = 0,
        x           = 1.15,
        xanchor     = "left",
        y           = 0.7,
        yanchor     = "top",
        buttons     = buttons2,
        bgcolor     = "white",
        bordercolor = "grey80",
        font        = list(size = 12)
      )
    ),
    margin = list(r = 200)
  )

## (b) trend changes chart
## ------------------------
library(crosstalk)
library(plotly)

# 1. DATA PREP
bubble_data2_yr <- data_fe %>%
  mutate(policy_label = recode(policy_type, !!!policy_type_labels)) %>%
  group_by(year, age_band, policy_label) %>%
  summarise(
    avg_claim_freq     = ifelse(sum(total_exposure) > 0,
                                sum(total_claims) / sum(total_exposure), NA),
    avg_claim_severity = ifelse(sum(total_claims) > 0,
                                sum(total_incurred) / sum(total_claims), NA),
    avg_profit_margin  = ifelse(sum(total_premium) > 0,
                                1 - sum(total_incurred) / sum(total_premium), NA),
    avg_premium        = mean(total_premium),
    avg_incurred       = mean(total_incurred),
    n_policies         = n(),
    .groups            = "drop"
  ) %>%
  filter(
    !is.na(avg_claim_freq),
    !is.na(avg_claim_severity),
    !is.na(avg_profit_margin),
    is.finite(avg_profit_margin),
    avg_claim_severity > 0,
    avg_claim_freq     > 0
  ) %>%
  mutate(
    profit_category = case_when(
      avg_profit_margin < 0   ~ "Loss",
      avg_profit_margin < 0.3 ~ "Low Profit",
      avg_profit_margin < 0.6 ~ "Medium Profit",
      TRUE                    ~ "High Profit"
    ),
    profit_color = case_when(
      avg_profit_margin < 0   ~ "#F44336",
      avg_profit_margin < 0.3 ~ "#FF9800",
      avg_profit_margin < 0.6 ~ "#FFC107",
      TRUE                    ~ "#4CAF50"
    ),
    segment  = paste(age_band, "-", policy_label),
    group_id = as.integer(factor(segment)),
    log_size = log(n_policies) * 0.8
  ) %>%
  as.data.frame()

# 2. SHARED DATA
d_bubble <- highlight_key(
  bubble_data2_yr %>% filter(year == max(year)),
  ~segment,
  group = "seg"
)

d_line <- highlight_key(
  bubble_data2_yr %>% arrange(group_id, year),
  ~segment,
  group = "seg"
)

# 3. BUBBLE CHART
p_bubble <- ggplot(
  data = d_bubble,
  aes(x     = avg_claim_freq,
      y     = avg_claim_severity,
      size  = log_size,
      color = profit_category,
      text  = paste(age_band, "-", policy_label,   # simplified tooltip
                    "<br>Profit:", profit_category,
                    "<br>Margin:", scales::percent(round(avg_profit_margin, 3)),
                    "<br>Freq:",   round(avg_claim_freq, 3),
                    "<br>Sev:",    scales::comma(round(avg_claim_severity, 0)),
                    "<br>N:",      scales::comma(n_policies))
  )
) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_identity() +
  scale_color_manual(values = c(
    "Loss"          = "#F44336",
    "Low Profit"    = "#FF9800",
    "Medium Profit" = "#FFC107",
    "High Profit"   = "#4CAF50"
  )) +
  geom_vline(xintercept = median(bubble_data2_yr$avg_claim_freq),
             linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = median(bubble_data2_yr$avg_claim_severity),
             linetype = "dashed", color = "grey50") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title    = paste("Claim Frequency vs Severity -", max(bubble_data2_yr$year)),
    subtitle = "Hover on a bubble to trace its profitability trend",
    x        = "Claim Frequency (log)",
    y        = "Claim Severity (log)"
  )

# 4. LINE CHART
p_line <- ggplot() +
  geom_line(
    data = bubble_data2_yr %>% arrange(group_id, year),
    aes(x     = year,
        y     = avg_profit_margin,
        group = group_id),
    color     = "grey70",
    linewidth = 0.5
  ) +
  geom_point(
    data = d_line,
    aes(x     = year,
        y     = avg_profit_margin,
        color = profit_category,
        text  = paste(age_band, "-", policy_label,   # simplified tooltip
                      "<br>Year:",   year,
                      "<br>Profit:", profit_category,
                      "<br>Margin:", scales::percent(round(avg_profit_margin, 3)),
                      "<br>Prem:",   scales::comma(round(avg_premium, 0)),
                      "<br>Inc:",    scales::comma(round(avg_incurred, 0)),
                      "<br>N:",      scales::comma(n_policies))
    ),
    size = 3
  ) +
  geom_hline(yintercept = 0,
             linetype  = "dashed",
             color     = "#F44336",
             linewidth = 0.5) +
  scale_color_manual(values = c(
    "Loss"          = "#F44336",
    "Low Profit"    = "#FF9800",
    "Medium Profit" = "#FFC107",
    "High Profit"   = "#4CAF50"
  )) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = unique(bubble_data2_yr$year)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title    = "Profitability Trend by Segment (2024)",
    subtitle = "All years - hover to highlight segment",
    x        = "Year",
    y        = "Profit Margin"
  )

# 5. PRE-RENDER
p_bubble_ly <- ggplotly(p_bubble, tooltip = "text", source = "bubble")
p_line_ly   <- ggplotly(p_line,   tooltip = "text", source = "line")

# 6. COMBINE AND LINK
trend_chart <- subplot(
  p_bubble_ly,
  p_line_ly,
  nrows  = 1,
  widths = c(0.55, 0.45),
  titleX = TRUE,
  titleY = TRUE
) %>%
  highlight(
    on         = "plotly_hover",
    off        = "plotly_doubleclick",
    persistent = FALSE,
    opacityDim = 0.3,
    debounce   = 100
  ) %>%
  layout(
    hovermode = "closest",
    title     = list(
      text = "Claim Risk Profile vs Profitability Trend by Segment",
      font = list(size = 16),
      x    = 0.5,          # center the title
      xanchor = "center"
    )
  )

## 6 Deep-dive on loss-making portfolios
## ----------------------------------------
## (a) Changes in claim patterns
## ------------------------------
## data wrangling
## ---------------
# DEEP DIVE DATA PREP
deep_dive <- data_fe %>%
  filter(policy_type == "COMP_N") %>%
  filter(age_band %in% c("Under 25", "25-34", "35-44", "65+")) %>%
  mutate(
    policy_label = recode(policy_type, !!!policy_type_labels),
    age_band     = droplevels(age_band)
  )

# Summarise claim frequency by coverage type and year
deep_freq <- deep_dive %>%
  group_by(year, age_band) %>%
  summarise(
    total_exposure          = sum(total_exposure),
    liability_freq          = sum(liability_claims)          / sum(total_exposure),
    liability_property_freq = sum(liability_property_claims) / sum(total_exposure),
    liability_injury_freq   = sum(liability_injury_claims)   / sum(total_exposure),
    property_freq           = sum(property_claims)           / sum(total_exposure),
    theft_freq              = sum(theft_claims)              / sum(total_exposure),
    fire_freq               = sum(fire_claims)               / sum(total_exposure),
    glass_freq              = sum(glass_claims)              / sum(total_exposure),
    legal_freq              = sum(legal_protection_claims)   / sum(total_exposure),
    occupants_freq          = sum(occupants_claims)          / sum(total_exposure),
    .groups = "drop"
  ) %>%
  select(-total_exposure) %>%
  pivot_longer(
    cols      = ends_with("_freq"),
    names_to  = "coverage_type",
    values_to = "claim_freq"
  ) %>%
  mutate(
    coverage_type = recode(coverage_type,
                           "liability_freq"          = "Liability",
                           "liability_property_freq" = "Liability Property",
                           "liability_injury_freq"   = "Liability Injury",
                           "property_freq"           = "Property",
                           "theft_freq"              = "Theft",
                           "fire_freq"               = "Fire",
                           "glass_freq"              = "Glass",
                           "legal_freq"              = "Legal Protection",
                           "occupants_freq"          = "Occupants"
    ),
    claim_freq = ifelse(claim_freq == 0, NA, claim_freq)
  ) %>%
  group_by(age_band) %>%
  complete(year, coverage_type, fill = list(claim_freq = NA)) %>%
  ungroup()

# Derived plotting datasets
deep_freq_points <- deep_freq %>%
  filter(!is.na(claim_freq))

deep_freq_lines <- deep_freq %>%
  filter(!is.na(claim_freq)) %>%
  group_by(age_band, coverage_type) %>%
  filter(n() >= 2) %>%
  ungroup()

# Summarise severity by coverage type and year
deep_sev <- data_fe %>%
  filter(policy_type == "COMP_N") %>%
  filter(age_band %in% c("Under 25", "25-34", "35-44", "65+")) %>%
  mutate(age_band = droplevels(age_band)) %>%
  group_by(year, age_band) %>%
  summarise(
    liability_sev          = ifelse(sum(liability_claims) > 0,
                                    sum(liability_incurred)          / sum(liability_claims), NA),
    liability_property_sev = ifelse(sum(liability_property_claims) > 0,
                                    sum(liability_property_incurred) / sum(liability_property_claims), NA),
    liability_injury_sev   = ifelse(sum(liability_injury_claims) > 0,
                                    sum(liability_injury_incurred)   / sum(liability_injury_claims), NA),
    property_sev           = ifelse(sum(property_claims) > 0,
                                    sum(property_incurred)           / sum(property_claims), NA),
    theft_sev              = ifelse(sum(theft_claims) > 0,
                                    sum(theft_incurred)              / sum(theft_claims), NA),
    fire_sev               = ifelse(sum(fire_claims) > 0,
                                    sum(fire_incurred)               / sum(fire_claims), NA),
    glass_sev              = ifelse(sum(glass_claims) > 0,
                                    sum(glass_incurred)              / sum(glass_claims), NA),
    legal_sev              = ifelse(sum(legal_protection_claims) > 0,
                                    sum(legal_protection_incurred)   / sum(legal_protection_claims), NA),
    occupants_sev          = ifelse(sum(occupants_claims) > 0,
                                    sum(occupants_incurred)          / sum(occupants_claims), NA),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = ends_with("_sev"),
    names_to  = "coverage_type",
    values_to = "claim_sev"
  ) %>%
  mutate(
    coverage_type = recode(coverage_type,
                           "liability_sev"          = "Liability",
                           "liability_property_sev" = "Liability Property",
                           "liability_injury_sev"   = "Liability Injury",
                           "property_sev"           = "Property",
                           "theft_sev"              = "Theft",
                           "fire_sev"               = "Fire",
                           "glass_sev"              = "Glass",
                           "legal_sev"              = "Legal Protection",
                           "occupants_sev"          = "Occupants"
    )
  ) %>%
  group_by(age_band) %>%
  complete(year, coverage_type, fill = list(claim_sev = NA)) %>%
  ungroup()

# Derived plotting datasets
deep_sev_points <- deep_sev %>%
  filter(!is.na(claim_sev))

deep_sev_lines <- deep_sev %>%
  filter(!is.na(claim_sev)) %>%
  group_by(age_band, coverage_type) %>%
  filter(n() >= 2) %>%
  ungroup()

# Colour palette — 9 distinct colours, no yellow
coverage_colours <- c(
  "Fire"                = "#E41A1C",
  "Glass"               = "#377EB8",
  "Legal Protection"    = "#4DAF4A",
  "Liability"           = "#984EA3",
  "Liability Injury"    = "#FF7F00",
  "Liability Property"  = "#A65628",
  "Occupants"           = "#F781BF",
  "Property"            = "#999999",
  "Theft"               = "#00CED1"
)


## #1 Claim Frequency
## -------------------
# CHART 1 — Claim Frequency by Coverage Type over Time
p_freq <- ggplot(deep_freq_lines,
                 aes(x     = year,
                     y     = claim_freq,
                     color = coverage_type,
                     group = coverage_type)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = deep_freq_points, size = 2) +
  facet_wrap(~ age_band, nrow = 2, scales = "free_y") +
  scale_x_continuous(breaks = unique(deep_freq$year)) +
  scale_color_manual(values = coverage_colours) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title    = "Claim Frequency by Coverage Type — Comprehensive (No Excess)",
    subtitle = "Loss-making age bands only",
    x        = "Year",
    y        = "Claims per Exposure Year",
    color    = "Coverage Type"
  )

## #2 Claim Severity
## ----------------------
# CHART 2 — Claim Severity by Coverage Type over Time
p_sev <- ggplot(deep_sev_lines,
                aes(x     = year,
                    y     = claim_sev,
                    color = coverage_type,
                    group = coverage_type)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = deep_sev_points, size = 2) +
  facet_wrap(~ age_band, nrow = 2, scales = "free_y") +
  scale_x_continuous(breaks = unique(deep_sev$year)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = coverage_colours) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title    = "Claim Severity by Coverage Type — Comprehensive (No Excess)",
    subtitle = "Loss-making age bands only",
    x        = "Year",
    y        = "Average Cost per Claim",
    color    = "Coverage Type"
  )

## #3 Composition
## ---------------
# CHART 3 — Claims Count Composition over Time

## 1. Data Prep
deep_count <- data_fe %>%
  filter(policy_type == "COMP_N") %>%
  filter(age_band %in% c("Under 25", "25-34", "35-44", "65+")) %>%
  group_by(year, age_band) %>%
  summarise(
    Liability            = sum(liability_claims),
    "Liability Property" = sum(liability_property_claims),
    "Liability Injury"   = sum(liability_injury_claims),
    Property             = sum(property_claims),
    Theft                = sum(theft_claims),
    Fire                 = sum(fire_claims),
    Glass                = sum(glass_claims),
    "Legal Protection"   = sum(legal_protection_claims),
    Occupants            = sum(occupants_claims),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = -c(year, age_band),
    names_to  = "coverage_type",
    values_to = "claim_count"
  ) %>%
  filter

## 2. Chart
p_count <- deep_count %>%
  ggplot(aes(x    = factor(year),
             y    = claim_count,
             fill = coverage_type,
             text = paste0(
               "Year: ",          year,                                        "<br>",
               "Age Band: ",      age_band,                                    "<br>",
               "Coverage Type: ", coverage_type,                               "<br>",
               "Claim Count: ",   round(claim_count, 2)
             ))) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ age_band, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_fill_manual(values = coverage_colours) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title    = "Claims Composition by Coverage Type — Comprehensive (No Excess)",
    subtitle = "Loss-making age bands only",
    x        = "Year",
    y        = "% of Total Claims",
    fill     = "Coverage Type"
  )

p_count <- ggplotly(p_count, tooltip = "text") %>%
  layout(legend = list(orientation = "h", y = -0.2))


## (b) Driving Factors
## --------------------

# Shared filter
loss_making_bands <- c("Under 25", "25-34", "35-44", "65+")

# Chart 1 data — average driving experience by portfolio group over time
deep_exp_avg <- data_fe %>%
  filter(policy_type == "COMP_N") %>%
  mutate(portfolio = ifelse(age_band %in% loss_making_bands,
                            "Loss-Making", "Profitable")) %>%
  group_by(year, portfolio) %>%
  summarise(
    mean_exp     = mean(driving_exp_proxy, na.rm = TRUE),
    sd_exp       = sd(driving_exp_proxy,   na.rm = TRUE),
    policy_count = n(),
    .groups      = "drop"
  ) %>%
  mutate(
    se_upper = mean_exp + 1.96 * sd_exp / sqrt(policy_count),
    se_lower = mean_exp - 1.96 * sd_exp / sqrt(policy_count)
  )

# Chart 2 data — experience composition within loss-making age bands
deep_exp_comp <- data_fe %>%
  filter(policy_type == "COMP_N",
         age_band %in% loss_making_bands) %>%
  mutate(age_band = droplevels(age_band)) %>%
  group_by(year, age_band, exp_band) %>%
  summarise(policy_count = n(), .groups = "drop") %>%
  filter(!is.na(exp_band))

# Chart 3 data — loss ratio heatmap by age x experience
deep_exp_lr <- data_fe %>%
  filter(policy_type == "COMP_N",
         age_band %in% loss_making_bands) %>%
  mutate(age_band = droplevels(age_band)) %>%
  group_by(year, age_band, exp_band) %>%
  summarise(
    loss_ratio     = sum(total_incurred) / sum(total_premium),
    total_exposure = sum(total_exposure),
    .groups        = "drop"
  ) %>%
  filter(!is.na(exp_band)) %>%
  mutate(
    age_band = as.character(age_band),
    exp_band = as.character(exp_band)
  ) %>%
  group_by(year) %>%
  complete(age_band, exp_band) %>%
  ungroup() %>%
  mutate(
    age_band = factor(age_band, levels = c("Under 25", "25-34", "35-44", "65+")),
    exp_band = factor(exp_band, levels = c("0-2 yrs", "3-5 yrs", "6-10 yrs",
                                           "11-20 yrs", "20+ yrs"))
  )

deep_exp_lr <- deep_exp_lr %>%
  mutate(
    loss_ratio_plot = ifelse(is.na(loss_ratio), -1, loss_ratio),
    label           = ifelse(is.na(loss_ratio), "",
                             scales::percent(round(loss_ratio, 2)))
  )

## #1 Driving Experience — native plot_ly()
## ------------------------------------------
portfolios  <- unique(deep_exp_avg$portfolio)
port_colors <- c("Loss-Making" = "#F44336", "Profitable" = "#4CAF50")

driving_experience <- plot_ly()

for (p in portfolios) {
  df <- deep_exp_avg %>% filter(portfolio == p)
  
  # confidence interval ribbon
  driving_experience <- driving_experience %>%
    add_trace(
      data      = df,
      x         = ~c(year, rev(year)),
      y         = ~c(se_upper, rev(se_lower)),
      type      = "scatter",
      mode      = "lines",
      fill      = "toself",
      fillcolor = ifelse(p == "Loss-Making",
                         "rgba(244,67,54,0.15)",
                         "rgba(76,175,80,0.15)"),
      line       = list(color = "transparent"),
      showlegend = FALSE,
      hoverinfo  = "skip",
      name       = paste0(p, " CI")
    )
  
  # main line + points
  driving_experience <- driving_experience %>%
    add_trace(
      data   = df,
      x      = ~year,
      y      = ~mean_exp,
      type   = "scatter",
      mode   = "lines+markers",
      name   = p,
      color  = I(port_colors[p]),
      line   = list(width = 2),
      marker = list(size = 8),
      text   = ~paste0(
        "Portfolio: ",       portfolio,          "<br>",
        "Year: ",            year,               "<br>",
        "Mean Experience: ", round(mean_exp, 2), " yrs", "<br>",
        "Policy Count: ",    policy_count
      ),
      hoverinfo = "text"
    )
}

driving_experience <- driving_experience %>%
  layout(
    title = list(
      text = "Average Driving Experience — Loss-Making vs Profitable Portfolio",
      font = list(size = 14)
    ),
    xaxis = list(
      title    = "Year",
      tickvals = unique(deep_exp_avg$year),
      ticktext = as.character(unique(deep_exp_avg$year))
    ),
    yaxis     = list(title = "Mean Driving Experience (Years)"),
    legend    = list(orientation = "h", y = -0.2),
    hovermode = "closest"
  )

## #2 Experience Mix — ggplotly
## -----------------------------
p_exp_comp <- deep_exp_comp %>%
  ggplot(aes(x    = factor(year),
             y    = policy_count,
             fill = exp_band,
             text = paste0(
               "Year: ",       year,         "<br>",
               "Age Band: ",   age_band,     "<br>",
               "Experience: ", exp_band,     "<br>",
               "Count: ",      policy_count
             ))) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ age_band, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title    = "Driving Experience Composition — Loss-Making Age Bands",
    subtitle = "Comprehensive (No Excess) | Share of policies by experience band",
    x        = "Year",
    y        = "% of Policies",
    fill     = "Experience Band"
  )

experience_mix <- ggplotly(p_exp_comp, tooltip = "text") %>%
  layout(legend = list(orientation = "h", y = -0.2))

## #3 Age x Experience Heatmap — native plot_ly()
## ------------------------------------------------
make_heatmap <- function(yr) {
  df <- deep_exp_lr %>% filter(year == yr)
  
  plot_ly(
    data      = df,
    x         = ~exp_band,
    y         = ~age_band,
    z         = ~loss_ratio_plot,
    type      = "heatmap",
    text      = ~paste0(
      "Age Band: ",   age_band, "<br>",
      "Experience: ", exp_band, "<br>",
      "Year: ",       year,     "<br>",
      "Loss Ratio: ", ifelse(is.na(loss_ratio), "N/A",
                             scales::percent(round(loss_ratio, 2))), "<br>",
      "Exposure: ",   ifelse(is.na(total_exposure), "N/A",
                             round(total_exposure, 2))
    ),
    hoverinfo  = "text",
    colorscale = list(
      c(0,    "rgb(200,200,200)"),
      c(0.25, "rgb(200,200,200)"),
      c(0.26, "#4CAF50"),
      c(0.5,  "#FFC107"),
      c(1,    "#F44336")
    ),
    zmin      = -1,
    zmax      =  2,
    showscale = (yr == max(deep_exp_lr$year))
  ) %>%
    add_annotations(
      x         = ~exp_band,
      y         = ~age_band,
      text      = ~label,
      showarrow = FALSE,
      font      = list(color = "white", size = 11)
    ) %>%
    layout(
      annotations = list(
        list(
          x         = 0.5,
          y         = 1.05,
          xref      = "paper",
          yref      = "paper",
          text      = as.character(yr),
          showarrow = FALSE,
          font      = list(size = 13)
        )
      ),
      xaxis = list(
        title         = "Driving Experience",
        tickangle     = -45,
        categoryorder = "array",
        categoryarray = c("0-2 yrs", "3-5 yrs", "6-10 yrs", "11-20 yrs", "20+ yrs")
      ),
      yaxis = list(
        title         = "Age Band",
        categoryorder = "array",
        categoryarray = c("Under 25", "25-34", "35-44", "65+")
      )
    )
}

age_exp_heatmap <- subplot(
  make_heatmap(2022),
  make_heatmap(2023),
  make_heatmap(2024),
  nrows  = 1,
  shareY = TRUE,
  titleX = TRUE
) %>%
  layout(
    title = list(
      text = "Loss Ratio by Age Band & Driving Experience",
      font = list(size = 14)
    ),
    margin = list(b = 100)
  )

## 7 Sensitivty Analysis and Scenario Analysis
## -------------------------------------------
## data wrangling
## ---------------
#| label: base-metrics
#| tbl-cap: "Base portfolio — all segments"

# ── Classify segments ──────────────────────────────────────────────────────────
portfolio_base <- data_fe %>%
  mutate(
    portfolio_type = case_when(
      loss_ratio < 0.70 ~ "Profitable",
      loss_ratio < 1.00 ~ "At-Risk",
      TRUE              ~ "Loss-Making"
    ),
    portfolio_type = factor(portfolio_type,
                            levels = c("Profitable", "At-Risk", "Loss-Making"))
  )

# ── Aggregate by segment ───────────────────────────────────────────────────────
base_by_seg <- portfolio_base %>%
  group_by(portfolio_type) %>%
  summarise(
    total_premium  = sum(total_premium,  na.rm = TRUE),
    total_incurred = sum(total_incurred, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    base_pnl       = total_premium - total_incurred,
    base_lr        = total_incurred / total_premium,
    portfolio_type = as.character(portfolio_type)
  )

# ── Add Total row ──────────────────────────────────────────────────────────────
base_all <- bind_rows(
  base_by_seg,
  summarise(base_by_seg,
            portfolio_type = "Total",
            total_premium  = sum(total_premium),
            total_incurred = sum(total_incurred),
            base_pnl       = sum(base_pnl),
            base_lr        = sum(total_incurred) / sum(total_premium)
  )
)

segs   <- base_all$portfolio_type
n_segs <- length(segs)     # 4: Profitable, At-Risk, Loss-Making, Total

base_all %>%
  mutate(
    Premium      = formatC(round(total_premium),  format = "d", big.mark = ","),
    Incurred     = formatC(round(total_incurred), format = "d", big.mark = ","),
    `P&L`        = formatC(round(base_pnl),       format = "d", big.mark = ","),
    `Loss Ratio` = paste0(round(base_lr * 100, 1), "%")
  ) %>%
  select(Segment = portfolio_type, Premium, Incurred, `P&L`, `Loss Ratio`) %>%
  knitr::kable(align = c("l", "r", "r", "r", "c"))

# ── Adjustment steps ───────────────────────────────────────────────────────────
adj_pct    <- c(-20, -15, -10, -5, 0, 5, 10, 15, 20)
adj_labels <- ifelse(adj_pct >= 0, paste0("+", adj_pct, "%"), paste0(adj_pct, "%"))
n_adj      <- length(adj_pct)

# ── Colour palette ─────────────────────────────────────────────────────────────
NEUTRAL    <- "#f8f9fa"
H_SEG      <- "#2c3e50"
H_BASE     <- "#566573"
WHITE      <- "white"

clr_seg <- function(s) {
  dplyr::case_when(
    s == "Profitable"  ~ "#eafaf1",
    s == "At-Risk"     ~ "#fef9e7",
    s == "Loss-Making" ~ "#fdedec",
    TRUE               ~ "#eaf2ff"   # Total
  )
}

clr_dpnl <- function(x) ifelse(x >  1, "#d5f5e3", ifelse(x < -1, "#fadbd8", NEUTRAL))
clr_dlr  <- function(x) ifelse(x < -0.0001, "#d5f5e3", ifelse(x > 0.0001, "#fadbd8", NEUTRAL))

# ── Format helpers ─────────────────────────────────────────────────────────────
fmt_n    <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_pct  <- function(x) paste0(round(x * 100, 1), "%")

fmt_dpnl <- function(x) {
  s <- formatC(abs(round(x)), format = "d", big.mark = ",")
  dplyr::case_when(x >  1 ~ paste0("+", s),
                   x < -1 ~ paste0("\u2212", s),
                   TRUE   ~ "\u2014")
}

fmt_dlr <- function(x) {
  s <- paste0(round(abs(x) * 100, 1), " pp")
  dplyr::case_when(x >  0.0001 ~ paste0("+", s),
                   x < -0.0001 ~ paste0("\u2212", s),
                   TRUE        ~ "\u2014")
}

# ── Shared table-trace builder ─────────────────────────────────────────────────
# d_new: data frame with columns new_pnl, new_lr, delta_pnl, delta_lr
# (one row per segment, same order as base_all)
make_table_trace <- function(fig, d_new, header_color, vis) {
  add_trace(
    fig,
    type    = "table",
    visible = vis,
    
    columnwidth = c(120, 80, 65, 90, 80, 65, 75),
    
    header = list(
      values = c(
        "<b>Segment</b>",
        "<b>Base P&L</b>",  "<b>Base LR</b>",
        "<b>New P&L</b>",   "<b>\u0394 P&L</b>",
        "<b>New LR</b>",    "<b>\u0394 LR</b>"
      ),
      fill   = list(color = c(H_SEG, H_BASE, H_BASE,
                              header_color, header_color,
                              header_color, header_color)),
      font   = list(color = WHITE, size = 11, family = "Arial"),
      align  = "center",
      height = 32
    ),
    
    cells = list(
      values = list(
        segs,
        fmt_n(base_all$base_pnl),  fmt_pct(base_all$base_lr),
        fmt_n(d_new$new_pnl),      fmt_dpnl(d_new$delta_pnl),
        fmt_pct(d_new$new_lr),     fmt_dlr(d_new$delta_lr)
      ),
      fill = list(color = list(
        clr_seg(segs),
        rep(NEUTRAL, n_segs),       rep(NEUTRAL, n_segs),
        clr_dpnl(d_new$delta_pnl), clr_dpnl(d_new$delta_pnl),
        clr_dlr(d_new$delta_lr),   clr_dlr(d_new$delta_lr)
      )),
      font   = list(size = 11, family = "Arial"),
      align  = c("left", "right", "center",
                 "right", "right", "center", "center"),
      height = 30
    )
  )
}

# ── Shared slider builder ──────────────────────────────────────────────────────
make_slider <- function(base_idx, n_traces) {
  steps <- lapply(seq_len(n_traces), function(i) {
    vis    <- rep(FALSE, n_traces)
    vis[i] <- TRUE
    list(
      args   = list(list(visible = as.list(vis))),
      label  = adj_labels[i],
      method = "restyle"
    )
  })
  list(
    list(
      active       = base_idx - 1,
      steps        = steps,
      x            = 0.0,
      len          = 1.0,
      xanchor      = "left",
      y            = 0, ### adjust distance between slider and table
      pad          = list(t = 20, b = 5), ## adjust slider
      bgcolor      = "#ecf0f1",
      bordercolor  = "#bdc3c7",
      currentvalue = list(
        prefix  = "At-Risk adjustment: ",
        visible = TRUE,
        xanchor = "center",
        font    = list(size = 13, color = "#2c3e50")
      ),
      font = list(size = 11)
    )
  )
}

## sensitivity analysis - record counts
## ------------------------------------
H_REC    <- "#1a5276"
base_idx <- which(adj_pct == 0)

# Pull out the At-Risk base row once
ar <- filter(base_all, portfolio_type == "At-Risk")

fig_rec <- plot_ly()

for (i in seq_along(adj_pct)) {
  fac <- 1 + adj_pct[i] / 100
  vis <- (i == base_idx)
  
  # At-Risk: both premium and incurred scale => LR unchanged
  ar_new_prem <- ar$total_premium  * fac
  ar_new_inc  <- ar$total_incurred * fac
  
  # Rebuild full table: only At-Risk row changes
  d_new <- base_all %>%
    mutate(
      new_premium  = dplyr::if_else(portfolio_type == "At-Risk",
                                    ar_new_prem, total_premium),
      new_incurred = dplyr::if_else(portfolio_type == "At-Risk",
                                    ar_new_inc,  total_incurred),
      # Recalculate Total row
      new_premium  = dplyr::if_else(
        portfolio_type == "Total",
        sum(dplyr::if_else(segs == "At-Risk", ar_new_prem, total_premium)[segs != "Total"]),
        new_premium),
      new_incurred = dplyr::if_else(
        portfolio_type == "Total",
        sum(dplyr::if_else(segs == "At-Risk", ar_new_inc, total_incurred)[segs != "Total"]),
        new_incurred),
      new_pnl   = new_premium  - new_incurred,
      new_lr    = new_incurred / new_premium,
      delta_pnl = new_pnl - base_pnl,
      delta_lr  = new_lr  - base_lr
    )
  
  fig_rec <- make_table_trace(fig_rec, d_new, H_REC, vis)
}

fig_rec <- layout(
  fig_rec,
  title  = list(
    text = paste0(
      "<b>Sensitivity 1: At-Risk Records</b>",
      "<br><sup>Profitable and Loss-Making segments unchanged &mdash; ",
      "only At-Risk and Total rows move</sup>"
    ),
    x    = 0.01,
    font = list(size = 14)
  ),
  sliders = make_slider(base_idx, n_adj),
  height = 420, ## adjust gap below slider
  margin  = list(t = 90, b = 90, l = 10, r = 10) ## b --> tightens below slider
)

## sensivity analysis - claim frequency
## ------------------------------------
H_FREQ <- "#1e8449"

fig_freq <- plot_ly()

for (i in seq_along(adj_pct)) {
  fac <- 1 + adj_pct[i] / 100
  vis <- (i == base_idx)
  
  # At-Risk: only incurred scales
  ar_new_inc <- ar$total_incurred * fac
  
  d_new <- base_all %>%
    mutate(
      new_premium  = total_premium,
      new_incurred = dplyr::if_else(portfolio_type == "At-Risk",
                                    ar_new_inc, total_incurred),
      new_incurred = dplyr::if_else(
        portfolio_type == "Total",
        sum(dplyr::if_else(segs == "At-Risk", ar_new_inc, total_incurred)[segs != "Total"]),
        new_incurred),
      new_pnl   = new_premium  - new_incurred,
      new_lr    = new_incurred / new_premium,
      delta_pnl = new_pnl - base_pnl,
      delta_lr  = new_lr  - base_lr
    )
  
  fig_freq <- make_table_trace(fig_freq, d_new, H_FREQ, vis)
}

fig_freq <- layout(
  fig_freq,
  title = list(
    text = paste0(
      "<b>Sensitivity 2: At-Risk Claim Frequency</b>",
      "<br><sup>Premium held constant &mdash; ",
      "a +10% frequency means 10% more claims at the same average cost</sup>"
    ),
    x    = 0.01,
    font = list(size = 14)
  ),
  sliders = make_slider(base_idx, n_adj),
  height = 420, 
  margin  = list(t = 90, b = 90, l = 10, r = 10)
)


## scenario analysis
## ------------------
scenarios <- list(
  list(
    name    = "Base Case",
    records =   0, freq =   0, sev =   0,
    color   = "#566573",
    note    = "Current portfolio — no change to records, frequency, or severity"
  ),
  list(
    name    = "Worst Case",
    records = -15, freq =  20, sev =  20,
    color   = "#c0392b",
    note    = "No new business, book matures (-15% records), claims spike (+20% freq, +20% sev)"
  ),
  list(
    name    = "Best Case",
    records =  15, freq = -15, sev = -10,
    color   = "#1e8449",
    note    = "Strong new business (+15% records), fewer and cheaper claims (-15% freq, -10% sev)"
  )
)

n_sc       <- length(scenarios)
fig_scen   <- plot_ly()

for (j in seq_along(scenarios)) {
  sc  <- scenarios[[j]]
  vis <- (j == 1)
  
  r_fac <- 1 + sc$records / 100
  f_fac <- 1 + sc$freq    / 100
  s_fac <- 1 + sc$sev     / 100
  
  d_sc <- base_all %>%
    mutate(
      new_premium  = dplyr::if_else(
        portfolio_type == "At-Risk", total_premium * r_fac, total_premium),
      new_incurred = dplyr::if_else(
        portfolio_type == "At-Risk", total_incurred * r_fac * f_fac * s_fac, total_incurred),
      # Recalculate Total
      new_premium  = dplyr::if_else(
        portfolio_type == "Total",
        sum(dplyr::if_else(segs == "At-Risk",
                           ar$total_premium  * r_fac, total_premium)[segs  != "Total"]),
        new_premium),
      new_incurred = dplyr::if_else(
        portfolio_type == "Total",
        sum(dplyr::if_else(segs == "At-Risk",
                           ar$total_incurred * r_fac * f_fac * s_fac,
                           total_incurred)[segs != "Total"]),
        new_incurred),
      new_pnl   = new_premium  - new_incurred,
      new_lr    = new_incurred / new_premium,
      delta_pnl = new_pnl - base_pnl,
      delta_lr  = new_lr  - base_lr
    )
  
  fig_scen <- add_trace(
    fig_scen,
    type    = "table",
    visible = vis,
    
    columnwidth = c(120, 80, 65, 90, 80, 65, 75),
    
    header = list(
      values = c(
        "<b>Segment</b>",
        "<b>Base P&L</b>",  "<b>Base LR</b>",
        "<b>New P&L</b>",   "<b>\u0394 P&L</b>",
        "<b>New LR</b>",    "<b>\u0394 LR</b>"
      ),
      fill   = list(color = c(H_SEG, H_BASE, H_BASE,
                              sc$color, sc$color,
                              sc$color, sc$color)),
      font   = list(color = WHITE, size = 11, family = "Arial"),
      align  = "center",
      height = 32
    ),
    
    cells = list(
      values = list(
        segs,
        fmt_n(base_all$base_pnl),  fmt_pct(base_all$base_lr),
        fmt_n(d_sc$new_pnl),       fmt_dpnl(d_sc$delta_pnl),
        fmt_pct(d_sc$new_lr),      fmt_dlr(d_sc$delta_lr)
      ),
      fill = list(color = list(
        clr_seg(segs),
        rep(NEUTRAL, n_segs),       rep(NEUTRAL, n_segs),
        clr_dpnl(d_sc$delta_pnl),  clr_dpnl(d_sc$delta_pnl),
        clr_dlr(d_sc$delta_lr),    clr_dlr(d_sc$delta_lr)
      )),
      font   = list(size = 11, family = "Arial"),
      align  = c("left", "right", "center",
                 "right", "right", "center", "center"),
      height = 30
    )
  )
}

# ── Scenario buttons ───────────────────────────────────────────────────────────
fmt_asmp <- function(x) ifelse(x >= 0, paste0("+", x, "%"), paste0(x, "%"))

buttons_scen <- lapply(seq_along(scenarios), function(j) {
  sc     <- scenarios[[j]]
  vis    <- rep(FALSE, n_sc)
  vis[j] <- TRUE
  list(
    args   = list(
      list(visible = as.list(vis)),
      list(title = list(
        text = paste0(
          "<b>", sc$name, "</b><br>",
          "<sup>", sc$note, "<br>",
          "Records: ", fmt_asmp(sc$records),
          "  |  Frequency: ", fmt_asmp(sc$freq),
          "  |  Severity: ",  fmt_asmp(sc$sev), "</sup>"
        ),
        x    = 0.01,
        font = list(size = 20, family = "Arial") ## this also adjust the text
      ))
    ),
    label  = sc$name,
    method = "update"
  )
})

fig_scen <- layout(
  fig_scen,
  title = list(
    text = paste0(
      "<b>Base Case</b><br>",
      "<sup>Current portfolio — no change to records, frequency, or severity<br>",
      "Records: +0%  |  Frequency: +0%  |  Severity: +0%</sup>"
    ),
    x    = 0.01,
    font = list(size = 20, family = "Arial") ## adjust the text size above the buttons
  ),
  updatemenus = list(
    list(
      type        = "buttons",
      direction   = "right",
      x           = 0.0,
      xanchor     = "left",
      y           = 1.35, # adjust the y-position of the buttons
      yanchor     = "top",
      buttons     = buttons_scen,
      font        = list(size = 16, family = "Arial", color = "#2c3e50"),
      bgcolor     = "#ecf0f1",
      bordercolor = "#bdc3c7",
      borderwidth = 2,
      pad         = list(t = 8, b = 8, l = 10, r = 10)   # adjust space in button
    )
  ),
  height = 380,
  margin = list(t = 200, b = 20, l = 10, r = 10) ## adjust 'space' for title + buttons
)

