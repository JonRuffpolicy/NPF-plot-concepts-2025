#install required packages if not already installed
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("ggalluvial")

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggalluvial)

plot_colors <- c(
  "None" = "#999999",
  "Structure" = "#ff0000",         # Red
  "Valence" = "#008000",           # Green
  "Causality" = "#0000ff",         # Blue
  "Structure & Valence" = "#b25a00", # Orange-brown blend
  "Structure & Causality" = "#ff00ff", # Magenta
  "Valence & Causality" = "#007fff",   # Sky blue
  "All 3" = "#804080"              # Purple
)
# Read the Excel data (adjust the file path as needed)
data <- read_excel("FILEPATH")

# Define the desired order of categories
cat_order <- c("None", "Structure", "Valence", "Causality", "Structure & Valence", "Structure & Causality", "Valence & Causality", "All 3")

# Create categories for D side and M side, and convert them to factors with the desired order
data <- data %>%
  mutate(
    D_cat = case_when(
      `D-Structure` == 0 & `D-Valence` == 0 & `D-Causality` == 0 ~ "None",
      `D-Structure` == 1 & `D-Valence` == 0 & `D-Causality` == 0 ~ "Structure",
      `D-Structure` == 0 & `D-Valence` == 1 & `D-Causality` == 0 ~ "Valence",
      `D-Structure` == 0 & `D-Valence` == 0 & `D-Causality` == 1 ~ "Causality",
      `D-Structure` == 1 & `D-Valence` == 1 & `D-Causality` == 0 ~ "Structure & Valence",
      `D-Structure` == 1 & `D-Valence` == 0 & `D-Causality` == 1 ~ "Structure & Causality",
      `D-Structure` == 0 & `D-Valence` == 1 & `D-Causality` == 1 ~ "Valence & Causality",
      `D-Structure` == 1 & `D-Valence` == 1 & `D-Causality` == 1 ~ "All 3"
    ),
    M_cat = case_when(
      `M-Structure` == 0 & `M-Valence` == 0 & `M-Causality` == 0 ~ "None",
      `M-Structure` == 1 & `M-Valence` == 0 & `M-Causality` == 0 ~ "Structure",
      `M-Structure` == 0 & `M-Valence` == 1 & `M-Causality` == 0 ~ "Valence",
      `M-Structure` == 0 & `M-Valence` == 0 & `M-Causality` == 1 ~ "Causality",
      `M-Structure` == 1 & `M-Valence` == 1 & `M-Causality` == 0 ~ "Structure & Valence",
      `M-Structure` == 1 & `M-Valence` == 0 & `M-Causality` == 1 ~ "Structure & Causality",
      `M-Structure` == 0 & `M-Valence` == 1 & `M-Causality` == 1 ~ "Valence & Causality",
      `M-Structure` == 1 & `M-Valence` == 1 & `M-Causality` == 1 ~ "All 3"
    ),
    
    # Convert to factors with the specified ordering
    D_cat = factor(D_cat, levels = cat_order),
    M_cat = factor(M_cat, levels = cat_order)
  )

# Aggregate counts for each (D_cat, M_cat) pair
agg_data <- data %>%
  group_by(D_cat, M_cat) %>%
  summarise(freq = n(), .groups = 'drop')

# Calculate the total number of articles
total <- nrow(data)

ggplot(agg_data, aes(axis1 = D_cat, axis2 = M_cat, y = freq)) +
  geom_alluvium(aes(fill = D_cat), width = 1/12) +
  geom_stratum(width = 2.2/12, fill = "grey", color = "black") +
  # Add labels that include the category name, count, and percentage
  geom_text(stat = "stratum", 
            aes(label = paste0(after_stat(stratum), "\n", 
                               after_stat(count), " (", 
                               round(after_stat(count) / total * 100, 0), "%)")),
            size = 2.3,) +
  scale_x_discrete(limits = c("Definitions of Plot", "Measurements of Plot"), expand = c(.1, .1)) +
  scale_fill_manual(
    values = plot_colors,
    name = "Definition Category"
  )
  labs(y = "Frequency",
       x = "",
       title = "Sankey Diagram: Transition from Definition to Measurement") +
  theme_minimal()

  ggsave("sankey_plot_transparent.png",
         width = 10, height = 6, dpi = 300,
         bg = "transparent")  # This line makes the background transparent
