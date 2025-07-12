# Install required packages if not already installed
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggalluvial")

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggalluvial)

# Define custom colors
plot_colors <- c(
    "None" = "#999999",
    "Structure" = "#ff0000",            # Red
    "Valence" = "#008000",              # Green
    "Causality" = "#0000ff",            # Blue
    "Structure & Valence" = "#b25a00",   # Orange-brown blend
    "Structure & Causality" = "#ff00ff", # Magenta
    "Valence & Causality" = "#007fff",   # Sky blue
    "Structure, Valence,\n& Causality" = "#804080" # Purple with line break between Valence & Causality
)

# Read data from Excel (update the file path as needed)
data <- read_excel("FILEPATH")

# Define desired order of categories with line break
cat_order <- c("None", "Structure", "Valence", "Causality",
               "Structure & Valence", "Structure & Causality",
               "Valence & Causality", "Structure, Valence,\n& Causality")

# Categorize data and convert to factors
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
            `D-Structure` == 1 & `D-Valence` == 1 & `D-Causality` == 1 ~ "Structure, Valence,\n& Causality"
        ),
        M_cat = case_when(
            `M-Structure` == 0 & `M-Valence` == 0 & `M-Causality` == 0 ~ "None",
            `M-Structure` == 1 & `M-Valence` == 0 & `M-Causality` == 0 ~ "Structure",
            `M-Structure` == 0 & `M-Valence` == 1 & `M-Causality` == 0 ~ "Valence",
            `M-Structure` == 0 & `M-Valence` == 0 & `M-Causality` == 1 ~ "Causality",
            `M-Structure` == 1 & `M-Valence` == 1 & `M-Causality` == 0 ~ "Structure & Valence",
            `M-Structure` == 1 & `M-Valence` == 0 & `M-Causality` == 1 ~ "Structure & Causality",
            `M-Structure` == 0 & `M-Valence` == 1 & `M-Causality` == 1 ~ "Valence & Causality",
            `M-Structure` == 1 & `M-Valence` == 1 & `M-Causality` == 1 ~ "Structure, Valence,\n& Causality"
        ),
        D_cat = factor(D_cat, levels = cat_order),
        M_cat = factor(M_cat, levels = cat_order)
    )

# Aggregate data for plotting
agg_data <- data %>%
    group_by(D_cat, M_cat) %>%
    summarise(freq = n(), .groups = 'drop')

# Calculate total count for percentages
total <- nrow(data)

# Generate Sankey plot with widened gray bars (removed title)
ggplot(agg_data, aes(axis1 = D_cat, axis2 = M_cat, y = freq)) +
    geom_alluvium(aes(fill = D_cat), width = 1/12) +
    geom_stratum(width = 2.7/12, fill = "grey", color = "black") +
    geom_text(stat = "stratum",
              aes(label = paste0(after_stat(stratum), "\n",
                                 after_stat(count), " (",
                                 round(after_stat(count) / total * 100, 0), "%)")),
              size = 2.5) +
    scale_x_discrete(limits = c("Definitions of Plot", "Measurements of Plot"), expand = c(.1, .1)) +
    scale_fill_manual(values = plot_colors, name = "Definition Category") +
    labs(y = "Frequency", x = "") +
    theme_minimal()

# Save the plot with transparent background
ggsave("sankey_plot_transparent.png",
       width = 12, height = 6, dpi = 300,
       bg = "transparent")
