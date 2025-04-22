# Install if needed
#install.packages("eulerr")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("ggforce")

library(readxl)
library(eulerr)
library(ggplot2)
library(ggforce)

# Load data
df <- read_excel("FILEPATH")

# Convert binary columns to logical
df[c("D-Structure", "D-Valence", "D-Causality",
     "M-Structure", "M-Valence", "M-Causality")] <-
  lapply(df[c("D-Structure", "D-Valence", "D-Causality",
              "M-Structure", "M-Valence", "M-Causality")], as.logical)

# ---------- FUNCTION TO PLOT EULER WITH LABELS ----------
plot_euler_with_labels <- function(euler_obj, title, filename, fallback_positions = NULL) {
  cat("=== Starting plot_euler_with_labels ===\n")
  
  # Circle (set) data
  circle_df <- as.data.frame(euler_obj$ellipses)
  circle_df$label <- rownames(circle_df)
  circle_df$r <- rowMeans(circle_df[, c("a", "b")])
  
  # External label positions
  circle_df$x_label <- circle_df$h
  circle_df$y_label <- circle_df$k
  circle_df$x_label[circle_df$label == "Valence"] <- circle_df$h[circle_df$label == "Valence"] - circle_df$r[circle_df$label == "Valence"] + 5.0
  circle_df$y_label[circle_df$label == "Valence"] <- circle_df$k[circle_df$label == "Valence"] - 3.0
  circle_df$x_label[circle_df$label == "Causality"] <- circle_df$h[circle_df$label == "Causality"] + circle_df$r[circle_df$label == "Causality"] - 5.5
  circle_df$y_label[circle_df$label == "Causality"] <- circle_df$k[circle_df$label == "Causality"] - 0.0
  circle_df$x_label[circle_df$label == "Structure"] <- circle_df$k[circle_df$label == "Structure"] + circle_df$r[circle_df$label == "Structure"] - 5.0
  circle_df$y_label[circle_df$label == "Structure"] <- circle_df$k[circle_df$label == "Structure"] + 5.0
  
  # Region counts and label text
  region_df <- as.data.frame(euler_obj$original.values)
  region_df$region <- rownames(region_df)
  colnames(region_df)[1] <- "count"
  region_df$percent <- round(100 * region_df$count / sum(region_df$count), 0)
  region_df$label_text <- paste0(region_df$count, "\n(", region_df$percent, "%)")
  
  # Try to get coordinates from eulerr
  coords_raw <- plot(euler_obj, quantities = TRUE, return_data = TRUE)$quantities
  coords_df <- as.data.frame(coords_raw)
  coords_df$region <- rownames(coords_df)
  
  if (ncol(coords_df) >= 2 && !all(c("x", "y") %in% names(coords_df))) {
    names(coords_df)[1:2] <- c("x", "y")
  }
  
  # Final label_df with fallback if needed
  if (all(c("x", "y") %in% names(coords_df)) && nrow(coords_df) > 0) {
    coords_df <- coords_df[complete.cases(coords_df[, c("x", "y")]), ]
    label_df <- merge(region_df, coords_df, by = "region")
    cat("USING EULERR COORDINATES\n")
    print(label_df)
  } else {
    if (is.null(fallback_positions)) {
      coords_pattern <- data.frame(
        x = c(0, -1.5, 1.5, -0.5, 0.5, 0, 0),
        y = c(2.8, 0.8, -1.6, 1.8, 0.4, -0.6, 0.8)
      )
      fallback_positions <- data.frame(
        region = region_df$region,
        x = rep(coords_pattern$x, length.out = nrow(region_df)),
        y = rep(coords_pattern$y, length.out = nrow(region_df))
      )
    }
    
    label_df <- merge(region_df, fallback_positions, by = "region")
    label_df <- label_df[!is.na(label_df$label_text), ]
    cat("USING FALLBACK COORDINATES\n")
    print(label_df)
  }
  
  # Build plot
  p <- ggplot() +
    geom_circle(data = circle_df, aes(x0 = h, y0 = k, r = r, fill = label),
                alpha = 0.4, color = "black") +
    geom_text(data = circle_df, aes(x = x_label, y = y_label, label = label),
              size = 5, fontface = "bold") +
    geom_text(data = label_df, aes(x = x, y = y, label = label_text),
              size = 5, fontface = "bold", color = "black") +
    coord_fixed() +
    theme_void() +
    scale_fill_manual(values = c("blue", "red", "green")) +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  
  # Save to your folder
  full_path <- file.path("C:/Users/ruffjo/OneDrive - Oregon State University/Desktop/Publications/2025/Appendices/NPF Plot/Venn", filename)
  ggsave(full_path, p, width = 9, height = 6, dpi = 300)
}

# ---------- D VARIABLES ----------
d_euler <- euler(setNames(
  df[, c("D-Causality", "D-Valence", "D-Structure")],
  c("Causality", "Valence", "Structure")
))

# Optional: fallback coordinates
d_fallback <- data.frame(
  region = c(
    "Structure", "Valence", "Causality",
    "Valence&Structure", "Causality&Structure",
    "Causality&Valence", "Causality&Valence&Structure"
  ),
  x = c(0, 3, -3, 0, .5, -1, -0.2),
  y = c(3, -3, -2, 2, 0.3, -1.5, -0.3)
)

plot_euler_with_labels(
  d_euler,
  "Descriptions of Plot in NPF Research (n=73)",
  "D_plot_euler_labeled.png",
  fallback_positions = d_fallback
)

# ---------- M VARIABLES ----------
m_euler <- euler(setNames(
  df[, c("M-Causality", "M-Valence", "M-Structure")],
  c("Causality", "Valence", "Structure")
))

m_fallback <- data.frame(
  region = c(
    "Structure", "Valence", "Causality",
    "Valence&Structure", "Causality&Structure",
    "Causality&Valence", "Causality&Valence&Structure"
  ),
  x = c(0, 0, -2.8, 2, -3, 0.8, 0),
  y = c(3.2, -1, -2.3, 0, 0, -1.2, 0.3)
)

plot_euler_with_labels(
  m_euler,
  "Approaches to Measuring Plot in NPF Research (n=59)",
  "M_plot_euler_labeled.png",
  fallback_positions = m_fallback
)
