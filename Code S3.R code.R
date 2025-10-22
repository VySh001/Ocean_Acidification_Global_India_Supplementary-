
library(ggplot2)
library(reshape2)

# Data input
oa_strategies <- c(
  "Artificial Reef Structures",
  "CO₂ Emission Reduction",
  "Coastal Pollution Control",
  "Ecosystem-Based Fisheries Management",
  "Enhanced Chemical Weathering (ECW)",
  "Enhanced Terrestrial Weathering",
  "Hybridisation for OA Resilience",
  "Integrated Multi-Trophic Aquaculture (IMTA)",
  "Marine Protected Areas (MPAs)",
  "Monitoring & Early Warning Systems",
  "Ocean Alkalinity Enhancement (OAE)",
  "Seagrass & Algae Restoration",
  "Selective Breeding",
  "Shell Reef Restoration"
)

feasibility <- c(7, 7, 6, 6, 4, 4, 6, 7, 7, 8, 5, 7, 8, 5)
cost_impact <- c(9, 9, 6, 8, 10, 5, 8, 8, 7, 9, 6, 7, 8, 6)

# Create dataframe with exact column names (including space)
data <- data.frame(
  Strategy = oa_strategies,
  Feasibility = feasibility,
  "Cost Impact" = cost_impact,
  check.names = FALSE
)

# Reshape for heatmap (melt works with check.names = FALSE)
data_melt <- melt(data, id.vars = "Strategy")

# Set order for y-axis
data_melt$variable <- factor(data_melt$variable, levels = c("Feasibility", "Cost Impact"))

# Plot
p <- ggplot(data_melt, aes(x = Strategy, y = variable, fill = value)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = value), color = "black", size = 4.5, fontface = "bold") +
  scale_fill_gradient2(low = "#4575b4", mid = "#f7f7f7", high = "#d73027", midpoint = 7,
                       name = NULL) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 15),
    axis.text.y = element_text(size = 15, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

print(p)

ggsave("OA_Strategy_Matrix_1200dpi.png", plot = p,
       width = 14, height = 5, dpi = 1200, bg = "white")


