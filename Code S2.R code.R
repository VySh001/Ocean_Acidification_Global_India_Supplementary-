# --- Libraries ---
library(readxl)
library(reshape2)
library(ggplot2)
library(scales)

# --- Read matrix from your Excel ---
file_path <- "C:/Users/vysakhs/Downloads/figure 3 (1).xlsx"
tab <- read_excel(file_path, sheet = "Figure3_Matrix")

# make row/col names
row_names <- tab[[1]]
mat <- as.matrix(tab[, -1])
rownames(mat) <- row_names
colnames(mat) <- colnames(tab)[-1]

# --- Long format, with explicit axis order ---
df <- melt(mat, varnames = c("Row", "Col"), value.name = "val")
df$Row <- factor(df$Row, levels = rownames(mat))        # <- ensures Y labels show
df$Col <- factor(df$Col, levels = colnames(mat))        # <- ensures X order

# --- Heatmap (drivers on BOTH axes) ---
p <- ggplot(df, aes(x = Col, y = Row, fill = val)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", val)), size = 3) +
  scale_fill_gradientn(
    colors = c("#2166AC", "white", "#053061"),
    values = rescale(c(-2, 0, 2)),
    breaks  = seq(-2, 2, 0.5),
    limits  = c(-2, 2),
    name = "Correlation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(hjust = 1),      # <- print Y labels
    panel.grid  = element_blank(),
    aspect.ratio = 1,
    plot.margin = margin(20, 20, 20, 140)       # <- extra left space for Y labels
  ) +
  labs(title = "Driver Interaction Heatmap", x = NULL, y = NULL) +
  coord_fixed()

# view
print(p)

# save (journal quality)
ggsave("figure3_heatmap.png", p, width = 10, height = 8, dpi = 600)
