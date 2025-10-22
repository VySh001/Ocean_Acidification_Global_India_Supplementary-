# --- Publication-Quality Heatmap for Figure 1 ---

# 1. Install and Load Necessary Libraries
if (!require("viridis")) install.packages("viridis")
if (!require("revtools")) install.packages("revtools")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")

library(viridis)
library(revtools)
library(ggplot2)
library(dplyr)
library(stringr)

# 2. Load Bibliographic Data (use your Desktop file path)
file_path <- "C:/Users/vysakhs/Desktop/articles.ris"
if (!file.exists(file_path)) {
  stop("Error: 'articles.ris' not found at the specified location.")
}
full_data <- read_bibliography(file_path)

# 3. Data Preparation: Extract Countries, Filter Years
country_list <- c(
  "United States", "China", "United Kingdom", "Germany", "Australia", "Canada",
  "France", "Japan", "Spain", "Italy", "Netherlands", "Sweden", "Switzerland",
  "South Korea", "Brazil", "India", "Russia", "Belgium", "Denmark", "Norway",
  "Finland", "Portugal", "New Zealand", "South Africa", "Mexico", "Chile",
  "Argentina", "Ireland", "Austria", "Poland", "Israel", "Taiwan", "Singapore",
  "Hong Kong", "Iran", "Turkey", "Greece", "Saudi Arabia", "Egypt", "Thailand"
)

processed_data <- full_data %>%
  filter(year >= 2005 & year <= 2025) %>%
  mutate(Country = str_extract(institution, paste(country_list, collapse = "|"))) %>%
  filter(!is.na(Country))

# 4. Get Top 25 Countries by Number of Publications
top_25_countries <- processed_data %>%
  count(Country, name = "TotalPublications") %>%
  arrange(desc(TotalPublications)) %>%
  slice_head(n = 25)

# 5. Prepare Plotting Data
plot_data <- processed_data %>%
  filter(Country %in% top_25_countries$Country) %>%
  count(Country, year, name = "Publications") %>%
  mutate(
    Country = factor(Country, levels = rev(top_25_countries$Country)),
    is_india = ifelse(Country == "India", "Yes", "No")
  )

# 6. Create the Heatmap
heatmap_plot <- ggplot(plot_data, aes(x = as.factor(year), y = Country, fill = Publications)) +
  geom_tile(aes(color = is_india, linewidth = is_india)) +
  geom_text(aes(label = Publications), color = "black", size = 3) +
  scale_fill_viridis_c(option = "D", name = "No. of Publications") +
  scale_color_manual(values = c("Yes" = "blue", "No" = "white"), guide = "none") +
  scale_linewidth_manual(values = c("Yes" = 1.2, "No" = 0.5), guide = "none") +
  theme_minimal() +
  labs(
    title = "Top 25 Most Productive Countries in Ocean Acidification Research (2005–2025)",
    x = "Publication Year",
    y = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black",
                               face = ifelse(levels(plot_data$Country) == "India", "bold.italic", "plain")),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# 7. Display and Save the Figure
print(heatmap_plot)
ggsave(
  "Figure1_Heatmap_1200dpi.png",
  plot = heatmap_plot,
  dpi = 1200,
  width = 12,
  height = 9,
  units = "in"
)

cat("--- Plot saved as 'Figure1_Heatmap_1200dpi.png' ---\n")


