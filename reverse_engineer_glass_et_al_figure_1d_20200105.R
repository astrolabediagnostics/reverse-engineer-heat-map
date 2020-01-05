# Reverse engineer the heat map in Glass et al., Figure 1D.

library(ggplot2)
`%>%` <- dplyr::`%>%`

# Path to PNG and CSV files.
path <- "."

# Map the rows to the cell subsets.
cell_subset_rows <- 
  c(
    Transitional = 50,
    Naive = 160,
    `Non-Switched` = 270,
    Switched = 380
  )
# Import the CSV with marker names and the column of each marker.
marker_cols <-
  tibble::deframe(read.csv(
    file.path(path, "marker_cols.csv"), stringsAsFactors = FALSE))
# Import legend PNG, keep only one row, and map to "Median scaled expression"
# values. "0" and "0.86" are taken from the legend labels in the image.
legend_png <- png::readPNG(file.path(path, "legend.png"))
legend_mtx <- legend_png[10, , ]
legend_vals <- seq(0, 0.86, length.out = nrow(legend_mtx))

# Import the heat map PNG and map the pixel RGB value of each (Subset, Marker)
# combination to its respective "Median scaled expression" value.
heat_map_png <- png::readPNG(file.path(path, "heat_map.png"))
heat_map_df <- lapply(names(marker_cols), function(marker) {
  lapply(names(cell_subset_rows), function(cell_subset) {
    v <- t(heat_map_png[cell_subset_rows[cell_subset], marker_cols[marker], ])
    dists <- apply(legend_mtx, 1, function(x) sqrt(sum((x - v) ^ 2)))
    data.frame(
      Marker = marker,
      CellSubset = cell_subset,
      Median = legend_vals[which.min(dists)],
      stringsAsFactors = FALSE
    )
  }) %>% dplyr::bind_rows()
}) %>% dplyr::bind_rows()

# Reproduce the heat map using the reverse-engineered values. Make sure we stick
# to the same row and column order.
heat_map_df$Marker <- 
  factor(heat_map_df$Marker, levels = names(marker_cols))
heat_map_df$CellSubset <-
  factor(heat_map_df$CellSubset, levels = rev(names(cell_subset_rows)))
obj <- 
  ggplot(heat_map_df, aes(x = Marker, y = CellSubset)) +
  geom_tile(aes(fill = Median), color = "white") +
  scale_fill_gradient2(
    name = "Median Scaled Expression",
    low = "black", mid = "red", high = "yellow",
    midpoint = 0.4) +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.4),
        axis.title = element_blank(),
        legend.position = "bottom",
        panel.background = element_blank())
ggsave(file.path(path, "heat_map_reproduction.png"),
       obj, width = 12, height = 3)
