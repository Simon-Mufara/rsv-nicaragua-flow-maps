###############################################################################
# RSV-A / RSV-B viral flow maps for Nicaragua
# ---------------------------------------------------------------------------
# For each virus: a world map of
#   IMPORTS : introductions INTO Nicaragua from other regions
#   EXPORTS : transmission OUT OF Nicaragua to other regions
# Curve thickness and point size scale with the number of events (rate).
#
# Outputs (in <data_dir>/figures):
#   - PNG + PDF per virus/direction
#   - combined 4-panel figure (A-D)
#   - CSV of the flow rates used (results/)
###############################################################################

# 0. Packages ----------------------------------------------------------------
pkgs <- c("here", "readxl", "dplyr", "tidyr", "tibble", "ggplot2", "sf",
          "rnaturalearth", "rnaturalearthdata", "patchwork")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

# 1. Configuration -----------------------------------------------------------
# Run from the repository root (open the .Rproj in RStudio, or setwd() to the
# repo folder). Paths are relative to the root via the `here` package, so no
# machine-specific paths need editing.
data_dir    <- here::here("data")
results_dir <- here::here("results")
out_dir     <- file.path(results_dir, "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

focal <- "Nicaragua"

inputs <- tribble(
  ~virus,  ~direction, ~file,
  "RSV-A", "import",   "RSVA_importsfromregionstoNic.xlsx",
  "RSV-A", "export",   "RSVA_exports_Nicto otherregions.xlsx",
  "RSV-B", "import",   "RSVB_regionstonicaragua.xlsx",
  "RSV-B", "export",   "RSVB_nicaraguatoregions.xlsx"
) |> mutate(path = file.path(data_dir, file))

missing <- inputs$path[!file.exists(inputs$path)]
if (length(missing)) stop("Input file(s) not found in data/:\n  ",
                          paste(basename(missing), collapse = "\n  "),
                          "\nSee data/README.md.")

# Anchor point, colour and arc curvature for each region.
region_info <- tribble(
  ~region,           ~lat,    ~lon,     ~colour,   ~curvature,
  "Africa",           4.55,    24.69,   "#1B7F2A",  0.25,
  "Asia",            42.06,    96.38,   "#9B30FF",  0.25,
  "Europe",          47.95,    10.52,   "#00B0F0",  0.20,
  "North America",   37.56,   -99.31,   "#E8C400", -0.30,
  "South America",   -2.33,   -75.89,   "#8B5A00",  0.30,
  "Oceania",        -28.67,   136.83,   "#20B2AA",  0.30,
  "South Korea",     36.50,   127.80,   "#E377C2",  0.25,
  "Middle East",     29.00,    45.00,   "#C2185B",  0.25,
  "Central America", 14.60,   -90.50,   "#E6550D",  0.40,
  "Caribbean",       18.20,   -72.00,   "#FF7F0E",  0.30,
  "Brazil",         -11.74,   -49.69,   "#E31A1C",  0.20,
  "Argentina",      -37.73,   -65.37,   "#FFA500",  0.30
)
focal_lat <- 12.87; focal_lon <- -85.21

# 2. Read a flow file --------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

clean_region <- function(x) {
  x <- trimws(gsub("([a-z])([A-Z])", "\\1 \\2", as.character(x)))
  gsub("_", " ", x)
}
is_focal <- function(x) grepl(focal, x, ignore.case = TRUE)

read_flows <- function(path, direction) {
  raw <- read_excel(path, .name_repair = "minimal")
  names(raw) <- trimws(names(raw))
  low <- tolower(names(raw))

  # event-list format: one row per event; region column found by content
  if (is.numeric(raw[[1]])) {
    score <- sapply(seq_along(raw)[-1], function(j) {
      v <- raw[[j]]
      if (!is.character(v)) return(0)
      v <- clean_region(v[!is.na(v) & v != "NA"])
      if (!length(v)) 0 else mean(v %in% region_info$region)
    })
    if (max(score, 0) > 0) {
      region_col <- seq_along(raw)[-1][which.max(score)]
      out <- tibble(event  = raw[[1]],
                    region = as.character(raw[[region_col]])) |>
        filter(!is.na(event), !is.na(region), region != "NA") |>
        mutate(region = clean_region(region)) |>
        group_by(region) |>
        summarise(rate = n_distinct(event), .groups = "drop")
      message(sprintf("  %s: region column = #%d ('%s'); %d events",
                      basename(path), region_col, names(raw)[region_col], sum(out$rate)))
      return(out)
    }
  }

  # long format
  if (all(c("from", "to") %in% low) && any(low %in% c("rate", "value", "mean"))) {
    rate_col <- names(raw)[match(TRUE, low %in% c("rate", "value", "mean"))]
    long <- tibble(from = clean_region(raw[[which(low == "from")]]),
                   to   = clean_region(raw[[which(low == "to")]]),
                   rate = as.numeric(raw[[rate_col]]))
    out <- if (direction == "import") filter(long, is_focal(to))   |> transmute(region = from, rate)
           else                       filter(long, is_focal(from)) |> transmute(region = to,   rate)
    return(out)
  }

  # matrix format
  row_lab <- clean_region(raw[[1]])
  mat <- as.matrix(sapply(raw[-1], as.numeric))
  rownames(mat) <- row_lab
  colnames(mat) <- clean_region(colnames(mat))
  take_row <- function() { i <- which(is_focal(rownames(mat)))[1]
                           if (is.na(i)) return(NULL)
                           tibble(region = colnames(mat), rate = mat[i, ]) }
  take_col <- function() { j <- which(is_focal(colnames(mat)))[1]
                           if (is.na(j)) return(NULL)
                           tibble(region = rownames(mat), rate = mat[, j]) }
  out <- if (direction == "import") (take_row() %||% take_col())
         else                       (take_col() %||% take_row())
  if (is.null(out)) stop("Could not find '", focal, "' in ", basename(path))
  out
}

tidy_flows <- function(df) {
  df |>
    filter(!is_focal(region),
           !tolower(region) %in% c("total", "sum", "mean", "all", ""),
           !is.na(rate), rate > 0) |>
    arrange(desc(rate))
}

flows <- inputs |>
  rowwise() |>
  mutate(data = list(tidy_flows(read_flows(path, direction)))) |>
  ungroup() |>
  select(virus, direction, data) |>
  tidyr::unnest(data)

unknown <- setdiff(flows$region, region_info$region)
if (length(unknown)) {
  stop("These regions have no coordinates in `region_info`: ",
       paste(unknown, collapse = ", "), "\nAdd them and re-run.")
}
print(flows, n = Inf)
write.csv(flows, file.path(results_dir, "nicaragua_rsv_flow_rates_used.csv"), row.names = FALSE)

rate_limits <- range(c(0, flows$rate))

# 3. World basemap -----------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(admin != "Antarctica")

# 4. Map builder -------------------------------------------------------------
make_flow_map <- function(virus_name, dir_name) {

  is_import <- identical(dir_name, "import")

  fl <- flows |>
    filter(.data$virus == virus_name, .data$direction == dir_name) |>
    left_join(region_info, by = "region") |>
    arrange(desc(rate)) |>
    mutate(
      x    = if (is_import) lon       else focal_lon,
      y    = if (is_import) lat       else focal_lat,
      xend = if (is_import) focal_lon else lon,
      yend = if (is_import) focal_lat else lat
    )

  present <- fl$region

  world_col <- world |>
    mutate(group = continent,
           group = ifelse(subregion %in% c("Central America", "Caribbean") &
                            subregion %in% present, subregion, group),
           group = ifelse(subregion == "Western Asia" & "Middle East" %in% present,
                          "Middle East", group),
           group = ifelse(admin %in% c("Brazil", "Argentina", "South Korea") &
                            admin %in% present, admin, group),
           group = ifelse(admin == focal, focal, group),
           group = ifelse(group %in% c(present, focal), group, "No data"),
           alpha = ifelse(group == focal, 1, 0.25))

  pal <- c(setNames(fl$colour, fl$region), setNames("black", focal), "No data" = "grey70")
  lab <- if (is_import) "Introduction rate" else "Export rate"
  top <- fl[1, ]
  top_label <- sprintf("Primary %s\nof %s",
                       if (is_import) "source" else "destination", virus_name)

  curve_layers <- lapply(seq_len(nrow(fl)), function(i) {
    geom_curve(data = fl[i, ],
               aes(x = x, y = y, xend = xend, yend = yend, linewidth = rate),
               colour = fl$colour[i], curvature = fl$curvature[i],
               lineend = "round", alpha = 0.9)
  })

  ggplot() +
    geom_sf(data = world_col, aes(fill = group, alpha = alpha), colour = NA) +
    curve_layers +
    geom_point(data = fl, aes(x = lon, y = lat, size = rate), colour = fl$colour) +
    geom_point(aes(x = focal_lon, y = focal_lat), shape = 18, size = 3, colour = "black") +
    annotate("text", x = top$lon, y = top$lat + 9, label = top_label,
             size = 3.2, lineheight = 0.9) +
    annotate("text", x = focal_lon - 4, y = focal_lat - 6, label = focal,
             size = 3, hjust = 1) +
    scale_fill_manual(values = pal, breaks = c(present, focal, "No data"), name = "Region") +
    scale_alpha_identity() +
    scale_size_continuous(range = c(2, 8), limits = rate_limits, name = lab) +
    scale_linewidth_continuous(range = c(0.4, 3.2), limits = rate_limits, guide = "none") +
    coord_sf(xlim = c(-170, 180), ylim = c(-56, 85), expand = FALSE) +
    labs(subtitle = paste0(virus_name, " \u2013 ", dir_name, "s",
                           if (is_import) " into " else " from ", focal)) +
    theme_void(base_size = 11) +
    theme(plot.subtitle   = element_text(face = "bold", hjust = 0.02),
          legend.title    = element_text(size = 10),
          legend.key.size = unit(0.4, "cm"),
          plot.background = element_rect(fill = "white", colour = NA))
}

# 5. Build, save, combine ----------------------------------------------------
plots <- list()
for (i in seq_len(nrow(inputs))) {
  v <- inputs$virus[i]; d <- inputs$direction[i]
  p <- make_flow_map(v, d)
  plots[[paste(v, d)]] <- p
  stem <- file.path(out_dir, paste0("map_", gsub("-", "", v), "_", d))
  ggsave(paste0(stem, ".png"), p, width = 25, height = 11, units = "cm", dpi = 600)
  ggsave(paste0(stem, ".pdf"), p, width = 25, height = 11, units = "cm")
}

combined <- wrap_plots(plots[c("RSV-A import", "RSV-A export",
                               "RSV-B import", "RSV-B export")], ncol = 2) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 16))

ggsave(file.path(out_dir, "Figure_RSV_Nicaragua_transmission.png"),
       combined, width = 40, height = 20, units = "cm", dpi = 600)
ggsave(file.path(out_dir, "Figure_RSV_Nicaragua_transmission.pdf"),
       combined, width = 40, height = 20, units = "cm")

print(combined)
message("Done. Figures written to: ", out_dir)
