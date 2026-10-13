###############################################################################
# RSV-A / RSV-B viral flow maps for Nicaragua  (WITH NODE-AGE ORIGIN DATES)
# ---------------------------------------------------------------------------
# Same maps as v5, except:
#   * "Primary source / destination" text removed
#   * each region node is labelled with the ORIGIN DATE = the earliest
#     "Node age" among that region's introductions/exportations.
#
# Excel layout this script expects (checked against your 4 files):
#   col 1  : event number (only on the first row of each block)
#   Region : e.g. SouthAmerica / MiddleEast / "South America" (cleaned below)
#   Node age : decimal year on the first row of each block, e.g. 2014.580
#              (Excel with a comma-decimal locale shows it as 2014,580)
#
# Run from the repo root:  source("R/rsv_nicaragua_flow_maps_with_dates_v3.R")
# Outputs: results/figures_with_dates/  (+ a CSV with the dates used)
###############################################################################

# 0. Packages ----------------------------------------------------------------
pkgs <- c("here", "readxl", "dplyr", "tidyr", "tibble", "ggplot2", "sf",
          "rnaturalearth", "rnaturalearthdata", "patchwork", "stringr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

# 1. Configuration -----------------------------------------------------------
focal <- "Nicaragua"

# How to turn "Node age" into a month:
#   "decimal_year" : 2014.580 = 58% of the way through 2014 (-> July 2014)  [correct
#                    for time-scaled phylogeny node ages; DEFAULT]
#   "first_digit"  : first decimal digit = month (2014.580 -> May 2014)
age_mode <- "decimal_year"

# Label style: "month" -> 2014-Jul ; "year" -> 2014
label_style <- "month"

inputs <- tribble(
  ~virus,  ~direction, ~file,
  "RSV-A", "import",   "RSVA_importsfromregionstoNic.xlsx",
  "RSV-A", "export",   "RSVA_exports_Nicto otherregions.xlsx",
  "RSV-B", "import",   "RSVB_regionstonicaragua.xlsx",
  "RSV-B", "export",   "RSVB_nicaraguatoregions.xlsx"
)

# Find the data folder robustly (works even if `here` picks the wrong root)
candidates <- unique(c(here::here("data"), file.path(getwd(), "data"),
                       file.path(dirname(getwd()), "data")))
data_dir <- candidates[file.exists(file.path(candidates, inputs$file[1]))][1]
if (is.na(data_dir)) stop("Could not find the data folder. Tried:\n  ",
                          paste(candidates, collapse = "\n  "),
                          "\nsetwd() to the repo root and re-run.")
results_dir <- file.path(dirname(data_dir), "results")
out_dir     <- file.path(results_dir, "figures_with_dates")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
message("Data folder:    ", data_dir)
message("Output folder:  ", out_dir)

inputs <- inputs |> mutate(path = file.path(data_dir, file))
missing <- inputs$path[!file.exists(inputs$path)]
if (length(missing)) stop("Input file(s) not found:\n  ",
                          paste(basename(missing), collapse = "\n  "))

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

# Where each date label sits relative to its node (touching the node, no gap):
# "up", "down", "left" or "right". Change a region here if a label clashes.
label_pos <- tribble(
  ~region,           ~pos,
  "North America",   "up",
  "South America",   "right",
  "Central America", "left",
  "Europe",          "up",
  "Asia",            "up",
  "Middle East",     "up",
  "Oceania",         "down",
  "South Korea",     "up",
  "Africa",          "up",
  "Caribbean",       "up",
  "Brazil",          "right",
  "Argentina",       "right"
)

# 2. Helpers -----------------------------------------------------------------
clean_region <- function(x) {
  x <- trimws(gsub("([a-z])([A-Z])", "\\1 \\2", as.character(x)))
  gsub("_", " ", x)
}
is_focal <- function(x) grepl(focal, x, ignore.case = TRUE)

# Node age -> numeric decimal year. Accepts numbers, or text like "2014, 939"
# / "2014,939" / "2014.939" (comma treated as the decimal mark).
to_decimal_year <- function(x) {
  if (is.numeric(x)) return(x)
  s <- gsub("\\s+", "", as.character(x))
  as.numeric(sub(",", ".", s, fixed = TRUE))
}

# Decimal year -> label
age_to_label <- function(x) {
  out <- rep(NA_character_, length(x))
  ok  <- !is.na(x) & x > 1900 & x < 2100
  if (!any(ok)) return(out)
  yr   <- floor(x[ok]); frac <- x[ok] - yr
  if (label_style == "year") { out[ok] <- as.character(yr); return(out) }
  if (age_mode == "decimal_year") {
    leap  <- (yr %% 4 == 0 & yr %% 100 != 0) | yr %% 400 == 0
    ndays <- ifelse(leap, 366, 365)
    d     <- as.Date(sprintf("%d-01-01", as.integer(yr))) + floor(frac * ndays)
    mo    <- as.integer(format(d, "%m"))
  } else {
    mo <- as.integer(substr(sprintf("%.3f", frac), 3, 3))   # first decimal digit
    mo[mo < 1] <- NA
  }
  out[ok] <- ifelse(is.na(mo), as.character(yr),
                    paste0(yr, "-", month.abb[mo]))
  out
}

read_flows <- function(path) {
  raw <- read_excel(path, .name_repair = "minimal")
  names(raw) <- trimws(names(raw))

  # event number = first column; only first row of each block is filled
  event <- raw[[1]]

  # region column: character column (not col 1) whose cleaned values best
  # match the known region names
  score <- sapply(seq_along(raw)[-1], function(j) {
    v <- raw[[j]]
    if (!is.character(v)) return(0)
    v <- clean_region(v[!is.na(v) & v != "NA"])
    if (!length(v)) 0 else mean(v %in% region_info$region)
  })
  if (max(score, 0) == 0) stop("No region column found in ", basename(path))
  region_col <- seq_along(raw)[-1][which.max(score)]

  # node age column: found by NAME first ("Node age"), then by content
  age_col <- which(grepl("^node.?age$", tolower(names(raw))))[1]
  if (is.na(age_col)) {
    cand <- setdiff(seq_along(raw), c(1, region_col))
    sc <- sapply(cand, function(j) {
      v <- suppressWarnings(to_decimal_year(raw[[j]])); v <- v[!is.na(v)]
      if (!length(v)) 0 else mean(v > 1900 & v < 2100)
    })
    if (max(sc, 0) > 0.8) age_col <- cand[which.max(sc)]
  }
  if (is.na(age_col)) stop("No 'Node age' column found in ", basename(path),
                           ". Columns are: ", paste(names(raw), collapse = " | "))

  ev <- tibble(event  = event,
               region = clean_region(raw[[region_col]]),
               age    = to_decimal_year(raw[[age_col]])) |>
    filter(!is.na(event), !is.na(region), region != "NA")

  out <- ev |>
    group_by(region) |>
    summarise(rate       = n_distinct(event),
              origin_age = if (all(is.na(age))) NA_real_ else min(age, na.rm = TRUE),
              .groups = "drop") |>
    mutate(origin = age_to_label(origin_age))

  message(sprintf("  %s: region col #%d ('%s'), node age col #%d ('%s'); %d events",
                  basename(path), region_col, names(raw)[region_col],
                  age_col, names(raw)[age_col], sum(out$rate)))
  out
}

tidy_flows <- function(df) {
  df |>
    filter(!is_focal(region),
           !tolower(region) %in% c("total", "sum", "mean", "all", ""),
           !is.na(rate), rate > 0) |>
    arrange(desc(rate))
}

message("Reading input files:")
flows <- inputs |>
  rowwise() |>
  mutate(data = list(tidy_flows(read_flows(path)))) |>
  ungroup() |>
  select(virus, direction, data) |>
  tidyr::unnest(data)

unknown <- setdiff(flows$region, region_info$region)
if (length(unknown)) {
  stop("These regions have no coordinates in `region_info`: ",
       paste(unknown, collapse = ", "), "\nAdd them and re-run.")
}
cat("\nFlows and origin dates that will be plotted:\n")
print(as.data.frame(flows), row.names = FALSE)
write.csv(flows, file.path(results_dir, "nicaragua_rsv_flow_rates_and_origin_dates.csv"),
          row.names = FALSE)

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
    left_join(label_pos, by = "region") |>
    mutate(
      pos = dplyr::coalesce(pos, "up"),
      r   = 2.2 + 2.6 * rate / rate_limits[2],   # distance = node radius + small gap
      lx  = lon + dplyr::case_when(pos == "right" ~ r, pos == "left" ~ -r, TRUE ~ 0),
      ly  = lat + dplyr::case_when(pos == "up" ~ r, pos == "down" ~ -r, TRUE ~ 0),
      hj  = dplyr::case_when(pos == "right" ~ 0, pos == "left" ~ 1, TRUE ~ 0.5),
      vj  = dplyr::case_when(pos == "up" ~ 0, pos == "down" ~ 1, TRUE ~ 0.5),
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
    # origin date (earliest node age) sitting right next to its node
    geom_text(data = fl, aes(x = lx, y = ly, label = origin, hjust = hj, vjust = vj),
              size = 3, fontface = "bold", na.rm = TRUE) +
    annotate("text", x = focal_lon - 3, y = focal_lat - 8, label = focal,
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
  stem <- file.path(out_dir, paste0("map_", gsub("-", "", v), "_", d, "_dates"))
  ggsave(paste0(stem, ".png"), p, width = 25, height = 11, units = "cm", dpi = 600)
  ggsave(paste0(stem, ".pdf"), p, width = 25, height = 11, units = "cm")
}

combined <- wrap_plots(plots[c("RSV-A import", "RSV-A export",
                               "RSV-B import", "RSV-B export")], ncol = 2) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 16))

ggsave(file.path(out_dir, "Figure_RSV_Nicaragua_transmission_dates.png"),
       combined, width = 40, height = 20, units = "cm", dpi = 600)
ggsave(file.path(out_dir, "Figure_RSV_Nicaragua_transmission_dates.pdf"),
       combined, width = 40, height = 20, units = "cm")

print(combined)
message("Done. Figures written to: ", out_dir)
