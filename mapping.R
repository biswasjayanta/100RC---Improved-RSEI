# ---- Packages ----
# install.packages(c("terra","ggplot2","dplyr","purrr","stringr","showtext"))
library(terra)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(showtext)

# ---- Config ----
ras_dir <- '/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/Reclassified'   # <- change me
file_ext  <- "tif"                # your raster extension WITHOUT the dot (e.g., "tif" or "tiff")
abbr_to_name <- c(cht="Chattogram", nag="Nagpur", pun="Pune", sur="Surat")
years <- c(2016, 2018, 2020, 2022, 2024)

classes  <- c("Very Poor","Poor","Moderate","Good","Very Good")
palette5 <- c("#8e0b0b", "#e4572e", "#f3a43b", "#a5d86a", "#1b8d4b")  # red→green, CVD-friendly

# ---------------- HELPERS ----------------
coerce_class <- function(r){
  r[r < 1 | r > 5] <- NA
  r
}

align_city <- function(rs){
  r0  <- rs[[1]]
  rs2 <- lapply(rs, function(r) if (crs(r) != crs(r0)) project(r, crs(r0), method="near") else r)
  # union of extents at first raster's resolution
  exts <- lapply(rs2, terra::ext)
  uext <- Reduce(function(a,b) union(a, b), exts)
  tmpl <- rast(ext = uext, resolution = res(r0), crs = crs(r0))
  lapply(rs2, function(r) coerce_class(resample(r, tmpl, method="near")))
}

theme_fig <- theme_void(base_family = "serif") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text  = element_text(size = 8),
    legend.key.height = unit(4, "pt"),
    legend.key.width  = unit(16, "pt"),
    strip.text = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 10.5, face = "bold", hjust = 0.5),
    plot.margin = margin(3, 3, 3, 3)
  )

make_city_png <- function(city_abbr, width_in = 7.5, height_in = 2.2, dpi = 600){
  city_name <- abbr_to_name[[city_abbr]]
  
  # Example filenames: "cht_16.tif", "cht_18.tif", ...
  pat   <- paste0("^", city_abbr, "_(16|18|20|22|24)\\.", file_ext, "$")
  files <- list.files(ras_dir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("No rasters found for: ", city_abbr, " in ", ras_dir)
  
  meta <- tibble(file = files) |>
    mutate(
      yr2  = str_match(basename(file), "_(16|18|20|22|24)")[,2],
      Year = recode(yr2, "16"=2016L,"18"=2018L,"20"=2020L,"22"=2022L,"24"=2024L)
    ) |>
    arrange(match(Year, years))
  
  rs  <- lapply(meta$file, rast)
  rsA <- align_city(rs)
  
  to_df <- function(r, yr){
    df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
    names(df)[3] <- "Class"
    df |>
      mutate(Year  = factor(yr, levels = years),
             Class = factor(Class, levels = 1:5, labels = classes))
  }
  df_all <- map2_dfr(rsA, meta$Year, to_df)
  df_all$City <- city_name
  
  p <- ggplot(df_all, aes(x = x, y = y, fill = Class)) +
    geom_raster() +
    coord_equal(expand = FALSE) +
    facet_grid(rows = vars(City), cols = vars(Year)) +
    scale_fill_manual(values = palette5, drop = FALSE, name = "Class") +
    guides(fill = guide_legend(direction = "horizontal", nrow = 1,
                               keywidth = unit(12, "pt"), keyheight = unit(10, "pt"))) +
    ggtitle(paste0(city_name, " — Classified Ecological Quality")) +
    theme_fig
  
  out <- file.path(ras_dir, paste0(gsub("\\s+","_", city_name), ".png"))
  ggsave(out, p, width = width_in, height = height_in, dpi = dpi, bg = "white")
  message("Saved: ", out)
  invisible(out)
}

# ---- Generate PNGs for all 4 cities ----
paths <- lapply(names(abbr_to_name), make_city_png)
