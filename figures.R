library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(patchwork)
library(circlize)
library(legendry)
library(conflicted)
library(shadowtext)
library(forcats)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::rename)

setwd('/Users/shrutijain/Library/CloudStorage/OneDrive-UniversiteitLeiden/data/farm_size/figures/')
year_end   <- 2050
size_scn_pick   <- "variable" # same/variable/consolidate/fragment
rcp_pick   <- 7.0         # 2.6/7
lib_pick   <- "low"       # low/high

# 3-category plots
fs_levels    <- c("Small","Medium","Large")
small_groups <- "Small"     # for heatmaps: small share = <5 ha

recode_region <- function(x) {
  dplyr::recode(as.character(x), "FSU" = "CNE")
}

# Common label maps
fg_map <- c("Fruits and vegetables"="Fruits & Veg", "Grains"="Grains",
            "Legumes, nuts and seeds"="Legumes & Nuts",
            "Oilcrops and sugar crops"="Oil & Sugar",
            "Roots and tubers"="Roots & Tubers", "Soybeans"="Soybeans")
fg_levels   <- c("Grains","Roots & Tubers","Fruits & Veg","Soybeans","Legumes & Nuts","Oil & Sugar")
regions_rev <- rev(c("CNE","EAP","EUR","LAC","MEN","NAM","SAS","SSA"))

diet_short  <- c("BMK", "FLX", "PSC", "VEG", "VGN")
diet_order  <- paste(year_end, diet_short)

# Colors for 3 bins + total
fill_colors <- c(
  "Small"  = "#1a5c2a",
  "Medium" = "#999999",
  "Large"  = "#D4A843",
  "Total"  = "#3D6B7A"
)
label_colors <- c("Small"="#1a5c2a","Medium"="#999999","Large"="#D4A843")

# for the 11 groups
fs_11_labels <- c(
  "1"    = "0–1",
  "2"    = "1–2",
  "5"    = "2–5",
  "10"   = "5–10",
  "20"   = "10–20",
  "50"   = "20–50",
  "100"  = "50–100",
  "200"  = "100–200",
  "500"  = "200–500",
  "1000" = "500–1000",
  "5000" = "1000–5000"
)

# A smooth green-to-gold gradient
fs_11_colors <- c(
  "0–1"      = "#0B3D1E",
  "1–2"      = "#1a5c2a",
  "2–5"      = "#2E7D3A",
  "5–10"     = "#a8a8a8",   
  "10–20"    = "#c8c8c8",   
  "20–50"    = "#dcdcdc",   
  "50–100"   = "#E8C46A",
  "100–200"  = "#D4A843",
  "200–500"  = "#B8882A",
  "500–1000" = "#8C6518",
  "1000–5000"= "#5E4410"
)

fs_category <- c(
  "0–1" = "Small", "1–2" = "Small", "2–5" = "Small",
  "5–10" = "Medium", "10–20" = "Medium", "20–50" = "Medium",
  "50–100" = "Large", "100–200" = "Large", "200–500" = "Large",
  "500–1000" = "Large", "1000–5000" = "Large"
)

# Shared reader: reads raw CSV once, adds fs_plot
read_raw <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(
      group = recode_region(group),
      farm_size_group = as.numeric(farm_size_group),
      fs_plot = case_when(
        farm_size_group <= 5  ~ "Small",
        farm_size_group <= 50 ~ "Medium",
        farm_size_group > 50  ~ "Large"
      ),
      fs_plot = factor(fs_plot, levels = fs_levels)
    )
}

# df: food groups aggregated, farm sizes aggregated (3 bins)
prep_summary <- function(path) {
  read_raw(path) %>%
    filter(!is.na(fs_plot)) %>%
    group_by(group, year, food_group, diet_scn, RCP, lib_scn, size_scn, fs_plot) %>%
    summarise(
      cons = sum(cons, na.rm = TRUE),
      prod = sum(prod, na.rm = TRUE),
      imp  = sum(imp,  na.rm = TRUE),
      exp  = sum(exp,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(group, year, food_group, diet_scn, RCP, lib_scn, size_scn) %>%
    mutate(
      cons_total = sum(cons), prod_total = sum(prod),
      imp_total  = sum(imp),  exp_total  = sum(exp),
      cons_pct = ifelse(cons_total > 0, 100 * cons / cons_total, NA_real_),
      prod_pct = ifelse(prod_total > 0, 100 * prod / prod_total, NA_real_),
      imp_pct  = ifelse(imp_total  > 0, 100 * imp  / imp_total,  NA_real_),
      exp_pct  = ifelse(exp_total  > 0, 100 * exp  / exp_total,  NA_real_)
    ) %>%
    ungroup()
}

# df_11: food groups aggregated, farm sizes disaggregated (11 classes)
prep_summary_11 <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(
      group = recode_region(group),
      farm_size_group = as.numeric(farm_size_group)) %>%
    group_by(group, year, food_group, diet_scn, RCP, lib_scn, size_scn, farm_size_group) %>%
    summarise(
      cons = sum(cons, na.rm = TRUE),
      prod = sum(prod, na.rm = TRUE),
      imp  = sum(imp,  na.rm = TRUE),
      exp  = sum(exp,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(group, year, food_group, diet_scn, RCP, lib_scn, size_scn) %>%
    mutate(
      cons_total = sum(cons), prod_total = sum(prod),
      cons_pct = ifelse(cons_total > 0, 100 * cons / cons_total, NA_real_),
      prod_pct = ifelse(prod_total > 0, 100 * prod / prod_total, NA_real_)
    ) %>%
    ungroup()
}

# df_crop: food groups disaggregated (individual commodities), farm sizes aggregated (3 bins)
prep_summary_crop <- function(path) {
  read_raw(path) %>%
    mutate(group = recode_region(group)) %>%
    filter(!is.na(fs_plot)) %>%
    group_by(group, year, food_group, IMPACT_code, diet_scn, RCP, lib_scn, size_scn, fs_plot) %>%
    summarise(
      cons = sum(cons, na.rm = TRUE),
      prod = sum(prod, na.rm = TRUE),
      imp  = sum(imp,  na.rm = TRUE),
      exp  = sum(exp,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(group, year, food_group, IMPACT_code, diet_scn, RCP, lib_scn, size_scn) %>%
    mutate(
      cons_total = sum(cons), prod_total = sum(prod),
      imp_total  = sum(imp),  exp_total  = sum(exp),
      cons_pct = ifelse(cons_total > 0, 100 * cons / cons_total, NA_real_),
      prod_pct = ifelse(prod_total > 0, 100 * prod / prod_total, NA_real_),
      imp_pct  = ifelse(imp_total  > 0, 100 * imp  / imp_total,  NA_real_),
      exp_pct  = ifelse(exp_total  > 0, 100 * exp  / exp_total,  NA_real_)
    ) %>%
    ungroup()
}

df      <- prep_summary("summary.csv")
df_11   <- prep_summary_11("summary.csv")
df_crop <- prep_summary_crop("summary.csv")



#######################################################################################
########################### BASELINE HEATMAPS #########################################
#######################################################################################

base <- df %>%
  filter(
    year == 2020
  ) %>%
  group_by(group, food_group) %>%
  summarise(
    prod_total = first(prod_total),
    cons_total = first(cons_total),
    prod_small = sum(prod[fs_plot %in% small_groups]),
    cons_small = sum(cons[fs_plot %in% small_groups]),
    .groups = "drop"
  ) %>%
  mutate(
    prod_mt = prod_total / 1e6,
    cons_mt = cons_total / 1e6,
    prod_small_pct = 100 * prod_small / prod_total,
    cons_small_pct = 100 * cons_small / cons_total,
    food   = factor(fg_map[food_group], levels = fg_levels),
    region = factor(group, levels = regions_rev)
  )

ht <- function(show_y = TRUE) {
  theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 11, margin = margin(b = 4)),
      axis.text.x      = element_text(size = 8, angle = 35, hjust = 1, face = "bold"),
      axis.text.y      = if (show_y) element_text(size = 9, face = "bold") else element_blank(),
      axis.title       = element_blank(),
      panel.grid       = element_blank(),
      legend.position  = "right",
      legend.title     = element_text(size = 7.5),
      legend.text      = element_text(size = 7),
      legend.key.height = unit(0.8, "cm"),
      legend.key.width  = unit(0.35, "cm"),
      plot.margin      = margin(5, 5, 5, 5)
    )
}

vol_max <- max(c(base$prod_mt, base$cons_mt), na.rm = TRUE)

p1 <- ggplot(base, aes(x = food, y = region, fill = prod_mt)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(prod_mt >= 1, format(round(prod_mt), big.mark = ","),
                               sprintf("%.1f", prod_mt))),
            size = 2.8, color = "grey20") +
  scale_fill_gradient(low = "#f7f4ee", high = "#2B5A8C",
                      name = "Mt", limits = c(0, vol_max)) +
  ggtitle("a. Production Volume (Mt)") + ht(show_y = TRUE) +
  theme(axis.text.x = element_blank())

p2 <- ggplot(base, aes(x = food, y = region, fill = cons_mt)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(cons_mt >= 1, format(round(cons_mt), big.mark = ","),
                               sprintf("%.1f", cons_mt))),
            size = 2.8, color = "grey20") +
  scale_fill_gradient(low = "#f7f4ee", high = "#8C4A2B",
                      name = "Mt", limits = c(0, vol_max)) +
  ggtitle("b. Consumption Volume (Mt)") + ht(show_y = FALSE) +
  theme(axis.text.x = element_blank())

p3 <- ggplot(base, aes(x = food, y = region, fill = prod_small_pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(prod_small_pct), "%")),
            size = 2.8, color = "grey20") +
  scale_fill_gradient(low = "#f7f0e6", high = "#2B6B3D",
                      name = "%", limits = c(0, 100),
                      labels = function(x) paste0(x, "%")) +
  ggtitle("c. Small Farm Share of Production (<5 ha)") + ht(show_y = TRUE)

p4 <- ggplot(base, aes(x = food, y = region, fill = cons_small_pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(cons_small_pct), "%")),
            size = 2.8, color = "grey20") +
  scale_fill_gradient(low = "#f7f0e6", high = "#2B6B3D",
                      name = "%", limits = c(0, 100),
                      labels = function(x) paste0(x, "%")) +
  ggtitle("d. Small Farm Share of Consumption (<5 ha)") + ht(show_y = FALSE)

baseline_heatmaps <- (p1 | p2) / (p3 | p4) 

baseline_heatmaps
ggsave("baseline_heatmaps.png", baseline_heatmaps, width = 12, height = 8, dpi = 300)



#######################################################################################
######################## STACKED FARM-SIZE COMPOSITION ################################
#######################################################################################

# Build composition dataset
comp_df_11 <- df_11 %>%
  filter(
    (year == 2020) |
      (year == year_end & diet_scn %in% diet_order & size_scn == size_scn_pick & RCP == rcp_pick & lib_scn == lib_pick)
  ) %>%
  group_by(year, diet_scn, farm_size_group) %>%
  summarise(prod_mt = sum(prod)/1e6, .groups = "drop") %>%
  group_by(year, diet_scn) %>%
  mutate(share = prod_mt / sum(prod_mt)) %>%
  ungroup() %>%
  mutate(
    farm_size_group = factor(farm_size_group),
    scenario = case_when(
      year == 2020 ~ "2020 baseline",
      TRUE ~ diet_scn
    ),
    scenario = factor(scenario, levels = c("2020 baseline", diet_order)),
    label_txt = ifelse(share >= 0.05,
                       paste0(round(share*100), "%"),
                       "")
  )

comp_df_11 <- comp_df_11 %>%
  mutate(
    fs_label = factor(
      fs_11_labels[as.character(farm_size_group)],
      levels = fs_11_labels
    )
  )

comp_df_11 <- comp_df_11 %>%
  mutate(period = factor(ifelse(year == 2020, "2020", "2050"),
                         levels = c("2020", "2050")))

p_comp_share <- ggplot(comp_df_11,
                          aes(x = scenario, y = share, fill = fs_label)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.4) +
  geom_shadowtext(aes(label = label_txt),
                  position = position_stack(vjust = 0.5),
                  size = 2.5, fontface = "bold",
                  color = "white",      # text fill
                  bg.color = "black",   # outline/halo color
                  bg.r = 0.12) +        # halo thickness (relative radius)
  facet_grid(~ period, scales = "free_x", space = "free_x") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = fs_11_colors,
    labels = paste0(names(fs_11_colors), " ha"),
    guide  = guide_legend_group(
      key  = key_group_lut(names(fs_11_colors), fs_category[names(fs_11_colors)]),
      ncol = 2,
      theme = theme(
        legend.title          = element_text(size = 10, hjust = 0.5),  
        legend.text           = element_text(size = 8),
        legend.spacing.x      = unit(0.5, "lines")
      )
    )
  ) +
  labs(
    x = NULL, y = "Share of total production", fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing.x = unit(1.5, "lines"),
    legend.position    = "bottom"
  )

p_comp_share
ggsave(paste0("farm_size_composition_share_", size_scn_pick, "_", year_end, ".png"),
       p_comp_share, width = 6, height = 6, dpi = 300)



#######################################################################################
################## STACKED FARM-SIZE COMPOSITION BY FOOD GROUP ########################
#######################################################################################

round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x  <- x * up
  y  <- floor(x)
  # how many points still need to be distributed
  short <- round(sum(x)) - sum(y)
  if (short > 0) {
    idx <- tail(order(x - y), short)
    y[idx] <- y[idx] + 1
  }
  y / up
}

comp_df_fg <- df %>%
  filter(
    (year == 2020) |
      (year == year_end & diet_scn %in% diet_order & size_scn == size_scn_pick & RCP == rcp_pick & lib_scn == lib_pick),
  ) %>%
  group_by(year, diet_scn, fs_plot, food_group) %>%
  summarise(prod_mt = sum(prod)/1e6, .groups = "drop") %>%
  group_by(year, diet_scn, food_group) %>%
  mutate(share = prod_mt / sum(prod_mt)) %>%
  ungroup()

comp_df_fg <- comp_df_fg %>%
  mutate(
    fs_plot = factor(fs_plot, levels = c("Small","Medium","Large")),
    scenario = case_when(
      year == 2020 ~ "2020",
      TRUE ~ diet_scn
    ),
    scenario = factor(scenario, levels = c("2020", diet_order))
  ) %>%
  group_by(year, diet_scn, food_group) %>%
  mutate(
    pct_int   = round_preserve_sum(share * 100, digits = 0),
    label_txt = ifelse(share >= 0.05, paste0(pct_int, "%"), "")
  ) %>%
  ungroup() %>%
  mutate(
    food_group = recode(food_group, !!!fg_map),
    food_group = factor(food_group, levels = fg_levels)
  )

comp_df_fg <- comp_df_fg %>%
  mutate(
    food_group = recode(food_group, !!!fg_map),
    food_group = factor(food_group, levels = fg_levels)
  )
p_comp_share_fg <- ggplot(comp_df_fg,
                          aes(x = scenario, y = share, fill = fs_plot)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.4) +
  geom_shadowtext(aes(label = label_txt),
                  position = position_stack(vjust = 0.5),
                  size = 2.5, fontface = "bold",
                  color = "white",      # text fill
                  bg.color = "black",   # outline/halo color
                  bg.r = 0.12) +        # halo thickness (relative radius)
  facet_wrap(~ food_group, scales = "free_x") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = fill_colors[names(fill_colors) != "Total"],
    labels = c(
      "Small"  = "Small (<5 ha)",
      "Medium" = "Medium (5–50 ha)",
      "Large"  = "Large (>50 ha)"
    )
  ) +
  labs(
    x = NULL,
    y = "Share of total production",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 7),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10)
  )
p_comp_share_fg

ggsave(paste0("farm_size_composition_share_by_foodgroup_", size_scn_pick, "_", year_end, ".png"),
       p_comp_share_fg, width = 10, height = 7, dpi = 300)


#######################################################################################
############################# GLOBAL PRODUCTION  ######################################
#######################################################################################

base_2020 <- df %>%
  filter(year == 2020) %>%
  group_by(fs_plot) %>%
  summarise(v = sum(prod)/1e6, .groups = "drop") %>%
  complete(fs_plot = fs_levels, fill = list(v = 0)) %>%
  mutate(fs_plot = factor(fs_plot, levels = fs_levels))

base_vec   <- setNames(base_2020$v, as.character(base_2020$fs_plot))
base_total <- sum(base_2020$v)

end_wide <- df %>%
  filter(year == year_end, size_scn == size_scn_pick, RCP == rcp_pick, lib_scn == lib_pick) %>%
  group_by(diet_scn, fs_plot) %>%
  summarise(v = sum(prod)/1e6, .groups = "drop") %>%
  complete(diet_scn = diet_order, fs_plot = fs_levels, fill = list(v = 0)) %>%
  pivot_wider(names_from = fs_plot, values_from = v, values_fill = 0)

end_wide$Total <- rowSums(end_wide[, fs_levels, drop = FALSE])

K     <- length(fs_levels)
bar_w <- 0.35
x_2020 <- 1
x0    <- 3.2
mult  <- K + 2

all_totals <- c(base_total, end_wide$Total)
y_lower <- floor(min(all_totals) * 0.97 / 100) * 100
y_upper <- ceiling(max(all_totals) * 1.03 / 100) * 100

bars <- tibble()
connectors <- tibble()
labels_df <- tibble()
xlabs_df <- tibble()

# 2020 total bar
bars <- bind_rows(bars, tibble(
  xmin = x_2020 - bar_w, xmax = x_2020 + bar_w,
  ymin = y_lower, ymax = base_total, fill = "Total", alpha = 1
))
labels_df <- bind_rows(labels_df, tibble(
  x = x_2020, y = base_total,
  label = format(round(base_total), big.mark = ","),
  color = "#333333", size = 3.3, fontface = "bold", vjust = -0.6
))
xlabs_df <- bind_rows(xlabs_df, tibble(
  x = x_2020,
  label = "2020 baseline"
))

for (i in seq_along(diet_order)) {
  scn <- diet_order[i]
  row <- end_wide %>% filter(diet_scn == scn)
  
  x_base <- x0 + (i - 1) * mult
  x_tot  <- x_base + K
  
  d_vec <- as.numeric(row[1, fs_levels])
  delta <- d_vec - base_vec[fs_levels]
  
  cfx <- if (i == 1) x_2020 + bar_w else (x0 + (i - 2) * mult + K + bar_w)
  connectors <- bind_rows(connectors, tibble(
    x = cfx, xend = x_base - bar_w, y = base_total, yend = base_total
  ))
  
  running_top <- base_total
  
  for (j in seq_len(K)) {
    fs <- fs_levels[j]
    dv <- delta[j]
    x_j <- x_base + (j - 1)
    new_top <- running_top + dv
    
    bars <- bind_rows(bars, tibble(
      xmin = x_j - bar_w, xmax = x_j + bar_w,
      ymin = min(running_top, new_top), ymax = max(running_top, new_top),
      fill = fs, alpha = 1
    ))
    
    labels_df <- bind_rows(labels_df, tibble(
      x = x_j, y = max(running_top, new_top),
      label = paste0(ifelse(dv >= 0, "+", ""), format(round(dv), big.mark = ",")),
      color = label_colors[fs], size = 2.8, fontface = "bold", vjust = -0.4
    ))
    
    if (j < K) {
      connectors <- bind_rows(connectors, tibble(
        x = x_j + bar_w, xend = x_j + 1 - bar_w, y = new_top, yend = new_top
      ))
    }
    
    running_top <- new_top
  }
  
  # total bar
  bars <- bind_rows(bars, tibble(
    xmin = x_tot - bar_w, xmax = x_tot + bar_w,
    ymin = y_lower, ymax = row$Total, fill = "Total", alpha = 1
  ))
  labels_df <- bind_rows(labels_df, tibble(
    x = x_tot, y = row$Total,
    label = format(round(row$Total), big.mark = ","),
    color = "#333333", size = 3.1, fontface = "bold", vjust = -0.5
  ))
  
  xlabs_df <- bind_rows(xlabs_df, tibble(
    x = x_base + K/2,
    label = scn   
  ))
}

shade_df <- tibble(
  xmin = x0 + (0:4) * mult - 0.7,
  xmax = x0 + (0:4) * mult + K + 0.7,
  ymin = y_lower, ymax = y_upper,
  shd  = rep(c("grey93", "transparent"), length.out = 5)
)

bars$fill <- factor(bars$fill, levels = c(fs_levels, "Total"))

p_global_prod <- ggplot() +
  geom_rect(data = shade_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = shade_df$shd, alpha = 0.5) +
  geom_segment(
    aes(x = x_2020 + bar_w, xend = x0 + (length(diet_order) - 1) * mult + K + bar_w,
        y = base_total, yend = base_total),
    linetype = "dashed", color = "#aaaaaa", linewidth = 0.4
  ) +
  geom_rect(data = bars,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = fill, alpha = alpha), color = NA) +
  scale_fill_manual(
    values = fill_colors,
    labels = c(
      Small  = "Small (<5 ha)",
      Medium = "Medium (5–50 ha)",
      Large  = "Large (>50 ha)",
      Total  = "Total"
      )
  ) +
  scale_alpha_identity() +
  geom_segment(data = connectors,
               aes(x = x, xend = xend, y = y, yend = yend),
               linetype = "dashed", color = "#aaaaaa", linewidth = 0.4) +
  geom_text(data = labels_df,
            aes(x = x, y = y, label = label),
            color = labels_df$color, size = labels_df$size,
            fontface = labels_df$fontface, vjust = labels_df$vjust) +
  scale_x_continuous(
    breaks = xlabs_df$x,
    labels = xlabs_df$label
  ) +
  coord_cartesian(ylim = c(y_lower, y_upper), clip = "off") +
  scale_y_continuous(breaks = pretty(c(y_lower, y_upper), n = 5), labels = comma) +
  labs(
    y = "Million tonnes", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x        = element_blank(),
    axis.text.x         = element_text(size = 10, color = "#333333", face = "bold",
                                       lineheight = 1.2, margin = margin(t = 6)),
    axis.ticks.x        = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor    = element_blank(),
    legend.position     = "bottom",
    legend.text         = element_text(size = 10),
    plot.margin         = margin(10, 15, 10, 10)
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1), nrow = 1))

p_global_prod

ggsave(paste0("total_production_", size_scn_pick, "_", year_end, ".png"), p_global_prod, width = 10.5, height = 6, dpi = 300)



#######################################################################################
########################### GLOBAL PRODUCTION by FOOD #################################
#######################################################################################

food_groups <- sort(unique(df$food_group))

all_bars <- tibble()
all_connectors <- tibble()
all_labels <- tibble()
all_shades <- tibble()

for (fg in food_groups) {
  
  fg_short <- fg_map[fg]
  
  base_fg <- df %>%
    filter(year == 2020,
           food_group == fg) %>%
    group_by(fs_plot) %>%
    summarise(v = sum(prod)/1e6, .groups = "drop") %>%
    complete(fs_plot = fs_levels, fill = list(v = 0))
  
  base_vec_fg   <- setNames(base_fg$v, as.character(base_fg$fs_plot))
  base_total_fg <- sum(base_fg$v)
  
  end_fg <- df %>%
    filter(year == year_end, size_scn == size_scn_pick, RCP == rcp_pick, lib_scn == lib_pick,
           food_group == fg) %>%
    group_by(diet_scn, fs_plot) %>%
    summarise(v = sum(prod)/1e6, .groups = "drop") %>%
    complete(diet_scn = diet_order, fs_plot = fs_levels, fill = list(v = 0)) %>%
    pivot_wider(names_from = fs_plot, values_from = v, values_fill = 0)
  
  end_fg$Total <- rowSums(end_fg[, fs_levels, drop = FALSE])
  
  all_tops <- c(base_total_fg, end_fg$Total)
  y_upper <- ceiling(max(all_tops) * 1.06 / 10) * 10
  y_lower <- floor(min(all_tops) * 0.92 / 10) * 10
  
  # 2020 bar
  all_bars <- bind_rows(all_bars, tibble(
    food = fg_short, xmin = x_2020 - bar_w, xmax = x_2020 + bar_w,
    ymin = y_lower, ymax = base_total_fg, fill = "Total", alpha = 1
  ))
  all_labels <- bind_rows(all_labels, tibble(
    food = fg_short, x = x_2020, y = base_total_fg,
    label = format(round(base_total_fg), big.mark = ","),
    color = "#333333", size = 2.8, fontface = "bold", vjust = -0.5
  ))
  
  for (i in seq_along(diet_order)) {
    scn <- diet_order[i]
    row <- end_fg %>% filter(diet_scn == scn)
    
    x_base <- x0 + (i - 1) * mult
    x_tot  <- x_base + K
    
    d_vec <- as.numeric(row[1, fs_levels])
    delta <- d_vec - base_vec_fg[fs_levels]
    
    cfx <- if (i == 1) x_2020 + bar_w else (x0 + (i - 2) * mult + K + bar_w)
    all_connectors <- bind_rows(all_connectors, tibble(
      food = fg_short, x = cfx, xend = x_base - bar_w, y = base_total_fg, yend = base_total_fg
    ))
    
    running_top <- base_total_fg
    
    for (j in seq_len(K)) {
      fs <- fs_levels[j]
      dv <- delta[j]
      x_j <- x_base + (j - 1)
      new_top <- running_top + dv
      
      all_bars <- bind_rows(all_bars, tibble(
        food = fg_short,
        xmin = x_j - bar_w, xmax = x_j + bar_w,
        ymin = min(running_top, new_top), ymax = max(running_top, new_top),
        fill = fs, alpha = 0.85
      ))
      all_labels <- bind_rows(all_labels, tibble(
        food = fg_short, x = x_j, y = max(running_top, new_top),
        label = paste0(ifelse(dv >= 0, "+", ""), format(round(dv), big.mark = ",")),
        color = label_colors[fs], size = 2.3, fontface = "bold", vjust = -0.4
      ))
      
      if (j < K) {
        all_connectors <- bind_rows(all_connectors, tibble(
          food = fg_short,
          x = x_j + bar_w, xend = x_j + 1 - bar_w, y = new_top, yend = new_top
        ))
      }
      
      running_top <- new_top
    }
    
    all_bars <- bind_rows(all_bars, tibble(
      food = fg_short, xmin = x_tot - bar_w, xmax = x_tot + bar_w,
      ymin = y_lower, ymax = row$Total, fill = "Total", alpha = 1
    ))
    all_labels <- bind_rows(all_labels, tibble(
      food = fg_short, x = x_tot, y = row$Total,
      label = format(round(row$Total), big.mark = ","),
      color = "#333333", size = 2.6, fontface = "bold", vjust = -0.5
    ))
  }
  
  all_shades <- bind_rows(all_shades, tibble(
    food = fg_short,
    xmin = x0 + (0:4) * mult - 0.7,
    xmax = x0 + (0:4) * mult + K + 0.7,
    ymin = y_lower, ymax = y_upper,
    shd  = rep(c("grey93", "transparent"), length.out = 5)
  ))
}

all_bars$food       <- factor(all_bars$food, levels = fg_levels)
all_bars$fill       <- factor(all_bars$fill, levels = c(fs_levels, "Total"))
all_connectors$food <- factor(all_connectors$food, levels = fg_levels)
all_labels$food     <- factor(all_labels$food, levels = fg_levels)
all_shades$food     <- factor(all_shades$food, levels = fg_levels)

xlabs_unique <- tibble(
  x = c(x_2020, x0 + (0:4) * mult + K/2),
  label = c("2020 baseline", diet_order)
)


all_baselines <- tibble()
for (fg in food_groups) {
  fg_short <- fg_map[fg]
  bt <- df %>%
    filter(year == 2020, food_group == fg) %>%
    summarise(v = sum(prod)/1e6) %>%
    pull(v)
  all_baselines <- bind_rows(all_baselines, tibble(
    food = fg_short,
    x    = x_2020 + bar_w,
    xend = x0 + (length(diet_order) - 1) * mult + K + bar_w,
    y    = bt
  ))
}
all_baselines$food <- factor(all_baselines$food, levels = fg_levels)


p_by_food <- ggplot() +
  geom_rect(data = all_shades,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = all_shades$shd, alpha = 0.5) +
  geom_segment(data = all_baselines,
               aes(x = x, xend = xend, y = y, yend = y),
               linetype = "dashed", color = "#aaaaaa", linewidth = 0.4) +
  geom_rect(data = all_bars,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = fill, alpha = alpha), color = NA) +
  scale_fill_manual(values = fill_colors,
                    labels = c(
                      "Small"  = "Small (<5 ha)",
                      "Medium" = "Medium (5–50 ha)",
                      "Large"  = "Large (>50 ha)",
                      "Total"  = "Total"
                    )) +
  scale_alpha_identity() +
  geom_segment(data = all_connectors,
               aes(x = x, xend = xend, y = y, yend = yend),
               linetype = "dashed", color = "#aaaaaa", linewidth = 0.3) +
  geom_text(data = all_labels,
            aes(x = x, y = y, label = label),
            color = all_labels$color, size = all_labels$size,
            fontface = all_labels$fontface, vjust = all_labels$vjust) +
  facet_wrap(~ food, scales = "free_y", nrow = 6) +
  scale_x_continuous(
    breaks = xlabs_unique$x,
    labels = xlabs_unique$label
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    y = "Million tonnes", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 9, color = "#333333", face = "bold",
                               lineheight = 1.1, margin = margin(t = 4)),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(10, 15, 10, 10)
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

p_by_food

ggsave(paste0("total_production_by_food_", size_scn_pick, "_", year_end, ".png"), p_by_food, width = 9.5, height = 15, dpi = 600)




#######################################################################################
###################### CONSUMPTION/PRODUCTION BY CROP/REGION ##########################
#######################################################################################

plot_total_range <- function(df, var = c("cons", "prod"), show_labels = FALSE) {
  
  var <- match.arg(var)
  
  var_label <- ifelse(var == "cons", "Consumption", "Production")
  x_label   <- paste0(var_label, " volume (million tonnes)")
  
  # 2020 baseline
  df_2020 <- df %>%
    filter(year == 2020) %>%
    group_by(group, food_group) %>%
    summarise(vol_2020 = sum(.data[[var]])/1e6, .groups = "drop")
  
  # 2030 range across diet scenarios
  df_end <- df %>%
    filter(year == year_end,
           RCP == rcp_pick,
           lib_scn == lib_pick,
           size_scn == size_scn_pick) %>%
    group_by(group, food_group, diet_scn) %>%
    summarise(vol = sum(.data[[var]])/1e6, .groups = "drop") %>%
    group_by(group, food_group) %>%
    summarise(
      vol_min   = min(vol),
      vol_max   = max(vol),
      diet_min  = diet_scn[which.min(vol)],
      diet_max  = diet_scn[which.max(vol)],
      .groups   = "drop"
    ) %>%
    mutate(
      diet_min = sub("^\\d+\\s+", "", diet_min),
      diet_max = sub("^\\d+\\s+", "", diet_max)
    )
  
  plot_df <- df_2020 %>%
    left_join(df_end, by = c("group", "food_group"))
  
  plot_df$food <- factor(fg_map[plot_df$food_group], levels = fg_levels)
  plot_df$region <- factor(plot_df$group, levels = regions_rev)
  
  # ---- alternating row shading ----
  row_shade_df <- expand.grid(
    food = levels(plot_df$food),
    region = levels(plot_df$region)
  ) %>%
    mutate(
      region_index = as.numeric(region),
      ymin = region_index - 0.5,
      ymax = region_index + 0.5,
      shade = ifelse(region_index %% 2 == 0, "grey95", NA)
    )
  
  p <- ggplot(plot_df, aes(y = region)) +
    
    # Row stripes
    geom_rect(
      data = row_shade_df %>% filter(!is.na(shade)),
      aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "grey95",
      alpha = 0.6
    ) +
    
    # 2030 range
    geom_segment(
      aes(x = vol_min, xend = vol_max, yend = region),
      linewidth = 2.2,
      color = "#6E6E6E",
      alpha = 0.85
    ) +
    geom_point(aes(x = vol_min), size = 1.2, color = "#6E6E6E") +
    geom_point(aes(x = vol_max), size = 1.2, color = "#6E6E6E") +
    
    # 2020 baseline marker |
    geom_point(
      aes(x = vol_2020),
      shape = 124,
      size = 6,
      color = "#000000",
      stroke = 1.2
    ) +
    
    facet_wrap(~ food, scales = "free_x", nrow = 2) +
    
    scale_x_continuous(
      labels = comma,
      expand = expansion(mult = c(0.1, 0.1))
    ) +
    
    labs(
      x = x_label,
      y = NULL
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "grey90", color = NA),
      axis.text.y = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.6),
      panel.spacing = unit(1.2, "lines"),
      plot.margin = margin(10, 15, 10, 10)
    )
  
  if (show_labels) {
    p <- p + 
      geom_text(
        aes(x = vol_min, label = diet_min),
        size = 2.2, hjust = 1.2, fontface = "bold", color = "#6E6E6E"
      ) +
      geom_text(
        aes(x = vol_max, label = diet_max),
        size = 2.2, hjust = -0.2, fontface = "bold", color = "#6E6E6E"
      ) 
  }
  
  p_key <- ggplot() +
    geom_segment(aes(x = 0.5, xend = 2.5, y = 0, yend = 0),
                 color = "grey55", linewidth = 3, alpha = 0.85,
                 arrow = arrow(ends = "both",
                               type = "closed",
                               angle=20,
                               length = unit(0.1, "inches"))) +
    geom_segment(aes(x = 1.5, xend = 1.5, y = -1, yend = 1),
                 color = "grey15", linewidth = 1) +
    annotate("text", x = 1.5, y = -1.8,
             label = "2020 baseline", hjust = 0.5, vjust = 0, size = 3,
             color = "grey15", fontface = "bold") +
    annotate("text", x = 1.5, y = 2.3,
             label = paste0("Range across ", year_end, " diet scenarios"), hjust = 0.5, vjust = 1.2, size = 3.5,
             color = "grey30") +
    coord_cartesian(xlim = c(-2.5, 5.5), ylim = c(-2, 2)) +
    theme_void() +
    theme(plot.margin = margin(0, 10, 0, 10))
  
  p_combined <- p / p_key + plot_layout(heights = c(15, 1.5))
  
  return(p_combined)
}

p_cons_range = plot_total_range(df, "cons")
p_cons_range
ggsave(paste0("range_chart_cons_vol_", year_end, ".png"), p_cons_range, width = 10, height = 6.5, dpi = 300)

p_prod_range = plot_total_range(df, "prod")
p_prod_range
ggsave(paste0("range_chart_prod_vol_", year_end, ".png"), p_prod_range, width = 10, height = 6.5, dpi = 300)



#######################################################################################
################ SMALL & LARGE SHARE (VOL): 2020 vs FUTURE RANGE #####################
#######################################################################################

plot_volume_range_by_size <- function(df, var = c("cons", "prod", "imp", "exp"), show_labels = FALSE) {
  
  var <- match.arg(var)
  var_label <- switch(var,
                      cons = "Consumption",
                      prod = "Production",
                      imp  = "Imports",
                      exp  = "Exports"
  )
  x_label   <- paste0(var_label, " volume (million tonnes)")
  
  # ---- 2020 baseline volumes by farm size ----
  df_2020 <- df %>%
    filter(year == 2020) %>%
    group_by(group, food_group, fs_plot) %>%
    summarise(vol = sum(.data[[var]])/1e6, .groups = "drop") %>%
    filter(fs_plot %in% c("Small", "Large")) %>%
    pivot_wider(names_from = fs_plot, values_from = vol,
                names_prefix = "vol_2020_") %>%
    rename(small_2020 = vol_2020_Small, large_2020 = vol_2020_Large)
  
  # ---- Future range across diet scenarios by farm size ----
  df_end <- df %>%
    filter(year == year_end,
           RCP == rcp_pick,
           lib_scn == lib_pick,
           size_scn == size_scn_pick,
           fs_plot %in% c("Small", "Large")) %>%
    group_by(group, food_group, diet_scn, fs_plot) %>%
    summarise(vol = sum(.data[[var]])/1e6, .groups = "drop") %>%
    pivot_wider(names_from = fs_plot, values_from = vol) %>%
    group_by(group, food_group) %>%
    summarise(
      small_min      = min(Small),
      small_max      = max(Small),
      small_diet_min = diet_scn[which.min(Small)],
      small_diet_max = diet_scn[which.max(Small)],
      large_min      = min(Large),
      large_max      = max(Large),
      large_diet_min = diet_scn[which.min(Large)],
      large_diet_max = diet_scn[which.max(Large)],
      .groups = "drop"
    ) %>%
    mutate(
      small_diet_min = sub("^\\d+\\s+", "", small_diet_min),
      small_diet_max = sub("^\\d+\\s+", "", small_diet_max),
      large_diet_min = sub("^\\d+\\s+", "", large_diet_min),
      large_diet_max = sub("^\\d+\\s+", "", large_diet_max)
    )
  
  plot_df <- df_2020 %>%
    left_join(df_end, by = c("group", "food_group"))
  
  # ---- Labels ----
  plot_df$food <- factor(fg_map[plot_df$food_group], levels = fg_levels)
  plot_df$region <- factor(plot_df$group, levels = regions_rev)
  
  # ---- Alternating row shading ----
  row_shade_df <- expand.grid(
    food = levels(plot_df$food),
    region = levels(plot_df$region)
  ) %>%
    mutate(
      region_index = as.numeric(region),
      ymin = region_index - 0.5,
      ymax = region_index + 0.5,
      shade = ifelse(region_index %% 2 == 0, "grey95", NA)
    )
  
  p <- ggplot(plot_df, aes(y = region)) +
    
    # Row stripes
    geom_rect(
      data = row_shade_df %>% filter(!is.na(shade)),
      aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "grey95",
      alpha = 0.6
    ) +
    
    # SMALL future range (green)
    geom_segment(
      aes(x = small_min, xend = small_max, yend = region, color = "Small"),
      linewidth = 2.2,
      alpha = 0.85
    ) +
    geom_point(aes(x = small_min), size = 1.2, color = "#1a5c2a") +
    geom_point(aes(x = small_max), size = 1.2, color = "#1a5c2a") +
    
    # LARGE future range (yellow)
    geom_segment(
      aes(x = large_min, xend = large_max, yend = region, color = "Large"),
      linewidth = 2.2,
      alpha = 0.85
    ) +
    geom_point(aes(x = large_min), size = 1.2, color = "#7a6518") +
    geom_point(aes(x = large_max), size = 1.2, color = "#7a6518") +
    
    # 2020 Small baseline
    geom_point(
      aes(x = small_2020),
      shape = 124,
      size = 6,
      color = "#1a5c2a",
      stroke = 1.2
    ) +
    
    # 2020 Large baseline
    geom_point(
      aes(x = large_2020),
      shape = 124,
      size = 6,
      color = "#7a6518",
      stroke = 1.2
    ) +
    
    facet_wrap(~ food, scales = "free_x", nrow = 2) +
    
    scale_x_continuous(
      labels = comma,
      expand = expansion(mult = c(0.1, 0.1))
    ) +
    
    scale_color_manual(
      name = NULL,
      values = c("Small" = "#6AAE7B", "Large" = "#D4A843"),
      labels = c("Small" = "Small (<5 ha)", "Large" = "Large (>50 ha)"),
      breaks = c("Small", "Large")
    ) +
    
    labs(
      x = x_label,
      y = NULL
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "grey90", color = NA),
      axis.text.y = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.6),
      panel.spacing = unit(1.2, "lines"),
      plot.margin = margin(10, 15, 10, 10)
    )
  
  if (show_labels){
    p <- p +
      geom_text(
        aes(x = small_min, label = small_diet_min),
        size = 2.2, hjust = 1.2, vjust = 0.5, nudge_y = 0.25,
        fontface = "bold", color = "#1a5c2a"
      ) +
        geom_text(
          aes(x = small_max, label = small_diet_max),
          size = 2.2, hjust = -0.2, vjust = 0.5, nudge_y = 0.25,
          fontface = "bold", color = "#1a5c2a"
        ) +
        
        # Large range labels (yellow – nudge DOWN)
        geom_text(
          aes(x = large_min, label = large_diet_min),
          size = 2.2, hjust = 1.2, vjust = 0.5, nudge_y = -0.25,
          fontface = "bold", color = "#7a6518"
        ) +
        geom_text(
          aes(x = large_max, label = large_diet_max),
          size = 2.2, hjust = -0.2, vjust = 0.5, nudge_y = -0.25,
          fontface = "bold", color = "#7a6518"
        ) 
  }
  
  # legend
  color_legend <- cowplot::get_legend(
    p + theme(legend.position = "bottom",
              legend.justification = "center",
              legend.text = element_text(size = 10),
              legend.key.width = unit(1, "cm"))
  )
  
  p_key <- ggplot() +
    geom_segment(aes(x = 0.5, xend = 2.5, y = 0, yend = 0),
                 color = "grey55", linewidth = 3, alpha = 0.85,
                 arrow = arrow(ends = "both",
                               type = "closed",
                               angle = 20,
                               length = unit(0.1, "inches"))) +
    geom_segment(aes(x = 1.5, xend = 1.5, y = -1, yend = 1),
                 color = "grey15", linewidth = 1) +
    annotate("text", x = 1.5, y = -1.8,
             label = "2020 baseline", hjust = 0.5, vjust = 0, size = 3,
             color = "grey15", fontface = "bold") +
    annotate("text", x = 1.5, y = 2.3,
             label = paste0("Range across ", year_end, " diet scenarios"), hjust = 0.5, vjust = 1.2, size = 3.5,
             color = "grey30") +
    coord_cartesian(xlim = c(-0.5, 3.5), ylim = c(-2, 2)) +
    theme_void() +
    theme(plot.margin = margin(0, 10, 0, 10))
  
  p_main <- p + theme(legend.position = "none")
  
  legend_row <- patchwork::wrap_plots(
    p_key,
    patchwork::wrap_elements(full = color_legend),
    nrow = 1,
    widths = c(1.5, 1.5)
  )
  
  p_combined <- p_main / legend_row +
    plot_layout(heights = c(15, 1.5))
  
  return(p_combined)
}

p_cons_vol_size <- plot_volume_range_by_size(df, "cons")
p_cons_vol_size
ggsave(paste0("range_chart_cons_vol_small_large_", size_scn_pick, "_", year_end, ".png"),
       p_cons_vol_size, width = 10, height = 6.5, dpi = 300)

p_prod_vol_size <- plot_volume_range_by_size(df, "prod")
p_prod_vol_size
ggsave(paste0("range_chart_prod_vol_small_large_", size_scn_pick, "_", year_end, ".png"),
       p_prod_vol_size, width = 10, height = 6.5, dpi = 300)

p_exp_vol_size <- plot_volume_range_by_size(df, "exp")
p_exp_vol_size
ggsave(paste0("range_chart_exp_vol_small_large_", size_scn_pick, "_", year_end, ".png"),
       p_exp_vol_size, width = 10, height = 6.5, dpi = 300)

p_imp_vol_size <- plot_volume_range_by_size(df, "imp")
p_imp_vol_size
ggsave(paste0("range_chart_imp_vol_small_large_", size_scn_pick, "_", year_end, ".png"),
       p_imp_vol_size, width = 10, height = 6.5, dpi = 300)


#######################################################################################
################ SMALL & LARGE SHARE (PERC): 2020 vs FUTURE RANGE #####################
#######################################################################################

plot_share_range <- function(df, var = c("cons", "prod", "imp", "exp"), show_labels = FALSE) {
  
  var <- match.arg(var)
  var_label <- switch(var,
                      cons = "Consumption",
                      prod = "Production",
                      imp  = "Imports",
                      exp  = "Exports"
  )
  total_var <- paste0(var, "_total")
  
  # ---- 2020 baseline shares ----
  df_2020 <- df %>%
    filter(year == 2020) %>%
    group_by(group, food_group) %>%
    summarise(
      small_2020 = sum(.data[[var]][fs_plot == "Small"]) / first(.data[[total_var]]),
      large_2020 = sum(.data[[var]][fs_plot == "Large"]) / first(.data[[total_var]]),
      .groups = "drop"
    )
  
  # ---- 2030 range across diet scenarios ----
  df_end <- df %>%
    filter(year == year_end,
           RCP == rcp_pick,
           lib_scn == lib_pick,
           size_scn == size_scn_pick) %>%
    group_by(group, food_group, diet_scn) %>%
    summarise(
      small = sum(.data[[var]][fs_plot == "Small"]) / first(.data[[total_var]]),
      large = sum(.data[[var]][fs_plot == "Large"]) / first(.data[[total_var]]),
      .groups = "drop"
    ) %>%
    group_by(group, food_group) %>%
    summarise(
      small_min      = min(small),
      small_max      = max(small),
      small_diet_min = diet_scn[which.min(small)],
      small_diet_max = diet_scn[which.max(small)],
      large_min      = min(large),
      large_max      = max(large),
      large_diet_min = diet_scn[which.min(large)],
      large_diet_max = diet_scn[which.max(large)],
      .groups = "drop"
    ) %>%
    mutate(
      small_diet_min = sub("^\\d+\\s+", "", small_diet_min),
      small_diet_max = sub("^\\d+\\s+", "", small_diet_max),
      large_diet_min = sub("^\\d+\\s+", "", large_diet_min),
      large_diet_max = sub("^\\d+\\s+", "", large_diet_max)
    )
  
  plot_df <- df_2020 %>%
    left_join(df_end, by = c("group", "food_group"))
  
  # ---- Labels ----
  plot_df$food <- factor(fg_map[plot_df$food_group], levels = fg_levels)
  plot_df$region <- factor(plot_df$group, levels = regions_rev)
  
  # ---- Alternating row shading ----
  row_shade_df <- expand.grid(
    food = levels(plot_df$food),
    region = levels(plot_df$region)
  ) %>%
    mutate(
      region_index = as.numeric(region),
      ymin = region_index - 0.5,
      ymax = region_index + 0.5,
      shade = ifelse(region_index %% 2 == 0, "grey95", NA)
    )
  
  p <- ggplot(plot_df, aes(y = region)) +
    
    # Row stripes
    geom_rect(
      data = row_shade_df %>% filter(!is.na(shade)),
      aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "grey95",
      alpha = 0.6
    ) +
    
    # SMALL 2030 range (green)
    geom_segment(
      aes(x = small_min, xend = small_max, yend = region, color="Small"),
      linewidth = 2.2,
      alpha = 0.85
    ) +
    geom_point(aes(x = small_min), size = 1.2, color = "#1a5c2a") +
    geom_point(aes(x = small_max), size = 1.2, color = "#1a5c2a") +
    
    # LARGE 2030 range (yellow)
    geom_segment(
      aes(x = large_min, xend = large_max, yend = region, color="Large"),
      linewidth = 2.2,
      alpha = 0.85
    ) +
    geom_point(aes(x = large_min), size = 1.2, color = "#7a6518") +
    geom_point(aes(x = large_max), size = 1.2, color = "#7a6518") +
    
    # 2020 Small baseline |
    geom_point(
      aes(x = small_2020),
      shape = 124,
      size = 6,
      color = "#1a5c2a",
      stroke = 1.2
    ) +
    
    # 2020 Large baseline |
    geom_point(
      aes(x = large_2020),
      shape = 124,
      size = 6,
      color = "#7a6518",
      stroke = 1.2
    ) +
    
    facet_wrap(~ food, nrow = 2) +
    
    scale_x_continuous(
      limits = c(-0.08, 1.08),
      breaks = seq(0, 1, by = 0.25),
      labels = scales::percent_format(accuracy = 1),
      expand = c(0, 0)
    ) +
    
    scale_color_manual(
      name = NULL,
      values = c("Small" = "#6AAE7B", "Large" = "#D4A843"),
      labels = c("Small" = "Small (<5 ha)", "Large" = "Large (>50 ha)"),
      breaks = c("Small", "Large")
    ) +
    
    labs(
      x = paste0("Share of ", tolower(var_label), " (%)"),
      y = NULL
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "grey90", color = NA),
      axis.text.y = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.6),
      panel.spacing = unit(1.2, "lines"),
      plot.margin = margin(10, 15, 10, 10)
    )
  
  if (show_labels){
    p <- p +
      geom_text(
        aes(x = small_min, label = small_diet_min),
        size = 2.2, hjust = 1.2, vjust = 0.5, nudge_y = 0.25,
        fontface = "bold", color = "#1a5c2a"
      ) +
      geom_text(
        aes(x = small_max, label = small_diet_max),
        size = 2.2, hjust = -0.2, vjust = 0.5, nudge_y = 0.25,
        fontface = "bold", color = "#1a5c2a"
      ) +
      
      geom_text(
        aes(x = large_min, label = large_diet_min),
        size = 2.2, hjust = 1.2, vjust = 0.5, nudge_y = -0.25,
        fontface = "bold", color = "#7a6518"
      ) +
      geom_text(
        aes(x = large_max, label = large_diet_max),
        size = 2.2, hjust = -0.2, vjust = 0.5, nudge_y = -0.25,
        fontface = "bold", color = "#7a6518"
      ) 
  }
  
  # legend
  color_legend <- cowplot::get_legend(
    p + theme(legend.position = "bottom",
              legend.justification = "center",
              legend.text = element_text(size = 10),
              legend.key.width = unit(1, "cm"))
  )
  
  p_key <- ggplot() +
    geom_segment(aes(x = 0.5, xend = 2.5, y = 0, yend = 0),
                 color = "grey55", linewidth = 3, alpha = 0.85,
                 arrow = arrow(ends = "both",
                               type = "closed",
                               angle = 20,
                               length = unit(0.1, "inches"))) +
    geom_segment(aes(x = 1.5, xend = 1.5, y = -1, yend = 1),
                 color = "grey15", linewidth = 1) +
    annotate("text", x = 1.5, y = -1.8,
             label = "2020 baseline", hjust = 0.5, vjust = 0, size = 3,
             color = "grey15", fontface = "bold") +
    annotate("text", x = 1.5, y = 2.3,
             label = paste0("Range across ", year_end, " diet scenarios"), hjust = 0.5, vjust = 1.2, size = 3.5,
             color = "grey30") +
    coord_cartesian(xlim = c(-0.5, 3.5), ylim = c(-2, 2)) +
    theme_void() +
    theme(plot.margin = margin(0, 10, 0, 10))
  
  p_main <- p + theme(legend.position = "none")
  
  legend_row <- patchwork::wrap_plots(
    p_key,
    patchwork::wrap_elements(full = color_legend),
    nrow = 1,
    widths = c(1.5, 1.5)
  )
  
  p_combined <- p_main / legend_row +
    plot_layout(heights = c(15, 1.5))
  
  return(p_combined)
}

p_cons_share = plot_share_range(df, "cons")
p_cons_share
ggsave(paste0("range_chart_cons_small_large_", size_scn_pick, "_", year_end, ".png"),
       p_cons_share, width = 10, height = 6.5, dpi = 300)


p_prod_share = plot_share_range(df, "prod")
p_prod_share
ggsave(paste0("range_chart_prod_small_large_", size_scn_pick, "_", year_end, ".png"),
       p_prod_share, width = 10, height = 6.5, dpi = 300)

p_exp_share = plot_share_range(df, "exp")
p_exp_share
ggsave(paste0("range_chart_exp_small_large_", size_scn_pick, "_", year_end, ".png"),
       p_exp_share, width = 10, height = 6.5, dpi = 300)

p_imp_share = plot_share_range(df, "imp")
p_imp_share
ggsave(paste0("range_chart_imp_small_large_", size_scn_pick, "_", year_end, ".png"),
       p_imp_share, width = 10, height = 6.5, dpi = 300)

#######################################################################################
#################################### BOXPLOTS #########################################
#######################################################################################

df_country <- read_csv("country_summary.csv", show_col_types = FALSE)

plot_boxplot_change <- function(df, var = c("cons", "prod")) {
  
  var <- match.arg(var)
  var_label <- ifelse(var == "cons", "Consumption", "Production")
  
  agg <- df %>%
    filter(
      year == year_end, 
      RCP == rcp_pick,
      lib_scn == lib_pick,
      size_scn == size_scn_pick) %>%
    group_by(abbreviation, diet_scn) %>%
    summarise(
      s20 = sum(.data[[paste0(var, "_small_2020")]]),
      m20 = sum(.data[[paste0(var, "_medium_2020")]]),
      l20 = sum(.data[[paste0(var, "_large_2020")]]),
      s_yr = sum(.data[[paste0(var, "_small")]]),
      m_yr = sum(.data[[paste0(var, "_medium")]]),
      l_yr = sum(.data[[paste0(var, "_large")]]),
      .groups = "drop"
    ) %>%
    mutate(
      pct_small  = (s_yr - s20) / s20 * 100,
      pct_medium = (m_yr - m20) / m20 * 100,
      pct_large  = (l_yr - l20) / l20 * 100
    )
  
  plot_df <- agg %>%
    select(abbreviation, diet_scn, pct_small, pct_medium, pct_large) %>%
    pivot_longer(
      cols = c(pct_small, pct_medium, pct_large),
      names_to = "farm_type",
      values_to = "pct_change"
    ) %>%
    filter(is.finite(pct_change)) %>%
    mutate(
      farm_label = case_when(
        farm_type == "pct_small"  ~ "Small",
        farm_type == "pct_medium" ~ "Medium",
        farm_type == "pct_large"  ~ "Large"
      ),
      farm_label = factor(farm_label, levels = c("Small", "Medium", "Large")),
      diet_short = factor(diet_scn, levels = diet_order)
    )
  
  p <- ggplot(plot_df,
              aes(x = diet_short, y = pct_change, fill = farm_label)) +
    
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "grey50",
               linewidth = 0.4) +
    
    geom_boxplot(
      width = 0.55,
      position = position_dodge(width = 0.7),
      outlier.size = 0.7,
      outlier.alpha = 0.3,
      color = "grey40",
      linewidth = 0.35
    ) +
    
    scale_fill_manual(
      values = c(
        "Small"  = "#1a5c2a",
        "Medium" = "#999999",
        "Large"  = "#D4A843"
      ),
      labels = c(
        "Small"  = "Small (<5 ha)",
        "Medium" = "Medium (5–50 ha)",
        "Large"  = "Large (>50 ha)"
      ),
      name = NULL
    ) +
    
    coord_cartesian(ylim = c(-50, 200)) +
    
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      breaks = seq(-100, 200, by = 20)
    ) +
    
    labs(
      x = NULL,
      y = paste0("Change in ", var_label, " Volume: 2020 to ", year_end)
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      axis.text.y        = element_text(size = 11),
      axis.text.x        = element_text(size = 11, color = "#333333", face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.text        = element_text(size = 11),
      plot.margin        = margin(10, 15, 10, 10)
    )
  
  return(p)
}

p_cons <- plot_boxplot_change(df_country, "cons")
p_cons
ggsave(paste0("boxplot_cons_change_", size_scn_pick, "_", year_end, ".png"), p_cons, width = 10, height = 6, dpi = 300)

p_prod <- plot_boxplot_change(df_country, "prod")
p_prod
ggsave(paste0("boxplot_prod_change_", size_scn_pick, "_", year_end, ".png"), p_prod, width = 10, height = 6, dpi = 300)



#######################################################################################
################################## BUBBLE CHART #######################################
#######################################################################################

df_country <- read_csv("country_summary.csv", show_col_types = FALSE)

oecd_countries <- c(
  "AUS", "AUT", "BLX", "CAN", "CHL", "CHP", "CZE", "DEU", "DNK",
  "FNP", "FRP", "GRC", "HUN", "IRL", "ISR", "ITP", "JPN", "KOR",
  "MEX", "NLD", "NOR", "NZL", "POL", "PRT", "SPP", "SVK", "SVN",
  "SWE", "TUR", "UKP", "USA"
)

plot_bubble_change <- function(df, diet_pick, var = c("cons", "prod")) {

  var <- match.arg(var)
  var_label <- ifelse(var == "cons", "Consumption", "Production")

  agg <- df %>%
    filter(
      year == year_end,
      diet_scn == paste0(year_end, " ", diet_pick),
      RCP == rcp_pick,
      lib_scn == lib_pick,
      size_scn == size_scn_pick) %>%
    group_by(abbreviation, diet_scn) %>%
    summarise(
      t20 = sum(.data[[paste0(var, "_total_2020")]]),
      s20 = sum(.data[[paste0(var, "_small_2020")]]),
      t_yr = sum(.data[[paste0(var, "_total")]]),
      s_yr = sum(.data[[paste0(var, "_small")]]),
      .groups = "drop"
    ) %>%
    mutate(
      rel_20_small  = (s20 / t20) * 100,
      rel_yr_small  = (s_yr / t_yr) * 100,
      rel_change_small  = ((s_yr / t_yr) - (s20 / t20)) * 100,
      label_x = pmin(rel_20_small, rel_yr_small) - 1,
      point_type = if_else(abbreviation %in% oecd_countries, "plus", "circle")
    ) %>%
    mutate(abbreviation = forcats::fct_reorder(abbreviation, rel_20_small))

  agg_circle <- dplyr::filter(agg, point_type == "circle")
  agg_plus   <- dplyr::filter(agg, point_type == "plus")
  
  p <- ggplot(agg) +
    geom_segment(aes(x = rel_20_small, xend = rel_yr_small,
                     y = abbreviation, yend = abbreviation),
                 colour = "grey80", linewidth = 0.5) +
    geom_point(
      data = agg_circle,
      aes(x = rel_20_small, y = abbreviation, colour = "2020"),
      size = 1.2,
      shape = 16
    ) +
    geom_point(
      data = agg_circle,
      aes(x = rel_yr_small, y = abbreviation, colour = paste(year_end, diet_pick)),
      size = 1.2,
      shape = 16
    ) +
    geom_point(
      data = agg_plus,
      aes(x = rel_20_small, y = abbreviation, colour = "2020"),
      size = 0.8,
      shape = 3,
      stroke = 0.8
    ) +
    geom_point(
      data = agg_plus,
      aes(x = rel_yr_small, y = abbreviation, colour = paste(year_end, diet_pick)),
      size = 0.8,
      shape = 3,
      stroke = 0.8
    ) +
    geom_text(
      aes(x = label_x, y = abbreviation, label = abbreviation),
      hjust = 1,
      size = 2.2
    ) +
    scale_colour_manual(
      values = setNames(c("#333333", "grey60"),
                        c("2020", paste(year_end, diet_pick))),
      name = NULL
    ) +
    scale_x_continuous(
      breaks = seq(0, 100, by = 10),
      labels = function(x) paste0(x, "%")) +
    labs(x = "Small-farm share of national consumption (%)", y = NULL) +
    guides(
      colour = guide_legend(
        override.aes = list(
          shape = c(16, 16),
          size = 3
        )
      )
    ) +
    theme_minimal(base_size = 9) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "top",
          legend.key.width = unit(0.45, "cm"),
          legend.spacing.x = unit(0.1, "cm"),
          legend.text = element_text(margin = margin(l = -0.5)),
          plot.margin = margin(5.5, 5.5, 5.5, 18)
          )

  return(p)
}

p_rel_cons <- plot_bubble_change(df_country, "FLX", "cons")
p_rel_cons
ggsave(paste0("bubble_cons_reliance_FLX_", size_scn_pick, "_", year_end, ".png"), p_rel_cons, width = 5, height = 16, dpi = 300)

p_rel_cons <- plot_bubble_change(df_country, "BMK", "cons")
p_rel_cons
ggsave(paste0("bubble_cons_reliance_BMK_", size_scn_pick, "_", year_end, ".png"), p_rel_cons, width = 5, height = 16, dpi = 300)


#######################################################################################
########################### TRADE NETWORKS ############################################
#######################################################################################

# Read trade data
trade_raw <- read_csv("trade_summary.csv", show_col_types = FALSE) %>%
  mutate(
    from_group = recode_region(from_group),
    to_group   = recode_region(to_group)
  )

# Filter to 2020 and chosen future year
trade <- trade_raw %>%
  filter(
    (year == 2020) |
      (year == year_end &
         RCP == rcp_pick &
         lib_scn == lib_pick &
         size_scn == size_scn_pick)
  ) %>%
  mutate(
    fs_plot = case_when(
      farm_size == "small"  ~ "Small",
      farm_size == "medium" ~ "Medium",
      farm_size == "large"  ~ "Large",
      TRUE ~ NA_character_
    ),
    fs_plot = factor(fs_plot, levels = fs_levels)
  ) %>%
  filter(!is.na(fs_plot)) %>%
  group_by(from_group, to_group, year, diet_scn, fs_plot) %>%
  summarise(trade = sum(trade), .groups = "drop")

# Define scenarios (rows)
scenarios <- list(
  list(year = 2020, diet = "2020 BMK", label = "2020"),
  list(year = year_end, diet = paste0(year_end, " BMK"), label = paste0(year_end, " BMK")),
  list(year = year_end, diet = paste0(year_end, " FLX"), label = paste0(year_end, " FLX")),
  list(year = year_end, diet = paste0(year_end, " PSC"), label = paste0(year_end, " PSC")),
  list(year = year_end, diet = paste0(year_end, " VEG"), label = paste0(year_end, " VEG")),
  list(year = year_end, diet = paste0(year_end, " VGN"), label = paste0(year_end, " VGN"))
)

# Regions (must match trade file region codes)
regions <- c("CNE","EAP","EUR","LAC","MEN","NAM","SAS","SSA")

region_cols <- c(
  "CNE" = "#00A087",
  "EAP" = "#E64B35",
  "EUR" = "#4DBBD5",
  "LAC" = "#3C5488",
  "MEN" = "#F39B7F",
  "NAM" = "#8491B4",
  "SAS" = "#91D1C2",
  "SSA" = "#B09C85"
)

fs_labels <- c(
  "Small"  = "Small (<5 ha)",
  "Medium" = "Medium (5-50 ha)",
  "Large"  = "Large (>50 ha)"
)

# Function to draw one chord diagram
draw_chord <- function(data) {
  
  mat <- matrix(0,
                nrow = length(regions),
                ncol = length(regions),
                dimnames = list(regions, regions))
  
  for (i in seq_len(nrow(data))) {
    from <- data$from_group[i]
    to   <- data$to_group[i]
    mat[from, to] <- data$trade[i] / 1e9  # convert to billion tonnes
  }
  
  circos.clear()
  circos.par(
    gap.after = rep(3, length(regions)),
    start.degree = 90,
    track.margin = c(0.01, 0.01)
  )
  
  chordDiagram(
    mat,
    grid.col = region_cols,
    transparency = 0.3,
    annotationTrack = c("grid"),
    preAllocateTracks = list(track.height = 0.08),
    directional = 1,
    direction.type = c("diffHeight", "arrows"),
    link.arr.type = "big.arrow",
    link.sort = TRUE,
    link.largest.ontop = TRUE
  )
  
  circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
      sector.name <- get.cell.meta.data("sector.index")
      xlim <- get.cell.meta.data("xlim")
      ylim <- get.cell.meta.data("ylim")
      circos.text(
        mean(xlim), ylim[1] + 0.3,
        sector.name,
        facing = "clockwise",
        niceFacing = TRUE,
        adj = c(0, 0.5),
        cex = 1.2,
        font = 2
      )
    },
    bg.border = NA
  )
  
}


# Create 6 × 3 grid (6 scenarios × 3 farm sizes)
png(paste0("chord_diagrams_", size_scn_pick, "_", year_end, ".png"),
    width = 3000, height = 5000, res = 200)

par(mfrow = c(length(scenarios), length(fs_levels)),
    mar = c(0, 0, 0, 0),
    oma = c(1, 11.5, 4, 1))

for (scn in scenarios) {

  for (fs in fs_levels) {

    d <- trade %>%
      filter(year == scn$year,
             diet_scn == scn$diet,
             fs_plot == fs)

    draw_chord(d)

  }
}

for(i in seq_along(fs_levels)) {
  mtext(fs_labels[fs_levels[i]], side = 3, outer = TRUE, line = 1,
        at = (i - 0.5) / length(fs_levels), font = 2, cex = 1.8)
}

for(i in 1:length(scenarios)) {
  mtext(scenarios[[i]]$label, side = 2, outer = TRUE, line = 2,
        at = 1 - (i - 0.5) / length(scenarios), font = 2, cex = 1.5, las = 1)
}

dev.off()


png(paste0("chord_diagram_2020.png"),
    width = 3000, height = 1000, res = 200)
par(mfrow = c(1, length(fs_levels)),
    mar = c(0, 0, 0, 0),
    oma = c(1, 1, 4, 1))

for (fs in fs_levels) {
  d <- trade %>%
    filter(year == 2020, diet_scn == "2020 BMK", fs_plot == fs)
  draw_chord(d)
}

for (i in seq_along(fs_levels)) {
  mtext(fs_labels[fs_levels[i]], side = 3, outer = TRUE, line = 1,
        at = (i - 0.5) / length(fs_levels), font = 2, cex = 1.8)
}

dev.off()