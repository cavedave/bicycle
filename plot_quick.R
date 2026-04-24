library(ggplot2)
library(ggrepel)
library(ggthemes)

df <- read.csv("tucker_1970_table1_cost_of_transport.csv", stringsAsFactors = FALSE)

# Clean: drop rows missing key values
df <- df[!is.na(df$weight_kg) & !is.na(df$cost_of_transport_kcal_per_kg_km), ]
df <- df[df$weight_kg > 0 & df$cost_of_transport_kcal_per_kg_km > 0, ]

# ── Keep only exemplar rows for a clean one-point-per-entry plot ─────────────
df <- df[df$exemplar == 1, ]

# ── Derive medium (air / water / land) ───────────────────────────────────────
vehicle_medium <- c(
  "Cherokee airplane"              = "air",
  "Grand Commander airplane"       = "air",
  "DC 8 jet transport"             = "air",
  "DC 9-10 jet transport"          = "air",
  "F105F jet fighter"              = "air",
  "Sikorsky S62 helicopter"        = "air",
  "Boeing 787-9 Dreamliner"        = "air",
  "Airbus A350-900"                = "air",
  "Paraglider (EN-C class)"        = "air",
  "Hang glider (rigid wing)"       = "air",
  "Daedalus 88 (human-powered)"    = "air",
  "VLCC crude oil tanker"          = "water",
  "Container ship (Triple-E class)"= "water",
  "Rowing (single scull)"          = "water",
  "Kayak (K1)"                     = "water",
  "Cadillac automobile"            = "land",
  "Volkswagen automobile"          = "land",
  "Petrol car (family sedan)"      = "land",
  "Electric car (BEV)"             = "land",
  "Motorcycle"                     = "land",
  "Human on bicycle (touring)"     = "land",
  "Human on bicycle (racing)"      = "land",
  "Velomobile"                     = "land",
  "Cross-country skiing (skating)" = "land",
  "Cross-country skiing (classical)"= "land",
  "Inline skating"                 = "land",
  "Ice speed skating"              = "land",
  "Diesel freight train (heavy haul)" = "land",
  "Suburban electric train (S-Bahn)"  = "land",
  "TGV Réseau (high-speed train)"     = "land",
  "Transrapid TR08 maglev"            = "land",
  "Urban tram (electric)"             = "land"
)

flyer_vehicles <- c(
  "Concorde", "Helicopter",
  "F1D indoor rubber free-flight", "Pennyplane (indoor rubber)",
  "Quadcopter drone (consumer)", "Electric RC model airplane"
)

df$medium <- with(df, ifelse(
  category == "flyer",   "air",
  ifelse(category == "swimmer", "water",
  ifelse(category == "walker_runner", "land",
  vehicle_medium[name]
))))

df$origin <- with(df, ifelse(
  category == "vehicle" | name %in% flyer_vehicles,
  "vehicle",
  "biological"
))

df$label <- df$display_name

# Human+equipment rows in the CSV use weight_kg = 70 kg reference person + gear
# (bicycle, shell, skis, boat, aircraft, etc.); metabolic_rate and cost_of_transport
# are rescaled so implied total power (met × mass) matches the source measurement.

# Omitted from redacted views that start at fruit-fly scale (full + animal panels)
redact_microbe_mole_names <- c(
  "E. coli (swimming)",
  "Paramecium caudatum",
  "C. elegans (swimming)",
  "Naked mole-rat (walking)",
  "Cape mole-rat (burrowing)",
  "Pocket gopher (burrowing)"
)

# Omitted from full_redact only (still on unredacted full + vehicle panels)
redact_full_redact_only_names <- c(
  "Hang glider (rigid wing)",
  "Paraglider (EN-C class)"
)

wilson_subtitle <- "Update of the \u201cBicycles for the mind\u201d plot (Wilson, Scientific American, 1973)"

# Tufte-style paper (warm off-white); also set ggsave(bg=…) so PNG is not transparent
paper <- "#F7F6F0"

# PNG size for sharing (e.g. Reddit): 200 dpi × 16×11 in → 3200×2200 px — text stays
# sharp after the site re-encodes; under typical image size limits.
out_w_in <- 16
out_h_in <- 11
out_dpi  <- 200
plots_dir <- "plots"
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# Quieter, slightly desaturated palette (Tufte: ink should not shout)
medium_colours <- c(
  "air"   = "#6B9BD1",
  "water" = "#4A9E8E",
  "land"  = "#C97A5A"
)

origin_shapes <- c(
  "biological" = 16,
  "vehicle"    = 17
)

# Shared Tufte theme (tweak legend block per-plot)
tufte_base <- function(legend_pos = c(0.985, 0.985), legend_just = c(1, 1)) {
  theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
    theme(
      plot.background  = element_rect(fill = paper, colour = NA),
      panel.background = element_rect(fill = paper, colour = NA),
      plot.caption     = element_text(
                          hjust = 1, size = 8, colour = "grey45",
                          face = "italic", margin = margin(t = 8)
                        ),
      plot.margin      = margin(14, 14, 18, 14),
      panel.grid       = element_blank(),
      axis.title       = element_text(colour = "grey25"),
      axis.text        = element_text(colour = "grey35"),
      axis.ticks       = element_line(colour = "grey50", linewidth = 0.3),
      legend.position      = legend_pos,
      legend.justification = legend_just,
      legend.background    = element_rect(
                               fill   = scales::alpha(paper, 0.97),
                               colour = "grey75", linewidth = 0.25
                             ),
      legend.key           = element_blank(),
      legend.title         = element_text(size = 9, face = "italic", colour = "grey35"),
      legend.text          = element_text(size = 9, colour = "grey30"),
      legend.margin        = margin(5, 6, 5, 6),
      legend.spacing.y     = unit(0.15, "cm")
    )
}

# ═══════════════════════════════════════════════════════════════════════════
# Full plot — animals + vehicles + human-powered machines
# ═══════════════════════════════════════════════════════════════════════════
# full_redact uses full_panel_*; unredacted full uses repel/title text one step smaller (denser plot)
full_panel_pt_size  <- 3.6
full_panel_lbl_size <- 4.2
full_unredact_lbl_size <- full_panel_lbl_size - 1

p_full <- ggplot(df, aes(x = weight_kg, y = cost_of_transport_kcal_per_kg_km,
                         colour = medium, shape = origin)) +
  geom_point(size = full_panel_pt_size, alpha = 0.88) +
  geom_text_repel(
    aes(label = label),
    size               = full_unredact_lbl_size,
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.35,
    min.segment.length = 0.15,
    box.padding        = 0.32,
    point.padding      = 0.26,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    seed               = 42
  ) +
  scale_x_log10(
    name     = "Body / vehicle mass (kg)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_y_log10(
    name     = "Cost of transport (kcal / kg / km)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_colour_manual(values = medium_colours, name = "Medium") +
  scale_shape_manual(values = origin_shapes, name = "Origin") +
  labs(
    title    = "How Animals and Machines Move: Cost of Transport from Bacteria to Oil Tankers",
    subtitle = wilson_subtitle,
    caption  = "Code and data: https://github.com/cavedave/bicycle"
  ) +
  tufte_base() +
  theme(
    plot.title    = element_text(
                     size = 21, face = "plain", colour = "grey10",
                     margin = margin(b = 5)
                   ),
    plot.subtitle = element_text(
                     size = 12, colour = "grey40", face = "italic",
                     lineheight = 1.2, margin = margin(b = 10)
                   ),
    plot.caption  = element_text(
                     hjust = 1, size = 7, colour = "grey45",
                     face = "italic", margin = margin(t = 8)
                   ),
    legend.title  = element_text(size = 10, face = "italic", colour = "grey35"),
    legend.text   = element_text(size = 9.5, colour = "grey30")
  )

ggsave(file.path(plots_dir, "Full_efficiency.png"), p_full,
       width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)
ggsave(file.path(plots_dir, "plot_quick.png"), p_full,
       width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)
# ── Full plot, redacted: animals + vehicles, fruit-fly scale and up ─────────
df_full_redact <- df[
  !df$name %in% redact_microbe_mole_names &
    !df$name %in% redact_full_redact_only_names,
]

# full_redact only: drop rows that duplicate another idea (CSV exemplars unchanged elsewhere)
full_redact_story_omit_names <- c(
  "Grand Commander airplane",       # second light fixed-wing; Cherokee → "Small prop plane"
  "Yellowfin tuna (juvenile)",      # second tuna vs bluefin
  "Adélie penguin",                 # second penguin; emperor swim + waddle kept (contrast)
  "American cockroach",             # same display as discoid cockroach
  "Pennyplane (indoor rubber)",     # second indoor rubber model vs F1D
  "Sikorsky S62 helicopter",        # second helicopter vs Bell JetRanger
  "Electric RC model airplane",     # overlaps indoor-model / small-aircraft story vs F1D
  "Common cuckoo",                  # dubious estimate; trims busy small-flyer band
  "Human (snowshoeing)",            # duplicate story vs walking on snow; kept in other plots
  "Cross-country skiing (classical)", # XC vs walking cluster confusing on this panel only
  "DC 9-10 jet transport",          # superseded / crowded vs modern narrow-bodies; kept in CSV + other plots
  "DC 8 jet transport",             # superseded / crowded vs modern wide-bodies; kept in CSV + other plots
  "Daedalus 88 (human-powered)"    # interesting human-powered aircraft; crowds bicycle cluster on this panel only
)
df_full_redact <- df_full_redact[
  !df_full_redact$name %in% full_redact_story_omit_names,
]

# Clearer repel text on full_redact (defaults still come from CSV `label`)
full_redact_label_alias <- c(
  "Cherokee airplane"          = "Small prop plane",
  "Human on bicycle (touring)" = "Bicycle",
  "Man"                        = "Walking",
  "Human (running)"            = "Running",
  "Human (front crawl)"        = "Swimming",
  "F105F jet fighter"          = "Jet fighter",
  "Boeing 787-9 Dreamliner"    = "Boeing 787",
  "Airbus A350-900"            = "Airbus A350",
  "VLCC crude oil tanker"      = "Oil tanker",
  "Container ship (Triple-E class)" = "Container ship",
  "Diesel freight train (heavy haul)" = "Freight train",
  "Suburban electric train (S-Bahn)" = "Commuter train",
  "TGV Réseau (high-speed train)"    = "High-speed train",
  "Transrapid TR08 maglev"     = "Maglev train",
  "Urban tram (electric)"      = "Tram",
  "Petrol car (family sedan)"  = "Petrol car",
  "Electric car (BEV)"       = "Electric car",
  "Pacific bluefin tuna (juvenile)" = "Bluefin tuna",
  "Emperor penguin (swimming)"      = "Emperor penguin",
  "Emperor penguin (waddling)"      = "Emperor penguin",
  "Fowler's toad (max aerobic hop)" = "Toad hopping",
  "Fowler's toad (slow walk)"       = "Toad (walk)",
  "Domestic cat"                    = "Cat"
)
df_full_redact$fr_label <- as.character(df_full_redact$label)
ix_al <- match(df_full_redact$name, names(full_redact_label_alias))
ok_al <- !is.na(ix_al)
df_full_redact$fr_label[ok_al] <- full_redact_label_alias[df_full_redact$name[ok_al]]

# Repel split: main bulk; per-row nudges (cat / crawl / manatee / sea turtle / bike / quad). Emperor swim+waddle: same short label, colour = medium.
nm_quad    <- df_full_redact$name == "Quadcopter drone (consumer)"
nm_bike    <- df_full_redact$name == "Human on bicycle (touring)"
nm_cat     <- df_full_redact$name == "Domestic cat"
nm_crawl   <- df_full_redact$name == "Human (crawling)"
nm_manatee <- df_full_redact$name == "West Indian manatee"
nm_turtle  <- df_full_redact$name == "Green sea turtle"
df_fr_main <- df_full_redact[
  !nm_quad & !nm_bike & !nm_cat & !nm_crawl & !nm_manatee & !nm_turtle,
]
df_fr_quad     <- df_full_redact[nm_quad, ]
df_fr_bike      <- df_full_redact[nm_bike, ]
df_fr_cat       <- df_full_redact[nm_cat, ]
df_fr_crawl     <- df_full_redact[nm_crawl, ]
df_fr_manatee   <- df_full_redact[nm_manatee, ]
df_fr_sea_turtle <- df_full_redact[nm_turtle, ]

p_full_redact <- ggplot(df_full_redact, aes(x = weight_kg, y = cost_of_transport_kcal_per_kg_km,
                                             colour = medium, shape = origin)) +
  geom_point(size = full_panel_pt_size, alpha = 0.88) +
  geom_text_repel(
    data               = df_fr_main,
    aes(label          = fr_label),
    inherit.aes        = TRUE,
    size               = full_panel_lbl_size,
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.35,
    min.segment.length = 0.15,
    box.padding        = 0.32,
    point.padding      = 0.26,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    seed               = 45
  ) +
  geom_text_repel(
    data               = df_fr_cat,
    aes(label          = fr_label),
    inherit.aes        = TRUE,
    size               = full_panel_lbl_size,
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.35,
    min.segment.length = 0.12,
    box.padding        = 0.26,
    point.padding      = 0.22,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    nudge_x            = 0.15,
    nudge_y            = 0.14,
    seed               = 45
  ) +
  geom_text_repel(
    data               = df_fr_manatee,
    aes(label          = fr_label),
    inherit.aes        = TRUE,
    size               = full_panel_lbl_size,
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.35,
    min.segment.length = 0.12,
    box.padding        = 0.28,
    point.padding      = 0.22,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    nudge_y            = 0.06,
    seed               = 45
  ) +
  geom_text_repel(
    data               = df_fr_crawl,
    aes(label          = fr_label),
    inherit.aes        = TRUE,
    size               = full_panel_lbl_size,
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.35,
    min.segment.length = 0.12,
    box.padding        = 0.28,
    point.padding      = 0.22,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    nudge_y            = 0.11,
    seed               = 45
  ) +
  geom_text_repel(
    data               = df_fr_sea_turtle,
    aes(label          = fr_label),
    inherit.aes        = TRUE,
    size               = full_panel_lbl_size,
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.35,
    min.segment.length = 0.1,
    box.padding        = 0.28,
    point.padding      = 0.22,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    nudge_y            = -0.048,
    seed               = 45
  ) +
  geom_text_repel(
    data               = df_fr_quad,
    aes(label          = fr_label),
    inherit.aes        = TRUE,
    size               = full_panel_lbl_size,
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.35,
    min.segment.length = 0,
    box.padding        = 0.12,
    point.padding      = 0.08,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    seed               = 45
  ) +
  geom_text_repel(
    data               = df_fr_bike,
    aes(label          = fr_label),
    inherit.aes        = TRUE,
    size               = full_panel_lbl_size,
    fontface           = "bold",
    family             = "sans",
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.35,
    min.segment.length = 0.08,
    box.padding        = 0.14,
    point.padding      = 0.1,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    nudge_y            = 0.056,
    seed               = 45
  ) +
  scale_x_log10(
    name     = "Body / vehicle mass (kg)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_y_log10(
    name     = "Cost of transport (kcal / kg / km)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_colour_manual(values = medium_colours, name = "Medium") +
  scale_shape_manual(values = origin_shapes, name = "Origin") +
  labs(
    title    = "How Animals and Machines Move: Cost of Transport from Fruit Fly to Oil Tankers",
    subtitle = wilson_subtitle,
    caption  = "Code and data: https://github.com/cavedave/bicycle"
  ) +
  tufte_base() +
  theme(
    plot.title    = element_text(
                     size = 22, face = "plain", colour = "grey10",
                     margin = margin(b = 5)
                   ),
    plot.subtitle = element_text(
                     size = 13, colour = "grey40", face = "italic",
                     lineheight = 1.2, margin = margin(b = 10)
                   ),
    legend.title  = element_text(size = 11, face = "italic", colour = "grey35"),
    legend.text   = element_text(size = 10.5, colour = "grey30")
  )

ggsave(file.path(plots_dir, "full_redact.png"), p_full_redact,
       width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)

# ═══════════════════════════════════════════════════════════════════════════
# Animal-only — no vehicles or human-powered machines (bicycles, ships, etc.)
# ═══════════════════════════════════════════════════════════════════════════
# Slightly larger points + labels than the combined plot (fewer points, more room)
panel_pt_size  <- 4.5
panel_lbl_size <- 5.0

df_bio <- df[df$origin == "biological", ]

p_animal <- ggplot(df_bio, aes(x = weight_kg, y = cost_of_transport_kcal_per_kg_km,
                               colour = medium)) +
  geom_point(size = panel_pt_size, alpha = 0.88, shape = 16) +
  geom_text_repel(
    aes(label = label),
    size               = panel_lbl_size,
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.35,
    min.segment.length = 0.15,
    box.padding        = 0.35,
    point.padding      = 0.28,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    seed               = 42
  ) +
  scale_x_log10(
    name     = "Body mass (kg)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_y_log10(
    name     = "Cost of transport (kcal / kg / km)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_colour_manual(values = medium_colours, name = "Medium") +
  labs(
    title    = "How Animals Move: Cost of Transport from Bacteria to Blue Whales",
    subtitle = wilson_subtitle,
    caption  = "Code and data: https://github.com/cavedave/bicycle"
  ) +
  tufte_base() +
  theme(
    plot.title    = element_text(
                     size = 20, face = "plain", colour = "grey10",
                     margin = margin(b = 5)
                   ),
    plot.subtitle = element_text(
                     size = 12, colour = "grey40", face = "italic",
                     lineheight = 1.2, margin = margin(b = 10)
                   ),
    legend.title  = element_text(size = 11, face = "italic", colour = "grey35"),
    legend.text   = element_text(size = 10.5, colour = "grey30")
  )

ggsave(file.path(plots_dir, "animal_efficiency.png"), p_animal,
       width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)

# ── Animal panel, redacted: same omissions as full_redact (biology only) ────
df_bio_redact <- df_bio[!df_bio$name %in% redact_microbe_mole_names, ]

p_animal_redact <- ggplot(df_bio_redact, aes(x = weight_kg, y = cost_of_transport_kcal_per_kg_km,
                                              colour = medium)) +
  geom_point(size = panel_pt_size, alpha = 0.88, shape = 16) +
  geom_text_repel(
    aes(label = label),
    size               = panel_lbl_size,
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.35,
    min.segment.length = 0.15,
    box.padding        = 0.35,
    point.padding      = 0.28,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    seed               = 44
  ) +
  scale_x_log10(
    name     = "Body mass (kg)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_y_log10(
    name     = "Cost of transport (kcal / kg / km)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_colour_manual(values = medium_colours, name = "Medium") +
  labs(
    title    = "How Animals Move: Cost of Transport from Fruit Fly to Blue Whale",
    subtitle = wilson_subtitle,
    caption  = "Code and data: https://github.com/cavedave/bicycle"
  ) +
  tufte_base() +
  theme(
    plot.title    = element_text(
                     size = 21, face = "plain", colour = "grey10",
                     margin = margin(b = 5)
                   ),
    plot.subtitle = element_text(
                     size = 13, colour = "grey40", face = "italic",
                     lineheight = 1.2, margin = margin(b = 10)
                   ),
    legend.title  = element_text(size = 11, face = "italic", colour = "grey35"),
    legend.text   = element_text(size = 10.5, colour = "grey30")
  )

ggsave(file.path(plots_dir, "animal_redact.png"), p_animal_redact,
       width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)

# ═══════════════════════════════════════════════════════════════════════════
# Vehicle-only — machines (land, sea, air); human-powered counted as vehicle
# ═══════════════════════════════════════════════════════════════════════════
# Fewer points than the animal panel → room for larger glyphs + labels
vehicle_pt_size  <- 4.8
vehicle_lbl_size <- 5.4

df_veh <- df[df$origin == "vehicle", ]

p_vehicle <- ggplot(df_veh, aes(x = weight_kg, y = cost_of_transport_kcal_per_kg_km,
                                colour = medium)) +
  geom_point(size = vehicle_pt_size, alpha = 0.88, shape = 17, stroke = 0.35) +
  geom_text_repel(
    aes(label = label),
    size               = vehicle_lbl_size,
    colour             = "grey20",
    # No leader lines — Tufte-style direct labelling; pull labels onto their points
    segment.size       = 0,
    segment.color      = NA,
    box.padding        = 0.22,
    point.padding      = 0.18,
    force                = 0.35,
    force_pull           = 1.8,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    seed               = 43
  ) +
  scale_x_log10(
    name     = "Vehicle mass (kg)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_y_log10(
    name     = "Cost of transport (kcal / kg / km)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_colour_manual(values = medium_colours, name = "Medium") +
  labs(
    title    = "Cost of Transport from Model Aircraft to Oil Tankers",
    subtitle = wilson_subtitle,
    caption  = "Code and data: https://github.com/cavedave/bicycle"
  ) +
  tufte_base() +
  theme(
    plot.title    = element_text(
                     size = 20, face = "plain", colour = "grey10",
                     margin = margin(b = 5)
                   ),
    plot.subtitle = element_text(
                     size = 12, colour = "grey40", face = "italic",
                     lineheight = 1.2, margin = margin(b = 10)
                   ),
    legend.title  = element_text(size = 12, face = "italic", colour = "grey35"),
    legend.text   = element_text(size = 11.5, colour = "grey30")
  )

ggsave(file.path(plots_dir, "vehicle_efficiency.png"), p_vehicle,
       width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)

# ═══════════════════════════════════════════════════════════════════════════
# People-powered only — Homo sapiens (on foot, swimming, human-powered craft)
# Excludes passive glide / soar modes (paraglider, hang glider): their CoT in
# the table is dominated by altitude loss and external lift (thermals, ridge),
# not sustained muscle power like cycling, rowing, or Daedalus.
# ═══════════════════════════════════════════════════════════════════════════
people_pt_size  <- 4.5
people_lbl_size <- 5.2

people_exclude_passive_glide <- c(
  "Paraglider (EN-C class)",
  "Hang glider (rigid wing)"
)

df_people <- df[
  !is.na(df$scientific_name) & df$scientific_name == "Homo sapiens" &
    !df$name %in% people_exclude_passive_glide,
]

# Shorter repel text on this panel only (whole plot is human; drop redundant words)
people_short_label <- c(
  "Man (walking)"    = "Walking",
  "Human swimming"   = "Swimming",
  "Human running"    = "Running"
)
df_people$people_label <- df_people$label
m_pl <- match(df_people$display_name, names(people_short_label))
ok_pl <- !is.na(m_pl)
df_people$people_label[ok_pl] <- people_short_label[df_people$display_name[ok_pl]]

# Touring bicycle label → "Bicycle" (plain, same as other labels); racing excluded via exemplar=0
df_people$people_label[df_people$name == "Human on bicycle (touring)"] <- "Bicycle"

p_people <- ggplot(df_people, aes(x = weight_kg, y = cost_of_transport_kcal_per_kg_km,
                                  colour = medium, shape = origin)) +
  geom_point(size = people_pt_size, alpha = 0.88, stroke = 0.35) +
  geom_text_repel(
    aes(label = people_label),
    size               = people_lbl_size,
    colour             = "grey20",
    segment.size       = 0,
    segment.color      = NA,
    box.padding        = 0.22,
    point.padding      = 0.18,
    force              = 0.35,
    force_pull         = 1.8,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    seed               = 41
  ) +
  scale_x_log10(
    name     = "Body / equipment mass (kg)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.12))
  ) +
  scale_y_log10(
    name     = "Cost of transport (kcal / kg / km)",
    labels   = scales::label_log(base = 10),
    expand   = expansion(mult = c(0.04, 0.08))
  ) +
  scale_colour_manual(values = medium_colours, name = "Medium") +
  scale_shape_manual(values = origin_shapes, name = "Origin") +
  labs(
    title    = "Human-Powered Locomotion: Cost of Transport on Land, Water, and in the Air",
    subtitle = wilson_subtitle,
    caption  = paste0(
      "Daedalus 88: plotted 102 kg = 70 kg reference + 32 kg airframe (record flight 104 kg with 72 kg pilot; Wikipedia \u201cMIT Daedalus\u201d); ",
      "log x-axis places the point near 10\u00b2 kg, not 10\u00b3. ",
      "Code and data: https://github.com/cavedave/bicycle"
    )
  ) +
  tufte_base() +
  theme(
    plot.title    = element_text(
                     size = 20, face = "plain", colour = "grey10",
                     margin = margin(b = 5)
                   ),
    plot.subtitle = element_text(
                     size = 12, colour = "grey40", face = "italic",
                     lineheight = 1.2, margin = margin(b = 10)
                   ),
    plot.caption  = element_text(
                     hjust = 1, size = 7, colour = "grey45",
                     face = "italic", margin = margin(t = 8), lineheight = 1.15
                   ),
    legend.title  = element_text(size = 12, face = "italic", colour = "grey35"),
    legend.text   = element_text(size = 11.5, colour = "grey30")
  )

ggsave(file.path(plots_dir, "people_efficiency.png"), p_people,
       width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)

# Same people panel with linear x and y (narrow mass range; easier to read offsets)
p_people_linear <- p_people +
  scale_x_continuous(
    name   = "Body / equipment mass (kg)",
    expand = expansion(mult = 0.06)
  ) +
  scale_y_continuous(
    name   = "Cost of transport (kcal / kg / km)",
    expand = expansion(mult = 0.06)
  ) +
  labs(
    title   = paste0(
      "Human-Powered Locomotion: Cost of Transport on Land, Water, and in the Air ",
      "(linear scales)"
    ),
    caption = paste0(
      "Daedalus 88: 102 kg = 70 kg reference + 32 kg airframe (Wikipedia \u201cMIT Daedalus\u201d). ",
      "Code and data: https://github.com/cavedave/bicycle"
    )
  )

ggsave(file.path(plots_dir, "people_efficiency_linear.png"), p_people_linear,
       width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)

cat("Saved PNGs under ", plots_dir, "/\n", sep = "")
