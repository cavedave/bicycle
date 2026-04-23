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

# Tufte-style paper (warm off-white); also set ggsave(bg=…) so PNG is not transparent
paper <- "#F7F6F0"

# PNG size for sharing (e.g. Reddit): 200 dpi × 16×11 in → 3200×2200 px — text stays
# sharp after the site re-encodes; under typical image size limits.
out_w_in <- 16
out_h_in <- 11
out_dpi  <- 200

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
p_full <- ggplot(df, aes(x = weight_kg, y = cost_of_transport_kcal_per_kg_km,
                         colour = medium, shape = origin)) +
  geom_point(size = 2.1, alpha = 0.88) +
  geom_text_repel(
    aes(label = label),
    size               = 2.1,
    colour             = "grey20",
    segment.color      = "grey55",
    segment.size       = 0.25,
    min.segment.length = 0.2,
    box.padding        = 0.22,
    point.padding      = 0.18,
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
    title    = "How animals and machines move: cost of transport from Bacteria to Oil Tankers",
    subtitle = "Update of the \u201cBicycles for the mind\u201d plot (Wilson, Scientific American, 1973)",
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
                   )
  )

ggsave("Full_efficiency.png", p_full, width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)
ggsave("plot_quick.png",     p_full, width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)

# ═══════════════════════════════════════════════════════════════════════════
# Animal-only — no vehicles or human-powered machines (bicycles, ships, etc.)
# ═══════════════════════════════════════════════════════════════════════════
# Slightly larger points + labels than the combined plot (fewer points, more room)
panel_pt_size  <- 3.5
panel_lbl_size <- 4.0

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
    title   = "How Animals Move: Cost of Transport from Bacteria to Blue Whales",
    caption = "Code and data: https://github.com/cavedave/bicycle"
  ) +
  tufte_base() +
  theme(
    plot.title    = element_text(
                     size = 20, face = "plain", colour = "grey10",
                     margin = margin(b = 10)
                   ),
    legend.title  = element_text(size = 11, face = "italic", colour = "grey35"),
    legend.text   = element_text(size = 10.5, colour = "grey30")
  )

ggsave("animal_efficiency.png", p_animal, width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)

# ── Animal panel, redacted: microbes + fossorial “mole” entries omitted ─────
redacted_animal_names <- c(
  "E. coli (swimming)",
  "Paramecium caudatum",
  "C. elegans (swimming)",
  "Naked mole-rat (walking)",
  "Cape mole-rat (burrowing)",
  "Pocket gopher (burrowing)"
)
df_bio_redact <- df_bio[!df_bio$name %in% redacted_animal_names, ]

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
    subtitle = "E. coli, Paramecium, C. elegans, and fossorial mammals (naked mole-rat, Cape mole-rat, pocket gopher) omitted.",
    caption  = "Code and data: https://github.com/cavedave/bicycle"
  ) +
  tufte_base() +
  theme(
    plot.title    = element_text(
                     size = 20, face = "plain", colour = "grey10",
                     margin = margin(b = 5)
                   ),
    plot.subtitle = element_text(
                     size = 11, colour = "grey40", face = "italic",
                     lineheight = 1.2, margin = margin(b = 10)
                   ),
    legend.title  = element_text(size = 11, face = "italic", colour = "grey35"),
    legend.text   = element_text(size = 10.5, colour = "grey30")
  )

ggsave("animal_redact.png", p_animal_redact, width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)

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
    title   = "Cost of Transport from Model Aircraft to Oil Tankers",
    caption = "Code and data: https://github.com/cavedave/bicycle"
  ) +
  tufte_base() +
  theme(
    plot.title    = element_text(
                     size = 20, face = "plain", colour = "grey10",
                     margin = margin(b = 10)
                   ),
    legend.title  = element_text(size = 12, face = "italic", colour = "grey35"),
    legend.text   = element_text(size = 11.5, colour = "grey30")
  )

ggsave("vehicle_efficiency.png", p_vehicle, width = out_w_in, height = out_h_in, dpi = out_dpi, bg = paper)

cat("Saved Full_efficiency.png, plot_quick.png, animal_efficiency.png, animal_redact.png, vehicle_efficiency.png\n")
