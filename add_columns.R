library(dplyr)

df <- read.csv("tucker_1970_table1_cost_of_transport.csv",
               stringsAsFactors = FALSE, check.names = FALSE)

# ── display_name ─────────────────────────────────────────────────────────────
# Cleaner labels for the plot. Parenthetical detail is preserved in `name`.
display_map <- c(
  # Insects / flies
  "Fruit fly"                          = "Fruit fly",
  "Black-fly"                          = "Black fly",
  "Blow fly"                           = "Blow fly",
  "Horse fly"                          = "Horse fly",
  "Honey bee"                          = "Honey bee",
  "Mosquito"                           = "Mosquito",
  "Desert locust"                      = "Desert locust",
  "Bumblebee"                          = "Bumblebee",
  "Tobacco hawkmoth"                   = "Hawkmoth",
  "Discoid cockroach"                  = "Cockroach",
  "American cockroach"                 = "Cockroach",

  # Birds
  "Budgerigar"                         = "Budgerigar",
  "Hummingbird"                        = "Hummingbird",
  "Laughing Gull"                      = "Laughing gull",
  "Pigeon"                             = "Pigeon",
  "Barn swallow"                       = "Barn swallow",
  "Thrush nightingale"                 = "Thrush nightingale",
  "Common cuckoo"                      = "Common cuckoo",
  "Dark-bellied brent goose"           = "Brent goose",
  "Wandering albatross"                = "Albatross",
  "Andean condor (thermal soaring)"    = "Andean condor",
  "White stork (thermal soaring)"      = "White stork",
  "Adélie penguin (swimming)"          = "Adélie penguin",
  "Emperor penguin (swimming)"         = "Emperor penguin",
  "Emperor penguin (waddling)"         = "Emperor penguin (waddling)",

  # Mammals – land
  "Lemming"                            = "Lemming",
  "Kangaroo rat"                       = "Kangaroo rat",
  "Ground squirrel"                    = "Ground squirrel",
  "Rabbit"                             = "Rabbit",
  "Rat"                                = "Rat",
  "Mouse"                              = "Mouse",
  "Dog"                                = "Dog",
  "Colt"                               = "Colt",
  "Horse"                              = "Horse",
  "Pony"                               = "Pony",
  "Sheep"                              = "Sheep",
  "Cattle"                             = "Cow",
  "Man"                                = "Man (walking)",
  "Lizard"                             = "Lizard",
  "Sidewinder rattlesnake"             = "Sidewinder",
  "Greater spear-nosed bat"            = "Spear-nosed bat",
  "Large fruit bat"                    = "Fruit bat",
  "Naked mole-rat (walking)"           = "Naked mole-rat",
  "Cape mole-rat (burrowing)"          = "Cape mole-rat (digging)",
  "Pocket gopher (burrowing)"          = "Pocket gopher (digging)",
  "Human (crawling)"                   = "Adult crawling",
  "Human (Groucho running)"            = "Groucho running",
  "Human (skipping)"                   = "Skipping",
  "Human (snowshoeing)"                = "Snowshoeing",
  "Red kangaroo (slow hop)"            = "Red kangaroo",
  "Red kangaroo (medium hop)"          = "Red kangaroo",
  "Red kangaroo (fast hop)"            = "Red kangaroo",
  "Chimpanzee (knuckle-walking)"       = "Chimpanzee",
  "Human (running)"                    = "Human running",
  "Ostrich"                            = "Ostrich",
  "Spider monkey (brachiation)"        = "Spider monkey",
  "Domestic cat"                       = "Domestic cat",

  # Amphibians
  "Fowler's toad (slow walk)"          = "Toad (walk)",
  "Fowler's toad (max aerobic hop)"    = "Toad (hop)",

  # Invertebrates
  "Moon jellyfish"                     = "Jellyfish",
  "Banana slug (crawling)"             = "Banana slug",
  "Ghost crab"                         = "Ghost crab",

  # Fish
  "Sockeye salmon"                     = "Sockeye salmon",
  "European eel"                       = "European eel",
  "Pacific bluefin tuna (juvenile)"    = "Bluefin tuna",
  "Yellowfin tuna (juvenile)"          = "Yellowfin tuna",
  "Nurse shark"                        = "Nurse shark",
  "Mako shark"                         = "Mako shark",

  # Marine mammals
  "Bottlenose dolphin"                 = "Dolphin",
  "Killer whale (orca)"                = "Orca",
  "Humpback whale"                     = "Humpback whale",
  "Blue whale"                         = "Blue whale",
  "West Indian manatee"                = "Manatee",

  # Marine reptiles / others
  "Green sea turtle"                   = "Sea turtle",
  "Squid (market squid)"               = "Squid",

  # Seals
  "Harbor seal (adult)"                = "Harbor seal",
  "Harbor seal (yearling)"             = "Harbor seal (young)",

  # Micro-organisms
  "C. elegans (swimming)"              = "C. elegans",
  "E. coli (swimming)"                 = "E. coli",

  # Human locomotion
  "Human on bicycle (touring)"         = "Bicycle (touring)",
  "Human on bicycle (racing)"          = "Bicycle (racing)",
  "Velomobile"                         = "Velomobile",
  "Cross-country skiing (skating)"     = "XC skiing (skate)",
  "Cross-country skiing (classical)"   = "XC skiing",
  "Inline skating"                     = "Inline skating",
  "Human (front crawl)"                = "Human swimming",
  "Ice speed skating"                  = "Speed skating",
  "Rowing (single scull)"              = "Rowing",
  "Kayak (K1)"                         = "Kayak",

  # Aircraft – civil
  "Boeing 787-9 Dreamliner"            = "Boeing 787",
  "Airbus A350-900"                    = "Airbus A350",
  "Grand Commander airplane"           = "Grand Commander",
  "Cherokee airplane"                  = "Cherokee",
  "DC 8 jet transport"                 = "DC-8",
  "DC 9-10 jet transport"              = "DC-9",
  "F105F jet fighter"                  = "F-105 fighter",
  "Concorde"                           = "Concorde",

  # Unpowered / human-powered aircraft
  "Paraglider (EN-C class)"            = "Paraglider",
  "Hang glider (rigid wing)"           = "Hang glider",
  "Daedalus 88 (human-powered)"        = "Daedalus 88",

  # Model aircraft
  "F1D indoor rubber free-flight"      = "F1D rubber model",
  "Pennyplane (indoor rubber)"         = "Pennyplane",
  "Quadcopter drone (consumer)"        = "Quadcopter",
  "Electric RC model airplane"         = "Electric RC plane",

  # Rotorcraft
  "Helicopter"                         = "Helicopter",
  "Sikorsky S62 helicopter"            = "Sikorsky S62",

  # Ground vehicles
  "Cadillac automobile"                = "Cadillac",
  "Volkswagen automobile"              = "VW Beetle",
  "Petrol car (family sedan)"          = "Petrol car",
  "Electric car (BEV)"                 = "Electric car",
  "Motorcycle"                         = "Motorcycle",

  # Rail
  "Diesel freight train (heavy haul)"  = "Freight train",
  "Suburban electric train (S-Bahn)"   = "Suburban train",
  "TGV Réseau (high-speed train)"      = "TGV",
  "Transrapid TR08 maglev"             = "Maglev",
  "Urban tram (electric)"              = "Tram",

  # Ships
  "VLCC crude oil tanker"              = "Oil tanker",
  "Container ship (Triple-E class)"    = "Container ship"
)

df$display_name <- display_map[df$name]
# Fallback: use name if not in map
df$display_name <- ifelse(is.na(df$display_name), df$name, df$display_name)

# ── dubious ──────────────────────────────────────────────────────────────────
# 1 = genuinely uncertain / contested; 0 = well-supported
dubious_names <- c(
  "Common cuckoo",              # migration CoT from limited DLW tracking; route disputed
  "Hang glider (rigid wing)",   # physics-derived, not directly measured; varies hugely with conditions
  "Paraglider (EN-C class)",    # same issue – calculated from L/D ratio, no respirometry
  "Pocket gopher (burrowing)",  # speed estimated; 1000× surface CoT midpoint of 360–3400× range
  "Cape mole-rat (burrowing)",  # burrowing speed not directly measured; estimated from literature
  "E. coli (swimming)",         # gross CoT is dominated by biosynthesis, not locomotion per se
  "Spider monkey (brachiation)", # CoT estimated (+30% allometric walking); no direct VO2 data in Parsons & Taylor 1977; true gibbons (Hylobatidae) never directly measured during brachiation
  "Human (crawling)"            # speed is inferred (not directly reported in source paper); VO2 extrapolated from 254% increase figure in Choi et al. 2024
)

df$dubious <- as.integer(df$name %in% dubious_names)

# ── exemplar ─────────────────────────────────────────────────────────────────
# 0 = not yet selected; 1 = chosen representative point for this species/vehicle
# Set to 0 by default; pick exemplars manually before plotting
if (!"exemplar" %in% names(df)) df$exemplar <- 0L

# ── write back ───────────────────────────────────────────────────────────────
write.csv(df, "tucker_1970_table1_cost_of_transport.csv", row.names = FALSE)

cat("Columns added.\n")
cat("display_name NA check:", sum(is.na(df$display_name)), "\n")
cat("dubious count:", sum(df$dubious), "\n")
cat("Unique display_names:", length(unique(df$display_name)), "\n")
cat("Total rows:", nrow(df), "\n")
