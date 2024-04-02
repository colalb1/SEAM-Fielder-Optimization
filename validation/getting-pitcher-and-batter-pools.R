
# libraries and tools
shiny_seam_helper = function(b, p, br, pr) {
  do_full_seam_matchup(
    .batter = lu_b(b_lu, b),
    .pitcher = lu_p(p_lu, p),
    .bip = bip,
    .batter_pool = batter_pool,
    .pitcher_pool = pitcher_pool,
    .ratio_batter = br,
    .ratio_pitcher = pr
  )
}

library(dplyr)
library(plotrix)
library(data.table)
library(GenSA)
library(ggplot2)
library(ggforce)
library(berryFunctions)

devtools::load_all()

# data
bip = readRDS("data/bip.Rds")
b_lu = data.frame(readRDS("data/b-lu.Rds"))
p_lu = data.frame(readRDS("data/p-lu.Rds"))
batter_pool = readRDS("data/batter-pool.Rds")
pitcher_pool = readRDS("data/pitcher-pool.Rds")
mlb_teams = readRDS("data/mlb-teams.Rds")
stadiums = readRDS("data/stadiums.Rds")
stadium_paths = readRDS("data/stadium-paths.Rds")

sCDF = data.frame(readRDS("stadiumCoordinates.rds")) # this is the file containing the stadium coordinates
dense_coordinates = data.frame(readRDS("dense-stadium-coordinates.rds"))
statcast_raw = data.table(readRDS("2022-full-validation.rds"))

# helper function and value
euclidean = function(a, b) {sqrt(sum((a - b) ^ 2))}
converter = 36.11364 / 90


# I took this from the SEAM repo: https://github.com/ecklab/seam/blob/master/R/similarity.R
# Used to calculate the similarity between a pitcher and batter
calc_sim_batter = function(b_study_char, b_pool_char, ratio = 0.85) {
  
  char = c("lf_prc", "cf_prc", "rf_prc", "launch_angle", "launch_speed")
  stuff = which(char %in% c("launch_angle", "launch_speed"))
  not_stuff = which(char %in% c("lf_prc", "cf_prc", "rf_prc"))
  
  char_study = b_study_char %>%
    dplyr::select(dplyr::all_of(char)) %>%
    as.numeric()
  
  char_pool = b_pool_char %>%
    dplyr::select(dplyr::all_of(char)) %>%
    as.matrix()
  
  v_dim = length(char_study)
  v = diag(1, v_dim, v_dim)
  
  for (s in stuff) {
    v[s, s] = ratio * v_dim / length(stuff)
  }
  
  for (s in not_stuff) {
    v[s, s] = (1 - ratio) * v_dim / length(not_stuff)
  }
  
  similarity = apply(char_pool, 1, calc_sim, char_study = char_study, v = v)
  weight = similarity / sum(similarity)
  
  data.frame(
    similarity = similarity,
    weight = weight
  )
}


# list of batter and pitcher names
batter_list = c("Jose Altuve",
               "Nolan Arenado",
               "Lorenzo Cain",
               "Evan Longoria", 
               "Albert Pujols",
               "Mike Trout",
               "Matt Carpenter",
               "Freddie Freeman",
               "Joey Gallo",
               "Dee Strange-Gordon",
               "Bryce Harper",
               "Jason Heyward",
               "Kevin Kiermaier",
               "Anthony Rizzo",
               "Joey Votto",
               "Hunter Renfroe",
               "Trey Mancini",
               "Vladimir Guerrero Jr.",
               "Tony Kemp",
               "Taylor Ward"
              )
pitcher_list = c("Logan Gilbert",
                "Merrill Kelly",
                "Kyle Wright",
                "Jordan Montgomery",
                "Framber Valdez",
                "Robbie Ray")

# initializing data table
similarity_dt = data.table(pitcher = character(),
                            batter = character(),
                            pitcher_hand = character(),
                            batter_hand = character(),
                            similarity = numeric())
similarity_dt = rbind(similarity_dt, list(pitcher = "",
                                    batter = "",
                                    pitcher_hand = "",
                                    batter_hand = "",
                                    similarity = 0))

# iterating through batters
for (batter_name1 in batter_list) {
  # extracting batter information
  temp_batter_frame = statcast_raw[batter_name == batter_name1]
  temp_batter_row = temp_batter_frame[1, ]
  batter_num = temp_batter_row$batter
  batter_handed = temp_batter_row$stand
  
  # iterating through pitchers
  for (pitcher_name1 in pitcher_list) {
    # extracting pitcher information
    temp_pitcher_frame = statcast_raw[pitcher_name == pitcher_name1]
    temp_pitcher_row = temp_pitcher_frame[1, ]
    pitcher_num = temp_pitcher_row$pitcher
    pitcher_handed = temp_pitcher_row$p_throws
    
    # choosing the top 3 most frequent pitches that a pitcher throws
    top_pitches = names(head(sort(table(temp_pitcher_frame$pitch_type), 
                                  decreasing = TRUE), 
                             3))
    similarity_sum = 0
    
    # iterating through top pitches and adding the similarity score for the batter against each pitch.
    for (pitch_types in top_pitches) {
      if (is.na(pitch_types)) {
        break
      }
      b_study_char = batter_pool %>%
        filter(.data$batter == batter_num) %>%
        filter(.data$stand == batter_handed) %>%
        filter(.data$pitch_type == pitch_types) %>%
        filter(.data$game_year == 0)
      
      p_bip = bip %>%
        filter(.data$batter != batter_num) %>%
        filter(.data$pitcher == pitcher_num) %>%
        filter(.data$p_throws == pitcher_handed) %>%
        filter(.data$stand == batter_handed) %>%
        filter(.data$pitch_type == pitch_types) %>%
        select(.data$game_year, .data$batter, .data$x, .data$y)
      
      # potential donor batters
      b_pool_char = batter_pool %>%
        filter(.data$batter %in% unique(p_bip$batter)) %>%
        filter(.data$pitch_type == pitch_types) %>%
        filter(.data$stand == batter_handed) %>%
        filter(.data$game_year != 0)
      
      # calculate similarity and weights for all potential donors
      b_pool_sims = calc_sim_batter(b_study_char = b_study_char,
                                    b_pool_char = b_pool_char,
                                    ratio = 0.85)
      similarity_sum = similarity_sum + sum(b_pool_sims$similarity)
    }
    
    # adding row to full table
    append_row = list(pitcher = pitcher_name1,
                      batter = batter_name1,
                      batter_hand = batter_handed,
                      pitcher_hand = pitcher_handed,
                      similarity = similarity_sum)
    similarity_dt = rbind(similarity_dt,
                       append_row)
  }
}

similarity_dt = similarity_dt[-1, ]
saveRDS(similarity_dt, "similarity-for-comparison.rds")

# Density distribution
plot(density(similarity_dt$similarity),
     xlab = "Similarity",
     ylab = "Density",
     main = "Density vs Similarity in BABIP")
rug(similarity_dt$similarity, col='red')


View(similarity_dt)

