# libraries and tools
library(remotes)
library(dplyr)

remotes::install_deps()
devtools::load_all()

# data from SEAM
bip = readRDS("data/bip.Rds")
b_lu = data.frame(readRDS("data/b-lu.Rds"))
p_lu = data.frame(readRDS("data/p-lu.Rds"))
batter_pool = readRDS("data/batter-pool.Rds")
pitcher_pool = readRDS("data/pitcher-pool.Rds")
mlb_teams = readRDS("data/mlb-teams.Rds")
stadiums = readRDS("data/stadiums.Rds")
stadium_paths = readRDS("data/stadium-paths.Rds")

# taken from SEAM devtools
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

# Data frame of matchup. The matchup doesn't matter, using the (x, y) coordinates from SEAM to figure out which are in fair territory.
matchup_df = shiny_seam_helper(
                  b = "Mike Trout",
                  p = "Justin Verlander",
                  br = .85,
                  pr = .85)

# helper function
euclidean <- function(a, b) sqrt(sum((a - b)^2))



# filters each field based on shape
filterToField = function(stadium) {
  
  # getting stadium paths
  inPlayFilter = function() {
    
    # astros field was rotated, others fine
    if (stadium != "astros") {
      tryFilter = stadium_paths %>%
        filter(team == stadium) %>% 
        filter(segment == "foul_lines") %>% 
        slice(1:25, 75:100)
    } else {
      
      # choosing the foul line as the lines I will filter on
      tryFilter = stadium_paths %>%
        filter(team == stadium) %>% 
        filter(segment == "foul_lines")
    }
    
    # some work to make sure I choose the correct lines on the field
    minimumFoulY = tryFilter[which.min(tryFilter$y), ]$y
    
    tryFilter1 = stadium_paths %>%
      filter(team == stadium) %>% 
      filter(segment == "outfield_outer") %>%
      filter(y <= minimumFoulY)
    
    # data rotation
    allFiltered = rbind(tryFilter, tryFilter1)
    allFiltered = allFiltered %>%
      mutate(x = x - 125.42,
             y = 198.27 - y)
    
    return(allFiltered)
  }
  
  tempDat = inPlayFilter()
  
  # filter by checking if there is a point to the left, right, above, and below
  # indices of coordinates that I will take out, aka coordinates outside of the coordinate lines
  takeOut = c()
  
  # iterating through coordinates to check if they are within some radius of seam distribution. If not, the coordinate will be removed
  for (i in 1:nrow(matchup_df$seam_df)) {
    left = NULL
    right = NULL
    above = NULL
    below = NULL
    
    #left
    for (j in 1:nrow(tempDat)) {
      if (tempDat$x[j] <= matchup_df$seam_df$x[i] & euclidean(tempDat$y[j], matchup_df$seam_df$y[i]) < 4.5) {
        left = tempDat$x[j]
        break
      }
    }
    
    #right
    for (j in 1:nrow(tempDat)) {
      if (tempDat$x[j] >= matchup_df$seam_df$x[i] & euclidean(tempDat$y[j], matchup_df$seam_df$y[i]) < 4.5) {
        right = tempDat$x[j]
        break
      }
    }
    
    #below
    for (j in 1:nrow(tempDat)) {
      if (tempDat$y[j] <= matchup_df$seam_df$y[i] & euclidean(tempDat$x[j], matchup_df$seam_df$x[i]) < 3) {
        below = tempDat$y[j]
        break
      }
    }
    
    #above
    for (j in 1:nrow(tempDat)) {
      if (tempDat$y[j] >= matchup_df$seam_df$y[i] & euclidean(tempDat$x[j], matchup_df$seam_df$x[i]) < 3) {
        above = tempDat$y[j]
        break
      }
    }
    
    if (length(c(left, right, above, below)) < 4) {
      takeOut = append(takeOut, i)
    }
  }
  
  # removing coordinates outside of fair territory
  matchup_dfSeamDF = matchup_df$seam_df[-takeOut, ]
  return(matchup_dfSeamDF)
  
}



# doing this for all fields
stadiumCoordinates = data.frame(matrix(nrow = length(stadiums), ncol=3))
colnames(stadiumCoordinates) = c("stadiumName", "xCoords", "yCoords")


for (i in 1:length(stadiums)) {
  stadiumCoordinates$stadiumName[i] = stadiums[i]
  setData = filterToField(stadiums[i])
  stadiumCoordinates$xCoords[i] = vector(mode = "double", length = length(setData$x))
  stadiumCoordinates$yCoords[i] = vector(mode = "double", length = length(setData$y))
  stadiumCoordinates$xCoords[i] = I(list(setData$x))
  stadiumCoordinates$yCoords[i] = I(list(setData$y))
}


saveRDS(stadiumCoordinates, "stadiumCoordinates.rds")
