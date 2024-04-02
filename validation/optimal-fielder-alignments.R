############################################### 
############################################### 
############################################### 
############################################### 

# NOTE TO READER: The maximization method is the same as in placement-validation.Rmd, see comments on the method in said file.
#                 I will comment on the code that differs from said file. This file could've been folded into placement-validation,
#                 but I did not need to generate the plots when I wrote placement-validation.

############################################### 
############################################### 
############################################### 
############################################### 

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

sCDF = data.frame(readRDS("stadiumCoordinates.rds")) #this is the file containing the stadium coordinates
dense_coordinates = data.frame(readRDS("dense-stadium-coordinates.rds"))

# helper function and value
euclidean = function(a, b) {sqrt(sum((a - b) ^ 2))}
converter = 36.11364 / 90

# plotting fielder positions
plot_the_sim = function(par, title, seam_ex_in_stadium) {
  x_s = c(par[1], par[3], par[5], par[7], par[9], par[11], par[13])
  y_s = c(par[2], par[4], par[6], par[8], par[10], par[12], par[14])
  
  ggplot(seam_ex_in_stadium, aes(x = x, y = y, color = z)) + 
    geom_point(size = 4) +
    scale_color_gradientn(colours = rev(rainbow(8))) +
    geom_point(data = data.frame(x_s = x_s, y_s = y_s), aes(x = x_s, y = y_s), color = "black", shape = 18, size = 5) +
    geom_point(data = data.frame(x_s = c(0, 0), y_s = c(0, 36.11364 / 90 * 60.5)), aes(x = c(0, 0), y = c(0, 36.11364 / 90 * 60.5)), color = "black", shape = 18, size = 5) + 
    geom_point(data = data.frame(x_s = c(-36.11364 / sqrt(2), 0,  36.11364 / sqrt(2)), 
                                 y_s = c(36.11364 / sqrt(2), 36.11364 * sqrt(2), 36.11364 / sqrt(2))), 
               aes(x = c(-36.11364 / sqrt(2), 0,  36.11364 / sqrt(2)), y = c(36.11364 / sqrt(2), 36.11364 * sqrt(2), 36.11364 / sqrt(2))), 
               color = "white", shape = 15, size = 6) + 
    xlab("X-vals") + ylab("Y-vals") + labs(title = title)
}

# barrier contraints
first_baseman_furthest = euclidean(c(38.52792, 124.159574), c(90/sqrt(2), 90/sqrt(2))) * 24.27639 / 60.5 / 2
first_base_coords = c(25.5362, 25.5362)
constant_range = 45

f_range_func = function(euclid) {
  f_range_data = data.table(x = c(0, 140 * converter, 250 * converter),
                            y = c(10 * converter, 30 * converter, 50 * converter)) #90
  f_range_parabola = lm(y ~ poly(x, 2, raw = TRUE), data = f_range_data)
  return(ifelse(euclid > 250 * converter,
                50 * converter, #90
                predict(f_range_parabola, newdata = data.table(x = euclid))))
}


sum_in_ellipse = function(par, index, filter_field) {
  
  x_j = par[index] # player x
  y_j = par[index + 1] # player y
  euclid = euclidean(c(x_j, y_j), c(0, 0))
  rotation_angle = -atan(y_j / x_j) - pi / 2
  
  if (rotation_angle == 0) {
    rotated_x_j = x_j
    rotated_y_j = y_j
  } else {
    rotated_x_j = x_j * cos(rotation_angle) - y_j * sin(rotation_angle) #rotate point
    rotated_y_j = y_j * cos(rotation_angle) + x_j * sin(rotation_angle) #rotate point
  }
  
  f_range = f_range_func(euclid)
  
  x_range = f_range; y_range = f_range * 0.85
  
  temp_set = as.data.table(filter_field)
  temp_set[,  ":=" (rotated_x = x * cos(rotation_angle) - y * sin(rotation_angle),
                    rotated_y = y * cos(rotation_angle) + x * sin(rotation_angle))] #computing distance to fielder
  temp_set[,  ":=" (in_ellipse = sqrt((rotated_x - rotated_x_j) ^ 2 / (x_range ^ 2) + (rotated_y - rotated_y_j) ^ 2 / (y_range ^ 2)) <= 1)]
  temp_set = temp_set[in_ellipse == TRUE, ]
  
  area_sum = sum(temp_set$z)
  
  barrier = function(player_x, player_y) {
    distance_from_first = euclidean(first_base_coords, c(player_x, player_y))
    
    if (distance_from_first > 40 * converter) {
      return(-Inf)
    }
    return(area_sum)
  }
  if (index == 13) {
    area_sum = barrier(x_j, y_j)
  }
  return(area_sum)
}


# filter field helper function
filter_field_value = function(field_params, seam_ex_in_stadium) {
  
  final_field = as.data.table(seam_ex_in_stadium)
  for (k in seq(1, length(field_params), 2)) {
    x_j = field_params[k]
    y_j = field_params[k + 1]
    
    rotation_angle = -atan(y_j / x_j) - pi / 2
    
    if (rotation_angle == 0) {
      rotated_x_j = x_j
      rotated_y_j = y_j
    } else {
      rotated_x_j = x_j * cos(rotation_angle) - y_j * sin(rotation_angle) #rotate point
      rotated_y_j = y_j * cos(rotation_angle) + x_j * sin(rotation_angle) #rotate point
    }
    
    # distance of player from home, used to compute range
    euclid = euclidean(c(x_j, y_j), c(0, 0))
    
    f_range = f_range_func(euclid)
    x_range = f_range; y_range = f_range * 0.85
    
    
    if (rotation_angle == 0) {
      temp_set = as.data.table(final_field)
      temp_set[,  ":=" (rotated_x = x,
                        rotated_y = y)] # computing distance to fielder
      temp_set[,  ":=" (in_ellipse = sqrt((rotated_x - rotated_x_j) ^ 2 / (x_range ^ 2) + (rotated_y - rotated_y_j) ^ 2 / (y_range ^ 2)) <= 1)]
      temp_set = temp_set[in_ellipse == FALSE, ]
      
    } else {
      temp_set = as.data.table(final_field)
      temp_set[,  ":=" (rotated_x = x * cos(rotation_angle) - y * sin(rotation_angle),
                        rotated_y = y * cos(rotation_angle) + x * sin(rotation_angle))]
      temp_set[,  ":=" (in_ellipse = sqrt((rotated_x - rotated_x_j) ^ 2 / (x_range ^ 2) + (rotated_y - rotated_y_j) ^ 2 / (y_range ^ 2)) <= 1)]
      temp_set = temp_set[in_ellipse == FALSE, ]
      temp_set[,  ":=" (x = rotated_x * cos(-rotation_angle) - rotated_y * sin(-rotation_angle),
                        y = rotated_y * cos(-rotation_angle) + rotated_x * sin(-rotation_angle))]
    }
    final_field = temp_set
  }
  return((sum(seam_ex_in_stadium$z) - sum(final_field$z)) / sum(seam_ex_in_stadium$z))
}


sum_maximization_coverage_function = function(point_iters, SANN_iters, seam_ex_in_stadium) {
  
  convergence_vec = c()
  fielder_parameters = c(0, converter * 321, converter * 297 * cos(117 * pi / 180), converter * 297 * sin(117 * pi / 180), converter * 294 * cos(62 * pi / 180), converter * 294 * sin(62 * pi / 180), converter * 118 * cos(120 * pi / 180), converter * 118 * sin(120 * pi / 180), converter * 148 * cos(101 * pi / 180), converter * 148 * sin(101 * pi / 180), converter * 151 * cos(78 * pi / 180), converter * 151 * sin(78 * pi / 180), converter * 110 * cos(55 * pi / 180), converter * 110 * sin(55 * pi / 180))
  
  for (i in seq(1, length(fielder_parameters) * point_iters, 2)) { # iterating over the points point_iters times
    if (i > length(fielder_parameters)) {
      i = i %% length(fielder_parameters)
    }
    
    
    # setting temporary field
    current_field = as.data.table(seam_ex_in_stadium)
    
    # filtering pitcher and catcher
    current_field = as.data.table(current_field); current_field[,  ":=" (dist_to_fielder = sqrt((x - 0) ^ 2 + (y - 0) ^ 2))]; current_field = current_field[dist_to_fielder >= 36.11364 / 90 * 20, ] #range was 5
    current_field = current_field[, .(x, y, z)]
    
    for (j in seq(1, length(fielder_parameters), 2)) { 
      
      # filtering the temporary field for all parameters besides the player I want to move
      x_j = fielder_parameters[j]
      y_j = fielder_parameters[j + 1]
      
      if (j == i) { # skipping if this is the fielder I am optimizing for
        next
      }
      
      rotation_angle = -atan(y_j / x_j) - pi / 2
      
      if (rotation_angle == 0) {
        rotated_x_j = x_j
        rotated_y_j = y_j
      } else {
        rotated_x_j = x_j * cos(rotation_angle) - y_j * sin(rotation_angle) #rotate point
        rotated_y_j = y_j * cos(rotation_angle) + x_j * sin(rotation_angle) #rotate point
      }
      
      # distance of player from home, used to compute range
      euclid = euclidean(c(x_j, y_j), c(0, 0))
      
      f_range = f_range_func(euclid)
      x_range = f_range; y_range = f_range * 0.85
      
      
      if (rotation_angle == 0) {
        temp_set = as.data.table(current_field)
        temp_set[,  ":=" (rotated_x = x,
                          rotated_y = y)] # computing distance to fielder
        temp_set[,  ":=" (in_ellipse = sqrt((rotated_x - rotated_x_j) ^ 2 / (x_range ^ 2) + (rotated_y - rotated_y_j) ^ 2 / (y_range ^ 2)) <= 1)]
        temp_set = temp_set[in_ellipse == FALSE, ]
        
      } else {
        temp_set = as.data.table(current_field)
        temp_set[,  ":=" (rotated_x = x * cos(rotation_angle) - y * sin(rotation_angle),
                          rotated_y = y * cos(rotation_angle) + x * sin(rotation_angle))]
        temp_set[,  ":=" (in_ellipse = sqrt((rotated_x - rotated_x_j) ^ 2 / (x_range ^ 2) + (rotated_y - rotated_y_j) ^ 2 / (y_range ^ 2)) <= 1)]
        temp_set = temp_set[in_ellipse == FALSE, ]
        temp_set[,  ":=" (x = rotated_x * cos(-rotation_angle) - rotated_y * sin(-rotation_angle),
                          y = rotated_y * cos(-rotation_angle) + rotated_x * sin(-rotation_angle))]
      }
      current_field = temp_set
    }
    
    # optimizing for current fielder
    temp_optim = optim(par = fielder_parameters,
                       index = i,
                       filter_field = current_field,
                       fn = sum_in_ellipse,
                       method = "SANN",
                       control = list(maxit = SANN_iters,
                                      fnscale = -1))
    
    
    # setting the parameters to the new parameters from the output
    fielder_parameters[i] = temp_optim$par[i]
    fielder_parameters[i + 1] = temp_optim$par[i + 1]
    
    final_fielder_parameters = fielder_parameters
    
    # Calculating final coverage value
    temp_coverage_val = filter_field_value(final_fielder_parameters, seam_ex_in_stadium)
    convergence_vec = c(convergence_vec, temp_coverage_val)
    if (length(convergence_vec) > 50) {
      if (convergence_vec[length(convergence_vec)] - convergence_vec[length(convergence_vec) - 25] < 0.0005) {
        return(list(final_fielder_parameters, convergence_vec))
      }
    }
  }
  return(list(final_fielder_parameters, convergence_vec))
}


generate_plot = function(batter_name, pitcher_name) {
  seam_ex = shiny_seam_helper(
    b = batter_name,
    p = pitcher_name,
    br = .85,
    pr = .85)$seam_df
  
  stadium_index = which(sCDF$stadiumName == "white_sox")
  
  stadium_df = data.frame(
    x = sCDF[stadium_index, 2][[1]],
    y = sCDF[stadium_index, 3][[1]]
  )
  
  seam_ex_in_stadium = seam_ex |> 
    filter(paste(x, y) %in% paste(stadium_df$x, stadium_df$y))
  
  #changed to data table for computational speed
  seam_ex_in_stadium = as.data.table(seam_ex_in_stadium)
  seam_ex_in_stadium[,  ":=" (euclid = sqrt(x ^ 2 + y ^ 2))]
  
  
  sim_ann_maximization = sum_maximization_coverage_function(500, 500, seam_ex_in_stadium)
  sim_ann_output = sim_ann_maximization[[1]]
  
  batter_last_name = strsplit(batter_name, " ")[[1]][2]
  pitcher_last_name = strsplit(pitcher_name, " ")[[1]][2]
  file_name = paste(batter_last_name, "vs", pitcher_last_name, sep = " ")
  
  the_plot = plot_the_sim(sim_ann_output, file_name, seam_ex_in_stadium)
  
  return(list(plot = the_plot, file_name = file_name))
}



# Generating plots
batter_list = c("Shohei Ohtani",
                "Mike Trout",
                "Pete Alonso",
                "Joey Votto",
                "Trea Turner",
                "Marcell Ozuna",
                "J. T. Realmuto",
                "Nico Hoerner",
                "Kyle Schwarber",
                "Bo Bichette",
                "Isaac Paredes",
                "Myles Straw",
                "Max Muncy",
                "Brandon Nimmo",
                "Mookie Betts")
pitcher_list = c("Shohei Ohtani",
                 "Shohei Ohtani",
                 "Jordan Hicks",
                 "Clayton Kershaw",
                 "Kyle Hendricks",
                 "Zack Wheeler",
                 "Max Fried",
                 "Charlie Morton",
                 "Logan Webb",
                 "Logan Webb",
                 "Michael Wacha",
                 "Yusei Kikuchi",
                 "Jon Gray",
                 "Rich Hill",
                 "Blake Snell")

for (i in 1:length(batter_list)) {
  matchup = generate_plot(batter_list[i], pitcher_list[i]); new_plot = matchup$plot; file_name = matchup$file_name
  png(filename=paste("~plots/", file_name, ".png", sep = ""), width = 740, height = 530)
  new_plot
  dev.off()
}
