
#Initialization
library(raylibr)

### Custom Functions


blend_colors <- function(c1, c2, p = 0.5) {
  p <- min(1, max(0, p))
  if (is.null(c1)) c1 <- "black"
  if (is.null(c2)) c2 <- "black"
  c1 <- as_color(c1)
  c2 <- as_color(c2)
  r <- (c1$r * p) + (c2$r * (1-p))
  g <- (c1$g * p) + (c2$g * (1-p))
  b <- (c1$b * p) + (c2$b * (1-p))
  color(r, g, b, c1$a)
}

get_tile <- function(x, y) {
  tile_grid[floor(y / 64) * tile_dims[1] + floor(x / 64) + 1]
}

get_diff <- function(pos) {
  diff <- pos - 64 * floor(pos / 64)
  over_half <- diff > (tile_size / 2)
  diff[over_half] <- 64 - diff[over_half]
  diff
}


### Open Window
init_window(1000, 600, "DOOM")


### Window Settings
window_size <- c(1024, 512) #sets window size
window_fps <- 100 # sets initial fps
debug_mode <- TRUE #Sets window into debug mode for testing, only turn on when changing code

viewport_size <- c(1000, 600) #size of the players view window
viewport_position <- c(512, 64) #position of the viewport in the window
viewport_resolution <- 128 #Viewport resultion
viewport_fov <- 90 #we run in QUAKEPRO BITCH
viewport_dof <- 8 #set depth-of-field
viewport_fog <- 0.175 #set amount of fog

player_x <- 2.5 #players initial position in the x direction
player_y <- 2.5 #players initial position in the y direction
player_position <- c(player_x, player_y) #adds them together to make it easier to use later
player_angle <- 0 #sets angle KEEP AT 0 or things will go to SSSHHHIIITTT

player_walkspeed <- 0.03 #walkspeed of player
player_strafespeed <- 0.009 #strafespeed, side to side movement (keep lower than walkspeed)


map_dims <- c(16, 16) #variable set for map dimensions
map_size <- 128 #variable set for map size
#the map layout is in the other section

color_fog <- blend_colors("grey", "black", 0.5) #uses the blend_colors custom function to blend them 
color_floor <- blend_colors("darkgrey", "darkorange", 0.1) #blends colors 
color_ceiling <- blend_colors("lightgrey", "black", 0.75) #blends colors
color_wall <- blend_colors("chocolate4", "darkslategrey", 0.3) #blends colors

# Enemy position and patrol bounds
enemy_pos <- c(3.5, 3.5)
enemy_patrol_a <- c(2.0, 3.5)
enemy_patrol_b <- c(5.0, 3.5)
enemy_speed <- 0.015
enemy_dir <- 1  # 1 = moving toward B, -1 = moving toward A

### Constants
frame <- 0
fps <- 0
two_pi <- pi*2
half_pi <- pi/2

help_lines <- c(
  sprintf("W: Forward"),
  sprintf("S: Backward         Enter: Toggle Fullscreen"),
  sprintf("A: Strafe Left      TAB: Toggle Debug"),
  sprintf("D: Strafe Right     Mouse: Move Camera       ESCAPE: EXIT")
)


### Map Layout
map <- matrix(c(
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
  1,0,0,2,2,2,0,0,0,0,0,0,3,3,0,1,
  1,0,0,2,0,0,0,0,0,0,0,0,3,0,0,1,
  1,0,0,2,0,0,4,4,4,0,0,0,3,0,0,1,
  1,0,0,0,0,0,4,0,0,0,0,0,3,0,0,1,
  1,0,0,0,0,0,4,0,0,0,0,0,0,0,0,1,
  1,0,0,0,0,0,4,4,4,0,0,0,0,0,0,1,
  1,0,0,0,0,0,0,0,0,0,0,5,5,5,0,1,
  1,0,0,0,0,0,0,0,0,0,0,5,0,0,0,1,
  1,0,0,0,0,0,0,0,0,0,0,5,0,6,6,1,
  1,0,0,0,0,0,0,0,0,0,0,0,0,6,0,1,
  1,0,0,0,0,0,0,0,0,0,0,0,0,6,0,1,
  1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
  1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
), nrow = 16, byrow = TRUE)

#Map tile colors
tile_colors <- list(
  "0" = "black",
  "1" = "grey",
  "2" = "red",
  "3" = "blue",
  "4" = "green",
  "5" = "yellow",
  "6" = "purple"
)





### Actually starts the window
while (!window_should_close()) { #Creates the while loop to check if the window should be closed via excape press or red x press.
  begin_drawing() #Wrap everything into a single frame, one pair per loop iteration.
  clear_background("black") #clears the background to prevent smearing of graphics, also do this once per frame wrap.
  

  
  ### Movement
  
  #Set angles
  player_angle <- (player_angle + get_mouse_delta()[1] / 60) %% two_pi # sets the player angle in relation to the mouse movement
  player_angle_s <- (player_angle + half_pi) %% two_pi #same as above but for strafing
  player_delta <- c(0,0) #sets initial delta so things dont go to shit later
  
  #Keypress movement
  if (is_key_down(key$w)) player_delta <- player_delta + c(cos(player_angle), sin(player_angle)) * player_walkspeed #if w is pressed then take the initial delta and add the cosine and sine of the angle times walkspeed to move player forward at the set walkspeed
  
  if (is_key_down(key$s)) player_delta <- player_delta - c(cos(player_angle), sin(player_angle)) * player_walkspeed# ifsw is pressed then take the initial delta and minus the cosine and sine of the angle times walkspeed to move player backward at the set walkspeed
  
  if (is_key_down(key$d)) player_delta <- player_delta + c(cos(player_angle_s), sin(player_angle_s)) * player_strafespeed #if d is pressed then take the initial delta and add the cosine and sine of the angle_s times walkspeed to move player forward at the set walkspeed
  
  if (is_key_down(key$a)) player_delta <- player_delta - c(cos(player_angle_s), sin(player_angle_s)) * player_strafespeed #if a is pressed then take the initial delta and minus the cosine and sine of the angle_s times walkspeed to move player forward at the set walkspeed
  
  player_position <- player_position + player_delta #creates the new player position
  
  
  ### Colision
  
  #Define player size radius (20 pixels)
  player_radius <- 0.3
  tile_size <- 1
  
  #Helper function
  is_wall <- function(grid_x, grid_y) {
    map[grid_y + 1, grid_x + 1] > 0
  }
  
  #Identify what ROW player is in
  center_grid_y <- floor(player_position[2] / tile_size)
  
  #What grids are touching players Left and Right
  grid_x_left <- floor((player_position[1] - player_radius) / tile_size)
  grid_x_right <- floor((player_position[1] + player_radius) / tile_size)
  
  if (is_wall(grid_x_left, center_grid_y)) {
    player_position[1] <- (grid_x_left + 1) * tile_size + player_radius
    #If hit wall on left side, move player right
  } else if (is_wall(grid_x_right, center_grid_y)) {
    player_position[1] <- (grid_x_right * tile_size) - player_radius
  }
  #If hit wall on right side, move player left
  
  #Revcalc center X 
  center_grid_x <- floor(player_position[1] / tile_size)
  
  #Calc grids on players top and bottom
  grid_y_top <- floor((player_position[2] - player_radius) / tile_size)
  grid_y_bottom <- floor((player_position[2] + player_radius) / tile_size)
  
  if (is_wall(center_grid_x, grid_y_top)) {
    #If player hits wall above, move down
    player_position[2] <- (grid_y_top + 1) * tile_size + player_radius
  } else if (is_wall(center_grid_x, grid_y_bottom)) {
    #IF player hits wall below, move up
    player_position[2] <- (grid_y_bottom * tile_size) - player_radius
  }
  
  
  
  #enemy movement
  if (enemy_dir == 1) {
    enemy_pos[1] <- enemy_pos[1] + enemy_speed
    
    if (enemy_pos[1] >= enemy_patrol_b[1]) enemy_dir <- -1
  } else {
    enemy_pos[1] <- enemy_pos[1] - enemy_speed
    if (enemy_pos[1] <= enemy_patrol_a[1]) enemy_dir <- 1
  }
  
  
  
  
  ### RayCasting
  
  # initialization
  
  #Convert 90 degree FOV into radians, DO NOT CHANGE EVEN IF FOV CHANGES
  fov_rad <- viewport_fov * (pi / 180)
  
  #Creates an array for all 128 rays (number of rays set via viewport_resolution)
  #Seq() = evenly spaced steps from left edge of FOV to right edge of FOV
  angle_offset <- seq(from = -fov_rad / 2, to = fov_rad / 2, length.out = viewport_resolution)
  
  #Add the offsets calced above to player's viewing angle
  ray_angles <- player_angle + angle_offset
  
  #Keep angles constant within 0 to 2*pi range
  ray_angles <- ray_angles %% two_pi
  
  
  
  #Horizontal
  
  #Figure out direction of rays
  #Y value increases as you go DOWN
  #Angles btwn pi and two_pi point up
  is_ray_facing_up <- ray_angles > pi & ray_angles < two_pi
  
  #Identify first horizontal grid line the ray hits
  #Move player position into the nearest grid
  ray_y <- floor(player_position[2])
  
  #If ray is down, itll hit bottom edge so add 1
  #If ray is up, itll hit top edge, so subtract -0.0001 
  ray_y <- ifelse(is_ray_facing_up, ray_y - 0.0001, ray_y + 1)
  
  #Takes first X coord on prev horizontal line
  ray_x <- player_position[1] + (ray_y - player_position[2]) / tan(ray_angles)
  
  #Calc stepping size
  #When stepping to horizontal line either add or subtract 1 to stay on edge
  y_step <- ifelse(is_ray_facing_up, -1, 1)
  
  #Calc how much X changes for 1 step in Y
  x_step <- y_step / tan(ray_angles)
  
  
  #fixing inf
  
  #Calcs tangent for all rays
  ray_tangent <- tan(ray_angles)
  
  #Dont allow tan() to divide by 0 or near 0
  ray_tangent[abs(ray_tangent) < 0.00001] <- 0.00001
  
  #Recalc ray_x and x_step with fixed tangent
  ray_x <- player_position[1] + (ray_y - player_position[2]) / ray_tangent
  x_step <- y_step / ray_tangent
  
  
  ### Horizontal DDA
  
  
  #Starts tracking rays / vectors
  does_it_hit_wall_x <- ray_x #Checks if hitting wall on X axis
  does_it_hit_wall_y <- ray_y #Checks if hitting wall on Y axis
  check_wall_type_on_hit <- rep(0, viewport_resolution) #Checks wall type (from preset wall types previously)
  is_ray_active <- rep(T, viewport_resolution) #Checks to see if the ray is still flying through air
  
  dof <- 0 #Set depth of field so on long hallway it doesnt continue infinitely 
  max_dof <- map_dims[1] #Sets max grids a ray can fly through (16 for 16x16 map)
  
  while (any(is_ray_active) && dof < max_dof) {
    active_rays <- which(is_ray_active) #What rays havent hit wall yet
    for (i in active_rays) {
      #Converting coordinates to R matrix
      grid_x <- floor(does_it_hit_wall_x[i]) + 1
      grid_y <- floor(does_it_hit_wall_y[i]) + 1
      if (grid_x >= 1 && grid_x <= map_dims[1] && grid_y >= 1 && grid_y <= map_dims[2]) {
        tile_type <- map[grid_y, grid_x]
        #Checks if coordinates are within map boundaries
        if (tile_type > 0) {
          check_wall_type_on_hit[i] <- tile_type #Remembers wall hit texture
          is_ray_active[i] <- F #If ray hit wall, stop steping forward
        }
      } else {
        is_ray_active[i] <- F # If ray leaves map boundaries, stop it
      }
    }
    #Steps rays forward, only if they are still active
    does_it_hit_wall_x[is_ray_active] <- does_it_hit_wall_x[is_ray_active] + x_step[is_ray_active]
    does_it_hit_wall_y[is_ray_active] <- does_it_hit_wall_y[is_ray_active] + y_step[is_ray_active]
    dof <- dof + 1
  }
  
  #Pythagorean theorem to get player distance from hit point
  distance_to_wall_horizontal <- sqrt((does_it_hit_wall_x - player_position[1])^2 + (does_it_hit_wall_y - player_position[2])^2)
  
  # #If ray never hits wall,
  distance_to_wall_horizontal[check_wall_type_on_hit == 0] <- Inf
  
  
  
  #Vert
  
  #Determines horizontal direction player is facing
  #Angles btwn 90 and 270 are LEFT else is RIGHT
  is_ray_facing_left <- ray_angles > half_pi & ray_angles < (3 * half_pi)
  
  #Finds first vert grid line (x axis) the ray intersects
  first_vert_x <- floor(player_position[1])
  #If ray points left, subtract 0.0001 to be just inside the tile
  #If ray points right, add 1 to go to next grid to thr right
  first_vert_x <- ifelse(is_ray_facing_left, first_vert_x - 0.0001, first_vert_x + 1)
  
  #Find Y coordinate on first vert line
  first_vert_y <- player_position[2] + (first_vert_x - player_position[1]) * tan(ray_angles)
  
  #Calc step incraments for later gruds
  #X changes by -1 when moving left and +1 when moving right
  x_step_vert <- ifelse(is_ray_facing_left, -1, 1)
  
  #Y Changes proportially based on tangent of ray angle (idk why tbh)
  y_step_vert <- x_step_vert * tan(ray_angles)
  
  
  # Vert DDA 
  
  
  #Sets initial starting positions
  vert_check_x <- first_vert_x
  vert_check_y <- first_vert_y
  
  #wall types and hitting
  vert_wall_type_on_hit <- rep(0, viewport_resolution)
  is_vert_ray_active <- rep(T, viewport_resolution)
  
  #Reset depth limit
  dof_vert <- 0
  max_dof_vert <- map_dims[2] #max cols a ray can go through is 16
  
  while (any(is_vert_ray_active) && dof_vert < max_dof_vert) {
    active_vert_rays <- which(is_vert_ray_active)
    for (i in active_vert_rays) {
      grid_x <- floor(vert_check_x[i] + 1) #whichever rays are active on x axis add 1 to go to next grid
      grid_y <- floor(vert_check_y[i] + 1) #same as above for y
      
      if (grid_x >= 1 && grid_x <= map_dims[1] && grid_y >= 1 && grid_y <= map_dims[2]) {
        tile_type <- map[grid_y, grid_x]
        
        if (tile_type > 0) {
          vert_wall_type_on_hit[i] <- tile_type
          is_vert_ray_active[i] <- F
        }
      } else {
        is_vert_ray_active[i] <- F
      }
    }  
    
    #steps rays to next gridline
    vert_check_x[is_vert_ray_active] <- vert_check_x[is_vert_ray_active] + x_step_vert[is_vert_ray_active]
    vert_check_y[is_vert_ray_active] <- vert_check_y[is_vert_ray_active] + y_step_vert[is_vert_ray_active]
    dof_vert <- dof_vert + 1
    
  }
  
  #Calc straight line distance to hit point on wall
  distance_to_wall_vert <- sqrt((vert_check_x - player_position[1])^2 + (vert_check_y - player_position[2])^2)
  
  #if a ray misses everything then set to Inf (ngl idk why exactly)
  distance_to_wall_vert[vert_wall_type_on_hit == 0] <- Inf
  
  
  # Wall hit
  
  is_vert_hit <- distance_to_wall_vert < distance_to_wall_horizontal #returns logical if vert is hit
  
  shortest_distance_to_wall <- ifelse( #takes the shortest distance for each ray
    is_vert_hit,
    distance_to_wall_vert,
    distance_to_wall_horizontal
  )
  
  #takes wall type 
  wall_closest_hit <- ifelse(
    is_vert_hit,
    vert_wall_type_on_hit,
    check_wall_type_on_hit
  )
  
  
  
  # Distortion
  
  #Normalizing the angle differences to fit within 0 to 2*pi
  angle_diff <- ray_angles - player_angle
  angle_diff <- atan2(sin(angle_diff), cos(angle_diff))
  
  #Correct distance using cosine of ray angle
  corr_dist <- shortest_distance_to_wall * cos(angle_diff)
  
  #stops R trying to divide by 0
  corr_dist <- pmax(corr_dist, 0.0001)
  
  screen_height <- viewport_size[2] #takes hight of screen
  screen_width <- viewport_size[1] #not important now but will be later
  line_height <- screen_height / corr_dist #calcs the hight of the line based on the height of screen
  
  #center lines on screen
  half_screen <- screen_height / 2
  half_line <- line_height / 2
  
  line_start <- half_screen - half_line
  line_end <- half_screen + half_line
  
  #Prevent lines from going beyond viewport limits
  line_start_clipped <- pmax(1, line_start)
  line_end_clipped <- pmin(screen_height, line_end)
  
  
  
  # wall rendering
  num_rays <- length(ray_angles)
  strip_width <- screen_width / num_rays
  
  
  draw_rectangle(0, 0, screen_width, screen_height / 2, "darkgrey")
  draw_rectangle(0, screen_height / 2, screen_width, screen_height / 2, "grey")
  
  for (i in 1:num_rays) {
    x_position <- (i - 1) * strip_width
    
    base_color <- tile_colors[[as.character(wall_closest_hit[i])]]
    
    if (is_vert_hit[i]) {
      base_color <- color_tint(base_color, "grey")
    }
    
    draw_height <- line_end_clipped[i] - line_start_clipped[i]
    
    draw_rectangle(
      as.integer(x_position),
      as.integer(line_start_clipped[i]),
      as.integer(ceiling(strip_width)),
      as.integer(draw_height),
      base_color
    )
  }
  
  
  
  ### Enemy rendering
  
  dx <- enemy_pos[1] - player_position[1]
  dy <- enemy_pos[2] - player_position[2]
  
  angle_to_enemy <- atan2(dy, dx)
  angle_diff <- angle_to_enemy - player_angle
  
  angle_diff <- atan2(sin(angle_diff), cos(angle_diff))
  
  if (abs(angle_diff) < (fov_rad / 2 + 0.2)) {
    raw_dist <- sqrt(dx^2 + dy^2)
    enemy_to_player_distance <- raw_dist * cos(angle_diff)
    
    if (enemy_to_player_distance > 0.2) {
      enemy_height <- screen_height / enemy_to_player_distance
      enemy_width <- enemy_height
      
      screen_center <- (angle_diff / fov_rad + 0.5) * screen_width
      
      enemy_x_start <- screen_center - (enemy_width / 2)
      enemy_x_end <- screen_center + (enemy_width / 2)
      enemy_y_start <- (screen_height / 2) - (enemy_height / 2)
      
      ray_start_match <- max(1, floor(enemy_x_start / strip_width))
      ray_end_match <- min(num_rays, ceiling(enemy_x_end / strip_width))
      
      if (ray_start_match <= ray_end_match) {
        for (r in ray_start_match:ray_end_match) {
          if (enemy_to_player_distance < corr_dist[r]) {
            slice_x <- (r - 1) * strip_width
            
            draw_rectangle(
              as.integer(slice_x),
              as.integer(pmax(1, enemy_y_start)),
              as.integer(ceiling(strip_width)),
              as.integer(pmin(screen_height, enemy_height)),
              "pink"
            )
          }
        }
      }
    }
  }
  
  
  #debug enemy
  draw_text(sprintf("Enemy X: %.2f | Dir: %d", enemy_pos[1], enemy_dir), 10, 30, 20, "yellow")
  
  
  end_drawing() #ends the wrap from begin_drawing()
}
close_window()



