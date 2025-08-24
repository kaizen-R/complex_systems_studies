## A simple Demo of Schelling Segreggation Model
# A World for Schelling Segreggation Model
# library(ggplot2)
require(MBCbook) # for imshow()


## Object makes more sense here
Schelling_Segregation_world <- setRefClass(
  Class = "Schelling_Segregation_world", 
  fields = list(world_matrix = "matrix",code_invisible = "numeric",
                code_empty = "numeric", code_agent = "vector",
                code_ag_1 = "vector", code_ag_2 = "vector"),
  methods = list(
    initialize = function(xdim, ydim) {
      ## Make decision for colors here but also in OTHER methods, CAREFUL!:
      .self$code_invisible <- -1
      .self$code_empty <- 0
      .self$code_agent <- c(1000) ## Agents IDs start at 1001
      
      init_empty_world <- function(xdim, ydim) {
        if(xdim < 3 || ydim < 3) {
          message("Min World side Size is 3, minimum total cells = 9.")
          return(NULL)
        }
        
        t_w <- matrix(rep(.self$code_empty, xdim*ydim), nrow=xdim, byrow=T)
        ## Neighbourhoods are 3+3 squares in this exercise
        t_w[,c(1, 2, ncol(t_w)-1, ncol(t_w))] <- .self$code_invisible
        t_w[c(1, 2, nrow(t_w)-1, nrow(t_w)),] <- .self$code_invisible
        t_w
      }
      
      t_w <- init_empty_world(xdim, ydim)
      
      .self$world_matrix <- t_w
    },
    
    get_world_matrix = function() {
      .self$world_matrix
    },
    
    add_1randomAgent = function(type=1) {
      
      t_w <- .self$world_matrix
      current_empty_cells <- which(t_w == .self$code_empty)
      if(length(current_empty_cells) < 1) { 
        message("Not enough space to add agent.")
      }
      
      t_id <- max(.self$code_agent)+1
      .self$code_agent <- c(.self$code_agent, t_id)
      
      t_w[sample(current_empty_cells, 1)] <- t_id
      
      if(type == 1) .self$code_ag_1 <- c(.self$code_ag_1, t_id)
      if(type == 2) .self$code_ag_2 <- c(.self$code_ag_2, t_id)
      
      .self$world_matrix <- t_w
      return(t_id)
    },
    
    get_agent_pos = function(agent_id) {
      return(which(.self$world_matrix == agent_id))
    },
    
    ## We assume in this model a 5*5 matrix for environment of an agent
    get_agent_env = function(agent_id) {
      t_w <- .self$world_matrix
      agent_pos <- which(t_w == agent_id)
      
      if(length(agent_pos) < 1) { message("No such agent in this world.")}
      agent_vert <- agent_pos+c(-2:2)
      
      agent_square <- c(agent_vert - 2*nrow(t_w),
                        agent_vert - nrow(t_w),
                        agent_vert,
                        agent_vert + nrow(t_w),
                        agent_vert + 2*nrow(t_w))
      
      t_w <- t_w[agent_square]
      
      return(matrix(t_w, ncol = 5))
    },
    
    get_agent_env_value = function(agent_id) {
      t_w <- .self$world_matrix
      agent_pos <- which(t_w == agent_id)
      
      if(length(agent_pos) < 1) { message("No such agent in this world.")}
      agent_vert <- agent_pos+c(-2:2)
      
      agent_square <- c(agent_vert - 2*nrow(t_w),
                        agent_vert - nrow(t_w),
                        agent_vert,
                        agent_vert + nrow(t_w),
                        agent_vert + 2*nrow(t_w))
      
      t_w <- t_w[agent_square]
      t_w <- t_w[-13]
      
      t_w[which(t_w %in% .self$code_ag_1)] <- 1
      t_w[which(t_w %in% .self$code_ag_2)] <- 2

      return(t_w)
    },
    
    get_world_plot2 = function(...) {
      t_cols_vec <- c("white", "green", "red")
      
      temp_world_colors <- .self$world_matrix
      
      temp_world_colors[which(temp_world_colors %in% .self$code_ag_1)] <- 1
      temp_world_colors[which(temp_world_colors %in% .self$code_ag_2)] <- 2
      
      #imshow(temp_world_colors, col=t_cols_vec)
      imageM(temp_world_colors, col = t_cols_vec, ...)
    },
    
    move_agent = function(agent_id) {
      new_cell <- sample(which(.self$world_matrix == .self$code_empty), 1)
      .self$world_matrix[[which(.self$world_matrix == agent_id)]] <- .self$code_empty
      .self$world_matrix[[new_cell]] <- agent_id
    }
  )
)


## inspired by ?image documentation:
tf <- function(m) t(m)[, nrow(m):1]
imageM <- function(m, grid = max(dim(m)) <= 25, asp = (nrow(m)-1)/(ncol(m)-1), ...) {
  image(tf(m), asp=asp, axes = FALSE, ...)
}






world1 <- Schelling_Segregation_world(25, 100)
# world1$get_world_matrix()
n_agents_1 <- 1002
n_agents_2 <- 1002

agents <- list()
for(i in 1:n_agents_1) {
  world1$add_1randomAgent(1)
  agents[[i]] <- list(id = 1000+i,
                   type = 1)
}
for(i in (n_agents_1+1):(n_agents_1+n_agents_2)) {
  world1$add_1randomAgent(2)
  agents[[i]] <- list(id = 1000+i,
                      type = 2)
}

screen_title <- paste("surface:", 21*96, "n_agents:", n_agents_1*2, "r", .7,"g",.7)

world1$get_world_plot2(main=screen_title)

verbose <- T
t_start <- Sys.time()
n_iterations <- 2000
decides_to_change <- rep(0, n_iterations)

Sys.sleep(5)
for(i in 1:n_iterations) {
  
  for(j in sample(1:(n_agents_1+n_agents_2), n_agents_1+n_agents_2, replace = F)) {
    t_agent <- 1000+j
    world1$get_agent_env(t_agent)
    cur_neighbours_vec <- world1$get_agent_env_value(t_agent)
    
    ## Let's take 40% threshold: You want 40% of your neighbors to be of same type as you
    same_type <- agents[[j]]$type
    length_neighbours <- length(which(cur_neighbours_vec %in% c(1,2)))
    if(length_neighbours > 0) {
      this_threshold <- length(which(cur_neighbours_vec == same_type))/length_neighbours
        if((same_type == 1 && this_threshold < .7) || 
           (same_type == 2 && this_threshold < .7)) {
          world1$move_agent(t_agent)
          decides_to_change[i] <- decides_to_change[i] + 1
          
          if(verbose && (j %% 200 == 0)) { 
            Sys.sleep(0.1) ; 
            # dev.off();
            world1$get_world_plot2(main=screen_title) 
          }
        }
    } ## Else, means no neighbours, here do nothing
    
  }
  
  print(decides_to_change[i])
  
  if(i > 5 && decides_to_change[i] == 0)
    break;
  
  Sys.sleep(0.1) ; 
  # dev.off();
  world1$get_world_plot2(main=screen_title) 
}
# plot(1:i, decides_to_change[1:i])
t_end <- Sys.time()
print(t_end - t_start)
world1$get_world_plot2(main=screen_title) 
