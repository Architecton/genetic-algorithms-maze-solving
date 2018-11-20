library(plyr)
library(rlist)
library(foreach)
library(doMC)
library(purrr)
library(ggplot2)
library(gsubfn)
library(stringr)
library(crayon)

# 1. Assignment for IS class

#########################################################################################
# Small program used to solve mazes using a simple implementation of genetic algorithms #
#########################################################################################

##  #  #  #  #  #  #  #  ##
#   Author: Jernej Vivod  #  
##  #  #  #  #  #  #  #  ##

####################################################################################################
#         ______      __    __              ____   ______            __             __             #
#        /_  __/___ _/ /_  / /__     ____  / __/  / ____/___  ____  / /____  ____  / /______       #
#         / / / __ `/ __ \/ / _ \   / __ \/ /_   / /   / __ \/ __ \/ __/ _ \/ __ \/ __/ ___/       #
#        / / / /_/ / /_/ / /  __/  / /_/ / __/  / /___/ /_/ / / / / /_/  __/ / / / /_(__  )        #
#       /_/  \__,_/_.___/_/\___/   \____/_/     \____/\____/_/ /_/\__/\___/_/ /_/\__/____/         #
####################################################################################################
#                                                                                                  #
# functions for maze visualization ......................................................... 49    #
# Functions for moving the agent in the maze ............................................... 95    #
# Solution simulations ..................................................................... 145   #
# Genetic algorithm implementation ......................................................... 396   #
# Fitness functions ........................................................................ 597   #
# Examples of mazes and their solutions used for testing ................................... 629   #
# Function for computing path that goes over the most coins (second part of assignment) .... 709   #
# User interface ........................................................................... 770   #
# Computing the results .................................................................... 881   #
# Visualizing the results .................................................................. 907   #
#                                                                                                  #
####################################################################################################


# Suppress warnings on console.
options(warn=-1)

# Register cores.
registerDoMC(detectCores(all.tests = FALSE, logical = TRUE))

# Functions for maze visualization #############################################################################

# Print maze on console.
printMaze <- function(maze, rows, cols) {
	for (x in seq(1, rows)) {
		print(maze[((x-1)*cols +1) : (x*cols)])
	}
}

# Visualize maze and found solution by printing the found path in the maze in red color.
# This function is called only if the exit is found.
printMazeSolVis <- function(maze, rows, cols) {
  for (x in seq(1, rows*cols)) {
    if (x %% cols == 0) {
      if(maze[x] == 'i') {
        cat(red('*'))
      } else if(maze[x] == 'y') {
        cat(yellow('*'))  
      } else if(maze[x] == 'e') {
        cat(green('E'))
      } else if(maze[x] == 's') {
        cat(green('S'))
      } else {
        cat(maze[x]) 
      }
      cat('\n')
    } else {
      if(maze[x] == 'i') {
        cat(red('*'))
      } else if(maze[x] == 'y') {
        cat(yellow('*'))
      } else if(maze[x] == 'e') {
        cat(green('E'))
      } else if(maze[x] == 's') {
        cat(green('S'))
      } else {
        cat(maze[x]) 
      }
    }
  }
}

################################################################################################################



# Functions for moving the agent in the maze ###################################################################

# Functions for applying moves. first element in returned vector signals whether a border was hit.
moveUp <- function(position, rows, cols) {
	newPosition <- position - cols
	if (newPosition < 1) {
		return (c(1, position))
	} else {
		return (c(0, newPosition))
	}
}

moveDown <- function(position, rows, cols) {
	newPosition <- position + cols
	if (newPosition > rows*cols) {
		return (c(1, position))
	} else { 
		return (c(0, (position + cols)))
	}
}

moveLeft <- function(position, rows, cols) {
	newPosition <- position - 1
	if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
		return (c(1, position))
	} else {
		return (c(0, position - 1))
	}
}

moveRight <- function(position, rows, cols) {
	newPosition <- position + 1
	if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
		return (c(1, position))
	} else { 
		return (c(0, position + 1))
	}
}

ind2sub <- function(rows, ind){
	c = ((ind-1) %% rows) + 1
	r = floor((ind-1) / rows) + 1
	return (c(r, c))
}

################################################################################################################




# Solution simulations #########################################################################################

# SimulateSolution: move in maze with specified number of rows and columns according to plan specified in
# solution. Return 1 if solution was passed over and 0 otherwise.
# if trace == True, the function prints the maze and the found solution.
simulateSolutionPath <- function(maze, rows, cols, solution, trace=FALSE) {
	# Starting position linear index.
	pos_start <- ind2sub(rows, match('s', maze))
	# Linear index of exit.
	pos_exit <- ind2sub(rows, match('e', maze))
	# Set penalty value.
	penalty <- 1
	# Set penalty multiplier.
	multiple <- 2
	# Set initial score.
	score <- 0
	found_exit <- FALSE
	# Count steps.
	step_counter <- 0
	currentPosition <- grep('s', maze)
	# Go over moves in solution.
	for (move in solution) {
		oldPosition <- currentPosition
		# Apply moves and penalize collisions.
		if (move == 'U') {
		  # Increment step counter.
		  step_counter <- step_counter + 1
			move_res <- moveUp(currentPosition, rows, cols)
			score <- score - move_res[1]*penalty*multiple
			currentPosition <- move_res[2]
			if(trace) {
			  if(maze[currentPosition] != 'e') {
			    maze[currentPosition] = 'i' 
			  }
			}
		} else if (move == 'D') {
		  # Increment step counter.
		  step_counter <- step_counter + 1
			move_res <- moveDown(currentPosition, rows, cols)
			score <- score - move_res[1]*penalty*multiple
			currentPosition <- move_res[2]
			if(trace) {
			  if(maze[currentPosition] != 'e') {
			    maze[currentPosition] = 'i' 
			  }
			}
		} else if (move == 'L') {
		  # Increment step counter.
		  step_counter <- step_counter + 1
			move_res <- moveLeft(currentPosition, rows, cols)
			score <- score - move_res[1]*penalty*multiple
			currentPosition <- move_res[2]
			if(trace) {
			  if(maze[currentPosition] != 'e') {
			    maze[currentPosition] = 'i' 
			  }
			}
		} else if (move == 'R') {
		  # Increment step counter.
		  step_counter <- step_counter + 1
			move_res <- moveRight(currentPosition, rows, cols)
			score <- score - move_res[1]*penalty*multiple
			currentPosition <- move_res[2];
			if(trace) {
			  if(maze[currentPosition] != 'e') {
			    maze[currentPosition] = 'i' 
			  }
			}
		} else if (move == 'O') {
		  
		} else {
			print('Error: Incorrect solution format')
			return(-1)
		}
		if (maze[currentPosition] == '#') {
			score <- score - penalty*multiple
			currentPosition <- oldPosition
		}
		# If reached exit:
		if (maze[currentPosition] == 'e') {
		  found_exit = TRUE
			# Compute travel score. (distance from start - distance from exit)
		  roof <- (rows + cols)*3
			pos_end <- ind2sub(rows, currentPosition)
			# distances that can be used in the fitness function computation (currently not implementae).
			diff1 <- sum(abs(pos_end-pos_start))
			diff2 <- sum(abs(pos_exit-pos_end))
			diff3 <- sum(abs(pos_exit-pos_start))
			# If tracing solution, print maze and path.
			if(trace) {
			  printMazeSolVis(maze, rows, cols)
			} else {
			  return(list(found_exit, step_counter, roof - diff2 - step_counter*0.1))
			}
		}
	}
	if(trace && !found_exit) {
	  cat('Exit not found.\n')
	}
	# If exit not reached.
	roof <- (rows + cols)*3
	pos_end <- ind2sub(rows, currentPosition)
	# distances that can be used in the fitness function computation.
	diff1 <- sum(abs(pos_end - pos_start))
	diff2 <- sum(abs(pos_exit-pos_end))
	diff3 <- sum(abs(pos_exit-pos_start))
	return(list(found_exit, step_counter, roof - diff2 - step_counter*0.1)) 
}

# SimulateSolution: move in maze with specified number of rows and columns according to plan specified in
# solution. Return 1 if solution was passed over and 0 otherwise.
# if trace == True, the function prints the maze and the found solution.
simulateSolutionCoins <- function(maze, rows, cols, solution, trace=FALSE) {
  maze_tab <- table(maze)
  num_coins <- maze_tab[names(maze_tab)=='c']
  # Starting position linear index.
  pos_start <- ind2sub(rows, match('s', maze))
  # Linear index of exit.
  pos_exit <- ind2sub(rows, match('e', maze))
  # Set penalty value.
  penalty <- 1
  # Set penalty multiplier.
  multiple <- 2
  # Set initial score.
  score <- 0
  coin_counter <- 0
  COIN_BONUS <- 10
  found_exit <- FALSE
  # Count steps.
  step_counter <- 0
  currentPosition <- grep('s', maze)
  # Go over moves in solution.
  for (move in solution) {
    oldPosition <- currentPosition
    # Apply moves and penalize collisions.
    if (move == 'U') {
      # Increment step counter.
      step_counter <- step_counter + 1
      move_res <- moveUp(currentPosition, rows, cols)
      score <- score - move_res[1]*penalty*multiple
      currentPosition <- move_res[2]
      if(trace) {
        if(maze[currentPosition] != 'e' && maze[currentPosition] != 'y') {
          if(maze[currentPosition] == 'c') {
            coin_counter <- coin_counter + 1
            maze[currentPosition] = 'y'
          } else {
            maze[currentPosition] = 'i' 
          }
        }
      }
    } else if (move == 'D') {
      # Increment step counter.
      step_counter <- step_counter + 1
      move_res <- moveDown(currentPosition, rows, cols)
      score <- score - move_res[1]*penalty*multiple
      currentPosition <- move_res[2]
      if(trace) {
        if(maze[currentPosition] != 'e' && maze[currentPosition] != 'y') {
          if(maze[currentPosition] == 'c') {
            coin_counter <- coin_counter + 1
            maze[currentPosition] = 'y'
          } else {
            maze[currentPosition] = 'i' 
          }
        }
      }
    } else if (move == 'L') {
      # Increment step counter.
      step_counter <- step_counter + 1
      move_res <- moveLeft(currentPosition, rows, cols)
      score <- score - move_res[1]*penalty*multiple
      currentPosition <- move_res[2]
      if(trace) {
        if(maze[currentPosition] != 'e' && maze[currentPosition] != 'y') {
          if(maze[currentPosition] == 'c') {
            coin_counter <- coin_counter + 1
            maze[currentPosition] = 'y'
          } else {
            maze[currentPosition] = 'i' 
          }
        }
      }
    } else if (move == 'R') {
      # Increment step counter.
      step_counter <- step_counter + 1
      move_res <- moveRight(currentPosition, rows, cols)
      score <- score - move_res[1]*penalty*multiple
      currentPosition <- move_res[2];
      if(trace) {
        if(maze[currentPosition] != 'e' && maze[currentPosition] != 'y') {
          if(maze[currentPosition] == 'c') {
            coin_counter <- coin_counter + 1
            maze[currentPosition] = 'y'
          } else {
            maze[currentPosition] = 'i' 
          }
        }
      }
    } else if (move == 'O') {
      
    } else {
      print('Error: Incorrect solution format')
      return(-1)
    }
    if (maze[currentPosition] == '#') {
      score <- score - penalty*multiple
      currentPosition <- oldPosition
    }
    if (maze[currentPosition] == 'c' && !trace) {
      coin_counter <- coin_counter + 1
      maze[currentPosition] = ' '
    }
    # If reached exit:
    if (maze[currentPosition] == 'e') {
      found_exit = TRUE
      # Compute travel score. (distance from start - distance from exit)
      roof <- (rows + cols)*3
      pos_end <- ind2sub(rows, currentPosition)
      # distances that can be used in the fitness function computation (currently not implementae).
      diff1 <- sum(abs(pos_end-pos_start))
      diff2 <- sum(abs(pos_exit-pos_end))
      diff3 <- sum(abs(pos_exit-pos_start))
      # If tracing solution, print maze and path.
      if(trace) {
        printMazeSolVis(maze, rows, cols)
        cat(yellow(sprintf("collected %d/%d coin%s (%d points).", coin_counter, num_coins, if (coin_counter == 1) '' else 's', coin_counter*10)))
      } else {
        return(list(found_exit, step_counter, roof - diff2 - step_counter*0.1 - (num_coins - coin_counter)*0.01))
      }
    }
  }
  if(trace && !found_exit) {
    cat('Exit not found.\n')Visualizing the results
    cat(yellow(sprintf("collected %d/%d coin%s (%d points).", coin_counter, num_coins, if (coin_counter == 1) '' else 's', coin_counter*10)))
  }
  # If exit not reached.
  roof <- (rows + cols)*3
  pos_end <- ind2sub(rows, currentPosition)
  # distances that can be used in the fitness function computation.
  diff1 <- sum(abs(pos_end - pos_start))
  diff2 <- sum(abs(pos_exit-pos_end))
  diff3 <- sum(abs(pos_exit-pos_start))
  return(list(found_exit, step_counter, roof - diff2 - step_counter*0.1 - (num_coins - coin_counter)*0.01)) 
}

################################################################################################################




# Genetic algorithm implementation #############################################################################

# crossover: perform crossover between breeder1, breeder2 and return the offspring.
crossover <- function(breeder1, breeder2, min_val, max_val, mutation_prob) {
	# Get chromosomes of agents.
	chr1 <- breeder1$chromosome
	chr2 <- breeder2$chromosome
	# Select crossover points.
	cp1 <- sample(c(seq_along(chr1)), size=1)
	#cp2 <- sample(c(seq_along(chr2)), size=1)
	# Crossover points are at same place on both chromosomes.
	cp2 <- cp1
	# Perform crossover to get offspring
	# Set limit for rightmost part of chromosome (handle special case where crossover on right edge).
	lim1 <- (cp1+1):length(chr1)
	if(length(chr1) == cp1) {
	  lim1 <- NULL
	}
	lim2 <- (cp2+1):length(chr2)
	if(length(chr2) == cp2) {
		lim2 <- NULL
	}
	off1_chr <- c(chr1[1:cp1], chr2[lim2])
	off2_chr <- c(chr2[1:cp2], chr1[lim1])
	
	# Perform mutations by computing mutations vectors and multiplying the offspring chromosomes by it.
	# The unit value for multiplication is selected with probability 1-Pm.
	mut1 <- sample(1:(max_val*10), size=length(off1_chr), replace=TRUE, prob=c(1-mutation_prob, replicate(max_val*10 - 1, mutation_prob)/(max_val*10 -1)))
	mut2 <- sample(1:(max_val*10), size=length(off2_chr), replace=TRUE, prob=c(1-mutation_prob, replicate(max_val*10 - 1, mutation_prob)/(max_val*10 -1)))
	off1_chr <- off1_chr * mut1
	off2_chr <- off2_chr * mut2
	off1_chr[off1_chr > 4] <- (off1_chr[off1_chr > 4] + sample(4, size=1)) %% 5
	off2_chr[off2_chr > 4] <- (off2_chr[off2_chr > 4] + sample(4, size=1)) %% 5
	# With probability of mutation, remove one codon from chromosome and append a new randomly chosen one.
	if(sample(c(1, 0), size=1, prob=c(mutation_prob, 1-mutation_prob)) == 1) {
		place <- sample(length(off1_chr), size=1)
		rem <- off1_chr[place]
		off1_chr <- off1_chr[-place]
		off1_chr <- append(off1_chr, rem)
	}
	if(sample(c(1, 0), size=1, prob=c(mutation_prob, 1-mutation_prob)) == 1) {
		place <- sample(length(off2_chr), size=1)
		rem <- off2_chr[place]
		off2_chr <- off2_chr[-place]
		off2_chr <- append(off2_chr, rem)
	}
	# Return pair of offsprings.
	return(list(list(fitness=0, chromosome=off1_chr), list(fitness=0, chromosome=off2_chr)))
}

# Perform tournament selection on list of agents, perform crossover on selected agents and return offspring.
# Return new list of agents that are the result of tournament selection.
tournament <- function(agents, num_groups, min_val, max_val) {
	# Split agents into groups.
	groups <- split(agents, ceiling(seq_along(agents)/num_groups))

	# Set probability of crossover and probability of mutation.
	Pc = 0.9
	Pm = 0.1
	# Select two breeders from each group. With probability Pc (crossover probability), 
	# replace two worst non-breeders from group with offspring of breeders.
	for (g_idx in 1:length(groups)) {
		# Define group to be group of agents in next group.
		group <- groups[[g_idx]]
		# Sort agents by their fitness.
		group <- group[list.order(group, (fitness))]

		# If crossover...
		if(sample(c(1, 0), size=1, prob=c(Pc, 1-Pc))) {
			# Select best agent with probability P, second best one with probability P*(1-P),
			# third best one with probability P*(1-P)^2 and so on.
			P_best = 0.9;  # Probability of selecting the best agent is 0.8
			prob_sel <- replicate(length(group), P_best)
			mult <- replicate(length(group), 1-P_best)^(0:(length(group)-1))
			prob_sel <- prob_sel * mult
			# Select breeders.
			breeders <- sample(group, size=2, replace=FALSE, prob=prob_sel)
			# Perform crossover to get offspring.
			offspring <- crossover(breeders[[1]], breeders[[2]], min_val, max_val, mutation_prob=Pm)
			# Replace worst two agents in group with offspring.
			group[[length(group) - 1]] = offspring[[1]]
			group[[length(group)]] = offspring[[2]]
			groups[[g_idx]] <- group
		}
	}
	# Join groups into list of agents (flatten) and return the list.
	return(unlist(groups, recursive = FALSE))
}

# geneticAlgorithm: Implementation of a genetic algorithm using tournament selection.
#
# population_size: size of agent population
# fitness_f: fitness function
# params: parameters for the fitness function
# min_len: minimum_length of the chromosome
# max_len: maximum length of the chromosome
# min_val: minimum value in chromosome
# max_val: maximum value in chromosome
# max_run: number of iterations with same maximum fitness value before stopping the algorithm.
# lim_run: maximum number of iterations of the algorithm.
# plot_iterations: produce a plot of max found cost by iteration.
geneticAlgorithm <- function(population_size, fitness_f, params, min_len, max_len, min_val, max_val, max_run, lim_run, plot_iterations=FALSE, verbose=TRUE) {
	# Generate population of agents of specified size and of random length from interval [min_len and max_len].
	# Initialize them with random plan (See above for direction encoding).
  agents <- vector("list", population_size)
	for(k in 1:population_size) {
		#chromosome_size <- sample(min_len:max_len, size=1, replace=TRUE)
		chromosome_size <- max_len
		chromosome <- sample(min_val:max_val, size=chromosome_size, replace=TRUE)
		agents[[k]] <- list(fitness = 0, chromosome = chromosome)
	}
  
  # If plotting iteration fitnesses, preallocate a vector for storing results for iterations and a vector for storing global cost increases.
  if (plot_iterations) {
    iteration_fitness_y <- vector("integer", lim_run)
    best_fitness_y <- vector("integer", lim_run)
  }

	# counters: iterations and iterations sequence with equal best value.
	iter_counter <- 0
	const_counter <- 0
	# Initialize index of agent with maximum fitness.
	max_fitness_all <- -1e10
	max_fitness_chromosome <- NULL
  max_fitness_chromosome_all_len <-0
	# Run genetic algorithm. Break when any of ending conditions evaluate to TRUE.
	repeat {
		# Increment iteration counter.
		iter_counter <- iter_counter + 1

		# Initialize maximum fitness and index of agent with maximum fitness with default values.
		max_fitness <- -1e10
		max_fitness_chromosome_len <- 0
		idx_max_fitness <- -1

		# Compute fitness values.
		for(k in 1:population_size) {
			fitness_nxt <- do.call(fitness_f, append(params, list(agents[[k]]$chromosome)))
			fitness_nxt_val <- fitness_nxt[[1]]
			fitness_nxt_steps <- fitness_nxt[[2]]
			if(fitness_nxt_val > max_fitness) {  # Compare to current max_fitness.
				max_fitness <- fitness_nxt_val
				max_fitness_chromosome_len <- fitness_nxt_steps
				idx_max_fitness <- k
			}
			agents[[k]]$fitness <- fitness_nxt_val
		}
		
		if(verbose) {
			print(sprintf("maximum fitness of iteration = %f", max_fitness))	
		}
		# If greater than maximum fitness value, reset const_counter counter and assign new previous fitness value.
		if(max_fitness > max_fitness_all) {
			const_counter <- 0
			max_fitness_all <- max_fitness
			max_fitness_chromosome <- agents[[idx_max_fitness]]$chromosome  # Save chromosome of current best agent.
			max_fitness_all_chromosome_len <- max_fitness_chromosome_len
		} else {
			const_counter <- const_counter + 1  # If no improvement, increment counter of iterations with no improvement.
		}
		
		# If ploting iteration data, plot results.
		if(plot_iterations) {
		  iteration_fitness_y[iter_counter] <- max_fitness
		  best_fitness_y[iter_counter] <- max_fitness_all
		}

		# If reached maximum number of iterations or if best fitness has not changed for max_run iterations, end.
		if(iter_counter >= lim_run || const_counter >= max_run) {
			break;
		}

		# Select agents for reproduction. Use tournament selection.
		# Replace two worst agents (that were not selected for breeding) in each group with offspring.
		# Num agents in each group.
		num_in_group <- 8;
		agents <- tournament(agents, num_in_group, min_val, max_val)
		if(verbose) {
			print(sprintf("iteration %f: maximum fitness = %f", iter_counter, max_fitness_all))
		}
	}

	# If plotting terations data, plot results.
	if(plot_iterations) {
	  x_data = 1:iter_counter
	  y_data1 = iteration_fitness_y[x_data]
	  y_data2 = best_fitness_y[x_data]
	  res <- data.frame(x_data, y_data1, y_data2)
	  #plt <- ggplot() + geom_line(data=res, aes(x=x_data, y=y_data1), color='red') + geom_line(data=res, aes(x=x_data, y=y_data2), color='blue')
	  plt <- ggplot() + geom_line(data=res, aes(x=x_data, y=y_data1), color="darkgreen")
	  plt <- plt + labs(x = "Generation") + labs(y = "Fitness") + labs(title = "Maximum Fitness in Each Generation")
	  plot(plt)
	}
	
	# Return gene of agent with maximum fitness.
	return (list(max_fitness_all, max_fitness_chromosome, max_fitness_all_chromosome_len))
}

################################################################################################################


# Fitness functions ############################################################################################

# fitness_maze_coins: fitness function for maze solution that finds the shortest path through the maze.
# Directions of movement are encoded as:
# 1 - up
# 2 - down
# 3 - left
# 4 - right
fitness_maze_path <- function(maze, rows, cols, plan_numeric) {
	# Decode numeric encoding of directions and evaluate plan based on success and length.
	plan <- mapvalues(plan_numeric, c(0, 1, 2, 3, 4), c('O', 'U', 'D', 'L', 'R'), warn_missing = FALSE)
	res <- simulateSolutionPath(maze, rows, cols, plan)
	return(c(res[3], res[2]))
}

# fitness_maze_coins: fitness function for maze solution that collects maximal number of coins.
# Directions of movement are encoded as:
# 1 - up
# 2 - down
# 3 - left
# 4 - right
fitness_maze_coins <- function(maze, rows, cols, plan_numeric) {
  # Decode numeric encoding of directions and evaluate plan based on success and length.
  plan <- mapvalues(plan_numeric, c(0, 1, 2, 3, 4), c('O', 'U', 'D', 'L', 'R'), warn_missing = FALSE)
  res <- simulateSolutionCoins(maze, rows, cols, plan)
  return(c(res[3], res[2]))
}

#####################################################################################################################



# Examples of mazes and their solutions used for testing ############################################################

# Example 1:
maze1A <- c(' ', ' ', ' ', ' ', 'e',
		        ' ', '#', '#', '#', '#',
		        ' ', ' ', 's', ' ', ' ',
		        '#', '#', '#', '#', ' ',
		        ' ', ' ', ' ', ' ', ' ')
rows1A <- 5
cols1A <- 5
solution1A <- c('L', 'L','U', 'U', 'R', 'R', 'R', 'R') 


# Example 2:
maze2A <- c('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#',
		   '#', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', ' ', ' ', '#', ' ', '#', '#',
		   '#', '#', 'e', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', '#', '#',
		   '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', ' ', ' ', ' ', '#', '#',
		   '#', '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', '#', ' ', ' ', ' ',
		   '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', ' ',
		   '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
		   '#', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
		   '#', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', '#', ' ', ' ', ' ',
		   '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ',
		   '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
		   '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
		   '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ',
		   '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ', '#', ' ',
		   '#', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', ' ', ' ', ' ', ' ', '#', 's',
		   '#', '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', ' ', '#', '#', '#', ' ', '#', ' ',
		   '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#',
		   '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#')

solution2A <- c('U', 'U', 'U', 'U', 'U', 'U', 'L', 'L', 'D', 'L', 'L', 'L', 'L', 'L', 'D', 'D', 'D', 'L', 'L', 'L', 'L', 'U', 'U', 'U', 'U', 'L', 'U', 'U', 'U', 'U', 'L', 'L', 'U', 'U')
cols2A <- 17
rows2A <- 18


# Examples for testing the algorithm for finding the path that contains the most coins.

# Example 1:
maze1B <- c(' ', ' ', 'c', ' ', 'e',
           ' ', '#', '#', '#', '#',
           ' ', ' ', 's', ' ', 'c',
           '#', '#', '#', '#', ' ',
           ' ', 'c', ' ', ' ', ' ')
rows1B <- 5
cols1B <- 5
solution1B <- c('R', 'R', 'D', 'D', 'L', 'L', 'L', 'R', 'R', 'R', 'U', 'U', 'L', 'L', 'L', 'L', 'U', 'U', 'R', 'R', 'R', 'R') 


# Example 2:
maze2B <- c('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', ' ', ' ', '#', 'c', '#', '#',
           '#', '#', 'e', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', ' ', 'c', ' ', '#', '#',
           '#', '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', '#', ' ', ' ', 'c',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', ' ',
           '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', 'c', ' ', ' ', 'c', ' ', '#', '#', ' ', '#', '#', ' ', ' ', ' ',
           '#', 'c', ' ', 'c', '#', ' ', '#', ' ', '#', ' ', 'c', ' ', ' ', ' ', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ',
           '#', ' ', 'c', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', ' ', ' ', ' ', ' ', '#', 's',
           '#', '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', ' ', '#', '#', '#', ' ', '#', ' ',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#')

solution2B <- c('U','U','U','U','U','U','U','U','U','U','l','l', 'U', 'U', 'U', 'D', 'L', 'D', 'R', 'D', 'D', 'D', 'D', 'D', 'D', 'L', 'L', 'L', 'L', 'L', 'D', 'D', 
                'D', 'L', 'L', 'U', 'U', 'U', 'U', 'L', 'L', 'L', 'U', 'U', 'U', 'U', 'L', 'L', 'D', 'D', 'D', 'D', 'D', 'L', 'D', 'D', 'D', 'D', 'R', 'R', 'U', 'U',
                'U', 'U', 'L','U','U','U','U','U','U','U')
cols2B <- 17
rows2B <- 18

#####################################################################################################################


# Function for computing path that goes over the most coins (second part of assignment) #############################

# coinPath: compute path that collects the maximum number of coins and finds exit.
# Return chrosomome representing the path. 
#
# maze ... maze to use
# rows ... number of rows in maze description
# cols ... number of columns in maze description
# population_size ... population size used in genetic algorithm
# fitness_func ... fitness function for evaluating individuals in genetic algorithms
# max_run ... number of generations with same maximal fitness before stopping the genetic algorithm
# lim_run ... maximum number of generations to compute
# verbose ... print data about generations in genetic algorithm
coinPath <- function(maze, rows, cols, population_size, fitness_func, max_run, lim_run, verbose) {
  maze_tab <- table(maze) 									# Compute number of coins found in the maze.
  num_coins <- maze_tab[names(maze_tab)=='c']
  num_coins_count <- num_coins
  fin <- c() 												         # vector for storing the results
  for (k in 1:num_coins_count) { 							# For each coin in the maze...
    res1 <- vector("list", as.numeric(num_coins))
    res1 <- foreach(i=1:num_coins) %dopar% { 				# Compute paths from starting point to each coin in maze in a parallelized loop.
      maze_aux <- maze 										        # Create an auxiliary maze descriptor vector.
      maze_aux[maze_aux == 'e'] <- ' ' 						# Remove exit.
      maze_aux[maze_aux == 'c'][i] <- 'e' 				# Make i-th coin the exit and compute path to it.
      geneticAlgorithm(population_size=as.numeric(population_size), fitness_f=fitness_func, params=list(maze_aux, rows, cols), min_len=length(maze2B), max_len=length(maze2B), min_val=0, max_val=4, max_run=as.numeric(max_run), lim_run=as.numeric(lim_run), plot_iterations=FALSE, verbose=verbose) 
    }
    col1 <- map(res1, {function(x) x[[1]]})  				# Create data fame with info about the results obtained by the genetic algorithm.
    col2 <- map(res1, {function(x) x[[2]]})
    col3 <- map(res1, {function(x) x[[3]]})
    df <- data.frame(fitness=unlist(col1, recursive = TRUE, use.names = TRUE), steps=unlist(col3, recursive = TRUE, use.names = TRUE))
    df$chromosomes <- col2
    idx_max_fitness <- which(df$fitness == max(df$fitness))[1] 	# Get index of individual with maximal fitness in the dataframe.
    chr_max <- df$chromosomes[idx_max_fitness] 					        # Obtain chromosome of individual with maximal fitness.
    chr_max <- Filter({function(x) x != 0}, chr_max[[1]]) 		  # Remove noop codons.
    chr_max <- chr_max[1:df$steps[idx_max_fitness]] 			      # Truncate.
    fin <- c(fin, chr_max) 										                  # Append individual's chromosome to vector storing the total chromosome for the whole path.
    maze[maze == 's'] <- ' ' 									                  # Remove start from maze.
    maze[maze == 'c'][idx_max_fitness] <- 's' 					        # Make coin that was reached the new start.
    num_coins <- num_coins - 1 									                # There is now one less coin.
  }
  res <- vector("list", as.numeric(num_iterations)) 			      # Allocate memory for chromosomes of individuals that result form running the genetic agorithm from final coin to maze exit.
  res <- foreach(i=1:num_iterations) %dopar% { 					        # Compute shortest path from last coin to exit.
    geneticAlgorithm(population_size=as.numeric(population_size), fitness_f=fitness_func, params=list(maze, rows, cols), min_len=length(maze2B), max_len=length(maze2B), min_val=0, max_val=4, max_run=as.numeric(max_run), lim_run=as.numeric(lim_run), plot_iterations=FALSE, verbose=verbose) 
  }
  
  # Prepare data frame columns for presenting the results of the final runs of the genetic algorithm.
  col1 <- map(res, {function(x) x[[1]]})
  col2 <- map(res, {function(x) x[[2]]})
  col3 <- map(res, {function(x) x[[3]]})
  df <- data.frame(fitness=unlist(col1, recursive = TRUE, use.names = TRUE), steps=unlist(col3, recursive = TRUE, use.names = TRUE))
  df$chromosomes <- col2
  idx_max_fitness <- which(df$fitness == max(df$fitness))  		# Store index of row with maximum fitness.
  chr_max <- df$chromosomes[idx_max_fitness] 					        # Get best chromosome and its length.
  chr_max <- Filter({function(x) x != 0}, chr_max[[1]]) 		  # Remove noop codons.
  num_steps <- df$steps[idx_max_fitness] 						          # Truncate chromosome.
  chr_max <- chr_max[1:num_steps]
  return (mapvalues(c(fin, chr_max), c(0, 1, 2, 3, 4), c('O', 'U', 'D', 'L', 'R'), warn_missing = FALSE))  # Map to characters describing the direction and return result.
}

#####################################################################################################################

# User interface ####################################################################################################

# Parse properties of the genetic algorithm from user input.
verbose <- TRUE
quick_settings <- FALSE

# Default parameter values - used when quick settings are selected.
num_iterations <- 8
population_size <- 8
lim_run <- 10000
max_run <- 1000
plot_it <- TRUE
maze_opt <- 0

repeat {
  set <- readline(prompt="Use quick settings? (y/n)")
  # Validate input.
  if(set == 'y' || set == 'n') {
    if(set == 'y'){
    	quick_settings <- TRUE
    	break;
    } else {
    	break;
    }
  } else {
    print("Invalid input. Please try again.") 
  }
}

repeat {
  mode <- readline(prompt="Compute shortest path to exit or path that collects the maximum number of coins? (1/2): ")
  # Validate input.
  if(!is.na(as.integer(mode)) && as.integer(mode) == 1 || as.integer(mode) == 2) {
    break;
  } else {
    print("Invalid input. Please try again.") 
  }
}

if(!quick_settings){
	repeat {
	  num_iterations <- readline(prompt="Enter number of times to initialize and run the genetic algorithm: ")
	  # Validate input.
	  if(!is.na(as.integer(num_iterations)) && as.integer(num_iterations) > 0) {
	  	if(as.integer(num_iterations) > 1) {
	  		verbose <- FALSE
	  	}
	    break;
	  } else {
	    print("Invalid input. Please try again.") 
	  }
	}

	repeat {
	  population_size <- readline(prompt="Enter population size: ")
	  # Validate input.
	  if(!is.na(as.integer(population_size)) && as.integer(population_size) > 0) {
	    break;
	  } else {
	    print("Invalid input. Please try again.") 
	  }
	}

	repeat {
	  lim_run <- readline(prompt="Enter maximum number of generations to compute: ")
	  # Validate input.
	  if(!is.na(as.integer(lim_run)) && as.integer(lim_run) > 0) {
	    break;
	  } else {
	    print("Invalid input. Please try again.") 
	  }
	}

	repeat {
	  max_run <- readline(prompt="Enter maximum number of consequent generations without improvement before halting the algorithm: ")
	  # Validate input.
	  if(!is.na(as.integer(max_run)) && as.integer(max_run) > 0) {
	    break;
	  } else {
	    print("Invalid input. Please try again.") 
	  }
	}

	plot_it <- FALSE
	if(as.integer(num_iterations) <= 1) {
		repeat {
		  plot_iterations <- readline(prompt="Create a plot of fitness values by iteration? (y/n): ")
		  # Validate input.
		  if(plot_iterations == 'y' || plot_iterations == 'n') {
		    plot_it <- if (plot_iterations == 'y') TRUE else FALSE
		    break;
		  } else {
		    print("Invalid input. Please try again.") 
		  }
	    }
	}
}

repeat {
  maze_opt <- readline(prompt="Choose maze to use (1, 2): ")
  # Validate input.
  if(maze_opt == '1' || maze_opt == '2') {
    maze_opt <- as.integer(maze_opt)
    break;
  } else {
    print("Invalid input. Please try again.") 
  }
}

#####################################################################################################################

# Computing the results ###################################################################################################
# If computing the shortest path...
if (as.integer(mode) == 1) {
  # Compute results in a paralellized loop.
  # Allocate memory for results.
  res <- vector("list", as.numeric(num_iterations))
  res <- foreach(i=1:num_iterations) %dopar% {
    if(maze_opt == 1) {
      geneticAlgorithm(population_size=as.numeric(population_size), fitness_f=fitness_maze_path, params=list(maze1A, rows1A, cols1A), min_len=length(maze1A), max_len=length(maze1A), min_val=0, max_val=4, max_run=as.numeric(max_run), lim_run=as.numeric(lim_run), plot_iterations=plot_it, verbose=verbose) 
    } else {
      geneticAlgorithm(population_size=as.numeric(population_size), fitness_f=fitness_maze_path, params=list(maze2A, rows2A, cols2A), min_len=length(maze2A), max_len=length(maze2A), min_val=0, max_val=4, max_run=as.numeric(max_run), lim_run=as.numeric(lim_run), plot_iterations=plot_it, verbose=verbose) 
    }
  } 
# If computing path that collects the most coins...
} else {
  # Get results.
  if(maze_opt == 1) {
    fitness_max_chromosome <- coinPath(maze=maze1B, rows=rows1B, cols=cols1B, population_size=population_size, fitness_func=fitness_maze_path, max_run=max_run, lim_run=lim_run, verbose=FALSE)
  } else {
    fitness_max_chromosome <- coinPath(maze=maze2B, rows=rows2B, cols=cols2B, population_size=population_size, fitness_func=fitness_maze_path, max_run=max_run, lim_run=lim_run, verbose=FALSE)
  }
}

#####################################################################################################################


# Visualizing the results ###########################################################################################

if (as.integer(mode) == 1) {
	# Extract and display properties of best agents.
	# Make data frame.

	# Prepare data frame columns.
	col1 <- map(res, {function(x) x[[1]]})
	col2 <- map(res, {function(x) x[[2]]})
	col3 <- map(res, {function(x) x[[3]]})
	df <- data.frame(fitness=unlist(col1, recursive = TRUE, use.names = TRUE), steps=unlist(col3, recursive = TRUE, use.names = TRUE))
	# Append chromosome vectors to data frame.
	df$chromosomes <- col2

	# Store index of row with maximum fitness.
	idx_max_fitness <- which(df$fitness == max(df$fitness))

	# Get best chromosome and its length.
	chr_max <- df$chromosomes[idx_max_fitness]
	# Remove noop value.
	chr_max <- Filter({function(x) x != 0}, chr_max[[1]])
	num_steps <- df$steps[idx_max_fitness]
	# Process data to get solution as string.
	fitness_max_chromosome <- mapvalues(chr_max, c(0, 1, 2, 3, 4), c('O', 'U', 'D', 'L', 'R'), warn_missing = FALSE)
	fitness_max_chromosome <- fitness_max_chromosome[1:num_steps]
	
	# Print results.
	cat(green(sprintf("Maximal fitness found: %f\n", df$fitness[idx_max_fitness])))
}

chr_string <- str_c(fitness_max_chromosome, sep = "", collapse = ', ')
cat(green(sprintf("Chromosome of max fitness (formatted as description of best found solution):\n")))
cat(yellow(sprintf("%s\n", chr_string)))

# Visualize the solution in the maze by tracing the path.
cat('Solution visualization:\n\n')
if (as.integer(mode) == 1) {
  if(maze_opt == 1) {
    simulateSolutionPath(maze1A, rows1A, cols1A, fitness_max_chromosome, trace=TRUE)
  } else {
    simulateSolutionPath(maze2A, rows2A, cols2A, fitness_max_chromosome, trace=TRUE)
  } 
} else {
  if(maze_opt == 1) {
    simulateSolutionCoins(maze1B, rows1B, cols1B, fitness_max_chromosome, trace=TRUE)
  } else {
    simulateSolutionCoins(maze2B, rows2B, cols2B, fitness_max_chromosome, trace=TRUE)
  }
}