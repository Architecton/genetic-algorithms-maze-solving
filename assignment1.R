library(plyr)
library(rlist)
library(foreach)
library(doMC)
library(purrr)
library(ggplot2)

# Register cores.
registerDoMC(detectCores(all.tests = FALSE, logical = TRUE))
# Auxiliary functions ###

# Print maze on console.
printMaze <- function(maze, rows, cols) {
	for (x in seq(1, rows)) {
		print(maze[((x-1)*cols +1) : (x*cols)])
	}
}

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

# SimulateSolution: move in maze with specified number of rows and columns according to plan specified in
# solution. Return 1 if solution was passed over and 0 otherwise.
simulateSolution <- function(maze, rows, cols, solution) {
	# Starting position linear index.
	pos_start <- ind2sub(rows, match('s', maze))
	# Linear index of exit.
	pos_exit <- ind2sub(rows, match('e', maze))
	# Set penalty value.
	penalty <- 25
	# Set penalty multiplier.
	multiple <- 3
	# Set initial score.
	score <- length(maze)
	found_exit <- FALSE
	# Count steps.
	step_counter <- 0
	currentPosition <- grep('s', maze)
	# Go over moves in solution.
	for (move in solution) {
		# Increment step counter.
		step_counter <- step_counter + 1
		oldPosition <- currentPosition
		# Apply moves and penalize hitting into walls.
		if (move == 'U') {
			move_res <- moveUp(currentPosition, rows, cols)
			score <- score - move_res[1]*penalty*multiple
			currentPosition <- move_res[2]
		} else if (move == 'D') {
			move_res <- moveDown(currentPosition, rows, cols)
			score <- score - move_res[1]*penalty*multiple
			currentPosition <- move_res[2]
		} else if (move == 'L') {
			move_res <- moveLeft(currentPosition, rows, cols)
			score <- score - move_res[1]*penalty*multiple
			currentPosition <- move_res[2]
		} else if (move == 'R') {
			move_res <- moveRight(currentPosition, rows, cols)
			score <- score - move_res[1]*penalty*multiple
			currentPosition <- move_res[2];
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
			bonus <- 1000  # Add bonus to score.
			# Compute travel score. (distance from start - distance from exit)
			pos_end <- ind2sub(rows, currentPosition)
			diff1 <- sum(abs(pos_start-pos_end))
			diff2 <- sum(abs(pos_exit-pos_end))
			dist_diff <- sum(abs(diff1-diff2))
			return(list(found_exit, score - dist_diff*penalty*multiple - step_counter*penalty + bonus))
		}
	}
	# If exit not reached.
	pos_end <- ind2sub(rows, currentPosition)
	diff1 <- sum(abs(pos_start-pos_end))
	diff2 <- sum(abs(pos_exit-pos_end))
	dist_diff <- sum(abs(diff1-diff2))
	return(list(found_exit, score - dist_diff*penalty*multiple - step_counter*penalty))
}

# crossover: perform crossover between breeder1, breeder2 and return the offspring.
crossover <- function(breeder1, breeder2, min_val, max_val, mutation_prob) {
	# Get chromosomes of agents.
	chr1 <- breeder1$chromosome
	chr2 <- breeder2$chromosome
	# Select crossover points.
	cp1 <- sample(c(seq_along(chr1)), size=1)
	#cp2 <- sample(c(seq_along(chr2)), size=1)
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
	Pm = 0.2
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
geneticAlgorithm <- function(population_size, fitness_f, params, min_len, max_len, min_val, max_val, max_run, lim_run, plot_iterations=FALSE) {
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

	# Run genetic algorithm. Break when any of ending conditions evaluate to TRUE.
	repeat {
		# Increment iteration counter.
		iter_counter <- iter_counter + 1

		# Initialize maximum fitness and index of agent with maximum fitness with default values.
		max_fitness <- -1e10
		idx_max_fitness <- -1

		# Compute fitness values.
		for(k in 1:population_size) {
			fitness_nxt <- do.call(fitness_f, append(params, list(agents[[k]]$chromosome)))
			if(fitness_nxt > max_fitness) {  # Compare to current max_fitness.
				max_fitness <- fitness_nxt
				idx_max_fitness <- k
			}
			agents[[k]]$fitness <- fitness_nxt
		}
		
		print(sprintf("maximum fitness of iteration = %f", max_fitness))
		# If greater than maximum fitness value, reset const_counter counter and assign new previous fitness value.
		if(max_fitness > max_fitness_all) {
			const_counter <- 0
			max_fitness_all <- max_fitness
			max_fitness_chromosome <- agents[[idx_max_fitness]]$chromosome  # Save chromosome of current best agent.
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
		print(sprintf("iteration %d: maximum fitness = %f", iter_counter, max_fitness_all))
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
	return (list(max_fitness_all, max_fitness_chromosome))
}

# fitness_f: fitness function for maze solution. 
# Directions of movement are encoded as:
# 1 - up
# 2 - down
# 3 - left
# 4 - right
fitness_maze <- function(maze, rows, cols, plan_numeric) {
	# Decode numeric encoding of directions and evaluate plan based on success and length.
	plan <- mapvalues(plan_numeric, c(0, 1, 2, 3, 4), c('O', 'U', 'D', 'L', 'R'), warn_missing = FALSE)
	res <- simulateSolution(maze, rows, cols, plan)
	if(res[[1]]) {
		return(res[[2]])
	} else {
		return(res[[2]])
	}
}


# Examples for testing.

# Example 1:
maze1 <- c(' ', ' ', ' ', ' ', 'e',
		   ' ', '#', '#', '#', '#',
		   ' ', ' ', 's', ' ', ' ',
		   '#', '#', '#', '#', ' ',
		   ' ', ' ', ' ', ' ', ' ')
rows1 <- 5
cols1 <- 5
solution1 <- c('L', 'L','U', 'U', 'R', 'R', 'R', 'R') 


# Example 2:
maze2 <- c('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#',
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

solution2 <- c('U', 'U', 'U', 'U', 'U', 'U', 'L', 'L', 'D', 'L', 'L', 'L', 'L', 'L', 'D', 'D', 'D', 'L', 'L', 'L', 'L', 'U', 'U', 'U', 'U', 'L', 'U', 'U', 'U', 'U', 'L', 'L', 'U', 'U')
cols2 <- 17
rows2 <- 18


# Run algorithm.
#list[max_fitness_all, sol] <- geneticAlgorithm(population_size=100, fitness_f=fitness_maze, params=list(maze1, rows1, cols1), min_len=length(maze1), max_len=length(maze1), min_val=0, max_val=4, max_run=1000, lim_run=1000)
#sol <- mapvalues(sol, c(0, 1, 2, 3, 4), c('O', 'U', 'D', 'L', 'R'), warn_missing = FALSE)

# Parse properties of the genetic algorithm from user input.
repeat {
  num_iterations <- readline(prompt="Enter number of iterations of the algorithm to compute: ")
  # Validate input.
  if(!is.na(as.integer(num_iterations)) && as.integer(num_iterations) > 0) {
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


# Compute results in a paralellized loop.
# Allocate memory for results.
res <- vector("list", as.numeric(num_iterations))
res <- foreach(i=1:num_iterations) %dopar% {
  geneticAlgorithm(population_size=as.numeric(population_size), fitness_f=fitness_maze, params=list(maze2, rows2, cols2), min_len=length(maze2), max_len=length(maze2), min_val=0, max_val=4, max_run=as.numeric(max_run), lim_run=as.numeric(lim_run), plot_iterations=plot_it)
}