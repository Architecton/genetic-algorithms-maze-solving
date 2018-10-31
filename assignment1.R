library(plyr)
library(rlist)

# Auxiliary functions ###
printMaze <- function(maze, rows, cols) {
	for (x in seq(1, rows)) {
		print(maze[((x-1)*cols +1) : (x*cols)])
	}
}

moveUp <- function(position, rows, cols) {
    newPosition <- position - cols
    if (newPosition < 1) {
        return (position)
    } else {
        return (newPosition)
    }
}

moveDown <- function(position, rows, cols) {
	newPosition <- position + cols
    if (newPosition > rows*cols) {
        return (position)
    } else { 
        return (position + cols)
    }
}

moveLeft <- function(position, rows, cols) {
    newPosition <- position - 1
    if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
        return (position)
    } else {
        return (position - 1)
    }
}

moveRight <- function(position, rows, cols) {
	newPosition <- position + 1
    if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
        return (position)
    } else { 
        return (position + 1)
    }
}
# Auxiliary functions ###

simulateSolution <- function(maze, solution, rows, cols) {
    # Update this function to serve as a fitness funcition
    # The simplest example is shown here: return 1 if the solution found the exit and 0 if it did not
    currentPosition <- grep('s', maze)
	for (move in solution) {
        oldPosition <- currentPosition
		if (move == 'U') {
			currentPosition <- moveUp(currentPosition, rows, cols)
		} else if (move == 'D') {
			currentPosition <- moveDown(currentPosition, rows, cols)       
		} else if (move == 'L') {
			currentPosition <- moveLeft(currentPosition, rows, cols)            
		} else if (move == 'R') {
			currentPosition <- moveRight(currentPosition, rows, cols)
		} else {
            print('Error: Incorrect solution format')
			return(-1)
		}
        if (maze[currentPosition] == '#') {
            currentPosition <- oldPosition
        }
		if (maze[currentPosition] == 'e') {
			return(1)
		}
	} 
	return(0)
}

# crossover: perform crossover between breeder1, breeder2 and return the offspring.
crossover <- function(breeder1, breeder2) {
	# TODO
}

# Perform tournament selection on list of agents, perform crossover on selected agents and return offspring.
# Return new list of agents that are the result of tournament selection.
tournament <- function(agents, num_groups) {
	# Split agents into groups.
	groups <- split(agents, ceiling(seq_along(agents)/num_groups))

	# Select best agent with probability P, second best one with probability P*(1-P),
	# third best one with probability P*(1-P)^2 and so on.
	P_best = 0.8;  # Probability of selecting the best agent is 0.8
	prob_sel <- replicate(length(group), P_best)
	mult <- replicate(length(group), 1-P_best)^(0:(length(group)-1))
	prob_sel <- prob_sel * mult
	Pc = 0.9

	# Select two breeders from each group. With probability Pc (crossover probability), 
	# replace two worst non-breeders from group with offspring of breeders.
	for (g_idx = 1:length(groups)) {
		# Define group to be group of agents in next group.
		group <- groups[[g_idx]]
		# Sort agents by their fitness.
		group <- group[list.order(group, (fitness))]

		# If crossover...
		if(sample(c(1, 0), size=1, prob=c(Pc, 1-Pc))) {
			# Select breeders.
			breeders <- sample(group, size=2, replace=FALSE, prob=prob_sel)
			# Perform crossover to get offspring.
			offspring <- crossover(breeders[[1]], breeders[[2]])
			# Replace worst two agents in group with offspring.
			group[length(group) - 1] = offspring[[1]]
			group[length(group)] = offspring[[2]]
		}
	}
	# Join groups into list of agents and return the list.
	# TODO TEST
}

# geneticAlgorithm: Implementation of a genetic algorithm
#
#
# Directions are encoded as:
# 1 - up
# 2 - down
# 3 - left
# 4 - right
geneticAlgorithm <- function(population_size, fitness_f, params, min_len, max_len, max_run, lim_run) {
	# Generate population of agents of specified size and of random length from interval [min_len and max_len].
	# Initialize them with random plan (See above for direction encoding).
	agents = vector("list", population_size)
	for(k in 1:population_size) {
		chromosome_size <- sample(min_len:max_len, size=1, replace=TRUE)
		chromosome <- sample(1:4, size=chromosome_size, replace=TRUE)
		agents[[k]] <-extends list(fitness = 0, chromosome = chromosome)
	}

	# counters: iterations and iterations sequence with equal best value.
	iter_counter <- 0
	const_counter <- 0
	# Initialize index of agent with maximum fitness.
	max_fitness_all <- -1e-10
	max_fitness_chromosome <- NULL

	# Run genetic algorithm. Break when any of ending conditions evaluate to TRUE.
	repeat {
		# Increment iteration counter.
		iter_counter <- iter_counter + 1

		# Initialize maximum fitness and index of agent with maximum fitness with default values.
		max_fitness <- -1e-10
		idx_max_fitness <- -1

		# Compute fitness values.
		for(k in 1:population_size) {
			fitness_nxt <- fitness_f(params[1], params[2], params[3], agents[[k]]$chromosome)
			if(fitness_nxt > max_fitness) {  # Compare to current max_fitness.
				max_fitness <- fitness_nxt
				idx_max_fitness <- k
			}
			agents[[k]]$fitness = fitness_nxt
		}
		
		# If greater than maximum fitness value, reset const_counter counter and assign new previous fitness value.
		if(max_fitness > max_fitness_all) {
			const_counter <- 0
			max_fitness_chromosome <- agents[[idx_max_fitness]]$chromosome  # Save chromosome of current best agent.
		} else {
			const_counter <- const_counter + 1  # If no improvement, increment counter of iterations with no improvement.
		}

		# If reached maximum number of iterations or if best fitness has not changed for max_run iterations, end.
		if(iter_counter >= lim_run || const_counter >= max_run) {
			break;
		}

		# Select agents for reproduction. Use tournament selection.
		# Replace two worst agents (that were not selected for breeding) in each group with offspring.
		num_groups = 5;
		agents <- tournament(agents, num_groups)
	}

	# Return gene of agent with maximum fitness.
	return (max_fitness_chromosome)
}

# fitness_f: fitness function for maze solution. 
fitness_f <- function(maze, rows, cols, plan_numeric) {
	# Decode numeric encoding of directions and evaluate plan based on success and length.
	plan <- mapvalues(plan_numeric, c(1, 2, 3, 4), c('U', 'D', 'L', 'R'), warn_missing = FALSE)

	# If reached goal, fitness is inversly proportional to solution length.
	if(simulateSolution(maze, plan, rows, cols)) {
		return(1/(length(plan)*100))
	} else {
		# If goal not reached, fitness is inversly proportional to length of plan.
		return(-length(plan))
	}
}


maze1 <- c(' ', ' ', ' ', ' ', 'e',
           ' ', '#', '#', '#', '#',
           ' ', ' ', 's', ' ', ' ',
           '#', '#', '#', '#', ' ',
           ' ', ' ', ' ', ' ', ' ')
rows1 <- 5
cols1 <- 5
solution1 <- c('L', 'L','U', 'U', 'R', 'R', 'R', 'R') 

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

a <- geneticAlgorithm(maze1, rows1, cols1, 5, fitness_f, 1, length(maze1), 10, 1)