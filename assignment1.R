library(plyr)

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

# TODO
# Return new agents that are the result of tournament selection.
tournament <- function(agents, num_groups) {
	# Split agents into groups.
	groups <- split(agents, ceiling(seq_along(agents)/num_groups))

	# Select two breeders from each group. With probability Pc (crossover probability), 
	# replace two worst non-breeders from group with offspring of breeders.
	for (g_idx = 1:length(groups)) {
		# Define group to be group of agents in next group.
		group <- groups[[g_idx]]
		# Select two breeders. Select best one with probability P, second best one with probability P*(1-P),
		# third best one with probability P*(1-P)^2 and so on.
		P_best = 0.8;
		# TODO
		breeders <- sample(group, size=2, replace=FALSE, prob=c(TODO))

	}
}


# 1 - up
# 2 - down
# 3 - left
# 4 - right
#
geneticAlgorithm <- function(maze, rows, cols, population_size, fitness_f, min_len, max_len, max_run, lim_run) {
	# Generate population of agents of specified size and of random length from interval [min_len and max_len].
	# Initialize them with random plan (See above for direction encoding).
	agents = vector("list", population_size)
	for(k in 1:population_size) {
		chromosome_size = sample(min_len:max_len, size=1, replace=TRUE)
		chromosome = sample(1:4, size=chromosome_size, replace=TRUE)
		agents[[k]] = list(fitness = 0, chromosome = chromosome)
	}

	# Run genetic algorithm. Break when any of ending conditions evaluate to TRUE.
	iter_counter <- 0;
	const_counter <- 0;
	repeat {
		# Increment iteration counter.
		iter_counter <- iter_counter + 1
		# Compute fitness values for each agent.
		for(k in 1:population_size) {
			agents[[k]]$fitness = fitness_f(maze, rows, cols, agents[[k]]$chromosome)
		}

		# Select agents for reproduction. Use tournament selection.
		# Replace two worst agents (that were not selected for breeding) in each group with offspring.
		num_groups = 5;
		new_agents <- tournament(agents, num_groups)


		# Create new agents by combining the candidates.

		# Replace old agents with new ones.

		# If reached maximum number of iterations or if best fitness has not changed for max_run iterations, end.
		if(iter_counter >= lim_run || const_counter >= max_run) {
			break;
		}
	}
	return(agents)
	
}

# fitness_f: fitness function for maze solution. 
fitness_f <- function(maze, rows, cols, plan_numeric) {
	# Decode numeric encoding of directions and evaluate plan based on success and length.
	plan <- mapvalues(plan_numeric, c(1, 2, 3, 4), c('U', 'D', 'L', 'R'), warn_missing = FALSE)
	return(simulateSolution(maze, plan, rows, cols)/(length(plan)*100))
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