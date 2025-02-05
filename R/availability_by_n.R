#' @title looking at resource availability by random step number
#' @export
#' @author Jack G Hendrix

library(dplyr)
library(ggplot2)
library(viridis)

# This does not involve generating MORE additional random steps, it's just subsampling within those you already have to see if the levelling off point is lower than your chosen n steps

# I started with 10 steps, and I have 5 predictors I am interested in the availability of:
# distance to new burns
# distance to old burns
# distance to Trans Canada Highway
# distance to minor roads
# proportion of forested habitat within 100m radius at end point

# For each of these, I want to know the range of values within each cluster for a given n random steps - decided to use standard deviation to represent that

availability_by_n <- function(DT) {

## Iterating through 2, 3, 4, ..., n steps per cluster ----

cluster <- data.frame() # write a blank data frame onto which you can r-bind your iterations

for(i in 2:10) {

	rand <- DT %>% filter(case_ == FALSE) %>%
			group_by(indiv_step_id) %>%
			slice_sample(n = i) %>% # takes a random sample of i rows from within your grouping variable
			group_by(season, id, indiv_step_id) %>% # I wanted to know whether certain animals or seasons had different levelling off points
			summarize(sd.new = sd(dist_to_new_burn),
								sd.old = sd(dist_to_old_burn),
								sd.tch = sd(dist_to_tch),
								sd.minor = sd(dist_to_minor),
								sd.forest = sd(prop_forest)) %>%
			mutate(rand_n = i) # add a column identifying which # of random steps these SD are from

		cluster <- rbind(cluster, rand) # bind each iteration onto that blank data frame, and then progressively adds the next iteration to the same data
	}

# Now using that cluster, I just wanted to visually assess whether standard deviation levelled off

# create a plot for each of your variables of interest ----

	g.new <- ggplot(cluster, aes(x = as.factor(rand_n), y = sd.new)) +
		# I did colour and not fill b/c I found it harder to assess with solid boxes, but that's just me
		geom_boxplot(outliers = FALSE) +
		# there are tons of super high values that make it hard to see what's going on
		scale_colour_viridis(discrete = TRUE) +
		# I just like viridis lol
		labs(x = "n random steps", y = "SD of distance to new burn within cluster") +
		# obviously change the y label to reflect your own variable of interest
		theme_bw()
	ggsave('graphics/rev/sensitivity/new_burn_avail.png',
				 g.new,
				 width = 10,
				 height = 8)
	# saving the plot to my graphics/sensitivity sub-folder


# do the same for the rest of your variables, just change the y and the names

	g.old <- ggplot(cluster, aes(x = as.factor(rand_n), y = sd.old)) +
		geom_boxplot(outliers = FALSE) +
		scale_colour_viridis(discrete = TRUE) +
		labs(x = "n random steps", y = "SD of distance to old burn within cluster") +
		plot_theme()
ggsave('graphics/rev/sensitivity/old_burn_avail.png',
			 g.old,
			 width = 10,
			 height = 8)

	g.tch <- ggplot(cluster, aes(x = as.factor(rand_n), y = sd.tch)) +
		geom_boxplot(outliers = FALSE) +
		scale_colour_viridis(discrete = TRUE) +
		labs(x = "n random steps", y = "SD of distance to TCH within cluster") +
		plot_theme()
ggsave('graphics/rev/sensitivity/TCH_avail.png',
			 g.tch,
			 width = 10,
			 height = 8)

	g.minor <- ggplot(cluster, aes(x = as.factor(rand_n), y = sd.minor)) +
		geom_boxplot(outliers = FALSE) +
		scale_colour_viridis(discrete = TRUE) +
		labs(x = "n random steps", y = "SD of distance to minor roads within cluster") +
		plot_theme()
ggsave('graphics/rev/sensitivity/minor_avail.png',
			 g.minor,
			 width = 10,
			 height = 8)

	g.forest <- ggplot(cluster, aes(x = as.factor(rand_n), y = sd.forest)) +
		geom_boxplot(outliers = FALSE) +
		scale_colour_viridis(discrete = TRUE) +
		labs(x = "n random steps", y = "SD of proportion forest within cluster") +
		plot_theme()
ggsave('graphics/rev/sensitivity/forest_avail.png',
			 g.forest,
			 width = 10,
			 height = 8)

return(cluster)

}
