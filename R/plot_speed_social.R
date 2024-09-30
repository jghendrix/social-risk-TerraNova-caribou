#' @title Plot speed alone vs. in dyad
#' @export
#' @author Jack G. Hendrix
plot_speed_social <- function(alone, dyad, theme) {

	alone %<>% group_by(x) %>%
		mutate(pop = mean(spd),
					 social = "alone") %>%
		ungroup()

	dyad %<>% group_by(x) %>%
		mutate(pop = mean(spd),
					 social = "dyad") %>%
		ungroup()

	DT <- rbind(alone, dyad)

	ggplot(data = DT,
				 aes(x = x/1000, y = spd)) +
		geom_line(aes(colour = id),
							linetype = 5,
							show.legend = F) +
		geom_line(aes(x = x/1000, y = pop), linewidth = 1.25, colour = "black") +
		scale_colour_viridis(discrete = TRUE) +
		plot_theme() +
		facet_wrap(. ~ social)
}
