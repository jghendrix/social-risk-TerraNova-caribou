#' @title Plot distribution
#' @export
#' @author Jack G. Hendrix
plot_dist <- function(DT, theme) {

	DT %<>% group_by(x) %>% mutate(pop = mean(spd)) %>% ungroup()

	ggplot(data = DT,
				 aes(x = x/1000, y = spd)) +
		geom_line(aes(colour = id),
							linetype = 5,
							show.legend = F) +
		geom_line(aes(x = x/1000, y = pop), linewidth = 1.25, colour = "black") +
		scale_colour_viridis(discrete = TRUE) +
		ylim(c(100, 500)) +
		plot_theme() +
		theme(axis.title = element_text(size = 12),
					axis.text = element_text(size = 10))
}
