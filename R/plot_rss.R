#' @title Plot RSS
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
plot_rss <- function(rss, theme) {

	rss %<>% mutate(x = ifelse(x > 1, x/1000, x))

ggplot(data = rss, aes(x, rss)) +
		geom_line(aes(group = id, colour = id, alpha = .0001),
							linetype = 'twodash', show.legend = F
							)+
		geom_smooth(size = 1.2,
								colour = "black",
								method = "lm",
								formula = ifelse(mean(rss$x) > 1, "y ~ log(x+1)", "y ~ x")) +
	# ^ If plotting against forest, we need a straight line, otherwise we want it log curved
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			size = .7
		) +
		scale_color_viridis(discrete = "TRUE")  +
		scale_fill_viridis() +
		#scale_y_continuous(limits = c(-4, 2)) +
		plot_theme()
}
