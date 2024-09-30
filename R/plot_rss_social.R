#' @title Plot RSS
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
plot_rss_social <- function(rss, theme) {


	rss %<>% mutate(idsoc = paste0(id, "_", social),
									x = ifelse(x > 1, x/1000, x)) %>%
		mutate(`Social context` = ifelse(social == "in_dyad", "in dyad", "not in dyad"))


	ggplot(data = rss, aes(x = x, y = rss, colour = `Social context`)) +
		geom_line(aes(group = idsoc, alpha = 0.5),
							linewidth = 0.5,
							linetype = "dashed",
							show.legend = F
		)+
		scale_color_viridis(discrete = "TRUE", option = "D", begin = 0.85, end = 0.2) +
		geom_smooth(size = 1.2, method = "lm",
								formula = ifelse(mean(rss$x) > 1,
																 "y ~ log(x+1)", "y ~ x")) +
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			size = .7
		) +
		plot_theme() +
		ylim(c(-0.5, 1.5)) +
		theme(plot.title = element_text(size = 12, hjust = 0.05),
					axis.title = element_text(size = 12),
					axis.text = element_text(size = 10))
}
