#' @title Plot RSS for social interaction using population-level estimates
#'
plot_rss_social_p <- function(rss, theme) {

	mean <- rss %>% filter(SE == "mean")
	min <- rss %>% filter(SE == "min") %>% dplyr::select(x, social, min_rss = rss)
	max <- rss %>% filter(SE == "max") %>% dplyr::select(x, social, max_rss = rss)

	rss <- left_join(mean, min, by = c("x", "social"))
	rss <- left_join(rss, max, by = c("x", "social"))

	rss %<>%
		mutate(`Social context` = ifelse(social == "dyad", "in dyad", "not in dyad"),
					 x = ifelse(x > 1, x/1000, x))

ggplot(data = rss) +
		geom_line(aes(x = x, y = rss, colour = `Social context`),
							linewidth = 1) +
	geom_ribbon(aes(x = x, ymin = min_rss, ymax = max_rss, fill = `Social context`),
									alpha = 0.5) +
		#geom_line(aes(x = x, y = min_rss, colour = `Social context`),
	#						linewidth = 0.5,
	#						linetype = "dashed",
	#						show.legend = F) +
	#	geom_line(aes(x = x, y = max_rss, colour = `Social context`),
	#						linewidth = 0.5,
	#						linetype = "dashed",
	#						show.legend = F) +
		scale_color_viridis(discrete = "TRUE", option = "D",
												begin = 0.85, end = 0.2) +
	scale_fill_viridis(discrete = "TRUE", option = "D",
										 begin = 0.85, end = 0.2) +
	geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			size = .7
		) +
		plot_theme() +
		#	ylim(c(-10, 3)) +
		theme(plot.title = element_text(size = 12, hjust = 0.05),
					axis.title = element_text(size = 12),
					axis.text = element_text(size = 10))
}
