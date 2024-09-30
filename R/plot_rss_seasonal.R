#' @title Plot RSS seasonally
#' @export
#' @author Jack G Hendrix
plot_rss_seasonal <- function(rss, theme, axis) {

# axis = the predictor variable of interest

	rss %<>% mutate(s_code = ifelse(season == "winter", 1, 2),
								 s_code = ifelse(season == "calving", 3, s_code),
								 s_code = ifelse(season == "autumn", 4, s_code))

	for(i in 1:4) {
		data <- subset(rss, s_code == i)

g <- ggplot(data, aes(x = x/1000, rss)) +
		geom_line(aes(group = id, colour = id, alpha = .0001),
							linetype = 'twodash',
							show.legend = F) +
	geom_smooth(size = 1.2,
							colour = "black",
							method = "lm",
							formula = y ~ log((x+1)/1000)) +
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			linewidth = .7) +
		scale_color_viridis(discrete = TRUE)  +
		scale_fill_viridis() +
	# the responses vary wildly in scale, does setting a fixed y-axis help compare across seasons?
		scale_y_continuous(limits = c(-3, 3)) +
		plot_theme() +
		labs(x = paste0('Distance to ', axis, ' (km)'), y = 'logRSS') +
		ggtitle(paste0('RSS for ', axis, ' - ', data$season)) +
	theme(plot.title = element_text(size = 12, hjust = 0.05),
				axis.title = element_text(size = 12),
				axis.text = element_text(size = 10))

ggsave(
	filename = paste0('graphics/rss/rss_', axis, '-', i, '.png'),
	width = 1200,
	height = 1200,
	units = "px",
	dpi = 320
)
	}

	return(g)
}
