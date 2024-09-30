#' @title Comparing RSS and speed by predictor/season
#' @export
#' @author Jack G Hendrix
plot_rss_vs_speed <- function(option, rss, speed) {


# Join RSS to speed by indiv ID, season, and predictor
df <- left_join(rss, speed, by = c("id", "season", "pred"))

# There's quite  abit of individual variation here. summarise each into a set of error bars
sum <- df %>% group_by(season, pred) %>%
	summarise(rss = mean(diff_rss),
						spd = mean(diff_spd),
						rss_error = sd(diff_rss)/sqrt(10),
						spd_error = sd(diff_spd)/sqrt(10))

g <- ggplot(sum, aes(x = spd, y = rss,
								xmin = spd - spd_error, xmax = spd + spd_error,
								ymin = rss - rss_error, ymax = rss + rss_error,
								colour = season, shape = pred)) +
	geom_point(size = 5) +
	geom_errorbar() +
	geom_errorbarh() +
	geom_hline(yintercept = 0,	colour = "black",
						 lty = 2, linewidth = .7) +
	geom_vline(xintercept = 0, colour = "black",
						 lty = 2, linewidth = .7) +
	xlab("Difference in speed btwn 1 and 5000m distance") +
	ylab("Difference in RSS btwn 0 and 20000m distance") +
	scale_colour_viridis(discrete = TRUE, option = "C") +
	theme_bw()

ggsave(filename = paste0("graphics/test/speed_selection_comp_", option, ".png"),
	width = 10,
	height = 10,
	dpi = 320
	)

return(g)
}
