#' @title individual seasonal model outputs
#' @export
#' @author Jack G Hendrix
sum_seasonal <- function(DT, theme, predictor) {

	#	popn <- DT %>%
	#	group_by(term, seasonality) %>%
	#	summarise(n = n(),
	#						mean = mean(estimate),
	#						mse = sd(estimate)/sqrt(n))

	fixed <- read.csv('input/popn_est.csv') %>%
		filter(model == predictor & season == unique(DT$seasonality)) %>%
		dplyr::select(popn = term, mean = estimate, mse = se) %>%
		mutate(source = "fixed")

	gbox <- ggplot(data = fixed, aes(x = popn, y = mean)) +
		geom_pointrange(data = DT,
										aes(x = term, y = estimate, ymin = estimate - se, ymax = estimate + se,
												colour = id, alpha = 0.5),
										position = position_jitter(width = 0.3),
										linetype = "dotted",
										show.legend = F) +
		geom_pointrange(aes(ymin = mean - mse, ymax = mean + mse)) +
		geom_hline(yintercept = 0, lty = 'dashed') +
		xlab("") +
		ylab("Estimate ± SE") +
		scale_color_viridis(discrete = "TRUE")  +
		scale_fill_viridis(discrete = 'TRUE') +
		coord_flip() +
		plot_theme() +
		theme(axis.text.x = element_text(size = 10, margin = margin(10, 10, 10, 10, "pt"))) +
		ggtitle(paste0(unique(DT$seasonality)," ", predictor, " model"))

	ggsave(
		filename = paste0('graphics/rev/summary/', predictor, '_indiv_responses_', unique(DT$seasonality), '.png'),
		gbox,
		width = 8,
		height = 8,
		dpi = 320
	)

	return(gbox)
}
