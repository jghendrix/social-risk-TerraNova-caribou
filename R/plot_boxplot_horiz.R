#' @title Plot boxplot of annual model estimates
#' @export
#' @author Jack G Hendrix
plot_box_horiz <- function(DT, theme, predictor) {

gbox <- ggplot(data = DT[term !='(Intercept)' & term != 'lc_adjother'],
				 aes(term, estimate, estimate - se, estimate + se)) +
		geom_boxplot(outlier.shape = NA) +
		geom_pointrange(aes(ymin = estimate - se, ymax = estimate + se, colour = id),
										position = position_jitter(width = 0.3),
										linetype = "dotted",
										show.legend = F) +
		geom_hline(yintercept = 0, lty = 'dashed') +
		coord_flip() +
		plot_theme() +
		ggtitle(paste0(predictor, ' model'))

ggsave(
	paste0('graphics/summary/boxplot_', predictor, '_indiv_selection.png'),
	gbox,
	width = 10,
	height = 10,
	dpi = 320
)

return(gbox)
}
