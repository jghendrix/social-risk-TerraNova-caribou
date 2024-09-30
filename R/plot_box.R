#' @title Plot boxplot
#' @export
#' @author Julie W. Turner
plot_box <- function(DT, theme) {

	ggplot(data = DT,
				 aes(as.logical(x), spd)) +
		geom_boxplot(aes(color = as.factor(x)),
								 show.legend = F) +
		geom_jitter(aes(color = as.factor(x)),
								show.legend = F) +
		scale_colour_viridis(discrete = "TRUE", option = "C", begin = 0.25, end = 0.7) +
		plot_theme()
}
