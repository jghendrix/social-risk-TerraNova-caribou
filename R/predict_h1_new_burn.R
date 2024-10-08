#' @title Predict H1 distance to burn
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
predict_h1_new_burn <- function(DT, model) {
	N <- 100L

	new_data <- DT[, .(
		sl_ = mean(sl_),
		forest = 0,
		open = 0,
		dist_to_new_burn = seq(from = 0, to = 20000, length.out = N),
		dist_to_old_burn = median(dist_to_old_burn, na.rm = TRUE),
		indiv_step_id = NA
	), by = id]

	new_data[, h1_new_burn := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x :=  seq(from = 0, to = 20000, length.out = N), by = id]
}
