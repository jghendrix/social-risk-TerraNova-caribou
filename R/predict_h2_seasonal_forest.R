#' @title Predict H2 seasonally for forest relative to 0
#' @export
#' @author Jack G Hendrix
predict_h2_seasonal_forest <- function(DT, model, predictor, season_key) {

	if(predictor == "fire")
		new_data <-		DT[, .(
			sl_ = mean(sl_),
			prop_forest = 0,
			dist_to_new_burn = median(dist_to_new_burn, na.rm = TRUE),
			dist_to_old_burn = median(dist_to_old_burn, na.rm = TRUE),
			indiv_step_id = NA),
			by = id]

	if(predictor == "road")
		new_data <-	DT[, .(
			sl_ = mean(sl_),
			prop_forest = 0,
			dist_to_tch = median(dist_to_tch, na.rm = TRUE),
			dist_to_minor = median(dist_to_minor, na.rm = TRUE),
			indiv_step_id = NA),
			by = id]


	new_data[, paste0("h2_s_", predictor) := predict(model, .SD, type = 'link', re.form = NULL)]
	new_data[, season := season_key$season]
}
