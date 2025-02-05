#' @title Model of caribou responses to burns
#' @export
#' @author Jack G Hendrix
model_fire_bin <- function(DT) {

	glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
			prop_forest +
			I(log(dist_to_new_burn + 1)) +
			I(log(dist_to_old_burn + 1)) +
			prop_forest:I(log(sl_)) +
			I(log(dist_to_new_burn + 1)):I(log(sl_)) +
			I(log(dist_to_old_burn + 1)):I(log(sl_)) +
			(1 | indiv_step_id) +
			(0 + prop_forest | id) +
			(0 + I(log(sl_)) | id) +
			(0 + I(log(sl_)):prop_forest | id) +
			(0 + I(log(dist_to_new_burn + 1)) | id) +
			(0 + I(log(dist_to_old_burn + 1)) | id) +
			(0 + I(log(dist_to_new_burn + 1)):I(log(sl_)) | id) +
			(0 + I(log(dist_to_old_burn + 1)):I(log(sl_)) | id)
		,
		data = DT,
		family = poisson(),
		map = list(theta = factor(c(NA, 1:7))),
	start = list(theta = c(log(1000), seq(0, 0, length.out = 7)))
	)
}
