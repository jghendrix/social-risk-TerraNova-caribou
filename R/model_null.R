#' @title null Model of caribou
#' @export
#' @author Jack G Hendrix
model_null <- function(DT, predictor) {

	if(predictor == "social fire") {

		DT %<>% dplyr::filter(season == "winter",
													!is.na(in_group))
	glmmTMB(
			case_ ~ -1 +
				I(log(sl_)) +
				I(log(dist_to_new_burn + 1)) +
				I(log(dist_to_new_burn + 1)):in_group +
				(1 | indiv_step_id)
				,
			data = DT,
			family = poisson(),
			map = list(theta = factor(c(1))),
			start = list(theta = c(log(1000)))
		)
	}
	else {
		if(predictor == "social road") {

			DT %<>% dplyr::filter(season == "winter",
														!is.na(in_group))
			glmmTMB(
				case_ ~ -1 +
					I(log(sl_)) +
					I(log(dist_to_tch + 1)) +
					I(log(dist_to_tch + 1)):in_group +
					(1 | indiv_step_id)
				,
				data = DT,
				family = poisson(),
				map = list(theta = factor(c(1))),
				start = list(theta = c(log(1000)))
			)
		}
	else {
	if(predictor == "fire") {
mf <- glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
			I(log(dist_to_new_burn + 1)) +
			I(log(dist_to_old_burn + 1)) +
			prop_forest +
			(1 | indiv_step_id)
		,
		data = DT,
		family = poisson(),
		map = list(theta = factor(c(1))),
		start = list(theta = c(log(1000)))
	)
}

	else {
		if(predictor == "road") {

	r <-		glmmTMB(
				case_ ~ -1 +
					I(log(sl_)) +
					I(log(dist_to_tch + 1)) +
					I(log(dist_to_minor + 1)) +
					prop_forest +
					(1 | indiv_step_id)
					,
				data = DT,
				family = poisson(),
				map = list(theta = factor(c(1))),
				start = list(theta = c(log(1000)))
			)
		}
		else {
		fm <-	glmmTMB(
				case_ ~ -1 +
					I(log(sl_)) +
					prop_forest +
					(1 | indiv_step_id)
					,
				data = DT,
				family = poisson(),
				map = list(theta = factor(c(0))),
				start = list(theta = c(log(1000))))

				}
			}
		}
	}
}
