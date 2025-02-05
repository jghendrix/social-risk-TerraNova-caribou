## response to linear features

model_roads_bin <- function(DT) {

	if(length(unique(DT$season)) > 1) {

		glmmTMB(
			case_ ~ -1 +
				I(log(sl_)) +
				prop_forest +
				I(log(dist_to_tch + 1)) +
				I(log(dist_to_minor + 1)) +
				prop_forest:I(log(sl_)) +
				I(log(dist_to_tch + 1)):I(log(sl_)) +
				I(log(dist_to_minor + 1)):I(log(sl_)) +
				(1 | indiv_step_id) +
				(0 + prop_forest | id) +
				(0 + I(log(sl_)) | id) +
				(0 + I(log(sl_)):prop_forest | id) +
				(0 + I(log(dist_to_tch + 1)) | id) +
				(0 + I(log(dist_to_minor + 1)) | id) +
				(0 + I(log(dist_to_tch + 1)):I(log(sl_)) | id) +
				(0 + I(log(dist_to_minor + 1)):I(log(sl_)) | id)
			,
			data = DT,
			family = poisson(),
			map = list(theta = factor(c(NA, 1:7))),
			start = list(theta = c(log(1000), seq(0, 0, length.out = 7)))
		)

	}

	else {

	if(unique(DT$season) == "calving") {

glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
			prop_forest +
			I(log(dist_to_tch + 1)) +
			I(log(dist_to_minor + 1)) +
			prop_forest:I(log(sl_)) +
			I(log(dist_to_tch + 1)):I(log(sl_)) +
			I(log(dist_to_minor + 1)):I(log(sl_)) +
			(1 | indiv_step_id) +
			(0 + prop_forest | id) +
			(0 + I(log(sl_)) | id) +
			(0 + I(log(sl_)):prop_forest | id) +
			(0 + I(log(dist_to_tch + 1)) | id) +
			(0 + I(log(dist_to_minor + 1)) | id) +
			(0 + I(log(dist_to_tch + 1)):I(log(sl_)) | id) +
			(0 + I(log(dist_to_minor + 1)):I(log(sl_)) | id)
		,
		data = DT,
		family = poisson(),
		control = glmmTMBControl(optimizer = optim, rank_check = "adjust", optArgs = list(method = "BFGS")), # the calving road model won't converge with default optimizer, if this optimizer is used instead it will. but it breaks the autumn model. Every other analysis uses the default optimizer, so just modify it for the calving model, otherwise comment this line out
		map = list(theta = factor(c(NA, 1:7))),
		start = list(theta = c(log(1000), seq(0, 0, length.out = 7)))
	)
	}

	else {

		glmmTMB(
			case_ ~ -1 +
				I(log(sl_)) +
				prop_forest +
				I(log(dist_to_tch + 1)) +
				I(log(dist_to_minor + 1)) +
				prop_forest:I(log(sl_)) +
				I(log(dist_to_tch + 1)):I(log(sl_)) +
				I(log(dist_to_minor + 1)):I(log(sl_)) +
				(1 | indiv_step_id) +
				(0 + prop_forest | id) +
				(0 + I(log(sl_)) | id) +
				(0 + I(log(sl_)):prop_forest | id) +
				(0 + I(log(dist_to_tch + 1)) | id) +
				(0 + I(log(dist_to_minor + 1)) | id) +
				(0 + I(log(dist_to_tch + 1)):I(log(sl_)) | id) +
				(0 + I(log(dist_to_minor + 1)):I(log(sl_)) | id)
			,
			data = DT,
			family = poisson(),
			map = list(theta = factor(c(NA, 1:7))),
			start = list(theta = c(log(1000), seq(0, 0, length.out = 7)))
		)

	}
}

}
