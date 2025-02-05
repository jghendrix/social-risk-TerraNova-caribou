## social response to linear features

model_road_social <- function(DT) {

	DT %<>% dplyr::filter(season == "winter",
												!is.na(in_group))

	glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
			prop_forest +
			prop_forest:in_group +
			I(log(dist_to_tch + 1)) +
			I(log(dist_to_tch + 1)):in_group +
			I(log(dist_to_minor + 1)) +
			I(log(dist_to_minor + 1)):in_group +
			I(log(sl_)):prop_forest +
			I(log(sl_)):I(log(dist_to_tch + 1)) +
			I(log(sl_)):I(log(dist_to_minor + 1)) +
			(1 | indiv_step_id) +
			(0 + I(log(sl_)) | id) +
			(0 + prop_forest | id) +
			(0 + I(log(dist_to_tch + 1)) | id) +
			(0 + I(log(dist_to_minor + 1)) | id) +
			(0 + I(log(sl_)):prop_forest | id) +
			(0 + I(log(dist_to_tch + 1)):I(log(sl_)) | id) +
			(0 + I(log(dist_to_minor + 1)):I(log(sl_)) | id)
		,
		data = DT,
		family = poisson(),
		control = glmmTMBControl(rank_check = "adjust"),
		map = list(theta = factor(c(NA, 1:7))),
		start = list(theta = c(log(1000), seq(0, 0, length.out = 7)))
	)
}
