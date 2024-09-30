#' @title Model of caribou responses to burns, including dyad formation
#' @export
#' @author Jack G Hendrix
model_fire_social <- function(DT) {

	DT %<>% dplyr::filter(season == "winter",
												!is.na(in_group))

	glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
			I(log(sl_)):forest +
			I(log(sl_)):open +
			open +
			forest:in_group +
			open:in_group +
			I(log(dist_to_new_burn + 1)) +
			I(log(dist_to_new_burn + 1)):in_group +
			I(log(dist_to_new_burn + 1)):I(log(sl_)) +
			I(log(dist_to_old_burn + 1)) +
			I(log(dist_to_old_burn + 1)):in_group +
			I(log(dist_to_old_burn + 1)):I(log(sl_)) +
			(1 | indiv_step_id) +
			(0 + I(log(sl_)) | id) +
			(0 + I(log(sl_)):open | id) +
			(0 + I(log(sl_)):forest | id) +
			(0 + forest | id) +
			(0 + open | id) +
			(0 + I(log(dist_to_new_burn + 1)) | id) +
			(0 + I(log(dist_to_old_burn + 1)) | id) +
			(0 + I(log(dist_to_new_burn + 1)):I(log(sl_)) | id) +
			(0 + I(log(dist_to_old_burn + 1)):I(log(sl_)) | id)
		,
		data = DT,
		family = poisson(),
		map = list(theta = factor(c(NA, 1:9))),
		start = list(theta = c(log(1000), seq(0, 0, length.out = 9)))
	)
}
