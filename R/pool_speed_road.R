#' @title Comparing RSS and speed by predictor/season
#' @export
#' @author Jack G Hendrix
pool_speed <- function(option, speed_a, speed_b, speed_s_a, speed_s_b) {

# For each speed df, take the max and min x-values

if (option == "fire") {
	speed_a %<>% dplyr::filter(x == 1 | x == 5000) %>%
		mutate(season = "annual",
					 pred = "new")
	speed_b %<>% dplyr::filter(x == 1 | x == 5000)  %>%
		mutate(season = "annual",
					 pred = "old")
	speed_s_a %<>% dplyr::filter(x == 1 | x == 5000)  %>%
		mutate(pred = "new")
	speed_s_b %<>% dplyr::filter(x == 1 | x == 5000)  %>%
		mutate(pred = "old")

}
else {
			speed_a %<>% dplyr::filter(x == 1 | x == 5000) %>%
		mutate(season = "annual",
					 pred = "tch")
	speed_b %<>% dplyr::filter(x == 1 | x == 5000)  %>%
		mutate(season = "annual",
					 pred = "minor")
	speed_s_a %<>% dplyr::filter(x == 1 | x == 5000)  %>%
		mutate(pred = "tch")
	speed_s_b %<>% dplyr::filter(x == 1 | x == 5000)  %>%
		mutate(pred = "minor")
}

pooled <- rbind(speed_a, speed_s_a, speed_b, speed_s_b) %>%
	group_by(id, season, pred) %>%
	mutate(diff_spd = spd - dplyr::lead(spd)) %>%
	dplyr::filter(!is.na(diff_spd)) %>%
	dplyr::select(-c(c(2,3)))



}
