#' @title Comparing RSS and speed by predictor/season
#' @export
#' @author Jack G Hendrix
pool_rss <- function(option, rss_a, rss_b, rss_s_a, rss_s_b) {

# For each RSS df, take the max and min x-values
if(option == "fire") {
		rss_a %<>% dplyr::filter(x == 0 | x == 20000) %>%
		dplyr::select(-c(c(3,4))) %>%
		mutate(season = "annual",
					 pred = "new")
	rss_b %<>% dplyr::filter(x == 0 | x == 20000)  %>%
		dplyr::select(-c(c(3,4))) %>%
		mutate(season = "annual",
					 pred = "old")
	rss_s_a %<>% dplyr::filter(x == 0 | x == 20000)  %>%
		dplyr::select(-c(c(4,5))) %>%
		mutate(pred = "new")
	rss_s_b %<>% dplyr::filter(x == 0 | x == 20000)  %>%
		dplyr::select(-c(c(4,5))) %>%
		mutate(pred = "old")

}
	else {
		rss_a %<>% dplyr::filter(x == 0 | x == 20000) %>%
			dplyr::select(-c(c(3,4))) %>%
			mutate(season = "annual",
						 pred = "tch")
		rss_b %<>% dplyr::filter(x == 0 | x == 20000)  %>%
			dplyr::select(-c(c(3,4))) %>%
			mutate(season = "annual",
						 pred = "minor")
		rss_s_a %<>% dplyr::filter(x == 0 | x == 20000)  %>%
			dplyr::select(-c(c(4,5))) %>%
			mutate(pred = "tch")
		rss_s_b %<>% dplyr::filter(x == 0 | x == 20000)  %>%
			dplyr::select(-c(c(4,5))) %>%
			mutate(pred = "minor")
	}

pooled <- rbind(rss_a, rss_s_a, rss_b, rss_s_b, fill = TRUE)
pooled %<>%
	group_by(id, season, pred) %>%
	mutate(diff_rss = rss - dplyr::lead(rss)) %>%
	dplyr::filter(!is.na(diff_rss)) %>%
	dplyr::select(-c(c(2,3)))


return(pooled)
}
