join_rss_p <- function(min_alone, min_dyad,
													mean_alone, mean_dyad,
													max_alone, max_dyad) {

	min_alone %<>% dplyr::select(x, rss) %>%
		mutate(social = "alone", SE = "min")
	min_dyad %<>%  dplyr::select(x, rss) %>%
		mutate(social = "dyad", 	SE = "min")
	mean_alone %<>%  dplyr::select(x, rss) %>%
		mutate(social = "alone", SE = "mean")
	mean_dyad %<>% dplyr::select(x, rss) %>%
		mutate(social = "dyad", SE = "mean")
	max_alone %<>% dplyr::select(x, rss) %>%
		mutate(social = "alone", SE = "max")
	max_dyad %<>%  dplyr::select(x, rss) %>%
		mutate(social = "dyad", SE = "max")

	DT <- rbind(min_alone, min_dyad,
							mean_alone, mean_dyad,
							max_alone, max_dyad)
	return(DT)

}
