#' @title Predict H1 distance to TCH at population level for social interaction
#' @export
#' @author Jack G Hendrix
predict_h1_minor_social_p <- function(DT, popn, social) {

	N <- 100L
	DT %<>% filter(season == "winter",
								 !is.na(in_group))
	popn %<>% slice(rep(1:n(), each = 100))


	distance_minor <- seq(from = 0, to = 20000, length.out = N)
	new_data <- as.data.frame(distance_minor) %>%
		mutate(dist_minor = log(distance_minor + 1),
					 sl = log(mean(DT$sl_)),
					 forest = mean(DT$prop_forest, na.rm = T),
					 dist_tch = log(median(DT$dist_to_tch, na.rm = T) + 1))

	new <- cbind(new_data, popn)
	setDT(new)

	if(social == "alone") {

		new[, h1_minor_mean :=
					sl*sl_B +
					forest*forest_B +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					forest*forestXalone_mean +
					sl*forest*slXforest +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]

		new[, h1_minor_min :=
					h1_minor_mean -
					dist_minor*dist_minor_B +
					dist_minor*minor_B_min]

		new[, h1_minor_max :=
					h1_minor_mean -
					dist_minor*dist_minor_B +
					dist_minor*minor_B_max]

		new[, x := seq(from = 0, to = 20, length.out = N)]

	}

	else {
		new[, h1_minor_min :=
					sl*sl_B +
					forest*forest_B +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					dist_tch*tchXdyad_mean +
					dist_minor*minorXdyad_min +
					sl*forest*slXforest +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]

		new[, h1_minor_mean :=
					sl*sl_B +
					forest*forest_B +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					dist_tch*tchXdyad_mean +
					dist_minor*minorXdyad_mean +
					sl*forest*slXforest +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]

		new[, h1_minor_max :=
					sl*sl_B +
					forest*forest_B +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					dist_tch*tchXdyad_mean +
					dist_minor*minorXdyad_max +
					sl*forest*slXforest +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]

		new[, x := seq(from = 0, to = 20, length.out = N)]

	}
}
