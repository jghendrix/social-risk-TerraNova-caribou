#' @title Predict H1 distance to old burn at population level for social interaction
#' @export
#' @author Jack G Hendrix
predict_h1_old_burn_social_p <- function(DT, popn, social) {

	N <- 100L
	DT %<>% filter(season == "winter",
								 !is.na(in_group))
	popn %<>% slice(rep(1:n(), each = 100))


	distance_old <- seq(from = 0, to = 20000, length.out = N)
	new_data <- as.data.frame(distance_old) %>%
		mutate(dist_old = log(distance_old + 1),
					 sl = log(mean(DT$sl_)),
					 forest = mean(DT$prop_forest, na.rm = T),
					 dist_new = log(median(DT$dist_to_new_burn, na.rm = T) + 1))

	new <- cbind(new_data, popn)
	setDT(new)

	if(social == "alone") {

		new[, h1_old_mean :=
					sl*sl_B +
					forest*forest_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					forest*forestXalone_mean +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]

		new[, h1_old_min :=
					h1_old_mean -
					dist_old*dist_old_B +
					dist_old*old_B_min]

		new[, h1_old_max :=
					h1_old_mean -
					dist_old*dist_old_B +
					dist_old*old_B_max]

		new[, x := seq(from = 0, to = 20, length.out = N)]

	}

	else {
		new[, h1_old_min :=
					sl*sl_B +
					forest*forest_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					dist_new*newXdyad_mean +
					dist_old*oldXdyad_min +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]

		new[, h1_old_mean :=
					sl*sl_B +
					forest*forest_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					dist_new*newXdyad_mean +
					dist_old*oldXdyad_mean +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]

		new[, h1_old_max :=
					sl*sl_B +
					forest*forest_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					dist_new*newXdyad_mean +
					dist_old*oldXdyad_max +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]

		new[, x := seq(from = 0, to = 20, length.out = N)]

	}
}
