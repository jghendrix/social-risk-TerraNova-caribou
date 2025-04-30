#' @title Predict H1 forest
#' @export
#' @author Julie W. Turner, Alec L. Robitaille
predict_h1_forest_social_popn <- function(DT, popn, predictor, sociality) {

	DT %<>% filter(season == "winter",
								 !is.na(in_group))
	N <- 100L

	forest <- seq(from = 0, to = 1, length.out = N)
	popn %<>% slice(rep(1:n(), each = 100))

	if(predictor == "fire") {

		if(sociality == "alone") {

			new_data <- as.data.frame(forest) %>%
				mutate(sl = log(mean(DT$sl_)),
							 dist_new = log(median(DT$dist_to_new_burn, na.rm = T) + 1),
							 dist_old = log(median(DT$dist_to_old_burn, na.rm = T) + 1))

			new <- cbind(new_data, popn)
			setDT(new)

			new[, h1_forest_min :=
							 	sl*sl_B +
							 	forest*forest_B +
							 	dist_new*dist_new_B +
							 	dist_old*dist_old_B +
							 	forest*forestXalone_min +
							 	sl*forest*slXforest +
							 	sl*dist_new*slXnew +
							 	sl*dist_old*slXold
			]

			new[, h1_forest_mean :=
						sl*sl_B +
						forest*forest_B +
						dist_new*dist_new_B +
						dist_old*dist_old_B +
						forest*forestXalone_mean +
						sl*forest*slXforest +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]
			new[, h1_forest_max :=
							sl*sl_B +
							forest*forest_B +
							dist_new*dist_new_B +
							dist_old*dist_old_B +
							forest*forestXalone_max +
							sl*forest*slXforest +
							sl*dist_new*slXnew +
							sl*dist_old*slXold
			]

			new[, x := seq(from = 0, to = 1, length.out = N)]

		}

		else {
			new_data <- as.data.frame(forest) %>%
				mutate(sl = log(mean(DT$sl_)),
							 dist_new = log(median(DT$dist_to_new_burn, na.rm = T) + 1),
							 dist_old = log(median(DT$dist_to_old_burn, na.rm = T) + 1))

			new <- cbind(new_data, popn)
			setDT(new)



			new[, h1_forest_min :=
						sl*sl_B +
						forest*forest_B +
						dist_new*dist_new_B +
						dist_old*dist_old_B +
						dist_new*newXdyad_min +
						dist_old*oldXdyad_min +
						sl*forest*slXforest +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]

				new[, h1_forest_mean :=
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

			new[, h1_forest_max :=
						sl*sl_B +
						forest*forest_B +
						dist_new*dist_new_B +
						dist_old*dist_old_B +
						dist_new*newXdyad_max +
						dist_old*oldXdyad_max +
						sl*forest*slXforest +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]

			new[, x := seq(from = 0, to = 1, length.out = N)]

		}
	}

	# and if not fire, then it's road
	else {

		if(sociality == "alone") {

			new_data <- as.data.frame(forest) %>%
				mutate(sl = log(mean(DT$sl_)),
							 dist_tch = log(median(DT$dist_to_tch, na.rm = T) + 1),
							 dist_minor = log(median(DT$dist_to_minor, na.rm = T) + 1))

			new <- cbind(new_data, popn)
			setDT(new)

			new[, h1_forest_min :=
						sl*sl_B +
						forest*forest_B +
						dist_tch*dist_tch_B +
						dist_tch*dist_tch_B +
						forest*forestXalone_min +
						sl*forest*slXforest +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]

			new[, h1_forest_mean :=
						sl*sl_B +
						forest*forest_B +
						dist_tch*dist_tch_B +
						dist_tch*dist_tch_B +
						forest*forestXalone_mean +
						sl*forest*slXforest +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]
			new[, h1_forest_max :=
						sl*sl_B +
						forest*forest_B +
						dist_tch*dist_tch_B +
						dist_tch*dist_tch_B +
						forest*forestXalone_max +
						sl*forest*slXforest +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
			]

			new[, x := seq(from = 0, to = 1, length.out = N)]

		}

		else {
			new_data <- as.data.frame(forest) %>%
				mutate(sl = log(mean(DT$sl_)),
							 dist_tch = log(median(DT$dist_to_minor, na.rm = T) + 1),
							 dist_minor = log(median(DT$dist_to_minor, na.rm = T) + 1))

			new <- cbind(new_data, popn)
			setDT(new)

			new[, h1_forest_min :=
						sl*sl_B +
						forest*forest_B +
						dist_tch*dist_tch_B +
						dist_minor*dist_minor_B +
						dist_tch*tchXdyad_min +
						dist_minor*minorXdyad_min +
						sl*forest*slXforest +
						sl*dist_tch*slXtch +
						sl*dist_minor*slXminor
			]

			new[, h1_forest_mean :=
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

			new[, h1_forest_max :=
						sl*sl_B +
						forest*forest_B +
						dist_tch*dist_tch_B +
						dist_minor*dist_minor_B +
						dist_tch*tchXdyad_max +
						dist_minor*minorXdyad_max +
						sl*forest*slXforest +
						sl*dist_tch*slXtch +
						sl*dist_minor*slXminor
			]

			new[, x := seq(from = 0, to = 1, length.out = N)]

		}
	}


}
