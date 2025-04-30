#' @title Predict H2
#' @export
#' @author Jack G Hendrix
predict_h2_p <- function(DT, popn, predictor, ref = "mean forest") {

	# default value for ref is to use the mean forest cover (when calculating RSS for distance to burns or roads)
	# if calculating RSS for forest itself, we want to use zero as ref level

	DT %<>% summarise(sl = log(mean(DT$sl_)),
										forest = ifelse(ref == "zero", 0,
																		mean(DT$prop_forest, na.rm = T)),
										dist_new = log(median(DT$dist_to_new_burn, na.rm = T) + 1),
										dist_old = log(median(DT$dist_to_old_burn, na.rm = T) + 1))

	new <- cbind(DT, popn)
	setDT(new)

	if(predictor == "fire alone") {

		new[, h2_min :=
					sl*sl_B +
					forest*forest_B +
					forest*forestXalone_min +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]

		new[, h2_mean :=
					sl*sl_B +
					forest*forest_B +
					forest*forestXalone_mean +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]
		new[, h2_max :=
					sl*sl_B +
					forest*forest_B +
					forest*forestXalone_max +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					sl*forest*slXforest +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]

		return(new)
}
	if(predictor == "fire dyad") {

	new[, h2_min :=
					sl*sl_B +
					forest*forest_B +
					sl*forest*slXforest +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					dist_new*newXdyad_min +
					dist_old*oldXdyad_min +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]
		new[, h2_mean :=
					sl*sl_B +
					forest*forest_B +
					sl*forest*slXforest +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					dist_new*newXdyad_mean +
					dist_old*oldXdyad_mean +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]
		new[, h2_max :=
						sl*sl_B +
					forest*forest_B +
					sl*forest*slXforest +
						dist_new*dist_new_B +
						dist_old*dist_old_B +
						dist_new*newXdyad_max +
						dist_old*oldXdyad_max +
						sl*dist_new*slXnew +
						sl*dist_old*slXold
		]
		return(new)
	}

	if(predictor == "road alone") {

	new[, h2_min :=
					sl*sl_B +
					forest*forest_B +
					forest*forestXalone_min +
					sl*forest*slXforest +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]
		new[, h2_mean :=
					sl*sl_B +
					forest*forest_B +
					forest*forestXalone_mean +
					sl*forest*slXforest +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
				]

		new[, h2_max :=
					sl*sl_B +
					forest*forest_B +
					forest*forestXalone_max +
					sl*forest*slXforest +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
				]

		return(new)
	}

	if(predictor == "road dyad") {

		new[, h2_min :=
					sl*sl_B +
					forest*forest_B +
					sl*forest*slXforest +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					dist_tch*tchXdyad_min +
					dist_minor*minorXdyad_min +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]
		new[, h2_mean :=
					sl*sl_B +
					forest*forest_B +
					sl*forest*slXforest +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					dist_tch*tchXdyad_mean +
					dist_minor*minorXdyad_mean +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]
		new[, h2_max :=
					sl*sl_B +
					forest*forest_B +
					sl*forest*slXforest +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					dist_tch*tchXdyad_max +
					dist_minor*minorXdyad_max +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]

		return(new)
}
}
