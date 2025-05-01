#' @title Predict H2
#' @export
#' @author Jack G Hendrix
predict_h2_p <- function(DT, popn, model, predictor, sociality) {

	# default value for ref is to use the mean forest cover (when calculating RSS for distance to burns or roads)
	# if calculating RSS for forest itself, we want to use zero as ref level

if(model == "fire") {

	DT %<>% summarise(sl = log(mean(DT$sl_)),
										forest = ifelse(predictor == "forest", 0,
																		mean(DT$prop_forest, na.rm = T)),
										dist_new = log(median(DT$dist_to_new_burn,
																					na.rm = T) + 1),
										dist_old = log(median(DT$dist_to_old_burn,
																					na.rm = T) + 1))

	new <- cbind(DT, popn)
	setDT(new)

	if(sociality == "alone") {

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

	if(predictor == "forest") {

		new[, h2_min :=
					h2_mean - forest*forestXalone_mean +
					forest*forestXalone_min
		]

		new[, h2_max :=
					h2_mean - forest*forestXalone_mean +
					forest*forestXalone_max
		]
}
		else {
			new[, h2_min := h2_mean]
			new[, h2_max := h2_mean]
		}
	 }
	else {

		new[, h2_mean :=
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

		if(predictor == "new burn") {

			new[, h2_min :=
						h2_mean - dist_new*newXdyad_mean +
						dist_new*newXdyad_min
					]
			new[, h2_max :=
						h2_mean - dist_new*newXdyad_mean +
						dist_new*newXdyad_max
					]
}
		else {

			if(predictor == "old burn") {

			new[, h2_min :=
						h2_mean - dist_old*oldXdyad_mean +
						dist_old*oldXdyad_min
			]
			new[, h2_max :=
						h2_mean - dist_old*oldXdyad_mean +
						dist_old*oldXdyad_max
		]

		}
			else{
				new[, h2_min := h2_mean]
				new[, h2_max := h2_mean]
							}

		}

		}

}

	else{

	DT %<>% summarise(sl = log(mean(DT$sl_)),
										forest = ifelse(predictor == "forest", 0,
																		mean(DT$prop_forest, na.rm = T)),
										dist_tch = log(median(DT$dist_to_tch,
																					na.rm = T) + 1),
										dist_minor = log(median(DT$dist_to_minor,
																					na.rm = T) + 1))

	new <- cbind(DT, popn)
	setDT(new)

	if(sociality == "alone") {

		new[, h2_mean :=
					sl*sl_B +
					forest*forest_B +
					forest*forestXalone_mean +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					sl*forest*slXforest +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]

		if(predictor == "forest") {

			new[, h2_min :=
						h2_mean - forest*forestXalone_mean +
						forest*forestXalone_min
			]

			new[, h2_max :=
						h2_mean - forest*forestXalone_mean +
						forest*forestXalone_max
			]
}
			else{
				new[, h2_min := h2_mean]
				new[, h2_max := h2_mean]
			}

	}

	else{

		new[, h2_mean :=
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

		if(predictor == "tch") {

			new[, h2_min :=
						h2_mean - dist_tch*tchXdyad_mean +
						dist_tch*tchXdyad_min
			]
			new[, h2_max :=
						h2_mean - dist_tch*tchXdyad_mean +
						dist_tch*tchXdyad_max
			]
}
			else {

				if(predictor == "minor") {

					new[, h2_min :=
								h2_mean - dist_minor*minorXdyad_mean +
								dist_minor*minorXdyad_min
					]
					new[, h2_max :=
								h2_mean - dist_minor*minorXdyad_mean +
								dist_minor*minorXdyad_max
					]

				}
				else{
					new[, h2_min := h2_mean]
					new[, h2_max := h2_mean]
				}

			}

		}
	}

	return(new)
}
