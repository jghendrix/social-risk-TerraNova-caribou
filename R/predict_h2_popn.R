#' @title Predict H2
#' @export
#' @author Jack G Hendrix
predict_h2_popn <- function(DT, popn, predictor) {

	# forest is set to 0 as the reference level so we can ignore it

	if(predictor == "fire alone") {

		DT %<>% summarise(sl = log(mean(DT$sl_)),
											dist_new = log(median(DT$dist_to_new_burn, na.rm = T) + 1),
											dist_old = log(median(DT$dist_to_old_burn, na.rm = T) + 1))

		new <- cbind(DT, popn)
		setDT(new)

		new[, h2_min :=
					sl*sl_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]
		new[, h2_mean := h2_min]
		new[, h2_max := h2_min]

		return(new)
}
	if(predictor == "fire dyad") {

		DT %<>% summarise(sl = log(mean(DT$sl_)),
											dist_new = log(median(DT$dist_to_new_burn, na.rm = T) + 1),
											dist_old = log(median(DT$dist_to_old_burn, na.rm = T) + 1))

		new <- cbind(DT, popn)
		setDT(new)

		new[, h2_min :=
					sl*sl_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					dist_new*newXdyad_min +
					dist_old*oldXdyad_min +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]
		new[, h2_mean :=
					sl*sl_B +
					dist_new*dist_new_B +
					dist_old*dist_old_B +
					dist_new*newXdyad_mean +
					dist_old*oldXdyad_mean +
					sl*dist_new*slXnew +
					sl*dist_old*slXold
		]
		new[, h2_max :=
						sl*sl_B +
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

		DT %<>% summarise(sl = log(mean(DT$sl_)),
											dist_tch = log(median(DT$dist_to_tch, na.rm = T) + 1),
											dist_minor = log(median(DT$dist_to_minor, na.rm = T) + 1))

		new <- cbind(DT, popn)
		setDT(new)

		new[, h2_min :=
					sl*sl_B +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]
		new[, h2_mean := h2_min]
		new[, h2_max := h2_min]

		return(new)
	}

	if(predictor == "road dyad") {

		DT %<>% summarise(sl = log(mean(DT$sl_)),
											dist_tch = log(median(DT$dist_to_tch, na.rm = T) + 1),
											dist_minor = log(median(DT$dist_to_minor, na.rm = T) + 1))

		tch <- cbind(DT, popn)
		setDT(tch)

		tch[, h2_min :=
					sl*sl_B +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					dist_tch*tchXdyad_min +
					dist_minor*minorXdyad_min +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]
		tch[, h2_mean :=
					sl*sl_B +
					dist_tch*dist_tch_B +
					dist_minor*dist_minor_B +
					dist_tch*tchXdyad_mean +
					dist_minor*minorXdyad_mean +
					sl*dist_tch*slXtch +
					sl*dist_minor*slXminor
		]
		tch[, h2_max :=
					sl*sl_B +
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
