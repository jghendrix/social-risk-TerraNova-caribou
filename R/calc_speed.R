#' @title Calculate speed
#' @export
#' @author Julie W. Turner
calc_speed <- function(DT, covariate, seq) {



	## fire model estimates:

	if(covariate == "forest fire")
	DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
															`I(log(sl_)):prop_forest`*seq +
															`I(log(dist_to_new_burn + 1)):I(log(sl_))`*log(med_new + 1) +
															`I(log(dist_to_old_burn + 1)):I(log(sl_))`*log(med_old + 1)
	)*(scale))),
	x = list(list(seq))),
	by=.(id)]


	if(covariate == "dist_to_new_burn")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																`I(log(sl_)):prop_forest`*mean_forest +
																`I(log(dist_to_new_burn + 1)):I(log(sl_))`*log(1 + seq) +
																`I(log(dist_to_old_burn + 1)):I(log(sl_))`*log(med_old + 1)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "dist_to_old_burn")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):prop_forest`*mean_forest +
																	`I(log(dist_to_new_burn + 1)):I(log(sl_))`*log(med_new + 1) +
																	`I(log(dist_to_old_burn + 1)):I(log(sl_))`*log(seq + 1)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

		## road model estimates:

		if(covariate == "forest road")
			DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):prop_forest`*seq +
																	`I(log(dist_to_minor + 1)):I(log(sl_))`*log(med_minor + 1) +
																	`I(log(dist_to_tch + 1)):I(log(sl_))`*log(med_tch + 1)
			)*(scale))),
			x = list(list(seq))),
			by=.(id)]

	if(covariate == "dist_to_tch")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																`I(log(sl_)):prop_forest`*mean_forest +
																`I(log(dist_to_minor + 1)):I(log(sl_))`*log(med_minor + 1) +
																`I(log(dist_to_tch + 1)):I(log(sl_))`*log(1 + seq)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "dist_to_minor")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																`I(log(sl_)):prop_forest`*mean_forest ++
																`I(log(dist_to_minor + 1)):I(log(sl_))`*log(1 + seq) +
																`I(log(dist_to_tch + 1)):I(log(sl_))`*log(med_tch + 1)
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]


	move <- DT[, .(spd = unlist(spd), x = unlist(x)), by=.(id)]

	move %<>% mutate(spd = spd/2
							#		 ,
							#	 spd = ifelse(spd < 0, NA, spd)
								 )
}
