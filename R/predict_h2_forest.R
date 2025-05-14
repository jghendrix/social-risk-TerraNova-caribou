#' @title Predict H2 for 0 forest
#' @export
#' @author Jack G Hendrix
predict_h2_forest <- function(DT, model, predictor) {

	if(predictor == "fire") {
	new_data <- DT[, .(
		sl_ = mean(sl_),
		prop_forest = 1,
		dist_to_new_burn = median(dist_to_new_burn, na.rm = T),
		dist_to_old_burn = median(dist_to_old_burn, na.rm = T),
		indiv_step_id = NA),
		by = id]

	new_data[, h2 := predict(model, .SD, type = 'link', re.form = NULL)]
	return(new_data)
	}

	if(predictor == "road") {
		new_data <-	DT[, .(
			sl_ = mean(sl_),
			prop_forest = 1,
			dist_to_tch = median(dist_to_tch, na.rm = T),
			dist_to_minor = median(dist_to_minor, na.rm = T),
			indiv_step_id = NA),
			by = id]

		new_data[, h2 := predict(model, .SD, type = 'link', re.form = NULL)]
		return(new_data)
	}

	if(predictor == "fire alone") {
		new_data <-	DT[, .(
			sl_ = mean(sl_),
			prop_forest = 1,
			in_group = "alone",
			dist_to_new_burn = median(dist_to_new_burn, na.rm = T),
			dist_to_old_burn = median(dist_to_old_burn, na.rm = T),
			indiv_step_id = NA),
			by = id]

		sl <- 0.316796
		new <- -0.065618
		old <- 0.023413
		newsl <- 0.006571
		oldsl <- -0.025328

		new_data[, h2 :=
						 	log(sl_)*sl +
						 	log(dist_to_new_burn + 1)*new +
						 	log(dist_to_old_burn + 1)*old +
						 	log(sl_)*log(dist_to_new_burn + 1)*newsl +
						 	log(sl_)*log(dist_to_old_burn + 1)*oldsl
		]
		return(new_data)
	}

	if(predictor == "fire dyad") {
		new_data <-	DT[, .(
			sl_ = mean(sl_),
			prop_forest = 1,
			in_group = "dyad",
			dist_to_new_burn = median(dist_to_new_burn, na.rm = T),
			dist_to_old_burn = median(dist_to_old_burn, na.rm = T),
			indiv_step_id = NA),
			by = id]

		sl <- 0.316796
		new <- -0.065618
		old <- 0.023413
		newdyad <- -0.120320
		olddyad <- -0.016986
		newsl <- 0.006571
		oldsl <- -0.025328

		new_data[, h2 :=
						 	log(sl_)*sl +
						 	log(dist_to_new_burn + 1)*new +
						 	log(dist_to_old_burn + 1)*old +
						 	log(dist_to_new_burn + 1)*newdyad +
						 	log(dist_to_old_burn + 1)*olddyad +
						 	log(sl_)*log(dist_to_new_burn + 1)*newsl +
						 	log(sl_)*log(dist_to_old_burn + 1)*oldsl
		]
		return(new_data)
	}

	if(predictor == "road alone") {
		new_data <- DT[, .(
			sl_ = mean(sl_),
			prop_forest = 1,
			in_group = "alone",
			dist_to_tch = median(dist_to_tch, na.rm = T),
			dist_to_minor = median(dist_to_minor, na.rm = T),
			indiv_step_id = NA
		), by = id]

		sl<- 0.44515
		f <- 0.87392
		tch <- 2.10887
		m <- 0.19497
		falone <- 0.68487
		fsl <- -0.29053
		tchsl <- -0.06906
		msl <- 0.04187

		new_data[, h2 :=
						 	log(sl_)*sl +
						 	prop_forest*f +
						 	log(dist_to_tch + 1)*tch +
						 	log(dist_to_minor + 1)*m +
						 	prop_forest*falone +
						 	log(sl_)*prop_forest*fsl +
						 	log(sl_)*log(dist_to_tch + 1)*tchsl +
						 	log(sl_)*log(dist_to_minor + 1)*msl
		]
		return(new_data)
	}

	if(predictor == "road dyad") {
		new_data <- DT[, .(
			sl_ = mean(sl_),
			prop_forest = 1,
			in_group = "dyad",
			dist_to_tch = median(dist_to_tch, na.rm = T),
			dist_to_minor = median(dist_to_minor, na.rm = T),
			indiv_step_id = NA
		), by = id]

		sl <- 0.44515
		f <- 0.87392
		tch <- 2.10887
		m <- 0.19497
		tchdyad <- -0.47487
		mdyad <- 0.3187
		fsl <- -0.29053
		tchsl <- -0.06906
		msl <- 0.04187

		new_data[, h2 :=
						 	log(sl_)*sl +
						 	prop_forest*f +
						 	log(dist_to_tch + 1)*tch +
						 	log(dist_to_minor + 1)*m +
						 	log(dist_to_tch + 1)*tchdyad +
						 	log(dist_to_minor + 1)*mdyad +
						 	log(sl_)*prop_forest*fsl +
						 	log(sl_)*log(dist_to_tch + 1)*tchsl +
						 	log(sl_)*log(dist_to_minor + 1)*msl
		]
		return(new_data)
	}

}
