#' @title Calculate speed from social model when not in dyad
#' @export
#' @author Jack G Hendrix
calc_speed_alone <- function(DT, covariate, seq, df) {


	DT %<>% mutate(new_minor_sl = (.[[2]]) + (.[[5]]),
								 old_tch_sl = (.[[3]]) + (.[[7]]))

	mean_soc <- df %>% dplyr::filter(season == "winter") %>%
		mutate(alone = ifelse(in_group == "alone", 1, 0),
					 dyad = ifelse(in_group == "dyad", 1, 0)) %>%
		group_by(id) %>%
		summarise(mean_alone = mean(alone, na.rm = T),
							mean_dyad = mean(dyad, na.rm = T))

	DT <- left_join(DT, mean_soc, by = "id")

## fire model estimates: ----

	if(covariate == "forest fire")
	DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																`I(log(sl_)):forest`*seq +
																`I(log(sl_)):open`*mean_open +
																new_minor_sl*log(med_new) +
																old_tch_sl*log(med_old) +
																`I(log(sl_)):in_groupalone`*mean_alone
	)*(scale))),
	x = list(list(seq))),
	by=.(id)]

	if(covariate == "open fire")
	DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																`I(log(sl_)):forest`*mean_forest +
																`I(log(sl_)):open`*seq +
																new_minor_sl*log(med_new) +
																old_tch_sl*log(med_old) +
																`I(log(sl_)):in_groupalone`*mean_alone
	)*(scale))),
	x = list(list(seq))),
	by=.(id)]

	if(covariate == "dist_to_new_burn")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*mean_forest +
																	`I(log(sl_)):open`*mean_open +
																	new_minor_sl*log(1 + seq) +
																	old_tch_sl*log(med_old) +
																	`I(log(sl_)):in_groupalone`*mean_alone
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

	if(covariate == "dist_to_old_burn")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*mean_forest +
																	`I(log(sl_)):open`*mean_open +
																	new_minor_sl*log(med_new) +
																	old_tch_sl*log(1 + seq) +
																	`I(log(sl_)):in_groupalone`*mean_alone
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]

## road model estimates: ----

		if(covariate == "forest road")
			DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																		`I(log(sl_)):forest`*seq +
																		`I(log(sl_)):open`*mean_open +
																		new_minor_sl*log(med_minor) +
																		old_tch_sl*log(med_tch) +
																		`I(log(sl_)):in_groupalone`*mean_alone
			)*(scale))),
			x = list(list(seq))),
			by=.(id)]

		if(covariate == "open road")
			DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																		`I(log(sl_)):forest`*mean_forest +
																		`I(log(sl_)):open`*seq +
																		new_minor_sl*log(med_minor) +
																		old_tch_sl*log(med_tch) +
																		`I(log(sl_)):in_groupalone`*mean_alone
			)*(scale))),
			x = list(list(seq))),
			by=.(id)]



	if(covariate == "dist_to_tch")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*mean_forest +
																	`I(log(sl_)):open`*mean_open +
																	new_minor_sl*log(med_minor) +
																	old_tch_sl*log(1 + seq) +
																	`I(log(sl_)):in_groupalone`*mean_alone
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]



	if(covariate == "dist_to_minor")
		DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
																	`I(log(sl_)):forest`*mean_forest +
																	`I(log(sl_)):open`*mean_open +
																	new_minor_sl*log(1 + seq) +
																	old_tch_sl*log(med_tch) +
																	`I(log(sl_)):in_groupalone`*mean_alone
		)*(scale))),
		x = list(list(seq))),
		by=.(id)]


	move <- DT[, .(spd = unlist(spd), x = unlist(x)), by=.(id)]
	move
}
