#' @title Prepare speed seasonally
#' @export
#' @author Jack G Hendrix
prepare_speed_seasonal <- function(DT, summary, model, params, season_key) {


	if(model == "fire")
		sum <- DT[,.(med_new = median(dist_to_new_burn, na.rm = T),
								 med_old = median(dist_to_old_burn, na.rm = T),
								 mean_forest = mean(forest, na.rm = T),
								 mean_open = mean(open, na.rm = T)),
							by = .(id)]

	if(model == "road")
		sum <- DT[,.(med_minor = median(dist_to_minor, na.rm = T),
								 med_tch = median(dist_to_tch, na.rm = T),
								 mean_forest = mean(forest, na.rm = T),
								 mean_open = mean(open, na.rm = T)),
							by = .(id)]

	dat.wide <- dcast(summary[term %like% 'sl'], id~ term, value.var = 'estimate')

	dat.wide <- setDT(merge(dat.wide, setDT(params)[,.(id = as.character(id),shape, scale, kappa)], by = 'id', all.x = T))
	dat.wide <- setDT(merge(dat.wide, sum, by = 'id', all.x = T))
	dat.wide[, season := season_key$season]
	dat.wide
}
