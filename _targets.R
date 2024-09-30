# === Targets: Terra Nova caribou iSSA workflow ----------------------------------------------
# Jack G Hendrix


# Source ------------------------------------------------------------------
targets::tar_source('R')


# Options -----------------------------------------------------------------
tar_option_set(format = 'qs')

# Variables ---------------------------------------------------------------
# Targets: prepare
id_col <- 'Animal_ID'
datetime_col <- 'DATETIME'
x_col <- 'Longitude'
y_col <- 'Lat'
epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)
tz <- 'America/St_Johns'

split_by <- id_col
seasonal_split <- "season"


# Targets: data ---------------------------------------------------------
targets_data <- c(
	tar_target(
		model_prep,
		readr::read_csv('model_prep.csv', show_col_types = FALSE)
	)
)

# Targets: annual fire model ----------------------------------------------------------
targets_fire <- c(
	tar_target(
		fire_model,
		model_fire_bin(model_prep)
	),
	tar_target(
		fire_model_check,
		model_check(fire_model)
	)
)

# Targets: seasonal fire model ----------------------------------------
targets_fire_seasonal <- c(

tar_target(
	season_prep,
	model_prep[, tar_group := .GRP, by = c('season')],
	iteration = 'group'
),
tar_target(
	season_key,
	unique(season_prep[, .SD, .SDcols = c(seasonal_split, 'tar_group')])
),
	tar_target(
		s_fire_model,
		model_fire_bin(season_prep),
		map(season_prep)
	),
	tar_target(
		s_fire_model_check,
		model_check(s_fire_model),
		map(s_fire_model)
	)
)

# Targets: fire output and effects ------------------------------------------------------------
targets_fire_effects <- c(
	tar_target(
		indiv_fire,
		indiv_estimates(fire_model)
	),
	tar_target(
		fire_boxplot,
		plot_box_horiz(indiv_fire, plot_theme(), "fire")
	),
	tar_target(
		s_indiv_fire,
		indiv_seasonal(s_fire_model, season_key),
		pattern = map(s_fire_model, season_key)
	),
	tar_target(
		s_fire_boxplot,
		plot_boxplot_seasonal(s_indiv_fire, plot_theme(), "fire"),
		pattern = map(s_indiv_fire)
	)
)

# Targets: speed from annual fire ------------------------------------------------------------
targets_speed_fire <- c(
	tar_target(
		prep_speed_fire,
		prepare_speed(
			DT = model_prep,
			summary = indiv_fire,
			model = "fire",
			params = dist_parameters
		)
	),
	tar_target(
		calc_speed_open_fire,
		calc_speed(prep_speed_fire, 'open fire', seq = 0:1)
	),
	tar_target(
		plot_speed_open_fire,
		plot_box(calc_speed_open_fire, plot_theme()) +
			labs(x = 'Open', y = 'Speed (m/2hr)')
	),
	tar_target(
		calc_speed_new_burn,
		calc_speed(prep_speed_fire, 'dist_to_new_burn', seq(1, 5000, length.out = 100L))
	),
	tar_target(
		plot_speed_new_burn,
		plot_dist(calc_speed_new_burn, plot_theme()) +
			labs(x = 'Distance to younger burns (km)', y = 'Speed (m/2hr)')
	),
	tar_target(
		calc_speed_old_burn,
		calc_speed(prep_speed_fire, 'dist_to_old_burn', seq(1, 5000, length.out = 100L))
	),
	tar_target(
		plot_speed_old_burn,
		plot_dist(calc_speed_old_burn, plot_theme()) +
			labs(x = 'Distance to older burns (km)', y = 'Speed (m/2hr)')
	),
	tar_target(
		fire_plots,
		save_plot(plot_speed_open_fire, "fire_model_speed_open",
							plot_speed_new_burn, "fire_model_speed_new_burn",
							plot_speed_old_burn, "fire_model_speed_old_burn")
	)
)

# Targets: speed from seasonal fire --------------------------
targets_speed_fire_seasonal <- c(

	tar_target(
		prep_speed_s_fire,
		prepare_speed_seasonal(
			DT = season_prep,
			summary = s_indiv_fire,
			model = "fire",
			params = dist_parameters,
			season_key = season_key
		),
		map(s_indiv_fire, season_key)
	),
	tar_target(
		calc_speed_open_s_fire,
		calc_speed_seasonal(prep_speed_s_fire, 'open', "fire", seq = 0:1, season_key),
		map(prep_speed_s_fire, season_key)
	),
	tar_target(
		plot_speed_open_s_fire,
		plot_box_seasonal_speed(calc_speed_open_s_fire, plot_theme(), "fire")
	),

	tar_target(
		calc_speed_s_new_fire,
		calc_speed_seasonal(prep_speed_s_fire, 'dist_to_new_burn', "fire", seq(1, 5000, length.out = 100L), season_key),
		map(prep_speed_s_fire, season_key)
	),
	tar_target(
		plot_speed_s_new_fire,
		plot_dist_seasonal(calc_speed_s_new_fire, plot_theme(), "younger burns")
	),

	tar_target(
		calc_speed_s_old_fire,
		calc_speed_seasonal(prep_speed_s_fire, 'dist_to_old_burn', "fire", seq(1, 5000, length.out = 100L), season_key),
		map(prep_speed_s_fire, season_key)
	),
	tar_target(
		plot_speed_s_old_fire,
		plot_dist_seasonal(calc_speed_s_old_fire, plot_theme(), "older burns")
	)
)

# Targets: RSS from annual fire model -----------------------------------------------------------
targets_rss_fire <- c(
	tar_target(
		pred_h1_new_burn,
		predict_h1_new_burn(model_prep, fire_model)
	),
	tar_target(
		pred_h1_old_burn,
		predict_h1_old_burn(model_prep, fire_model)
	),
	tar_target(
		pred_h1_forest_fire,
		predict_h1_forest(model_prep, fire_model, "fire", "none")
	),
	tar_target(
		pred_h2_fire,
		predict_h2(model_prep, fire_model, "fire")
	),
	tar_target(
		rss_forest_fire,
		calc_rss(pred_h1_forest_fire, 'h1_forest', pred_h2_fire, 'h2')
	),
	tar_target(
		rss_old_burn,
		calc_rss(pred_h1_old_burn, 'h1_old_burn', pred_h2_fire, 'h2')
	),
	tar_target(
		rss_new_burn,
		calc_rss(pred_h1_new_burn, 'h1_new_burn', pred_h2_fire, 'h2')
	),
	tar_target(
		plot_rss_forest_fire,
		plot_rss(rss_forest_fire, plot_theme()) +
			labs(x = 'Forest', y = 'logRSS',
					 title = 'RSS compared to 0 forest (fire model)')
	),
	tar_target(
		plot_rss_new_burn,
		plot_rss(rss_new_burn, plot_theme()) +
			labs(x = 'Distance to younger burns (km)', y = 'logRSS',
				 title = 'RSS compared to median distance from post-1992 burns')
	),
	tar_target(
		plot_rss_old_burn,
		plot_rss(rss_old_burn, plot_theme()) +
			labs(x = 'Distance to older burns (km)', y = 'logRSS',
				 title = 'RSS compared to median distance from pre-1992 burns')
	),
	tar_target(
		fire_rss_plots,
		save_rss_plot(plot_rss_forest_fire, "rss_forest_fire-model",
									plot_rss_old_burn, "rss_dist_to_old_burn",
									plot_rss_new_burn, "rss_dist_to_new_burn")
	)
)

# Targets: RSS from seasonal fire model -----------------------------------------------------------
targets_rss_fire_seasonal <- c(

	tar_target(
		pred_h1_forest_s_fire,
		predict_h1_forest_seasonal(season_prep, s_fire_model, "fire", season_key),
		pattern = map(s_fire_model, season_prep, season_key)
	),
	# what does open look like
	tar_target(
		pred_h1_open_s_fire,
		predict_h1_open_seasonal(season_prep, s_fire_model, "fire", season_key),
		pattern = map(s_fire_model, season_prep, season_key)
	),

	tar_target(
		pred_h1_s_new_burn,
		predict_h1_new_burn_seasonal(season_prep, s_fire_model, season_key),
		pattern = map(season_prep, s_fire_model, season_key)
	),
	tar_target(
		pred_h1_s_old_burn,
		predict_h1_old_burn_seasonal(season_prep, s_fire_model, season_key),
		pattern = map(season_prep, s_fire_model, season_key)
	),
	tar_target(
		pred_h2_s_fire,
		predict_h2_seasonal(season_prep, s_fire_model, "fire", season_key),
		pattern = map(season_prep, s_fire_model, season_key)
	),

	tar_target(
		rss_forest_s_fire,
		calc_rss_seasonal(pred_h1_forest_s_fire, 'h1_forest_s_fire', pred_h2_s_fire, 'h2_s_fire', season_key),
		map(pred_h1_forest_s_fire, season_key)
	),

	tar_target(
		rss_open_s_fire,
		calc_rss_seasonal(pred_h1_open_s_fire, 'h1_open_s_fire', pred_h2_s_fire, 'h2_s_fire', season_key),
		map(pred_h1_open_s_fire, season_key)
	),

	tar_target(
		rss_s_new_burn,
		calc_rss_seasonal(pred_h1_s_new_burn, 'h1_new_burn_s', pred_h2_s_fire, 'h2_s_fire', season_key),
		pattern = map(pred_h1_s_new_burn, season_key)
	),
	tar_target(
		rss_s_old_burn,
		calc_rss_seasonal(pred_h1_s_old_burn, 'h1_old_burn_s', pred_h2_s_fire, 'h2_s_fire', season_key),
		pattern = map(pred_h1_s_old_burn, season_key)
	),


	tar_target(
		plot_rss_forest_s_fire,
		plot_rss_seasonal_forest(rss_forest_s_fire, plot_theme(), "fire")
	),

	tar_target(
		plot_rss_open_s_fire,
		plot_rss_seasonal_open(rss_open_s_fire, plot_theme(), "fire")
	),

	tar_target(
		plot_rss_s_new_burn,
		plot_rss_seasonal(rss_s_new_burn, plot_theme(), "younger burns")
	),
	tar_target(
		plot_rss_s_old_burn,
		plot_rss_seasonal(rss_s_old_burn, plot_theme(), "older burns")
	)
)

# Roads models: This is idential to everything above, but now running through an annual & seasonally-mapped model looking at distances to the Trans Canada and minor roads, instead of old & new burns --------

# Targets: annual road model ----------------------------------------------------------
targets_road <- c(

	tar_target(
		road_model,
		model_roads_bin(model_prep)
	),
	tar_target(
		road_model_check,
		model_check(road_model)
	)
)

# Targets: seasonal road model ----------------------------------------
targets_road_seasonal <- c(

	tar_target(
		s_road_model,
		model_roads_bin(season_prep),
		map(season_prep)
	),
	tar_target(
		s_road_model_check,
		model_check(s_road_model),
		map(s_road_model)
	)
)

# Targets: road output and effects ------------------------------------------------------------
targets_road_effects <- c(
	tar_target(
		indiv_road,
		indiv_estimates(road_model)
	),
	tar_target(
		road_boxplot,
		plot_box_horiz(indiv_road, plot_theme(), "road")
	),
	tar_target(
		s_indiv_road,
		indiv_seasonal(s_road_model, season_key),
		pattern = map(s_road_model, season_key)
	),
	tar_target(
		s_road_boxplot,
		plot_boxplot_seasonal(s_indiv_road, plot_theme(), "road"),
		pattern = map(s_indiv_road)
	)
)

# Targets: speed from annual road model ------------------------------------------------------------
targets_speed_road <- c(
	tar_target(
		prep_speed_road,
		prepare_speed(
			DT = model_prep,
			summary = indiv_road,
			model = "road",
			params = dist_parameters
		)
	),
	tar_target(
		calc_speed_open_road,
		calc_speed(prep_speed_road, 'open road', seq = 0:1)
	),
	tar_target(
		plot_speed_open_road,
		plot_box(calc_speed_open_road, plot_theme()) +
			labs(x = 'Open', y = 'Speed (m/2hr)')
	),
	tar_target(
		calc_speed_tch,
		calc_speed(prep_speed_road, 'dist_to_tch', seq(1, 5000, length.out = 100L))
	),
	tar_target(
		plot_speed_tch,
		plot_dist(calc_speed_tch, plot_theme()) +
			labs(x = 'Distance to TCH (km)', y = 'Speed (m/2hr)')
	),

	tar_target(
		calc_speed_minor,
		calc_speed(prep_speed_road, 'dist_to_minor', seq(1, 5000, length.out = 100L))
	),
	tar_target(
		plot_speed_minor,
		plot_dist(calc_speed_minor, plot_theme()) +
			labs(x = 'Distance to minor roads (km)', y = 'Speed (m/2hr)')
	),
	tar_target(
		road_plots,
		save_plot(plot_speed_open_road, "road_model_speed_open",
							plot_speed_tch, "road_model_speed_tch",
							plot_speed_minor, "road_model_speed_minor")
	)
)

# Targets: speed from seasonal roads --------------------------
targets_speed_road_seasonal <- c(

	tar_target(
		prep_speed_s_road,
		prepare_speed_seasonal(
			DT = season_prep,
			summary = s_indiv_road,
			model = "road",
			params = dist_parameters,
			season_key = season_key
		),
		map(s_indiv_road, season_key)
	),
	tar_target(
		calc_speed_open_s_road,
		calc_speed_seasonal(prep_speed_s_road, 'open', "road", seq = 0:1, season_key),
		map(prep_speed_s_road, season_key)
	),
	tar_target(
		plot_speed_open_s_road,
		plot_box_seasonal_speed(calc_speed_open_s_road, plot_theme(), "road")
	),

	tar_target(
		calc_speed_s_tch,
		calc_speed_seasonal(prep_speed_s_road, 'dist_to_tch', "road", seq(1, 5000, length.out = 100L), season_key),
		map(prep_speed_s_road, season_key)
	),
	tar_target(
		plot_speed_s_tch,
		plot_dist_seasonal(calc_speed_s_tch, plot_theme(), "tch")
	),
	tar_target(
		calc_speed_s_minor,
		calc_speed_seasonal(prep_speed_s_road, 'dist_to_minor', "road", seq(1, 5000, length.out = 100L), season_key),
		map(prep_speed_s_road, season_key)
	),
	tar_target(
		plot_speed_s_minor,
		plot_dist_seasonal(calc_speed_s_minor, plot_theme(), "minor road")
	)
)


# Targets: RSS from annual road model -----------------------------------------------------------
targets_rss_road <- c(
	tar_target(
		pred_h1_tch,
		predict_h1_tch(model_prep, road_model)
	),
	tar_target(
		pred_h1_minor,
		predict_h1_minor(model_prep, road_model)
	),
	tar_target(
		pred_h1_forest_road,
		predict_h1_forest(model_prep, road_model, "road", "none")
	),
	tar_target(
		pred_h2_road,
		predict_h2(model_prep, road_model, "road")
	),
	tar_target(
		rss_forest_road,
		calc_rss(pred_h1_forest_road, 'h1_forest', pred_h2_road, 'h2')
	),
	tar_target(
		rss_tch,
		calc_rss(pred_h1_tch, 'h1_tch', pred_h2_road, 'h2')
	),
	tar_target(
		rss_minor,
		calc_rss(pred_h1_minor, 'h1_minor', pred_h2_road, 'h2')
	),
	tar_target(
		plot_rss_forest_road,
		plot_rss(rss_forest_road, plot_theme()) +
			labs(x = 'Forest', y = 'logRSS',
					 title = 'RSS compared to 0 forest (road model)')
	),
	tar_target(
		plot_rss_tch,
		plot_rss(rss_tch, plot_theme()) +
			labs(x = 'Distance to TCH (km)', y = 'logRSS',
					 title = 'RSS compared to median distance from TCH')
	),
	tar_target(
		plot_rss_minor,
		plot_rss(rss_minor, plot_theme()) +
			labs(x = 'Distance to minor roads (km)', y = 'logRSS',
					 title = 'RSS compared to median distance from minor roads')
	),
	tar_target(
		road_rss_plots,
		save_rss_plot(plot_rss_forest_road, "rss_forest_road-model",
									plot_rss_tch, "rss_dist_to_tch",
									plot_rss_minor, "rss_dist_to_minor_roads")
	)
)


# Targets: RSS from seasonal road model -----------------------------------------------------------
targets_rss_road_seasonal <- c(

	tar_target(
		pred_h1_forest_s_road,
		predict_h1_forest_seasonal(season_prep, s_road_model, "road", season_key),
		pattern = map(s_road_model, season_prep, season_key)
	),
	tar_target(
		pred_h1_s_tch,
		predict_h1_tch_seasonal(season_prep, s_road_model, season_key),
		pattern = map(season_prep, s_road_model, season_key)
	),
	tar_target(
		pred_h1_s_minor,
		predict_h1_minor_seasonal(season_prep, s_road_model, season_key),
		pattern = map(season_prep, s_road_model, season_key)
	),
	tar_target(
		pred_h2_s_road,
		predict_h2_seasonal(season_prep, s_road_model, "road", season_key),
		pattern = map(season_prep, s_road_model, season_key)
	),

	tar_target(
		rss_forest_s_road,
		calc_rss_seasonal(pred_h1_forest_s_road, 'h1_forest_s_road', pred_h2_s_road, 'h2_s_road', season_key),
		map(pred_h1_forest_s_road, season_key)
	),
	tar_target(
		rss_s_tch,
		calc_rss_seasonal(pred_h1_s_tch, 'h1_tch', pred_h2_s_road, 'h2_s_road', season_key),
		pattern = map(pred_h1_s_tch, season_key)
	),
	tar_target(
		rss_s_minor,
		calc_rss_seasonal(pred_h1_s_minor, 'h1_minor', pred_h2_s_road, 'h2_s_road', season_key),
		pattern = map(pred_h1_s_minor, season_key)
	),


	tar_target(
		plot_rss_forest_s_road,
		plot_rss_seasonal_forest(rss_forest_s_road, plot_theme(), "road")
	),
	tar_target(
		plot_rss_s_tch,
		plot_rss_seasonal(rss_s_tch, plot_theme(), "TCH")
	),
	tar_target(
		plot_rss_s_minor,
		plot_rss_seasonal(rss_s_minor, plot_theme(), "minor roads")
	)
)


# Targets: incorporating sociality into fire model ----------------------------------------------------------

# dyad formation is pretty rare, more common in winter ~16% but very rare in calving and spring migration
# indivs vary a bit too but only in winter is it above 10% for every animal, let's just focus on winter for the social model for now

targets_social_fire <- c(
	tar_target(
		social_fire_model,
		model_fire_social(model_prep)
	),
	tar_target(
		social_fire_model_check,
		model_check(social_fire_model)
	)
)

# Targets: Social model output ----------------------
targets_social_fire_effects <- c(
	tar_target(
		indiv_social_fire,
		indiv_estimates(social_fire_model)
	),
	tar_target(
		social_fire_boxplot,
		plot_box_horiz(indiv_social_fire, plot_theme(), "social fire")
	)
)

# Social roads model speeds ----------------
#
# targets_speed_road_social <- c(
# 	tar_target(
# 		prep_speed_fire_social,
# 		prepare_speed(
# 			DT = subset(model_prep, season == "winter"),
# 			summary = indiv_social_fire,
# 			model = "fire",
# 			params = dist_parameters
# 		)
# 	),
#
# 	tar_target(
# 		calc_speed_new_alone,
# 		calc_speed_alone(prep_speed_fire_social, 'dist_to_new_burn', seq(1, 5000, length.out = 100L), model_prep)
# 	),
#
# 	tar_target(
# 		calc_speed_new_dyad,
# 		calc_speed_dyad(prep_speed_fire_social, 'dist_to_new_burn', seq(1, 5000, length.out = 100L), model_prep)
# 	),
#
# 	tar_target(
# 		plot_speed_new_social,
# 		plot_speed_social(calc_speed_new_alone, calc_speed_new_dyad, plot_theme()) +
# 			labs(x = 'Distance to new burn (km)', y = 'Speed (m/2hr)')
# 	)
#
# )
#
#
#

# Targets: RSS from social fire model -----------------------------------------------------------
targets_rss_fire_social <- c(
	tar_target(
		fire_pred_h1_forest_dyad,
		predict_h1_forest(subset(model_prep, season == "winter"),
											social_fire_model, "fire", "dyad")
	),
tar_target(
	fire_pred_h1_forest_alone,
	predict_h1_forest(subset(model_prep, season == "winter"),
										social_fire_model, "fire", "alone")
),


tar_target(
	pred_h1_new_burn_alone,
	predict_h1_new_burn_social(subset(model_prep, season == "winter"),
										social_fire_model, "alone")
),
tar_target(
	pred_h1_new_burn_dyad,
	predict_h1_new_burn_social(subset(model_prep, season == "winter"),
														 social_fire_model, "dyad")
),

tar_target(
	pred_h1_old_burn_alone,
	predict_h1_old_burn_social(subset(model_prep, season == "winter"),
														 social_fire_model, "alone")
),
tar_target(
	pred_h1_old_burn_dyad,
	predict_h1_old_burn_social(subset(model_prep, season == "winter"),
														 social_fire_model, "dyad")
),


tar_target(
	fire_pred_h2_dyad,
	predict_h2(subset(model_prep, season == "winter"),
						 social_fire_model, "fire dyad")
),
tar_target(
	fire_pred_h2_alone,
	predict_h2(subset(model_prep, season == "winter"),
						 social_fire_model, "fire alone")
),

	tar_target(
		fire_rss_forest_dyad,
		calc_rss(fire_pred_h1_forest_dyad, 'h1_forest', fire_pred_h2_dyad, 'h2')
	),
	tar_target(
		fire_rss_forest_alone,
		calc_rss(fire_pred_h1_forest_alone, 'h1_forest', fire_pred_h2_alone, 'h2')
	),
	tar_target(
		fire_rss_forest_social,
		join_rss(fire_rss_forest_alone, fire_rss_forest_dyad)
	),


tar_target(
	rss_new_burn_dyad,
	calc_rss(pred_h1_new_burn_dyad, 'h1_new_burn', fire_pred_h2_dyad, 'h2')
),
tar_target(
	rss_new_burn_alone,
	calc_rss(pred_h1_new_burn_alone, 'h1_new_burn', fire_pred_h2_alone, 'h2')
),
tar_target(
	rss_new_burn_social,
	join_rss(rss_new_burn_alone, rss_new_burn_dyad)
),

tar_target(
	rss_old_burn_dyad,
	calc_rss(pred_h1_old_burn_dyad, 'h1_old_burn', fire_pred_h2_dyad, 'h2')
),
tar_target(
	rss_old_burn_alone,
	calc_rss(pred_h1_old_burn_alone, 'h1_old_burn', fire_pred_h2_alone, 'h2')
),
tar_target(
	rss_old_burn_social,
	join_rss(rss_old_burn_alone, rss_old_burn_dyad)
),

	tar_target(
		fire_plot_rss_forest_social,
		plot_rss_social(fire_rss_forest_social, plot_theme()) +
			labs(x = 'Forest', y = 'logRSS',
					 title = 'Social RSS compared to 0 forest')
	),
tar_target(
	plot_rss_new_burn_social,
	plot_rss_social(rss_new_burn_social, plot_theme()) +
		labs(x = 'Distance to younger burns (km)', y = 'logRSS',
				 title = 'Social RSS for younger burns')
),
tar_target(
	plot_rss_old_burn_social,
	plot_rss_social(rss_old_burn_social, plot_theme()) +
		labs(x = 'Distance to older burns (km)', y = 'logRSS',
				 title = 'Social RSS for older burns')
),

tar_target(
	social_fire_rss_plots,
	save_social_rss_plot(fire_plot_rss_forest_social, "rss_forest_social_fire",
								plot_rss_new_burn_social, "rss_new_burn_social",
								plot_rss_old_burn_social, "rss_old_burn_social")
)

)





# Targets: incorporating sociality into roads model ----------------

targets_social_roads <- c(
	tar_target(
		social_road_model,
		model_road_social(model_prep)
	),
	tar_target(
		social_road_model_check,
		model_check(social_road_model)
	)
)

# Targets: Social model output ----------------------
targets_social_road_effects <- c(
	tar_target(
		indiv_social_road,
		indiv_estimates(social_road_model)
	),
	tar_target(
		social_road_boxplot,
		plot_box_horiz(indiv_social_road, plot_theme(), "social road")
	)
)

# Social roads model speeds ----------------
#
# targets_speed_road_social <- c(
# 	tar_target(
# 		prep_speed_road_social,
# 		prepare_speed(
# 			DT = subset(model_prep, season == "winter"),
# 			summary = indiv_social_road,
# 			model = "road",
# 			params = dist_parameters
# 		)
# ),
#
# tar_target(
# 	calc_speed_tch_alone,
# 	calc_speed_alone(prep_speed_road_social, 'dist_to_tch', seq(1, 20000, length.out = 100L), model_prep)
# ),
#
# tar_target(
# 	calc_speed_tch_dyad,
# 	calc_speed_dyad(prep_speed_road_social, 'dist_to_tch', seq(1, 20000, length.out = 100L), model_prep)
# ),
#
# tar_target(
# 	plot_speed_tch_social,
# 	plot_speed_social(calc_speed_tch_alone, calc_speed_tch_dyad, plot_theme()) +
# 		labs(x = 'Distance to TCH (km)', y = 'Speed (m/2hr)')
# )
#
# )
#
#




# Targets: RSS from social road model -----------------------------------------------------------
targets_rss_road_social <- c(
	tar_target(
		road_pred_h1_forest_dyad,
		predict_h1_forest(subset(model_prep, season == "winter"),
											social_road_model, "road", "dyad")
	),
	tar_target(
		road_pred_h1_forest_alone,
		predict_h1_forest(subset(model_prep, season == "winter"),
											social_road_model, "road", "alone")
	),

	tar_target(
		road_pred_h2_dyad,
		predict_h2(subset(model_prep, season == "winter"),
							 social_road_model, "road dyad")
	),
	tar_target(
		road_pred_h2_alone,
		predict_h2(subset(model_prep, season == "winter"),
							 social_road_model, "road alone")
	),

	tar_target(
		road_rss_forest_dyad,
		calc_rss(road_pred_h1_forest_dyad, 'h1_forest', road_pred_h2_dyad, 'h2')
	),
	tar_target(
		road_rss_forest_alone,
		calc_rss(road_pred_h1_forest_alone, 'h1_forest', road_pred_h2_alone, 'h2')
	),
	tar_target(
		road_rss_forest_social,
		join_rss(road_rss_forest_alone, road_rss_forest_dyad)
	),


	tar_target(
		pred_h1_tch_alone,
		predict_h1_tch_social(subset(model_prep, season == "winter"),
													social_road_model, "alone")
	),
	tar_target(
		pred_h1_tch_dyad,
		predict_h1_tch_social(subset(model_prep, season == "winter"),
													social_road_model, "dyad")
	),

	tar_target(
		pred_h1_minor_alone,
		predict_h1_minor_social(subset(model_prep, season == "winter"),
														social_road_model, "alone")
	),
	tar_target(
		pred_h1_minor_dyad,
		predict_h1_minor_social(subset(model_prep, season == "winter"),
														social_road_model, "dyad")
	),

	tar_target(
		rss_tch_dyad,
		calc_rss(pred_h1_tch_dyad, 'h1_tch', road_pred_h2_dyad, 'h2')
	),
	tar_target(
		rss_tch_alone,
		calc_rss(pred_h1_tch_alone, 'h1_tch', road_pred_h2_alone, 'h2')
	),
	tar_target(
		rss_tch_social,
		join_rss(rss_tch_alone, rss_tch_dyad)
	),

	tar_target(
		rss_minor_dyad,
		calc_rss(pred_h1_minor_dyad, 'h1_minor', road_pred_h2_dyad, 'h2')
	),
	tar_target(
		rss_minor_alone,
		calc_rss(pred_h1_minor_alone, 'h1_minor', road_pred_h2_alone, 'h2')
	),
	tar_target(
		rss_minor_social,
		join_rss(rss_minor_alone, rss_minor_dyad)
	),

	tar_target(
		road_plot_rss_forest_social,
		plot_rss_social(road_rss_forest_social, plot_theme()) +
			labs(x = 'Forest', y = 'logRSS',
					 title = 'RSS compared to 0 forest (social road model)')
	),
	tar_target(
		plot_rss_tch_social,
		plot_rss_social(rss_tch_social, plot_theme()) +
			labs(x = 'Distance to TCH (km)', y = 'logRSS',
					 title = 'Social RSS for TCH')
	),
	tar_target(
		plot_rss_minor_social,
		plot_rss_social(rss_minor_social, plot_theme()) +
			labs(x = 'Distance to minor roads (km)', y = 'logRSS',
					 title = 'Social RSS for minor roads')
	),

	tar_target(
		social_road_rss_plots,
		save_social_rss_plot(road_plot_rss_forest_social, "road_rss_forest_social",
												 plot_rss_tch_social, "rss_tch_social",
												 plot_rss_minor_social, "rss_minor_social")
	)

)

# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
