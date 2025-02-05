#' @title summarizing dyad rates
#'  @export
#' @author Jack G Hendrix
dyad_stats <- function(DT) {

	DT %<>% dplyr::filter(case_ == 1)

	DT$season <- factor(DT$season, levels = c("winter", "spring migration", "calving", "autumn migration"))

	# Before looking at dyads specifically, what does the distribution of steps look like by animal?
	g0 <- ggplot(DT, aes(x = t1_, y = id, colour = season)) +
		geom_jitter(alpha = 0.5) +
		scale_colour_viridis(discrete = TRUE) +
		xlab("Date") +
		plot_theme()

	ggsave(
		paste0('graphics/rev/summary/dates_by_indiv.png'),
		g0,
		width = 10,
		height = 10,
		dpi = 320
	)

# What is collar success rate?
steps <- DT %>% group_by(id) %>%
	summarise(n.s = n(),
						start.s = min(t1_),
						end.s = max(t2_),
						h.s = as.numeric(end.s - start.s)*12,
						success.s = n.s/h.s)
mean(steps$success.s)
# success rates are... quite low, 14% on average, driven down to 2% by tn85853 with the particularly bad collar
# especially once we've progressed to steps
locs <- read.csv("input/TNNP_ALL_Caribou.csv") %>%
	filter(!is.na(DATETIME)) %>%
	group_by(Animal_ID) %>%
	mutate(date = lubridate::as_datetime(DATETIME)) %>%
	summarise(n.l = n(),
						start.l = min(date),
						end.l = max(date),
						dur.l = as.numeric(end.l - start.l)*12,
						success.l = n.l/dur.l)
# starting dataset = 40047 GPS fixes from all animals
mean(locs$success.l)
# mean fix rate success of about 53 %, though only 27 % for 85853 (without her, 55 % avg)
# once we process that down to steps, lose even more

suc <- left_join(locs, steps, join_by("Animal_ID" == "id")) %>%
	mutate(s_per_l = n.s/n.l)

# because steps require consecutive fixes, each animal only has ~25 % steps per successful fix



# When do dyads occur ----
	rates <- DT %>% group_by(id, season, in_group) %>%
		summarise(n = n()) %>%
		filter(!is.na(in_group))

	rates %<>% group_by(id, season) %>%
		mutate(prop = n/sum(n))

	lone <- rates %>% filter(prop == 1) %>%
		mutate(in_group = "dyad",
					 n = 0,
					 prop = 0)

rates <- rbind(rates, lone)

sum <- rates %>% filter(in_group == "dyad") %>%
	group_by(season) %>%
	summarise(mean = mean(prop),
						se = sd(prop)/sqrt(10))

g1 <- ggplot(subset(rates, in_group == "dyad"), aes(x = season, y = prop)) +
	geom_jitter(aes(colour = id),
							width = 0.3,
							show.legend = F) +
	geom_pointrange(data = sum, aes(x = season, y = mean,
																	ymin = mean - se, ymax = mean + se)) +
	scale_colour_viridis(discrete = "TRUE") +
	ylab("Proportion of steps in dyad") +
	xlab("") +
	plot_theme()

	ggsave(
		paste0('graphics/rev/summary/dyad_props.png'),
		g1,
		width = 10,
		height = 10,
		dpi = 320
	)

	# any group sizes larger than 2?
	DT <- setDT(DT)
	spatsoc::group_times(DT, datetime = 't1_', threshold = '5 minutes')

	DT <- spatsoc::group_pts(
		DT,
		threshold = 50,
		id = 'id',
		coords = c('x1_', 'y1_'),
		timegroup = 'timegroup'
	)

	sizes <- DT %>% group_by(group) %>%
		summarise(size = n()) %>%
		dplyr::filter(size > 1)

	# only 4 occurrences of n = 3 animals in a group, everything else is dyadic

	# g12 = 44, 45, 47 in tg12 (14 March 2020)
	# g199 = 44, 45, 46 in tg199 (10 June 2020)
	# g5094 = 49, 50, 52 in tg4169 (26 March 2020)
	# g5464  = 49, 52, 53 in tg4318 (December 13 2020)

	# does not appear that they are joining up in larger groups, for the most part?

	## Duration of dyadic events?

	series <- DT %>% filter(in_group == "dyad") %>%
		group_by(id) %>%
		arrange(timegroup, .by_group = TRUE) %>%
		mutate(run = cumsum(start = c(1, diff(timegroup) > 1)))

	dur <- series %>% group_by(id, season, run) %>%
		summarise(n = n())

	sea <- dur %>% group_by(season) %>%
		summarise(count = n(),
							mean = mean(2*n),
							med = median(2*n),
							se = sd(2*n)/sqrt(count),
							iqr = IQR(2*n))

g2 <-	ggplot(dur, aes(x = season, y = 2*n)) +
		geom_point(position = position_jitter(width = 0.3, height = 0.05),
							 alpha = 0.5,
							aes(colour = dur$id),
						 	show.legend = F) +
		geom_pointrange(data = sea, aes(x = season, y = mean,
																		ymin = mean - se, ymax = mean + se)) +
		scale_colour_viridis(discrete = TRUE) +
		plot_theme() +
		xlab("") +
		ylab("Duration of dyad (hr)") +
	scale_y_continuous(breaks = c(4, 8, 12))

ggsave(
	paste0('graphics/rev/summary/dyad_durations.png'),
	g2,
	width = 10,
	height = 10,
	dpi = 320
)

# group sizes?
groups <- read.csv("input/Fogo-group-data.csv") %>%
	dplyr::rename(ids = collars_IDs)

calves <- read.csv("input/calf_ids.csv")

library(stringr)
remove <- calves$id
groups$ids <- str_remove_all(groups$ids, paste(remove, collapse = "|"))
# removes the calf IDs from the column so we don't overestimate the number of collared indivs per group

coll <- groups %>%
	mutate(collars = str_count(ids, "FO"),
				 collars = ifelse(is.na(collars), 0, collars)) %>%
	mutate(total = dplyr::select(., ad_male:unknown) %>% rowSums(na.rm = TRUE))
# some of the total group sizes seem wrong, just summing those columns again

summary(lm(total ~ collars, data = coll))
# for total group size, R2 = 59%

summary(lm(ad_female ~ collars, data = coll))
# for just adult females, R2 = 51%

 g3 <- ggplot(coll, aes(x = collars, y = total)) +
	geom_point() +
	geom_smooth(method = "lm") +
 	xlab("Collared individuals") +
 	ylab("Total group size") +
 	plot_theme()

 ggsave(
 	paste0('graphics/rev/summary/group_sizes_by_collar_number.png'),
 	g3,
 	width = 10,
 	height = 10,
 	dpi = 320
 )

dyads <- coll %>% mutate(dyad = ifelse(collars == 1, "alone", ifelse(collars == 2, "dyad", NA))) %>%
	filter(!is.na(dyad))
summary(lm(total ~ dyad, data = dyads))
# even just looking at single vs. dyad, it is a signifcant predictor of larger groups
# though R2 is only 25% now

g4 <- ggplot(dyads, aes(x = dyad, y = total)) +
	geom_boxplot(outliers = FALSE) +
	geom_jitter(alpha = 0.5) +
	xlab("Collared individuals") +
	ylab("Total group size") +
	plot_theme()

ggsave(
	paste0('graphics/rev/summary/group_sizes_by_dyad.png'),
	g4,
	width = 10,
	height = 10,
	dpi = 320
)

dyads %>% group_by(dyad) %>% summarise(n = n(),
																		mean = mean(total),
																		med = median(total),
																		se = sd(total)/sqrt(n))
	}
