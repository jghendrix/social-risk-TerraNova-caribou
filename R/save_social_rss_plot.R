save_social_rss_plot <- function(A, nameA, B, nameB, C, nameC)

{

ggsave(paste0("graphics/rev/rss/social/", nameA, ".png"),
			 plot = A,
			 width = 2500,
			 height = 1800,
			 dpi = 320,
			 units="px")

ggsave(paste0("graphics/rev/rss/social/", nameB, ".png"),
			 plot = B,
			 width = 2500,
			 height = 1800,
			 dpi = 320,
			 units="px")

ggsave(paste0("graphics/rev/rss/social/", nameC, ".png"),
			 plot = C,
			 width = 2500,
			 height = 1800,
			 dpi = 320,
			 units="px")

}
