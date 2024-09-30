save_social_rss_plot <- function(A, nameA, B, nameB, C, nameC)

{

ggsave(paste0("graphics/social/", nameA, ".png"),
			 plot = A,
			 width = 2500,
			 height = 1800,
			 dpi = 320,
			 units="px")

ggsave(paste0("graphics/social/", nameB, ".png"),
			 plot = B,
			 width = 2500,
			 height = 1800,
			 dpi = 320,
			 units="px")

ggsave(paste0("graphics/social/", nameC, ".png"),
			 plot = C,
			 width = 2500,
			 height = 1800,
			 dpi = 320,
			 units="px")

}
