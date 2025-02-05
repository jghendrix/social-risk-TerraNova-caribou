save_rss_plot <- function(A, nameA, B, nameB, C, nameC)

{

ggsave(paste0("graphics/rev/rss/",nameA, ".png"),
			 plot = A,
			 width = 2000,
			 height = 1500,
			 dpi = 320,
			 units="px")

ggsave(paste0("graphics/rev/rss/",nameB, ".png"),
			 plot = B,
			 width = 2000,
			 height = 1500,
			 dpi = 320,
			 units="px")

ggsave(paste0("graphics/rev/rss/",nameC, ".png"),
			 plot = C,
			 width = 2000,
			 height = 1500,
			 dpi = 320,
			 units="px")

}
