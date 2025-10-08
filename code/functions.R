###
# Project: Decompose the recent fertility decline
# Purpose: Functions
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 2025/07/29
###


### Functions -----------------------------

save_fig <- function(filename, figure=last_plot(), height=15, width=15) {
  ggsave(plot=figure, filename=file.path("figures", paste0(filename, ".pdf")), height=height, width=width, unit="cm")
  ggsave(plot=figure, filename=file.path("figures", paste0(filename, ".svg")), height=height, width=width, unit="cm")
  
}

### Set the graphic scheme ----------------

theme_set(theme_classic(base_size=14, base_family="serif"))
theme_update(
  legend.position="bottom"
)

### END ###################################