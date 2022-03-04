
cat("Running packaes")

# R libraries -------------------------------------------------------------

library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(scales)
library(slider)
library(ggrepel)
library(patchwork)
library(WDI)
library(osfr)
library(broom)
library(rlang)
library(yaml)
library(knitr)
library(rmarkdown)
library(foreign)
library(wpp2019)

# Theme for charts -------------------------------------------------------------

theme_custom <- function(tisize = 11.5, txtsize = 10, axsize = 9, scale_f = 1, ...) { 
  theme_gray() + 
    theme(
      plot.margin = margin(3, 1.5, 1.5, 1.5, "pt"), 
      plot.subtitle = element_text(size = txtsize*scale_f), 
      plot.title = element_text(size = tisize*scale_f, hjust = 0, face = 'bold'), 
      axis.text.x = element_text(hjust = 0.5), 
      axis.text.y = element_text(hjust = 1), 
      axis.title.y = element_text(color="black", size = txtsize*scale_f, angle = 90, vjust = 1, hjust = 1), 
      axis.title.x = element_text(color="black", hjust = 0.5, size=txtsize*scale_f), 
      axis.text = element_text(size=axsize*scale_f, color = "black"), 
      #panel.grid = element_line(color = "#F5E3E0", size = 0.25), 
      plot.caption=element_text(hjust=0, color="grey40", size=txtsize*scale_f), 
      panel.background = element_rect(fill = "#ffffff"), 
      rect = element_rect(fill = "#ffffff", colour = "#ffffff", size = 0.5, linetype = 1), 
      axis.line.x = element_line(color = "black", size=0.5), 
      axis.line.y = element_line(color = "black", size=0.5), 
      strip.background = element_rect(fill = "#ffffff"), 
      strip.text = element_text(size = txtsize*scale_f, face = 'bold'), 
      strip.text.y = element_text(angle = 90), 
      legend.title = element_text(size = txtsize*scale_f), 
      legend.text = element_text(size = txtsize*scale_f), 
      legend.key.height = unit(1, 'lines'), 
      legend.key.width = unit(1, 'lines'), 
      legend.background = element_rect(fill = "#ffffff", colour = "#ffffff"), 
      legend.key = element_rect(colour = "transparent", fill = "white"), 
      plot.title.position = "plot", 
      plot.caption.position = "plot", 
      ...
    ) 
}


# Helper functions -------------------------------------------------------------

'%not_in%' <- function(x,y)!('%in%'(x,y))

clean <- function(x) { 
  round(x/1e6, 1)
}

clean_comma <- function(x) { 
  if (max(range(x, na.rm = TRUE)) < 1) {
    round(x*100, 2)
  } else {
    prettyNum(round(x, 0), big.mark = ",")
  }
}

conf_interval <- function(mean, lci, uci) { 
  paste0(mean, " (", lci, "-", uci, ")")
}

bind_rows_with_factor_columns <- function(...) {
  purrr::pmap_df(list(...), function(...) {
    cols_to_bind <- list(...)
    if (all(purrr::map_lgl(cols_to_bind, is.factor))) {
      forcats::fct_c(cols_to_bind)
    } else {
      unlist(cols_to_bind)
    }
  })
}
