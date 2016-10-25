theme_simple <- function(font_size = 8) {
    theme_bw() +
    theme(axis.text.x          = element_text(size = font_size, lineheight=0.7),
          axis.title.x         = element_blank(),
          axis.title.y         = element_text(size = font_size),
          axis.text.y          = element_text(size = font_size),
          axis.line            = element_line(colour = "gray"),
          plot.title           = element_text(size = font_size),
          panel.background     = element_blank(), #element_rect(fill = NA, colour = NA),
          panel.grid.major     = element_blank(),
          panel.grid.minor     = element_blank(),
          panel.border         = element_blank(),
          plot.background      = element_blank(), #element_rect(fill = NA, colour = NA)
          # plot.background      = element_rect(size = 1, colour = "black"),
          strip.background     = element_blank(),
          plot.margin = unit(c(0.05,0,0,0), "cm"))
}

element90 <- function() { element_text(angle = 90, hjust = 1, vjust=0.5) }

my_theme <- function(font_size = 8, legend.pos = c(1,0)) {
  theme_bw() +
  theme(
    legend.justification=legend.pos, legend.position=legend.pos,
    # no legend title, background, key
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.margin = unit(-0.05, "cm"),
    legend.direction = "horizontal",
    text = element_text(size=font_size),
    # No x title
    axis.title.x = element_blank(),
    # No x title
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = font_size, lineheight=0.7),
    axis.text.y = element_text(size = font_size),
    legend.text = element_text(size = font_size),
    plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")
  )
}

get_color_map <- function(exp_names, palette = "YlGnBu") {
  exp_colors <- brewer.pal(5, palette)
  names(exp_colors) <- exp_names
  exp_colors
}

get_colors1 <- function(breaks, color_map) {
  sapply(breaks, function(x) color_map[[x]])
}

get_colors <- function(names, color_map) {
  sapply(names, get_color)
}

get_color <- function(name) {
  if (name == "Unsafe") {
    brewer.pal(5, "Paired")[5]
  } else if (name == "Safe") {
    brewer.pal(5, "YlGn")[4]
  } else if (name == "All Shared" || name == "No Deep Sharing") {
    brewer.pal(5, "YlGnBu")[5]
  } else if (name == "Scala Akka") {
    brewer.pal(5, "YlOrRd")[2]
  }
}

get_dual_color <- function(name) {
  if (name == "Unsafe") {
    brewer.pal(5, "Paired")[5] # brewer.pal(5, "Reds")[2]
  } else if (name == "Safe") {
    brewer.pal(5, "Greens")[5]
  }
}

exp_names <- c("All Shared", "Scala Akka", "No Deep Sharing", "Safe", "Unsafe")
