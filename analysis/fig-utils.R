library(cowplot)
library(grid)
library(gridExtra)
library(patchwork)

pdf.options(useDingbats = FALSE, useKerning = FALSE)

annotate_text <- function(x, y, label, parse = TRUE, ...) {
  annotate('text', x = x, y = y, label = label, parse = parse, ...)
}
