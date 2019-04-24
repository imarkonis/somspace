#' @export

plot.ts.regs <- function(x, nregions){
  ids <- data.table(id = x$regions[[1]], region = x$regions[[nregions + nregions]])
  to_plot <- merge(x$input_dt, ids, by = "id")  
  to_plot <- unique(to_plot[, .(variable = mean(variable)), .(time, region)])
  ggplot(to_plot, aes(x = time, y = variable)) +
    geom_line(alpha = 0.3) +
    geom_smooth(method = 'loess', span = 0.1, col =  "black", fill = 'dark red') +
    facet_wrap(~region) +
    labs(x = "Time", y = "Variable") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey20")) +
    theme(strip.text.x = element_text(colour = "grey90", size = 10))
}
