library(tidyverse)
library(patchwork)

transformation_of_RV_graph <- function() {
  mlog2 = -log(2)
  a <- ggplot() + xlim(0,3) +
    geom_function(fun = exp) +
    geom_segment(aes(x=0, xend=2, y=exp(2), yend=exp(2)), linetype="dashed") +
    geom_segment(aes(x=2, xend=2, y=0, yend=exp(2)), linetype="dashed") +
    annotate("text",  x=2.5, y=8, label="g(s)") +
    ylim(0,10) +
    theme_classic() +
    labs(x=element_blank(), y=element_blank(), title=element_blank()) +
    scale_x_continuous(breaks=c(2), labels=c("x")) +
    scale_y_continuous(breaks=c(exp(2)), labels=c("g(x)"))
  b <- ggplot() + xlim(1,3) +
    geom_function(fun = \(x) -log(x)) +
    geom_segment(aes(x=1, xend=2, y=mlog2, yend=mlog2), linetype="dashed") +
    geom_segment(aes(x=2, xend=2, y=-1, yend=mlog2), linetype="dashed") +
    annotate("text",  x=2.5, y=-0.75, label="h(s)") +
    theme_classic() +
    xlim(1,3) + ylim(-1,0) +
    labs(x=element_blank(), y=element_blank(), title=element_blank()) +
    scale_x_continuous(breaks=c(2), labels=c("x")) +
    scale_y_continuous(breaks=c(-log(2)), labels=c("h(x)"))
  a + b + plot_annotation(tag_levels = "a")
}

powergraph <- function() {
  deltas <- seq(0, 0.75, by=0.01)
  n <- c(50, 100, 250, 500, 1000, 2500) 
  df <- expand.grid(deltas, n)
  names(df) <- c("delta", "n")
  df$power = rep_along(df$n,0)
  for (i in seq_along(df$n)) {
    ns <- df$n[i]/2
    ds <- df$delta[i]
    pwr <- power.t.test(n=ns, delta=ds)
    df$power[i] = pwr$power
  }
  df |> mutate(nf = factor(n)) |>
    ggplot(aes(x=delta, y=power, group=nf)) +
    geom_line(aes(linetype=nf)) + 
    theme_minimal() +
    labs(x = "Effect size (in SDs)", y = "Power (given alpha=0.05)", linetype="Total n:")
}

standard_normal_table <- function() {
  ys <- seq(0, 3.0, by=0.1)
  xs <- seq(0,0.09, by=0.01)
  l <- length(ys)
  b <- length(xs)
  m <- matrix(0, nrow=l, ncol=b)
  for (i in seq_along(ys)) {
    for (j in seq_along(xs)) { 
      v <- ys[i] + xs[j]
      m[i,j] <- pnorm(v)
    }
  }
  df <- m |> as_tibble() 
  names(df) <- c("0.00", "0.01", "0.02", "0.03", "0.04", "0.05", "0.06","0.07", "0.08", "0.09")
  df$z <- as.character(format(ys, digits = 2))
  df |> dplyr::select(c(z,`0.00`, `0.01`, `0.02`, `0.03`, `0.04`, `0.05`, `0.06`,`0.07`, `0.08`, `0.09`)) |> 
                        gt::gt() |> gt::fmt_number(decimals = 3) |>
    gt::tab_header(title="Standard normal table") |>
    gt::as_latex()
}


#transformation_of_RV_graph()
#ggsave(here::here("graphs","transformation_of_RV.pdf"), width=12.8, height = 6,units = "cm" )

#powergraph()
#ggsave(here::here("graphs","powergraph.pdf"), width=12.8, height=6, units = "cm")