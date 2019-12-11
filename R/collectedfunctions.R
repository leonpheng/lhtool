#' Covariate plots function
#'
#' @param data Data frame. Use the codes below as template 
#' @keywords cov_plots()
#' @export
#' @examples
cov_fn<-function(...){
library(grid)
require(gpairs)
gpairs<-function (x, upper.pars = list(scatter = "points", conditional = "barcode", 
                                       mosaic = "mosaic"), lower.pars = list(scatter = "points", 
                                                                             conditional = "boxplot", mosaic = "mosaic"), diagonal = "default", 
                  outer.margins = list(bottom = unit(2, "lines"), left = unit(2, 
                                                                              "lines"), top = unit(2, "lines"), right = unit(2, "lines")), 
                  xylim = NULL, outer.labels = NULL, outer.rot = c(0, 90), 
                  gap = 0.05, buffer = 0.02, reorder = NULL, cluster.pars = NULL, 
                  stat.pars = NULL, scatter.pars = NULL, bwplot.pars = NULL, 
                  stripplot.pars = NULL, barcode.pars = NULL, mosaic.pars = NULL, 
                  axis.pars = NULL, diag.pars = NULL, whatis = FALSE) 
{
  if (!is.data.frame(x)) {
    if (is.matrix(x)) 
      x <- as.data.frame(x)
    else stop("What did you give me? You might want to use Excel. (Only one column in argument to gpairs.\n\n")
  }
  zc <- function(x) length(unique(x)) <= 1
  if (any(sapply(x, zc), na.rm = TRUE)) {
    warning(paste(sum(sapply(x, zc), na.rm = TRUE), "columns with less than two distinct values eliminated"))
    x <- x[, !(sapply(x, zc))]
  }
  if (!is.null(lower.pars) & !is.list(lower.pars)) {
    warning("lower.pars is not a list, proceed with caution.")
  }
  if (!is.null(upper.pars) & !is.list(upper.pars)) {
    warning("upper.pars is not a list, proceed with caution.")
  }
  if (!is.null(reorder)) {
    if (pmatch(reorder, "cluster", nomatch = FALSE)) {
      if (is.null(cluster.pars)) {
        cluster.pars <- list(dist.method = "euclidean", 
                             hclust.method = "complete")
      }
      x.num <- as.matrix(as.data.frame(lapply(x, as.numeric)))
      x.clust <- hclust(dist(t(x.num), method = cluster.pars$dist.method), 
                        method = cluster.pars$hclust.method)
      x <- x[, x.clust$order]
    }
  }
  if (is.null(lower.pars$scatter.pars)) {
    lower.pars$scatter.pars <- "points"
  }
  if (is.null(lower.pars$conditional)) {
    lower.pars$conditional <- "boxplot"
  }
  if (is.null(lower.pars$mosaic)) {
    lower.pars$mosaic <- "mosaic"
  }
  if (is.null(upper.pars$scatter.pars)) {
    upper.pars$scatter.pars <- "points"
  }
  if (is.null(upper.pars$conditional)) {
    upper.pars$conditional <- "barcode"
  }
  if (is.null(upper.pars$mosaic)) {
    upper.pars$mosaic <- "mosaic"
  }
  if (!is.list(outer.margins)) {
    if (length(outer.margins) == 4) {
      if (is.unit(outer.margins[1])) {
        outer.margins <- list(bottom = outer.margins[1], 
                              left = outer.margins[2], top = outer.margins[3], 
                              right = outer.margins[4])
      } else {
        outer.margins <- list(bottom = unit(outer.margins[1], 
                                            "lines"), left = unit(outer.margins[2], "lines"), 
                              top = unit(outer.margins[3], "lines"), right = unit(outer.margins[4], 
                                                                                  "lines"))
      }
    } else {
      stop("outer.margins are not valid.")
    }
  }
  if (is.null(outer.labels)) {
    outer.labels$top <- rep(FALSE, ncol(x))
    outer.labels$top[seq(2, ncol(x), by = 2)] <- TRUE
    outer.labels$left <- rep(FALSE, ncol(x))
    outer.labels$left[seq(2, ncol(x), by = 2)] <- TRUE
    outer.labels$right <- !outer.labels$left
    outer.labels$bottom <- !outer.labels$top
  } else {
    if (pmatch(as.character(outer.labels), "all", nomatch = FALSE)) {
      all.labeling <- TRUE
    } else if (pmatch(as.character(outer.labels), "none", nomatch = FALSE)) {
      all.labeling <- FALSE
    } else {
      stop("argument to outer.labels not understood\n")
    }
    outer.labels <- NULL
    outer.labels$top <- rep(all.labeling, ncol(x))
    outer.labels$left <- rep(all.labeling, ncol(x))
    outer.labels$bottom <- rep(all.labeling, ncol(x))
    outer.labels$right <- rep(all.labeling, ncol(x))
  }
  if (is.null(stat.pars$fontsize)) {
    stat.pars$fontsize <- 7
  }
  if (is.null(stat.pars$signif)) {
    stat.pars$signif <- 0.05
  }
  if (is.null(stat.pars$verbose)) {
    stat.pars$verbose <- FALSE
  }
  if (is.null(stat.pars$use.color)) {
    stat.pars$use.color <- TRUE
  }
  if (is.null(stat.pars$missing)) {
    stat.pars$missing <- "missing"
  }
  if (is.null(stat.pars$just)) {
    stat.pars$just <- "centre"
  }
  if (is.null(scatter.pars$pch)) {
    scatter.pars$pch <- 1
  }
  if (is.null(scatter.pars$size)) {
    scatter.pars$size <- unit(0.25, "char")
  }
  if (is.null(scatter.pars$col)) {
    scatter.pars$col <- "black"
  }
  if (is.null(scatter.pars$plotpoints)) {
    scatter.pars$plotpoints <- TRUE
  }
  if (is.null(axis.pars$n.ticks)) {
    axis.pars$n.ticks <- 5
  }
  if (is.null(axis.pars$fontsize)) {
    axis.pars$fontsize <- 9
  }
  if (axis.pars$n.ticks < 3) {
    axis.pars$n.ticks <- 3
    warning("Fewer than 3 axis ticks might cause problems.")
  }
  if (is.null(diag.pars$fontsize)) {
    diag.pars$fontsize <- 9
  }
  if (is.null(diag.pars$show.hist)) {
    diag.pars$show.hist <- TRUE
  }
  if (is.null(diag.pars$hist.color)) {
    diag.pars$hist.color <- "black"
  }
  if (is.null(stripplot.pars$pch)) {
    stripplot.pars$pch <- 1
  }
  if (is.null(stripplot.pars$size)) {
    stripplot.pars$size <- unit(0.5, "char")
  }
  if (is.null(stripplot.pars$col)) {
    stripplot.pars$col <- "black"
  }
  if (is.null(stripplot.pars$jitter)) {
    stripplot.pars$jitter <- FALSE
  }
  if (is.null(barcode.pars$nint)) {
    barcode.pars$nint <- 0
  }
  if (is.null(barcode.pars$ptsize)) {
    barcode.pars$ptsize <- unit(0.25, "char")
  }
  if (is.null(barcode.pars$ptpch)) {
    barcode.pars$ptpch <- 1
  }
  if (is.null(barcode.pars$bcspace)) {
    barcode.pars$bcspace <- NULL
  }
  if (is.null(barcode.pars$use.points)) {
    barcode.pars$use.points <- FALSE
  }
  if (is.null(mosaic.pars$gp_labels)) {
    mosaic.pars$gp_labels <- gpar(fontsize = 9)
  }
  if (is.null(mosaic.pars$gp_args)) {
    mosaic.pars$gp_args <- list()
  }
  draw.axis <- function(x, y, axis.pars, xpos, ypos, cat.labels = NULL, 
                        horiz = NULL, xlim = NULL, ylim = NULL) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (is.null(xlim)) {
      px <- pretty(x, axis.pars$n.ticks)
      px <- px[px > min(x, na.rm = TRUE) & px < max(x, 
                                                    na.rm = TRUE)]
    } else {
      px <- pretty(xlim, axis.pars$n.ticks)
      px <- px[px > min(xlim, na.rm = TRUE) & px < max(xlim, 
                                                       na.rm = TRUE)]
    }
    if (is.null(ylim)) {
      py <- pretty(y, axis.pars$n.ticks)
      py <- py[py > min(y, na.rm = TRUE) & py < max(y, 
                                                    na.rm = TRUE)]
    } else {
      py <- pretty(ylim, axis.pars$n.ticks)
      py <- py[py > min(ylim, na.rm = TRUE) & py < max(ylim, 
                                                       na.rm = TRUE)]
    }
    k <- length(cat.labels)
    if (!is.null(xpos)) {
      if (!is.null(cat.labels) && !horiz) {
        grid.text(cat.labels, x = unit(1:k, "native"), 
                  y = unit(rep(1 * (1 - xpos), k), "npc") + unit(rep(-1 * 
                                                                       xpos + 1 * (1 - xpos), k), "lines"), rot = outer.rot[1], 
                  gp = gpar(fontsize = axis.pars$fontsize))
      } else grid.xaxis(at = px, gp = gpar(fontsize = axis.pars$fontsize), 
                        main = xpos)
    }
    if (!is.null(ypos)) {
      if (!is.null(cat.labels) && horiz) {
        grid.text(cat.labels, y = unit(1:k, "native"), 
                  x = unit(rep(1 * (1 - ypos), k), "npc") + unit(rep(-1 * 
                                                                       ypos + 1 * (1 - ypos), k), "lines"), rot = outer.rot[2], 
                  gp = gpar(fontsize = axis.pars$fontsize))
      }else grid.yaxis(at = py, gp = gpar(fontsize = axis.pars$fontsize), 
                       main = ypos)
    }
  }
  qq.panel <- function(x, y, scatter.pars, axis.pars, xpos, 
                       ypos, xlim, ylim) {
    pushViewport(viewport(xscale = xlim, yscale = ylim))
    draw.axis(x, y, axis.pars, xpos, ypos, NULL, NULL, xlim, 
              ylim)
    popViewport(1)
    pushViewport(viewport(xscale = xlim, yscale = ylim, clip = TRUE))
    grid.rect(gp = gpar(fill = scatter.pars$frame.fill, col = scatter.pars$border.col))
    x <- sort(x)
    y <- sort(y)
    grid.lines(unit(x, "native"), unit(y, "native"))
    popViewport(1)
  }
  scatterplot.panel <- function(x, y, type, scatter.pars, axis.pars, 
                                xpos, ypos, xylim) {
    if (is.null(xylim)) {
      xlim <- range(x, na.rm = TRUE) + c(-buffer * (max(x, 
                                                        na.rm = TRUE) - min(x, na.rm = TRUE)), buffer * 
                                           (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      ylim <- range(y, na.rm = TRUE) + c(-buffer * (max(y, 
                                                        na.rm = TRUE) - min(y, na.rm = TRUE)), buffer * 
                                           (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)))
    }    else {
      xlim <- xylim
      ylim <- xylim
    }
    pushViewport(viewport(xscale = xlim, yscale = ylim))
    draw.axis(x, y, axis.pars, xpos, ypos, NULL, NULL, xlim, 
              ylim)
    popViewport(1)
    pushViewport(viewport(xscale = xlim, yscale = ylim, clip = TRUE))
    grid.rect(gp = gpar(fill = scatter.pars$frame.fill, col = scatter.pars$border.col))
    if (scatter.pars$plotpoints & (type == "points" || type == 
                                   "lm" || type == "ci" || type == "symlm" || type == 
                                   "loess")) {
      grid.points(x, y, pch = scatter.pars$pch, size = scatter.pars$size, 
                  gp = gpar(col = scatter.pars$col))
    }
    if (type == "lm") {
      xy.lm <- lm(y ~ x)
      panel.abline(xy.lm$coef[1], xy.lm$coef[2], col = "red", 
                   lwd = 2)
    }
    if (type == "ci") {
      xy.lm <- lm(y ~ x)
      xy <- data.frame(x = seq(min(x, na.rm = TRUE), max(x, 
                                                         na.rm = TRUE), length.out = 20))
      yhat <- predict(xy.lm, newdata = xy, interval = "confidence")
      ci <- data.frame(lower = yhat[, "lwr"], upper = yhat[, 
                                                           "upr"])
      grid.lines(x = c(xy$x), y = c(ci$lower), default.units = "native")
      grid.lines(x = c(xy$x), y = c(ci$upper), default.units = "native")
      grid.polygon(x = c(xy$x, xy$x[length(xy$x):1]), y = c(ci$lower, 
                                                            ci$upper[length(ci$upper):1]), gp = gpar(fill = "grey"), 
                   default.units = "native")
    }
    if (type == "loess") {
      junk <- try(panel.loess(x, y, color = "red", span = 1))
      if (class(junk) == "try-error") 
        warning("An error in loess occurred and was ignored; no line was plotted.")
    }
    if (type == "symlm") {
      pcs <- try(prcomp(cbind(x, y)))
      if (class(pcs) == "try-error") 
        warning("An error in symlm occurred and was ignored; no line was plotted.")
      else {
        slope <- abs(pcs$rotation[1, 2]/pcs$rotation[1, 
                                                     1])
        if (cor(x, y) < 0) 
          slope <- -1 * slope
        panel.abline(pcs$center[2] - slope * pcs$center[1], 
                     slope, col = "blue")
      }
    }
    if (type == "corrgram") {
      pear.test <- cor.test(x, y, method = "pearson", alternative = "two.sided")
      corr <- format(pear.test$estimate, digits = 2)
      if (as.numeric(corr) > 0) {
        panel.fill(col = hsv(h = 0.5, s = abs(as.numeric(corr)), 
                             v = 1), border = hsv(h = 0.5, s = abs(as.numeric(corr)), 
                                                  v = 1))
        grid.lines(x = unit(c(0, 1), "npc"), y = unit(c(0, 
                                                        1), "npc"), gp = gpar(col = "white", lwd = 2))
      } else {
        panel.fill(col = hsv(h = 0, s = abs(as.numeric(corr)), 
                             v = 1), border = hsv(h = 0, s = abs(as.numeric(corr)), 
                                                  v = 1))
        grid.lines(x = unit(c(0, 1), "npc"), y = unit(c(1, 
                                                        0), "npc"), gp = gpar(col = "white", lwd = 2))
      }
    }
    if (type == "qqplot") {
      qq.panel(x, y, scatter.pars, axis.pars, xpos, ypos, 
               xlim, ylim)
    }
    if (type == "stats") {
      complete.obs <- nrow(na.omit(cbind(x, y)))
      missing <- length(x) - complete.obs
      pear.test <- cor.test(x, y, method = "pearson", alternative = "two.sided")
      corr <- sprintf("%03.2f", pear.test$estimate)
      rho.test <- cor.test(x, y, method = "spearman", alternative = "two.sided")
      tau.test <- cor.test(x, y, method = "kendall", alternative = "two.sided")
      rho <- sprintf("%03.2f", rho.test$estimate)
      tau <- sprintf("%03.2f", tau.test$estimate)
      xy.lm <- lm(y ~ x)
      r2 <- sprintf("%03.2f", summary(xy.lm)$r.squared)
      p <- sprintf("%06.4f", pf(q = as.numeric(summary(xy.lm)$fstatistic)[1], 
                                df1 = as.numeric(summary(lm(xy.lm))$fstatistic)[2], 
                                df2 = as.numeric(summary(lm(xy.lm))$fstatistic)[3], 
                                lower.tail = FALSE))
      bonfp <- stat.pars$signif/(N * (N - 1))/2
      sig <- 1
      sigrho <- NULL
      sigtau <- NULL
      sigcor <- NULL
      sigp <- NULL
      if (pear.test$p.value < bonfp) {
        sig <- sig + 1
        sigcor <- "*"
      }
      if (rho.test$p.value < bonfp) {
        sig <- sig + 1
        sigrho <- "*"
      }
      if (tau.test$p.value < bonfp) {
        sig <- sig + 1
        sigtau <- "*"
      }
      if (as.numeric(p) < bonfp) {
        sig <- sig + 1
        sigp <- "*"
      }
      if (mean(as.numeric(rho), as.numeric(tau), as.numeric(corr)) > 
          0) {
        text.color <- "black"
        if (sig == 1) 
          box.color <- 0.5
        else if (sig > 1 && sig < 5) 
          box.color <- 0.75
        else if (sig == 5) 
          box.color <- 1
      } else if (mean(as.numeric(rho), as.numeric(tau), as.numeric(corr)) < 
                 0) {
        text.color <- "white"
        if (sig == 1) 
          box.color <- 0.5
        else if (sig > 1 && sig < 5) 
          box.color <- 0.25
        else if (sig == 5) 
          box.color <- 0
      }
      if (!stat.pars$use.color) {
        panel.fill(col = grey(box.color), border = grey(box.color))
      } else {
        text.color <- "black"
        if (as.numeric(corr) > 0) {
          panel.fill(col = hsv(h = 0.5, s = abs(as.numeric(corr)), 
                               v = 1), border = hsv(h = 0.5, s = abs(as.numeric(corr)), 
                                                    v = 1))
        } else {
          panel.fill(col = hsv(h = 0, s = abs(as.numeric(corr)), 
                               v = 1), border = hsv(h = 0, s = abs(as.numeric(corr)), 
                                                    v = 1))
        }
      }
      if (!is.na(stat.pars$verbose)) {
        if (stat.pars$verbose == TRUE) {
          # grid.text(bquote(rho == .(rho) * .(sigrho)), 
          #          x = 0.5, y = 0.9, just = stat.pars$just, 
          #       gp = gpar(fontsize = stat.pars$fontsize, 
          #                col = text.color))
          # grid.text(bquote(tau == .(tau) * .(sigtau)), 
          #          x = 0.5, y = 0.7, just = stat.pars$just, 
          #         gp = gpar(fontsize = stat.pars$fontsize, 
          #                  col = text.color))
          grid.text(paste("r=", corr, sigcor, sep = ""), 
                    x = 0.5, y = 0.5, just = stat.pars$just, 
                    gp = gpar(fontsize = stat.pars$fontsize, 
                              col = text.color))
          grid.text(paste("p=", p, sigp, sep = ""), x = 0.5, 
                    y = 0.3, just = stat.pars$just, gp = gpar(fontsize = stat.pars$fontsize, 
                                                              col = text.color))
          if (missing > 0) 
            grid.text(paste(missing, stat.pars$missing), 
                      x = 0.5, y = 0.1, just = stat.pars$just, 
                      gp = gpar(fontsize = stat.pars$fontsize, 
                                col = "red"))
        } else {
          
          grid.text(paste(corr, sigcor, sep = ""), x = 0.5, 
                    y = 0.7, just = stat.pars$just, gp = gpar(fontsize = stat.pars$fontsize, 
                                                              col = text.color))
          if (missing > 0) 
            grid.text(paste(missing, "missing"), x = 0.5, 
                      y = 0.3, just = stat.pars$just, gp = gpar(fontsize = stat.pars$fontsize, 
                                                                col = text.color))
        }
      }
    }
    popViewport(1)
  }
  mosaic.panel <- function(x, y, mosaic.pars, axis.pars, xpos, 
                           ypos) {
    if (!is.null(xpos) & !is.null(ypos)) {
      strucplot(table(y, x), margins = c(0, 0, 0, 0), newpage = FALSE, 
                pop = FALSE, keep_aspect_ratio = FALSE, shade = mosaic.pars$shade, 
                legend = FALSE, gp = mosaic.pars$gp, gp_args = mosaic.pars$gp_args, 
                labeling_args = list(tl_labels = c(xpos, !ypos), 
                                     gp_labels = mosaic.pars$gp_labels, varnames = c(FALSE, 
                                                                                     FALSE), rot_labels = c(outer.rot, outer.rot)))
    }  else {
      if (is.null(xpos) & is.null(ypos)) {
        strucplot(table(y, x), margins = c(0, 0, 0, 0), 
                  shade = mosaic.pars$shade, legend = FALSE, 
                  gp = mosaic.pars$gp, gp_args = mosaic.pars$gp_args, 
                  newpage = FALSE, pop = FALSE, keep_aspect_ratio = FALSE, 
                  labeling = NULL)
      }  else {
        if (is.null(xpos)) {
          strucplot(table(y, x), margins = c(0, 0, 0, 
                                             0), newpage = FALSE, pop = FALSE, keep_aspect_ratio = FALSE, 
                    shade = mosaic.pars$shade, legend = FALSE, 
                    gp = mosaic.pars$gp, gp_args = mosaic.pars$gp_args, 
                    labeling_args = list(labels = c(TRUE, FALSE), 
                                         tl_labels = c(ypos, FALSE), gp_labels = mosaic.pars$gp_labels, 
                                         varnames = c(FALSE, FALSE), rot_labels = c(outer.rot, 
                                                                                    outer.rot)))
        } else {
          strucplot(table(y, x), margins = c(0, 0, 0, 
                                             0), newpage = FALSE, pop = FALSE, keep_aspect_ratio = FALSE, 
                    shade = mosaic.pars$shade, legend = FALSE, 
                    gp = mosaic.pars$gp, gp_args = mosaic.pars$gp_args, 
                    labeling_args = list(labels = c(FALSE, TRUE), 
                                         tl_labels = c(FALSE, !xpos), gp_labels = mosaic.pars$gp_labels, 
                                         varnames = c(FALSE, FALSE), rot_labels = c(outer.rot, 
                                                                                    outer.rot)))
        }
      }
    }
  }
  boxplot.panel <- function(x, y, type, axis.pars, xpos, ypos, 
                            xylim) {
    xlim <- NULL
    ylim <- NULL
    old.color <- trellis.par.get("box.rectangle")$col
    trellis.par.set(name = "box.rectangle", value = list(col = "black"))
    trellis.par.set(name = "box.umbrella", value = list(col = "black"))
    trellis.par.set(name = "box.dot", value = list(col = "black"))
    trellis.par.set(name = "plot.symbol", value = list(col = "black"))
    if (is.factor(x)) {
      cat.labels <- levels(x)
      k <- length(levels(x))
      cat.var <- as.numeric(x)
      cont.var <- y
      horiz <- FALSE
    } else {
      cat.labels <- levels(y)
      k <- length(levels(y))
      cat.labels <- cat.labels[k:1]
      cat.var <- k + 1 - as.numeric(y)
      cont.var <- x
      horiz <- TRUE
    }
    if (horiz) {
      if (is.null(xylim)) {
        xlim <- range(cont.var, na.rm = TRUE) + c(-buffer * 
                                                    (max(cont.var, na.rm = TRUE) - min(cont.var, 
                                                                                       na.rm = TRUE)), buffer * (max(cont.var, na.rm = TRUE) - 
                                                                                                                   min(cont.var, na.rm = TRUE)))
      } else {
        xlim <- xylim
      }
      pushViewport(viewport(xscale = xlim, yscale = c(0.5, 
                                                      max(cat.var, na.rm = TRUE) + 0.5)))
      if (is.null(ypos)) 
        cat.labels <- NULL
      draw.axis(cont.var, cat.var, axis.pars, xpos, ypos, 
                cat.labels, horiz, xlim, ylim)
      popViewport(1)
      pushViewport(viewport(xscale = xlim, yscale = c(0.5, 
                                                      max(cat.var, na.rm = TRUE) + 0.5), clip = TRUE))
      if (type == "boxplot") 
        panel.bwplot(cont.var, cat.var, horizontal = horiz, 
                     col = "black", pch = "|", gp = gpar(box.umbrella = list(col = "black")))
      if (type == "stripplot") 
        panel.stripplot(cont.var, cat.var, horizontal = horiz, 
                        jitter.data = stripplot.pars$jitter, col = stripplot.pars$col, 
                        cex = stripplot.pars$size, pch = stripplot.pars$pch)
    }else {
      if (is.null(xylim)) {
        ylim <- range(cont.var, na.rm = TRUE) + c(-buffer * 
                                                    (max(cont.var, na.rm = TRUE) - min(cont.var, 
                                                                                       na.rm = TRUE)), buffer * (max(cont.var, na.rm = TRUE) - 
                                                                                                                   min(cont.var, na.rm = TRUE)))
      }else {
        ylim <- xylim
      }
      pushViewport(viewport(yscale = ylim, xscale = c(0.5, 
                                                      max(cat.var, na.rm = TRUE) + 0.5)))
      if (is.null(xpos)) 
        cat.labels <- NULL
      draw.axis(cat.var, cont.var, axis.pars, xpos, ypos, 
                cat.labels, horiz, xlim, ylim)
      popViewport(1)
      pushViewport(viewport(yscale = ylim, xscale = c(0.5, 
                                                      max(cat.var, na.rm = TRUE) + 0.5), clip = TRUE))
      if (type == "boxplot") 
        panel.bwplot(cat.var, cont.var, horizontal = horiz, 
                     col = "black", pch = "|", gp = gpar(box.umbrella = list(col = "black")))
      if (type == "stripplot") 
        panel.stripplot(cat.var, cont.var, horizontal = horiz, 
                        jitter.data = stripplot.pars$jitter, col = stripplot.pars$col, 
                        cex = stripplot.pars$size, pch = stripplot.pars$pch)
    }
    grid.rect(gp = gpar(fill = NULL))
    popViewport(1)
    trellis.par.set(name = "box.rectangle", value = list(col = old.color))
    trellis.par.set(name = "box.umbrella", value = list(col = old.color))
    trellis.par.set(name = "box.dot", value = list(col = old.color))
    trellis.par.set(name = "plot.symbol", value = list(col = old.color))
  }
  diag.panel <- function(x, varname, diag.pars, axis.pars, 
                         xpos, ypos, xylim) {
    x <- x[!is.na(x)]
    if (is.null(xylim)) {
      xlim <- range(as.numeric(x), na.rm = TRUE) + c(-buffer * 
                                                       (max(as.numeric(x), na.rm = TRUE) - min(as.numeric(x), 
                                                                                               na.rm = TRUE)), buffer * (max(as.numeric(x), 
                                                                                                                             na.rm = TRUE) - min(as.numeric(x), na.rm = TRUE)))
    }else {
      xlim <- xylim
    }
    ylim <- xlim
    pushViewport(viewport(xscale = xlim, yscale = ylim))
    draw.axis(as.numeric(x), as.numeric(x), axis.pars, xpos, 
              ypos, NULL, NULL, xlim, ylim)
    popViewport(1)
    pushViewport(viewport(xscale = xlim, yscale = ylim, clip = TRUE))
    if (!diag.pars$show.hist) {
      grid.rect()
      grid.text(varname, 0.5, 0.5, gp = gpar(fontsize = diag.pars$fontsize, 
                                             fontface = 2))
    }
    popViewport(1)
    if (diag.pars$show.hist) {
      if (!is.factor(x)) {
        pushViewport(viewport(xscale = xlim, yscale = c(0, 
                                                        100), clip = TRUE))
        panel.histogram(as.numeric(x), breaks = NULL, 
                        type = "percent", col = diag.pars$hist.color)
      }else {
        pushViewport(viewport(xscale = c(min(as.numeric(x), 
                                             na.rm = TRUE) - 1, max(as.numeric(x), na.rm = TRUE) + 
                                           1), yscale = c(0, 100), clip = TRUE))
        panel.barchart(1:length(table(x)), 100 * table(x)/sum(table(x)), 
                       horizontal = FALSE, col = diag.pars$hist.color)
      }
      grid.text(varname, 0.5, 0.85, gp = gpar(fontsize = diag.pars$fontsize))
      popViewport(1)
    }
  }
  grid.newpage()
  N <- ncol(x)
  vp.main <- viewport(x = outer.margins$bottom, y = outer.margins$left, 
                      width = unit(1, "npc") - outer.margins$right - outer.margins$left, 
                      height = unit(1, "npc") - outer.margins$top - outer.margins$bottom, 
                      just = c("left", "bottom"), name = "main", clip = "off")
  pushViewport(vp.main)
  for (i in 1:N) {
    for (j in 1:N) {
      if (diagonal == "default") 
        labelj <- j
      else labelj <- N - j + 1
      x[is.infinite(x[, i]), i] <- NA
      x[is.infinite(x[, j]), j] <- NA
      vp <- viewport(x = (labelj - 1)/N, y = 1 - i/N, width = 1/N, 
                     height = 1/N, just = c("left", "bottom"), name = as.character(i * 
                                                                                     N + j))
      pushViewport(vp)
      vp.in <- viewport(x = 0.5, y = 0.5, width = 1 - gap, 
                        height = 1 - gap, just = c("center", "center"), 
                        name = paste("IN", as.character(i * N + j)))
      pushViewport(vp.in)
      xpos <- NULL
      if (i == 1 && outer.labels$top[j]) {
        xpos <- FALSE
      }
      if (i == N && outer.labels$bottom[j]) {
        xpos <- TRUE
      }
      ypos <- NULL
      if (j == N && outer.labels$right[i]) {
        ypos <- FALSE
      }
      if (j == 1 && outer.labels$left[i]) {
        ypos <- TRUE
      }
      if (!is.null(ypos) & diagonal != "default") {
        ypos <- !ypos
      }
      if (i == j) {
        diag.panel(x[, i], names(x)[i], diag.pars, axis.pars, 
                   xpos, ypos, xylim)
      }else {
        if (is.factor(x[, i]) + is.factor(x[, j]) == 
            1) {
          if (i < j & upper.pars$conditional != "barcode") 
            boxplot.panel(x[, j], x[, i], upper.pars$conditional, 
                          axis.pars, xpos, ypos, xylim)
          if (i > j & lower.pars$conditional != "barcode") 
            boxplot.panel(x[, j], x[, i], lower.pars$conditional, 
                          axis.pars, xpos, ypos, xylim)
          if (i < j & upper.pars$conditional == "barcode") {
            if (is.factor(x[, i])) {
              barcode(split(x[, j], x[, i])[length(levels(x[, 
                                                            i])):1], horizontal = TRUE, xlim = xylim, 
                      labelloc = ypos, axisloc = xpos, labelouter = TRUE, 
                      newpage = FALSE, fontsize = axis.pars$fontsize, 
                      buffer = buffer, nint = barcode.pars$nint, 
                      ptsize = barcode.pars$ptsize, ptpch = barcode.pars$ptpch, 
                      bcspace = barcode.pars$bcspace, use.points = barcode.pars$use.points)
            } else {
              if (!is.null(ypos)) 
                ypos <- !ypos
              barcode(split(x[, i], x[, j])[length(levels(x[, 
                                                            j])):1], horizontal = FALSE, xlim = xylim, 
                      labelloc = xpos, axisloc = ypos, labelouter = TRUE, 
                      newpage = FALSE, fontsize = axis.pars$fontsize, 
                      buffer = buffer, nint = barcode.pars$nint, 
                      ptsize = barcode.pars$ptsize, ptpch = barcode.pars$ptpch, 
                      bcspace = barcode.pars$bcspace, use.points = barcode.pars$use.points)
            }
          }
          if (i > j & lower.pars$conditional == "barcode") {
            if (is.factor(x[, i])) {
              barcode(split(x[, j], x[, i])[length(levels(x[, 
                                                            i])):1], horizontal = TRUE, xlim = xylim, 
                      labelloc = ypos, axisloc = xpos, labelouter = TRUE, 
                      newpage = FALSE, fontsize = axis.pars$fontsize, 
                      buffer = buffer, nint = barcode.pars$nint, 
                      ptsize = barcode.pars$ptsize, ptpch = barcode.pars$ptpch, 
                      bcspace = barcode.pars$bcspace, use.points = barcode.pars$use.points)
            } else {
              if (!is.null(ypos)) 
                ypos <- !ypos
              barcode(split(x[, i], x[, j])[length(levels(x[, 
                                                            j])):1], horizontal = FALSE, xlim = xylim, 
                      labelloc = xpos, axisloc = ypos, labelouter = TRUE, 
                      newpage = FALSE, fontsize = axis.pars$fontsize, 
                      buffer = buffer, nint = barcode.pars$nint, 
                      ptsize = barcode.pars$ptsize, ptpch = barcode.pars$ptpch, 
                      bcspace = barcode.pars$bcspace, use.points = barcode.pars$use.points)
            }
          }
        }
        if (is.factor(x[, i]) + is.factor(x[, j]) == 
            0) {
          if (i < j) 
            type <- upper.pars$scatter
          else type <- lower.pars$scatter
          scatterplot.panel(x[, j], x[, i], type, scatter.pars, 
                            axis.pars, xpos, ypos, xylim)
        }
        if (is.factor(x[, i]) + is.factor(x[, j]) == 
            2) {
          if (i < j) 
            mosaic.panel(x[, j], x[, i], mosaic.pars, 
                         axis.pars, xpos, ypos)
          else mosaic.panel(x[, j], x[, i], mosaic.pars, 
                            axis.pars, xpos, ypos)
        }
      }
      popViewport(1)
      upViewport()
    }
  }
  popViewport()
  if (whatis) 
    whatis(x)
}

gpar <- function(...) {
  gp <- validGP(list(...))
  class(gp) <- "gpar"
  gp
}

is.gpar <- function(x) {
  inherits(x, "gpar")
}

print.gpar <- function(x, ...) {
  print(unclass(x), ...)
  invisible(x)
}

validGP <- function(gpars) {
  # Check a (non-NULL) gpar is not of length 0
  check.length <- function(gparname) {
    if (length(gpars[[gparname]]) == 0)
      stop(gettextf("'gpar' element '%s' must not be length 0", gparname),
           domain = NA)
  }
  # Check a gpar is numeric and not NULL
  numnotnull <- function(gparname) {
    if (!is.na(match(gparname, names(gpars)))) {
      if (is.null(gpars[[gparname]]))
        gpars[[gparname]] <<- NULL
      else {
        check.length(gparname)
        gpars[[gparname]] <<- as.numeric(gpars[[gparname]])
      }
    }
  }
  # fontsize, lineheight, cex, lwd should be numeric and not NULL
  numnotnull("fontsize")
  numnotnull("lineheight")
  numnotnull("cex")
  numnotnull("lwd")
  numnotnull("lex")
  # gamma defunct in 2.7.0
  if ("gamma" %in% names(gpars)) {
    warning("'gamma' 'gpar' element is defunct")
    gpars$gamma <- NULL
  }
  numnotnull("alpha")
  # col and fill are converted in C code
  # BUT still want to check length > 0
  if (!is.na(match("col", names(gpars)))) {
    if (is.null(gpars$col))
      gpars$col <- NULL
    else
      check.length("col")
  }
  if (!is.na(match("fill", names(gpars)))) {
    if (is.null(gpars$fill))
      gpars$fill <- NULL
    else
      check.length("fill")
  }
  # lty converted in C code
  # BUT still want to check for NULL and check length > 0
  if (!is.na(match("lty", names(gpars)))) {
    if (is.null(gpars$lty))
      gpars$lty <- NULL
    else
      check.length("lty")
  }
  if (!is.na(match("lineend", names(gpars)))) {
    if (is.null(gpars$lineend))
      gpars$lineend <- NULL
    else
      check.length("lineend")
  }
  if (!is.na(match("linejoin", names(gpars)))) {
    if (is.null(gpars$linejoin))
      gpars$linejoin <- NULL
    else
      check.length("linejoin")
  }
  # linemitre should be larger than 1
  numnotnull("linemitre")
  if (!is.na(match("linemitre", names(gpars)))) {
    if (any(gpars$linemitre < 1))
      stop("invalid 'linemitre' value")
  }
  # alpha should be 0 to 1
  if (!is.na(match("alpha", names(gpars)))) {
    if (any(gpars$alpha < 0 || gpars$alpha > 1))
      stop("invalid 'alpha' value")
  }
  # font should be integer and not NULL
  if (!is.na(match("font", names(gpars)))) {
    if (is.null(gpars$font))
      gpars$font <- NULL
    else {
      check.length("font")
      gpars$font <- as.integer(gpars$font)
    }
  }
  # fontfamily should be character
  if (!is.na(match("fontfamily", names(gpars)))) {
    if (is.null(gpars$fontfamily))
      gpars$fontfamily <- NULL
    else {
      check.length("fontfamily")
      gpars$fontfamily <- as.character(gpars$fontfamily)
    }
  }
  # fontface can be character or integer;  map character to integer
  # store value in font
  # Illegal to specify both font and fontface
  if (!is.na(match("fontface", names(gpars)))) {
    if (!is.na(match("font", names(gpars))))
      stop("must specify only one of 'font' and 'fontface'")
    gpars$font <-
      if (is.null(gpars$fontface)) NULL # remove it
    else {
      check.length("fontface")
      if (is.numeric(gpars$fontface))
        as.integer(gpars$fontface)
      else
        vapply(as.character(gpars$fontface),
               function(ch) # returns integer
                 switch(ch,
                        plain = 1L,
                        bold  = 2L,
                        italic=, oblique = 3L,
                        bold.italic = 4L,
                        symbol= 5L,
                        # These are Hershey variants
                        cyrillic=5L,
                        cyrillic.oblique=6L,
                        EUC   = 7L,
                        stop("invalid fontface ", ch)), 0L)
    }
  }
  gpars
}

# Method for subsetting "gpar" objects
`[.gpar` <- function(x, index, ...) {
  if (length(x) == 0)
    return(gpar())
  maxn <- do.call("max", lapply(x, length))
  newgp <- lapply(x, rep, length.out=maxn)
  newgp <- lapply(X = newgp, FUN = "[", index, ...)
  class(newgp) <- "gpar"
  newgp
}

# possible gpar names
# The order must match the GP_* values in grid.h
.grid.gpar.names <- c("fill", "col", "gamma", "lty", "lwd", "cex",
                      "fontsize", "lineheight", "font", "fontfamily",
                      "alpha", "lineend", "linejoin", "linemitre",
                      "lex",
                      # Keep fontface at the end because it is never
                      # used in C code (it gets mapped to font)
                      "fontface")

set.gpar <- function(gp) {
  if (!is.gpar(gp))
    stop("argument must be a 'gpar' object")
  temp <- grid.Call(L_getGPar)
  # gamma defunct in 2.7.0
  if ("gamma" %in% names(gp)) {
    warning("'gamma' 'gpar' element is defunct")
    gp$gamma <- NULL
  }
  # Special case "cex" (make it cumulative)
  if (match("cex", names(gp), nomatch=0L))
    tempcex <- temp$cex * gp$cex
  else
    tempcex <- temp$cex
  # Special case "alpha" (make it cumulative)
  if (match("alpha", names(gp), nomatch=0L))
    tempalpha <- temp$alpha * gp$alpha
  else
    tempalpha <- temp$alpha
  # Special case "lex" (make it cumulative)
  if (match("lex", names(gp), nomatch=0L))
    templex <- temp$lex * gp$lex
  else
    templex <- temp$lex
  # All other gpars
  temp[names(gp)] <- gp
  temp$cex <- tempcex
  temp$alpha <- tempalpha
  temp$lex <- templex
  # Do this as a .Call.graphics to get it onto the base display list
  grid.Call.graphics(L_setGPar, temp)
}

get.gpar <- function(names=NULL) {
  if (is.null(names)) {
    result <- grid.Call(L_getGPar)
    # drop gamma
    result$gamma <- NULL
  } else {
    if (!is.character(names) ||
        !all(names %in% .grid.gpar.names))
      stop("must specify only valid 'gpar' names")
    # gamma deprecated
    if ("gamma" %in% names) {
      warning("'gamma' 'gpar' element is defunct")
      names <- names[-match("gamma", names)]
    }
    result <- unclass(grid.Call(L_getGPar))[names]
  }
  class(result) <- "gpar"
  result
}

# When editing a gp slot, only update the specified gpars
# Assume gp is NULL or a gpar
# assume newgp is a gpar (and not NULL)
mod.gpar <- function(gp, newgp) {
  if (is.null(gp))
    gp <- newgp
  else
    gp[names(newgp)] <- newgp
  gp
}
}



#' template for Covariate plots
#'
#' @param data Data frame. Use the codes below as template 
#' @keywords cov_plots()
#' @export
#' @examples
cov_plots<-function(...){
  library(reshape2)
  library(plyr)
  require(PCSmisc)
  require(foreign)
  require(Hmisc)
  library(sas7bdat)
  library(lhtool)
  library(lhwordtool)
  library(gridExtra)
  library(ggplot2)
  library(gridExtra)
  require(reshape)
  library(ggpubr)
  cov_fn()
  library(dplyr)
  dir<-"//certara.com/sites/S02-Cary/Consulting/Projects/projects/"
  cov1<-read.csv(file.path(dir,"dataset.csv"))
  cov1<-cov1[cov1$mdv1==0,]

  cov<-cov1[!duplicated(cov1$subject),]

  r<-read.csv("Eta.csv")

  head(r)
  unique(r$Name)
  names(r)<-tolower(names(r))
  
  head(cov)
  #ID for for merging
  id<-"id"
  #Keep ETA
  keta<-c("ndur","ntlag","nv","ncl")
  #Keep Categorical COV
  head(r)
  #cov<-cov[cov$id%in%r$id,]
  
  cca<-c("study","sex","agecat","country","bmic", "ethnic","egfrcatn","phasen","oster","health","hepcat","corti","race","ethnic1","patient")
  
  #KEEP Cont COV
  cco<-c("wt","bsa","age","bmi" ,"alb","alt","ast","bil","dose","uacr","bvas","egfr")
  
  cov<-chclass(cov,cca,"char")
  
  #ORDER BASELINE CATEGORICAL
  one(cov,"race")
  cov$race<-as.character(cov$race)
  dat1<-reflag(cov,"sex",c("Female", "Male"),c("Female","Male"))
  dat1<-reflag(dat1,"agecat",c(">=18 & <50", ">=50 & <65",">=65 & <75", ">=75"))
  dat1<-reflag(dat1,"bmic",c("0","1","Missing"),c("<30 kg/m^2", ">=30 kg/m^2","Missing"))
  dat1<-reflag(dat1,"ethnic1",c("Hispanic or Latino","Not Hispanic or Latino","Unknown","Not reported"),c("Hispanic or Latino","Not Hispanic or Latino","Unknown","Unknown"))
  
  dat1<-reflag(dat1,"race",c(" White","White"," Black Or African American","Black"," Asian","Asian"," Other"," White_ Native Hwaiian/Pacific Islander"),c("White","White","Black","Black","Asian","Asian","Other","Other"))
  
  dat1<-reflag(dat1,"patient",c("other", "AAV","IgAN"))
  
  dat1<-reflag(dat1,"hepcat",c("Healthy","Mild","Moderate","Others"))
  
  dat1<-reflag(dat1,"ethnic",c( "non-Japanese","Japanese", "Unknown"))
  dat1<-reflag(dat1,"health",c(0,3,1,2),c("Other","Other","AAV","IgAN"))
  dat1<-reflag(dat1,"egfrcatn",c("0","1", "2", "3" ),c("Normal","Mild", "Moderate", "Severe" ))
  dat1<-reflag(dat1,"phasen",c("1","2" ),
               c("Phase 1","Phase 2"))
  dat1<-reflag(dat1,"oster",c("0","1"),c("No","Yes"))
  dat1<-reflag(dat1,"corti",c("No","No with AAV","Yes"))
  dat1<-chclass(dat1,cco,"num")
  
  dat2<-lhcut(dat1[!is.na(dat1$bmi),],"bmi",c(20,30))
  dat3<-dat1[is.na(dat1$bmi),];dat3$bmicat<-"Missing"
  dat1<-rbind(dat2,dat3)
  

  #GROUP CONTINUOUS COVARIATES
  ###########RUN THE FOLLOWING LINES AS IS FOR MOST OF THE TIMES#############################
  cov<-chclass(cov,cco,"num")
  names(r)<-tolower(names(r))
  
  
  eta<-r[,c(id,keta)]
  cateta<-join(eta,dat1[,c(id,cca)],type="left")
  nrow(eta);nrow(cateta)
  head(cateta)
  
  
  cat1<-lhlong(cateta,names(cateta[,(length(keta)+2):ncol(cateta)]))
  
  head(cat1)
  
  names(cat1)[names(cat1)=="variable"]<-"Covariate"
  names(cat1)[names(cat1)=="value"]<-"Categorical"
  cat1<-chclass(cat1,c("Covariate","Categorical"),"char")
  cat1<-addvar(cat1,c("Covariate","Categorical"),keta[1],"length(x)","yes","count")
  cat1$Cat1<-paste0(cat1$Categorical,"\n (n=",cat1$count,")")
  
  cat1<-lhlong(cat1,keta)
  head(cat1)
  cat1<-chclass(cat1,c("Covariate","Categorical","variable"),"char")
  unique(cat1$Categorical)
  head(cat1)
  cat1$variable<-factor(cat1$variable,levels=keta)
  
  catnum<-addvar(nodup(cat1,c("Covariate","Categorical"),"var"),"Covariate","Categorical","length(x)","no","catnumber")
  
  for(i in cca[cca%in%catnum$Covariate[catnum$catnumber>0]]){
    head(cateta)
    dcat<-cat1[cat1$Covariate%in%i,]
    ord<-sort(unique(cateta[,i]))
    label<-nodup(dcat,c("Categorical","Cat1"),"var")
    label$Categorical<-factor(label$Categorical,levels=ord)
    lablel<-label$Cat1[order(label$Categorical)]
    dcat$Cat1<-factor(dcat$Cat1,levels=lablel)

    p<-ggplot(dcat,aes(x=Cat1,y=value))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(position=position_jitter(0.1),col="grey")+
      geom_hline(yintercept=0,linetype=2, color="red",size=1)+
      ylab("Individual Random Effect")+xlab("")+
      facet_wrap(~variable,scale="free")+
      theme_bw()+
      theme(axis.text.x = element_text(angle =45, hjust =0.5, vjust = 0.5))
    ggsave(paste0("1_",i,"_boxplot.png"),p,width=5.7,height=5.7)
  }
  
  
  ########GPAIRS##########
  coneta<-join(eta,cov[,c("id",cco)],type="left")

  names(coneta)<-tolower(names(coneta))
  cco<-tolower(cco)
  cco<-cco
  coneta<-chclass(coneta,cco,"num")
  
  congr<-NULL
  congr[[1]]<-c("wt","age","bsa","bmi")
  congr[[2]]<-c("alb","alt","ast","bil","dose")
  congr[[3]]<-c("uacr","bvas","egfr")

  for(i in 1:length(congr)){
    png(file=paste0("scatter",i,".png"),width=768,height=768,pointsize = 16)
    print(gpairs(x=coneta[,c(keta,congr[[i]])], 
                 upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
                 lower.pars = list(scatter = 'stats',conditional = "barcode"),
                 diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
                 stat.pars =list(fontsize = 11, signif =F, verbose =T, use.color = TRUE, missing = 'missing', just = 'centre'),
                 scatter.pars = list(pch = 20)))
    dev.off()
  }

 
  # Distribution of ETA 
  p1<-ggplot(r,aes(x=ntlag))+  geom_density(fill="royalblue3",col=NA,alpha=0.3)+
    geom_histogram(aes(x=ntlag,y=..density..),fill=NA,col="black")+
    geom_vline(xintercept=0,col="red",size=1.5,linetype = "dashed")
  
  p2<-ggplot(r,aes(x=ndur))+  geom_density(fill="royalblue3",col=NA,alpha=0.3,bin=60)+
    geom_histogram(aes(x=ndur,y=..density..),fill=NA,col="black")+
    geom_vline(xintercept=0,col="red",size=1.5,linetype = "dashed")
  
  p3<-ggplot(r,aes(x=ncl))+  geom_density(fill="royalblue3",col=NA,alpha=0.3,bin=60)+
    geom_histogram(aes(x=ncl,y=..density..),fill=NA,col="black")+
    geom_vline(xintercept=0,col="red",size=1.5,linetype = "dashed")
  
  p4<-ggplot(r,aes(x=nv))+  geom_density(fill="royalblue3",col=NA,alpha=0.3,bin=60)+
    geom_histogram(aes(x=nv,y=..density..),fill=NA,col="black")+
    geom_vline(xintercept=0,col="red",size=1.5,linetype = "dashed")
  
  library(gridExtra)
  ggsave("ETA_CWRES_distribution.png",
         ggarrange(p1,p2,p3,p4),dpi = 300, width =12, height = 8,units = c("in"))  

}
  
  
#' template for GOF
#'
#' @param data Data frame. Use the codes below as template 
#' @keywords gof_plots()
#' @export
#' @examples 

gof_plots<-function(...){
  rm(list=ls())
  library(reshape2)
  library(plyr)
  require(PCSmisc)
  require(foreign)
  require(Hmisc)
  library(sas7bdat)
  library(lhtool)
  library(ggplot2)
  library(lhwordtool)
  library(gridExtra)
  library(ggpubr)
    ###########
  #install.packages("qqplotr")
  ###########
  # Functions
  qqplot.cwres <- function(dat, ...) {
    ylim <-c(-10, 10)
    xlim <-c(-4, 4)
    xlab <- "Quantiles of Standard Normal"
    ylab <- "Conditional Weighted Residuals"
    with(dat, qqnorm(CWRES, ylim=ylim, xlim=xlim, xlab=xlab, ylab=ylab, ...))
    with(dat, qqline(CWRES))
    abline(a=0,b=1,col="red") #add the standard normal qqplot
  }
  
  histogram.cwres <- function(dat, ...) {
    ylim <-c(0, 0.55)
    xlab <- "Conditional Weighted Residuals"
    with(dat, hist(CWRES, ylim=ylim, xlab=xlab, main="", freq=FALSE, ...))
    abline(v=0, lty=2, lwd=3, col="gray")
    xs <- seq(-10, 140, len=100)
    lines(xs, dnorm(xs), col="gray", lwd=3)
  }
  #------------------------------------------------------------------------------

  dir<-"//certara.com/sites/S02-Cary/Consulting/Projects/projects/"
  dat<-read.csv(file.path(dir,"dataset.csv"))
  cov<-dat[!duplicated(dat$subject),]#read.csv(file.path(path,"Individual of Covariates_ximab.csv"))  
  r<-read.csv("Residuals.csv")
  dat<-dat[dat$mdv==0,]
  unique(r$IVAR-dat$rtime)
  nrow(dat)
  nrow(r)
  
  
  r$cwres<-r$CWRES
  # CWRES distribution
  png(file = "dist_QQ_CWRES.png", width = 8, height = 6, units = 'in', res = 300)
  par(mfrow = c(1, 2))
  qqplot.cwres(r)
  histogram.cwres(r)
  dev.off()
  
  IPRED<-"Individual Predicted Concentration (ng/mL)"
  PRED<-"Population Predicted Concentration (ng/mL)"
  DV<-"Observed Concentration (ng/mL)"
  TAD<-"Time After Dose (h)"
  RTIME<-"Time After First Dose (h)"
  conc<-"Concentration (ng/mL)"
  IVAR<-"Time After First Dose (h)"
  CWRES<-"Conditional Weighted Residuals"
  
  r$IPRED<-exp(r$IPRED);r$DV<-exp(r$DV)
  r$PRED<-exp(r$PRED)
  
  dem<-nodup(dat,"subject","all")

  r1<-join(r,dem[,c("id","study","subject")],type="left")
  
  outl<-r1[abs(r1$CWRES)>4,c("study","subject","IVAR","TAD","DV","PRED","IPRED","CWRES")]
  outl<-outl[order(outl$TAD),]
  
  exp<-dat[dat$subject%in%outl$subject[outl$PRED>1],c("subject","rtime","TAD","dv1")]
  nrow(outl)/nrow(dat)*100
  
  exp<-left_join(exp,lhmutate(outl[outl$PRED>1,c("subject","IVAR","DV","CWRES")],c("DV=outlier","IVAR=rtime")))
  write.csv(outl,"Outliers.csv")
  

  limx<-limy<-range(c(r1$IPRED,r1$DV))
  limx2<-limy2<-range(c(r1$PRED,r1$DV))
  limx1<-limy1<-c(0.001,10000)
  
  r1$STUDYID<-as.character(r1$study)

  
  library(RColorBrewer)
  cols<-brewer.pal(n =6, name = 'Paired')
  cols<-c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "black", "#FF7F00")
  #cols<-c("grey","green","red","black","blue")
  
  p1<-ggplot(r1,aes(x=IPRED,y=DV))+
    geom_point(aes(col=STUDYID))+
    scale_x_continuous(limits=limy,labels = function(x) format(x, scientific =F))+
    scale_y_continuous(limits=limx,labels = function(x) format(x, scientific =F))+
    xlab(IPRED)+ylab(DV)+
    geom_abline(slope=1,size=1)+
    geom_hline(aes(yintercept=0,col="Identity"))+
    geom_smooth(aes(col="LOESS"),se=F,span=1,size=1)+
    
    scale_colour_manual(name="",values=cols, 
                        guide = "legend") + 
    theme_bw()+
    guides(colour=guide_legend(override.aes=list(linetype=c(rep(0,6),1,1),shape=c(rep(16,6),NA,NA))))+
    theme(axis.text.y = element_text(angle = 90, hjust =0.5, vjust = 0.5),legend.position=c(0.8,0.2),legend.box.margin=NULL, legend.background = element_rect(fill="transparent"))
  
  
  p2<-ggplot(r1,aes(x=IPRED,y=DV))+
    #geom_point(col="grey")+
    geom_point(aes(col=factor(STUDYID)))+
    scale_y_log10(limits =limx,breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    scale_x_log10(limits =limx , breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    annotation_logticks(sides = "lb") +
    geom_abline(slope=1)+
    xlab(IPRED)+ylab(DV)+
    geom_abline(slope=1,size=1)+
    geom_hline(aes(yintercept=0,col="Identity"))+
    geom_smooth(aes(col="LOESS"),se=F,span=1,size=1)+
    
    scale_colour_manual(name="",values=cols, 
                        guide = "legend") + 
    theme_bw()+
    guides(colour=guide_legend(override.aes=list(linetype=c(rep(0,6),1,1),shape=c(rep(16,6),NA,NA))))+
    theme(axis.text.y = element_text(angle = 90, hjust =0.5, vjust = 0.5),legend.position=c(0.8,0.2),legend.box.margin=NULL, legend.background = element_rect(fill="transparent"))
  
  p1a<-ggplot(r1,aes(x=PRED,y=DV))+
    #geom_point(col="grey")+
    geom_point(aes(col=factor(STUDYID)))+
    scale_x_continuous(limits=limy2,labels = function(x) format(x, scientific =F))+
    scale_y_continuous(limits=limx2,labels = function(x) format(x, scientific =F))+
    xlab(PRED)+ylab(DV)+
    geom_abline(slope=1,size=1)+
    geom_hline(aes(yintercept=0,col="Identity"))+
    geom_smooth(aes(col="LOESS"),se=F,span=1,size=1)+
    
    scale_colour_manual(name="",values=cols, 
                        guide = "legend") + 
    theme_bw()+
    guides(colour=guide_legend(override.aes=list(linetype=c(rep(0,6),1,1),shape=c(rep(16,6),NA,NA))))+
    theme(axis.text.y = element_text(angle = 90, hjust =0.5, vjust = 0.5),legend.position=c(0.8,0.2),legend.box.margin=NULL, legend.background = element_rect(fill="transparent"))
  
  
  p2a<-ggplot(r1,aes(x=PRED,y=DV))+
    #geom_point(ae(col="grey")+
    geom_point(aes(col=factor(STUDYID)))+
    scale_y_log10(limits =limx2,breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    scale_x_log10(limits =limx2 , breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    annotation_logticks(sides = "lb") +
    geom_abline(slope=1)+
    xlab(PRED)+ylab(DV)+
    geom_abline(slope=1,size=1)+
    geom_hline(aes(yintercept=0,col="Identity"))+
    geom_smooth(aes(col="LOESS"),se=F,span=1,size=1)+
    
    scale_colour_manual(name="",values=cols, 
                        guide = "legend") + 
    theme_bw()+
    guides(colour=guide_legend(override.aes=list(linetype=c(rep(0,6),1,1),shape=c(rep(16,6),NA,NA))))+
    theme(axis.text.y = element_text(angle = 90, hjust =0.5, vjust = 0.5),legend.position=c(0.8,0.2),legend.box.margin=NULL, legend.background = element_rect(fill="transparent"))
  
  ggsave("GOF.png",
         ggarrange(p1,p1a,p2,p2a, ncol=2, nrow=2, common.legend = TRUE, legend="bottom"),dpi = 300, width =12, height = 8,units = c("in"))
  
  #######################################################################################
  #cols<-brewer.pal(n =9, name = 'Paired')
  cols<-c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "black", "#FF7F00","#CAB2D6")
  
  p3a<-ggplot(r1,aes(x=TAD,y=DV))+
    geom_point(aes(color=STUDYID))+
    #geom_point(aes(col=factor(STUDYID)))+
    xlab(TAD)+ylab(conc)+
    #geom_abline(slope=1)+
    scale_y_log10(limits =limx,breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    
    scale_x_continuous(breaks =seq(0,1500,7*24),
                       labels =seq(0,1500,7*24))+
    annotation_logticks(sides = "l") +
    geom_smooth(aes(y=DV,col="LOESS_OBS"),se=F)+
    geom_smooth(aes(y=IPRED,col="LOESS_IPRED"),se=F)+
    geom_smooth(aes(y=PRED,col="LOESS_PRED"),se=F)+
    scale_colour_manual(name="",values=cols, 
                        guide = "legend") + 
    theme_bw()+
    guides(colour=guide_legend(override.aes=list(linetype=c(rep(0,6),1,1,1),shape=c(rep(16,6),NA,NA,NA))))+
    theme(axis.text.y = element_text(angle = 90, hjust =0.5, vjust = 0.5),legend.position=c(0.8,0.2),legend.box.margin=NULL, legend.background = element_rect(fill="transparent"))
  
  p3b<-ggplot(r1,aes(x=TAD,y=DV))+
    geom_point(aes(color=STUDYID))+
    #geom_point(aes(col=factor(STUDYID)))+
    xlab(TAD)+ylab("Concentration (ng/mL)")+
    #geom_abline(slope=1)+
    scale_x_continuous(breaks =seq(0,1500,7*24),
                       labels =seq(0,1500,7*24))+
    #scale_y_continuous(limits=limx,breaks =seq(0,10^6,10^5),labels = seq(0,10))+
    geom_smooth(aes(y=DV,col="LOESS_OBS"),se=F)+
    geom_smooth(aes(y=IPRED,col="LOESS_IPRED"),se=F)+
    geom_smooth(aes(y=PRED,col="LOESS_PRED"),se=F)+
    scale_colour_manual(name="",values=cols, 
                        guide = "legend") + 
    theme_bw()+
    guides(colour=guide_legend(override.aes=list(linetype=c(rep(0,6),1,1,1),shape=c(rep(16,6),NA,NA,NA))))+
    theme(axis.text.y = element_text(angle = 0, hjust =0.5, vjust = 0.5),legend.position=c(0.8,0.2),legend.box.margin=NULL, legend.background = element_rect(fill="transparent"))
  
  
  p3c<-ggplot(r1,aes(x=TAD,y=DV))+
    geom_point(aes(color=STUDYID))+
    #geom_point(aes(col=factor(STUDYID)))+
    xlab(TAD)+ylab(conc)+
    #geom_abline(slope=1)+
    scale_y_log10(limits =limx,breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    scale_x_log10()+
    #scale_x_continuous()+
    annotation_logticks(sides = "l") +
    geom_smooth(aes(y=DV,col="LOESS_OBS"),se=F)+
    geom_smooth(aes(y=IPRED,col="LOESS_IPRED"),se=F)+
    geom_smooth(aes(y=PRED,col="LOESS_PRED"),se=F)+
    scale_colour_manual(name="",values=cols, 
                        guide = "legend") +
    theme_bw()+
    guides(colour=guide_legend(override.aes=list(linetype=c(rep(0,6),1,1,1),shape=c(rep(16,6),NA,NA,NA))))+
    theme(axis.text.y = element_text(angle = 90, hjust =0.5, vjust = 0.5),legend.position=c(0.8,0.2),legend.box.margin=NULL, legend.background = element_rect(fill="transparent"))
  
  ggsave("profile.png",
         ggarrange(p3a,p3b,p3c, ncol=2, nrow=2, common.legend = TRUE, legend="bottom"),dpi = 300, width =12, height = 8,units = c("in"))
  
  ########################################################################################
  
  #cols<-c("grey","black","blue")
  cols<-c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "black", "#FF7F00")
  p4<-ggplot(r1,aes(x=PRED,y=CWRES))+
    geom_point(aes(col=factor(STUDYID)))+
    xlab(PRED)+ylab(CWRES)+
    geom_hline(aes(yintercept=0, col="Line_zero")) +
    geom_smooth(aes(col="LOESS"),se=F)+
    scale_x_continuous(labels = function(x) format(x, scientific =F))+
    scale_y_continuous(limits=c(-6,6),breaks=seq(-6,6,2))+
    theme_bw()+
    scale_colour_manual(name="Observed",values=cols, 
                        guide = "legend") + 
    guides(colour=guide_legend(override.aes=list(linetype=c(rep(0,6),1,1),shape=c(rep(16,6),NA,NA))))+
    theme(legend.position=c(0.8,0.2),legend.box.margin=NULL, legend.background = element_rect(fill="transparent"))
  
  p4a<-ggplot(r1,aes(x=IVAR,y=CWRES))+
    geom_point(aes(col=factor(STUDYID)))+
    xlab(IVAR)+ylab(CWRES)+
    geom_hline(aes(yintercept=0, col="Line_zero")) +
    geom_smooth(aes(col="LOESS"),se=F,span=1)+
    # scale_x_continuous(labels = function(x) format(x, scientific =F))+
    scale_x_continuous(breaks =seq(0,1500,7*24),
                       labels =seq(0,1500,7*24))+
    scale_y_continuous(limits=c(-6,6),breaks=seq(-6,6,2))+
    theme_bw()+
    scale_colour_manual(name="Observed",values=cols, 
                        guide = "legend") + 
    guides(colour=guide_legend(override.aes=list(linetype=c(rep(0,6),1,1),shape=c(rep(16,6),NA,NA))))+
    theme(legend.position=c(0.8,0.2),legend.box.margin=NULL, legend.background = element_rect(fill="transparent"))
  
  p4b<-ggplot(r1,aes(x=TAD,y=CWRES))+
    geom_point(aes(col=factor(STUDYID)))+
    xlab(TAD)+ylab(CWRES)+
    geom_hline(aes(yintercept=0, col="Line_zero")) +
    geom_smooth(aes(col="LOESS"),se=F,span=1)+
    # scale_x_continuous(labels = function(x) format(x, scientific =F))+
    scale_x_continuous(breaks =seq(0,1500,7*24),
                       labels =seq(0,1500,7*24))+
    scale_y_continuous(limits=c(-6,6),breaks=seq(-6,6,2))+
    theme_bw()+
    scale_colour_manual(name="Observed",values=cols, 
                        guide = "legend") + 
    guides(colour=guide_legend(override.aes=list(linetype=c(rep(0,6),1,1),shape=c(rep(16,6),NA,NA))))+
    theme(legend.position=c(0.8,0.2),legend.box.margin=NULL, legend.background = element_rect(fill="transparent"))

  ggsave("CWRES.png",
         ggarrange(p4,p4a,p4b, ncol=1, nrow=3, common.legend = TRUE, legend="bottom"),dpi = 300, width =12, height = 8,units = c("in"))
  
}

#' template for VPC
#'
#' @param data Data frame Use the codes below as template
#' @keywords vpc_plots()
#' @export
#' @examples 

vpc_plots<-function(...){
  
  range(output$SIM.bin)
  out1$SIM.bin<-as.numeric(as.character(out1$SIM.bin))
  
  p<-ggplot(out1, aes(x=SIM.bin,group=OBS.variable)) +
    geom_ribbon(aes(ymin=SIM.low, ymax=SIM.up, fill=SIM.variable, col=SIM.variable, group=SIM.variable), alpha=0.1,col=NA) +
    geom_line(aes(y=SIM.med, col=SIM.variable, group=SIM.variable), size=1) +
    geom_line(aes(y=OBS.value, linetype=SIM.variable), size=1) +
    scale_y_log10()+
    scale_x_continuous(breaks=seq(0,100,12))+
    scale_colour_manual(
      name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
      breaks=c("qt05", "qt50", "qt95"),
      values=c("red", "blue", "red"),
      labels=c("5%", "50%", "95%")) +
    scale_fill_manual(
      name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
      breaks=c("qt05", "qt50", "qt95"),
      values=c("red", "blue", "red"),
      labels=c("5%", "50%", "95%")) +
    scale_linetype_manual(
      name="Observed Percentiles\n(black lines)",
      breaks=c("qt05", "qt50", "qt95"),
      values=c("dotted", "solid", "dashed"),
      labels=c("5%", "50%", "95%")) +
    guides(
      fill=guide_legend(order=2),
      colour=guide_legend(order=2),
      linetype=guide_legend(order=1)) +
    theme(legend.position="top",
          legend.key.width=grid::unit(2, "cm")) +
    labs(x="Time (h)", y="Concentration (ng/mL)")
  p+geom_point(aes(y=DV),col="grey")
  
  blq<-out1[,c("SIM.bin","blqo","blqs")]
  blq$SIM.bin<-as.numeric(as.character(blq$SIM.bin))
  
  ##EXAMPLE BLQ PLOT
  ggplot(blq,aes(x=SIM.bin,y=blqo))+
    geom_point(aes(col="Observed"))+
    geom_line(aes(col="Observed"))+
    geom_point(aes(x=SIM.bin,y=blqs,col="Predicted"))+
    geom_line(aes(x=SIM.bin,y=blqs,col="Predicted"))
   }


#' Compute VPC stats 
#'
#' @param obs.data Observed data. Type vpc_plots forbasic R code to be used with the output 
#' @param sim.data Sim data
#' @param bin Binning. This should be created in both datasets
#' @param prob Quantile probability.  
#' @param sort Sorting as vector 
#' @param dv Concentration or response.Same name in both datasets   
#' @param tad Time after dose. In obs.data
#' @param rtime Time after first dose. In obs.data
#' @param blq LLOQ. Could be single value or vector. If vector, both datasets should have the same variable name. This will compute the % of BLQ for each bin.
#' @param replicate Replicate in sim.data only
#' @param pred.corr Pred-corrected. ex: C("PRED","lin") use the column PRED and corrected in linear scale or log for log scale
#' @keywords lhvpc_stat()
#' @export
#' @examples 

lhvpc_stat<-function (obs.data = obs, sim.data = sim, bin = "bin", prob = c(0.05, 
                                                                            0.5, 0.95), sort = NULL, dv = "DV", tad = "TAD", rtime = "IVAR", 
                      blq = NULL, replicate = "REPLICATE", pred.corr = NULL) 
{
  library(reshape)
  if (!is.null(pred.corr)) {
    medpred <- median(obs.data[, pred.corr[1]])
    if (pred.corr[2] == "lin") {
      obs.data[, dv] <- obs.data[, dv] * medpred/obs.data[, 
                                                          pred.corr[1]]
    } else {
      obs.data[, dv] <- obs.data[, dv] + medpred - obs.data[, 
                                                            pred.corr[1]]
    }
  } else {
    obs.data[, dv] <- obs.data[, dv]
  }
  var <- NULL
  for (i in 1:length(prob)) {
    namqt <- paste0("qt", prob[i] * 100)
    exp<-paste0("quantile(x,",prob[i],")")
    var <- c(var, namqt)
    if (prob[i] == prob[1]) {
      obs1 <- addvar(obs.data, c(sort, "bin"), dv, exp, 
                     "no", namqt)
    } else {
      obs1 <- dplyr::left_join(obs1, addvar(obs.data, c(sort, 
                                                        "bin"), dv,exp, "no", namqt))
    }
  }
  obs1 <- lhlong(obs1, var)
  if (!is.null(pred.corr)) {
    medpred <- median(sim.data[, pred.corr[1]])
    if (pred.corr[2] == "lin") {
      sim.data[, dv] <- sim.data[, dv] * medpred/sim.data[, 
                                                          pred.corr[1]]
    } else {
      sim.data[, dv] <- sim.data[, dv] + medpred - sim.data[, 
                                                            pred.corr[1]]
    }
  } else {
    sim.data[, dv] <- sim.data[, dv]
  }
  var <- NULL
  for (i in 1:length(prob)) {
    namqt <- paste0("qt", prob[i] * 100)
    exp<-paste0("quantile(x,",prob[i],")")
    var <- c(var, namqt)
    if (prob[i] == prob[1]) {
      s1 <- addvar(sim.data, c(sort, bin, replicate), dv, 
                   exp, "no", namqt)
    }else {
      s1 <- dplyr::left_join(s1, addvar(sim.data, c(sort, 
                                                    bin, replicate), dv, exp, "no", 
                                        namqt))
    }
  }
  s1 <- lhlong(s1, var)
  namvar <- c("low", "med", "up")
  for (j in 1:3) {
    exp<-paste0("quantile(x,",prob[j],")")
    if (j == 1) {
      s2 <- addvar(s1, c(bin, "variable"), "value", exp, 
                   "no", namvar[1])
    } else {
      s2 <- dplyr::left_join(s2, addvar(s1, c(bin, "variable"), 
                                        "value", exp, "no", namvar[j]))
    }
  }
  if (!is.null(blq)) {
    if (is.numeric(blq)) {
      obs.data$blq <- obs.data[, dv] <= blq
      sim.data$blq <- sim.data[, dv] <= blq
    }
    else {
      obs.data$blq <- obs.data[, dv] <= obs.data[, blq]
      sim.data$blq <- sim.data[, dv] <= sim.data[, blq]
    }
    blqo <- addvar(obs.data, bin, "blq", "sum(x)", "no", 
                   "blqo")
    blqo <- dplyr::left_join(blqo, addvar(obs.data, bin, 
                                          "blq", "length(x)", "no", "nblqo"))
    blqo$blqo <- blqo$blqo/blqo$nblqo * 100
    blqs <- addvar(sim.data, bin, "blq", "sum(x)", "no", 
                   "blqs")
    blqs <- dplyr::left_join(blqs, addvar(sim.data, bin, 
                                          "blq", "length(x)", "no", "nblqs"))
    blqs$blqs <- blqs$blqs/blqs$nblqs * 100
  } else {
    blqo <- NULL
    blqs <- NULL
  }
  names(obs1) <- paste0("OBS.", names(obs1))
  names(s2) <- paste0("SIM.", names(s2))
  output <- lhcbind(obs1, s2)
  out1 <- dplyr::left_join(lhmutate(obs.data[, c(sort, "bin", 
                                                 dv, tad, rtime)], "bin=SIM.bin"), output)
  if (!is.null(blqo)) {
    out1 <- dplyr::left_join(lhmutate(blqo, "bin=SIM.bin"), 
                             out1)
    out1 <- dplyr::left_join(lhmutate(blqs, "bin=SIM.bin"), 
                             out1)
  } else {
    out1 <- out1
  }
  out1
}

#' Look for keyword across dataset
#'
#' @param data Data frame 
#' @param var Variable to be cut
#' @param file.or.path File path ("xxx/zzz/") or data frame (c("lb","dm")) 
#' @param fpattern File extention: ex. "csv", all csv file in folder will be used  
#' @param filename specify full file name. set fpattern to Null
#' @keywords lhseek()
#' @export
#' @examples 




lhseek<-function (var, file.or.path, fpattern = NULL, filename = NULL)
  
{
  
  if (!is.null(fpattern)) {
    
    list <- dir(file.or.path)[grep(fpattern, dir(file.or.path))]
    
  }
  
  if (!is.null(filename)) {
    
    list = filename
    
  }
  
  if (is.null(filename) & is.null(fpattern)) {
    
    list = file.or.path
    
  }
  
  out <- NULL
  
  for (i in tolower(var)) {
    
    for (j in list) {
      
      b <- function(x) {
        
      }
      
      if (!is.null(filename) | !is.null(fpattern)) {
        
        d <- file.or.path
        
        xt <- substring(j, nchar(j) - 2)
        
        if(xt=="dat"){xt="sas"}else{xt=xt}
        
        rt <- c("read.csv", "haven::read_xpt", "haven::read_sas")
        
        rt <- rt[grep(xt, rt)]
        
        txt <- paste0(rt, "(file.path(d,j))")
        
      } else {
        
        txt <- j
        
      }
      
      body(b) <- parse(text = txt)
      
      inp <- as.data.frame(b())
      
      for (z in names(inp)) {
        
        if (length(tolower(z)[grep(i, tolower(z))]) >
            
            0) {
          
          xx <- paste0(j, ":", z)
          
        } else {
          
          xx = NULL
          
        }
        
        if (length(unique(inp[, z][grep(i, tolower(inp[,
                                                       
                                                       z]))])) > 0) {
          
          out1 <- paste0(xx, j, ";", z, unique(inp[,
                                                   
                                                   z][grep(i, inp[, z])]), ":", i)
          
        } else {
          
          out1 <- c(xx, NULL)
          
        }
        
        out <- c(out, out1)
        
      }
      
      print(out)
      
    }
    
  }
  
}





#' Cut values and create category
#'
#' @param data Data frame 
#' @param var Variable to be cut
#' @param breaks break points 
#' @param labels category name. If fancy, the categories will be created according to the break points 
#' @param right If false, right value will be exclusive
#'  @param newvar vector name of the categorical. If default, the var with suffix"cat" will be used as default name
#' @keywords lhcut()
#' @export
#' @examples 

lhcut<-function(data,var,breaks,labels="fancy",right=F,newvar="default"){
  brk=c(min(data[,var]),breaks,max(data[,var])^2)
  if(newvar=="default"){nvar=paste0(var,"cat")}else{nvar=newvar}
  if(labels=="fancy"){
    lab1=c(paste0("<",breaks))
    lab2<-c(paste0(">=",breaks))
    lab3<-c(paste0("<=",breaks))
    lab4<-c(paste0(">",breaks))
    lab11<-NULL;lab22<-NULL
    for(i in 1:(length(breaks)-1)){
      lab11<-c(lab11,paste(lab2[i],"&",lab1[i+1]))
      lab22<-c(lab22,paste(lab4[i],"&",lab3[i+1]))
    }
    if(right){
      labels1<-c(lab3[1],lab22,lab4[length(lab4)])}else{ 
        labels1<-c(lab1[1],lab11,lab2[length(lab2)])
      } }else{labels1=labels}
  
  data[,nvar]<-cut(data[,var],breaks=brk,labels=labels1,right=right)
  print(addvar(data,nvar,var,"range(x)","no"))
  data}


#' Change factor level of a variable using matched level of another variable in the same dataset
#'
#' @param data Data frame 
#' @param leader lead Variable to be used for factor level of the follower variable
#' @param follower follower Variable  
#' @keywords lhfactor()
#' @export
#' @examples 

lhfactor<-function(data=test,leader="AGEcat",follower="catt"){
  lab<-nodup(data,c(leader,follower),"var");lab<-lab[order(lab[,leader]),follower]
  data<-reflag(data,follower,lab)
}




#' Combine variables in the same column
#'
#' @param data Data frame 1 and 2 with long vectors and values. Note: no duplicated sorting vector allowed 
#' @param combine.var Variable name, c(var1,var2). var1 will be stacked over var 2
#' @keywords stackvar()
#' @export
#' @examples
stackvar<-function(data,combine.var=c("xxx","variable")){
  z$dum<-seq(nrow(z))
  z1<-z
  z1$dum<-z1$dum-1
  z1<-nodup(z1,combine.var[1],"all")
  keep<-c(combine.var[1],combine.var[2],"dum")
  z1[,combine.var[2]]<-z1[,combine.var[1]];z1[,!names(z1)%in%keep]<-""
  z<-rbind(z,z1[,names(z)]); z<-z[order(z$dum),]
  z[,c(combine.var[1],"dum")]<-NULL
  z}



#' mutate variable names
#'
#' @param data Data frame 1 and 2 with long vectors and values. Note: no duplicated sorting vector allowed 
#' @param mutate Vector to be mutated ex. "xxx=yyy" for renaming xxx as yyy
#' @keywords lhmutate()
#' @export
#' @examples
lhmutate<-function(data,mutate){
  keep<-sub("=.*","",mutate)%in%names(data)
  imp<-sub(".*=","",mutate)[keep]
  bimp<-sub("=.*","",mutate)[keep]
  print(c("Not found:",sub("=.*","",mutate)[!sub("=.*","",mutate)%in%names(data)]))
  
  for(i in 1:length(bimp)){
    names(data)[names(data)==bimp[i]]<-imp[i]
  }
  data
}



#' merge and make long to wide data frame
#'
#' @param dat1,dat2 Data frame 1 and 2 with long vectors and values. Note: no duplicated sorting vector allowed 
#' @param long.vector Vector containing wide variable names
#' @param value Column containing values associated with each wide variable. 
#' @keywords lhmerge_long_data()
#' @export
#' @examples
lhmerge_long_data<-function (dat1=dd1, dat2=dd2, long.vector1="PARAMCD", long.vector2="PARAMCD", value1="AVAL", value2="CHG") 
{
  names(dat1)[names(dat1) == long.vector1] <- "x"
  names(dat1)[names(dat1) == value1] <- "y"
  names(dat2)[names(dat2) == long.vector2] <- "x"
  names(dat2)[names(dat2) == value2] <- "y"
  dat2$x[dat2$x == dat1$x] <- paste0(dat2$x[dat2$x == dat1$x], 
                                     "dat2")
  datall <- lhrbind(dat1, dat2)
  head(datall)
  if(nrow(dup1(datall, names(datall)[!names(datall)%in%"y"],"all"))>0){
    print("duplicated data")
    output<-dup1(datall, names(datall)[!names(datall)%in%"y"],"all")
  }else{output <- lhwide(datall, "y", "x")}
}

#' merge wide data frames
#'
#' @param dat1,dat2 Data frame 1 and 2 with long vectors and values. Note: no duplicated sorting vector allowed 
#' @param by1,by2 Vectors containing matched values from each data frames. 
#' @keywords lhmerge_long_data()
#' @export
#' @examples
lhmerge_wide_data<-function(dat1,dat2,by1,by2){
  d1<-chclass(dat1,names(dat1),"char");d2<-chclass(dat2,names(dat2),"char")
  d1<-lhlong(d1,names(d1)[!names(d1)%in%by1]);d2<-lhlong(d2,names(d2)[!names(d2)%in%by2])
  head(d1)
  d1<-chclass(d1,names(d1),"char");d2<-chclass(d2,names(d2),"char")
  d2$variable<-as.character(d2$variable);d1$variable<-as.character(d1$variable)
  d2$variable[d2$variable%in%d1$variable]<-paste0(d2$variable[d2$variable%in%d1$variable],"d2")
  datall<-lhrbind(d1,d2)
  head(datall)
  unique(datall$variable)
  test<-nodup(datall,c(by1,by2,"variable"),"all")
  test<-test[order(test$USUBJID,as.Date(test$date)),]
  output<-lhwide(test,"value","variable")
}


#' Reshape wide
#'
#' @param data Dataset
#' @param wide.data Name of vector containing data to be dcasted
#'  @param wide.vector Name of vector to be reshape as heading 
#' @param data Dataset 
#' @keywords lhwide()
#' @export
#' @examples
#' lhwide()
lhwide<-function(data,wide.data,wide.vector){
  b <- function(x) {}
  x1<-paste(paste(names(data)[!names(data)%in%c(wide.data,wide.vector)],collapse="+"),"~",wide.vector)
  body(b) <- parse(text = x1)
  z1<-dcast(data,b())}

#' Reshape long
#'
#' @param data Dataset
#' @param long.vector List of vector to be melted
#'
#' @keywords lhlong()
#' @export
#' @examples
#' lhlong()
lhlong<-function(data,long.vector){
  z1<-melt(data,names(data)[!names(data)%in%long.vector])
}


#' find different values between two datasets
#'
#' @param dat1,dat2 Dataset 1 and 2" 
#' @keywords findiff()
#' @export
#' @examples
#' findiff()

findiff<-function(dat1,dat2){
 # stopifnot(nrow(dat1)==nrow(dat2))
  dum1a<-""
  nm1<-"dat1"
  dum2a<-""
  nm2<-"dat2"
  for(i in 1:length(names(dat1))){
    nm1<-paste(nm1,names(dat1)[i],sep="/")
    dum1a<-paste(dum1a,dat1[,names(dat1)[i]],sep="/")
    nm2<-paste(nm2,names(dat2)[i],sep="/")
    dum2a<-paste(dum2a,dat2[,names(dat2)[i]],sep="/")
  }
  a<-setdiff(dum1a,dum2a)
  b<-setdiff(dum2a,dum1a)
  out<-data.frame(nm1=unique(a));names(out)<-nm1
  out1<-data.frame(nm2=unique(b));names(out1)<-nm2
  row1<-data.frame(N1=length(dum1a),N2=length(dum2a))
  out3<-lhcbind(out,out1)
  out3<-lhcbind(out3,row1)
  out3 }


#' lhjoin funtion
#'Join two datasets and print report of joining procedure
#' @param dat1,by1 Data frame 1 and variables to be matched. If NULL, match="all"
#' @param dat2,by2 Data frame 2 and variables to be matched. If by1=NULL then by2=NULL then match="all"
#' @param type could be "full", "left","right" or "inner"
#' @keywords lhjoin()
#' @export
#' @examples
#' lhjoin()
lhjoin<-function(dat1,by1=NULL,dat2,by2=NULL,type="full"){
  invar<-intersect(names(dat1),names(dat2))
  if(is.null(by1)){
    by1=invar}else{
      by1=by1}
  if(is.null(by2)){
    by2=invar
  }else{
    by2=by2
    names(dat2)[names(dat2)%in%invar]<-paste0("df2_",names(dat2)[names(dat2)%in%invar])
  }
  
  if(length(by1)>1){
    dat1[,"dum"]<-dat1[,by1[1]]
    for(i in 2:length(by1)){
      dat1[,"dum"]<-paste(dat1[,"dum"],dat1[,by1[i]],sep="-")
    }}else{dat1[,"dum"]<-dat1[,by1[1]]}
  
  by2[!by2%in%by1]<-paste0("df2_",by2[!by2%in%by1])
  
  if(length(by2)>1){
    dat2[,"dum"]<-dat2[,by2[1]]
    for(i in 2:length(by2)){
      dat2[,"dum"]<-paste(dat2[,"dum"],dat2[,by2[i]],sep="-")
    }}else{dat2[,"dum"]<-dat2[,by2[1]]}
  
  dat<-plyr::join(dat1,dat2,by="dum",type=type)
  
  report<-data.frame(nrow_data1=nrow(dat1),
                     nrow_data2=nrow(dat2),
                     nrow_joint=nrow(dat))
  for(c in 1:length(by1)){    
    x<-data.frame(z=setdiff(dat1[,by1[c]],dat2[,by2[c]]))
    names(x)<-paste0(by1[c],"_not_in_data2")
    y<-data.frame(z=setdiff(dat2[,by2[c]],dat1[,by1[c]]))
    names(y)<-paste0(by1[c],"_not_in_data1")
    zz<-lhcbind(x,y)
    report<-lhcbind(report,zz)
  }
  print(report)
  dat}



#' lhorder funtion
#'
#' Make simple table. Use data frame created by addvar2
#' @param dat Datframe
#' @param var Order by variables. ex: ":Trt,:Agegr"
#' @keywords lhorder()
#' @export
#' @examples
#' lhorder()

lhorder<-function(dat,var){
  data<-dat
  x<-paste0("data[order(",gsub(":","data$",var),"),]")
  b<- function(x) {}
  body(b) <- parse(text = x)
  data<-b()
}



#' lhtab funtion
#'
#' Make simple table. Use data frame created by addvar2
#' @param data Datframe
#' @param vh Vertical and horizontal headers. ex: "Trt+Agegr~Param"
#' @param value Values. Example: c("mean","SD","Mean (CV)")
#' @param ord Order variables. ex: ":Trt,:Agegr"
#' @param save.name Save table as word document. Enter the file name: ex "test.docx"
#' @param output output="csv" for csv output, else output will be in FlexTable format
#' @keywords lhtab()
#' @export
#' @examples
#' lhtab()

lhtab<-function (data, vh, value, ord = NULL, save.name = NULL, output = "csv") 
{
  library(reshape)
  b <- function(x) {
  }
  body(b) <- parse(text = vh)
  
  #vh<-"group~label+ss+test"
  v <- gsub("+", ":", sub("~.*", "", vh), fixed = T)
  v <- unlist(strsplit(v, ":")) 
  h <- gsub("+", ":", sub(".*~", "", vh), fixed = T)
  list(gsub(":", ",", h))
  h <- unlist(strsplit(h, ":"))
  data$dum<-""
  for(i in h){
    if(i==h[1]){
      data$dum<-data[,i]}else{data$dum<-paste(data$dum,data[,i],sep="_")}
  }  
  
  
  hd<-nodup(data,c("dum",v,h),"var")
  w<-NULL
  for(uu in value){
    data[,"stats"]<-uu
    w1 <- reshape(data[,c(v,"dum",uu,"stats")], 
                  timevar ="dum",
                  idvar =c(v,"stats"),
                  direction = "wide")
    for(u in names(data[,c(v,"dum",uu,"stats")])){
      rm<-paste0(u,".")
      names(w1)<-gsub(rm,"",names(w1),fixed = T)
    }
    w<-rbind(w,w1)}
  hw<-NULL
  
  for(d in h){
    hd[,"stats"]<-"stats"
    hw1 <- reshape(hd[,c("dum",v,d,"stats")], 
                   timevar ="dum",
                   idvar =c(v,"stats"),
                   direction = "wide")
    
    for(u in names(hd[,c("dum",v,d,"stats")])){
      rm<-paste0(u,".")
      names(hw1)<-gsub(rm,"",names(hw1),fixed = T)
    }
    hw1<-nodup(hw1,names(hw1)[!names(hw1)%in%c(v,"stats")],"all")
    hw<-rbind(hw,hw1)
  }
  hw<-hw[,unique(names(hw))]
  for(vv in v){
    hw[,vv]<-vv 
  }
  
  setdiff(names(w),names(hw))
  
  hw1<-rbind(hw,w)
  head(hw1,10)
  
  
  if (!is.null(ord)) {
    y <- paste0(ord, ",:stats")
  }else {
    y <- ":stats"
  }
  stor<-c("stats",value)
  hw1[,"stats"]<-factor(hw1[,"stats"],level=stor)
  head(hw1)
  
  hw1 <- lhorder(hw1,y)
  
  hw1 <- chclass(hw1, names(hw1), "char")
  tab <- ReporteRs::FlexTable(hw1, header.columns = FALSE)
  
  for (y in c(v,"stats")) {
    tab = ReporteRs::spanFlexTableRows(tab, j = y, runs = as.character(hw1[, 
                                                                           y]))
  }
  t4 <- hw1
  colnames(t4) <- NULL
  rownames(t4) <- NULL
  for (z in 1:length(h)) {
    tab = ReporteRs::spanFlexTableColumns(tab, i = z, runs = paste(t4[z, 
                                                                      ]))
  }
  tab[1:length(h), ] = ReporteRs::textProperties(font.weight = "bold")
  tab[, names(hw1)] = ReporteRs::parCenter()
  if (!is.null(save.name)) {
    doc <- ReporteRs::docx()
    doc <- ReporteRs::addFlexTable(doc, tab)
    ReporteRs::writeDoc(doc, save.name)
    ReporteRs::writeDoc(doc, save.name)
  }
  if (output != "csv") {
    res <- tab
  }
  else {
    res <- hw1
  }
  res
}




#' txt funtion
#'
#' Clone expression function for adding special formats and symbol to plots.
#' @param c text. Example: c("Concentration mg L","-1::s"," AUC::u"," Delta::i","moles::e"). s=subscript, u=underline, Delta= capital greek delta letter, i= italic, e=superscript
#' @keywords txt()
#' @export
#' @examples
#' txt()

txt<-function(c){
  z1<-""
  for(j in 1:length(c)){
    if(length(grep("::",sub(".*::","::", c[j])))==0){z=c[j]}else{
      if(length(grep(":e",sub(".*:e",":e", c[j])))!=0){
        z=paste0("^{",gsub(sub(".*::", "::",c[j]),"",c[j]),"}")}
      if(length(grep(":s",sub(".*:s",":s", c[j])))!=0){
        z=paste0("[",gsub(sub(".*::", "::",c[j]),"",c[j]),"]")}
      if(length(grep(":u",sub(".*:u",":u", c[j])))!=0){
        z=paste0(" underline(",gsub(sub(".*::", "::",c[j]),"",c[j]),")")}
      if(length(grep(":i",sub(".*:i",":i", c[j])))!=0){
        z=paste0(" italic(",gsub(sub(".*::", "::",c[j]),"",c[j]),")")}}
    z1=paste0(z1,z)}
  z1=gsub(" ","~",z1)
  z1=paste0("expression(",z1,")")
  b <- function(x) {}
  body(b)<-parse(text=z1)
  text<-b()
}




#' install.pack
#'
#' To install require packages. Use ipak function to install desired packages.
#' @param packages pre-define packages list.
#' @keywords install.pack
#' @export
#' @examples
#' install.pack()

install.pack<-function(...){
  packages <- c("SASxport", "reshape", "Hmisc", "tidyr","ReporteRs","plyr","downloader")
  ipak(packages)}


#' lhtemplate
#'
#' To install require packages. Use ipak function to install desired packages.
#' @param repo leonpheng
#' @param user logo.style
#' @keywords lhtemplate
#' @export
#' @examples
#' lhtemplate()
lhtemplate <- function(){
  require(downloader)
  dir.create("c:/lhtemplate")
  wf<-"c:/lhtemplate"
  url <- sprintf("https://github.com/%s/%s/archive/master.zip", "leonpheng","logo.style")
  tmp <- tempfile(fileext = "styleapp.zip",tmpdir=wf)
  download(url, tmp)
  unzip(tmp,exdir=wf)

  #download_repo("logo.style","leonpheng")
  zipF<-paste0(wf,"/logo.style-master/styleapp.zip")
  unzip(zipF,exdir=wf)
  zipF<-paste0(wf,"/logo.style-master/logostyle.zip")
  unzip(zipF,exdir=wf)
  frm<-dir(wf)
  index<-c(grep("zip",frm))
  frm<-frm[index]
  for(i in frm){
    file.remove(paste(wf,i,sep="/"))
  }
  unlink(paste0(wf,"/logo.style-master"), recursive = T)
}

#' lhdoc
#'
#' Create doc for word document using this fonction or you can do it manually.
#' Type lhtext and copy the template to R workspace and start writing.
#' @param t template name see at the end of lhtext function.
#' @param toc.level maximimum toc level
#' @param template Word document template could be used for styles. Styles should be mapped in style.to.map. Template is also available at github: to load it, just run  lhtemp() once to download and store the templates in your PC at "c:lhtemplate. Note that the templates and logo are also used in xptdef package.

#' @param TOC Set to F if no TOC wanted
#' @param style.to.map Map the styles in template to be used. Ex: mypar is for footnote (font size)
#' @keywords lhdoc
#' @export
#' @examples
lhdoc<-function(toc.level=4,template="default",TOC=F,
        style.to.map="default",logo.certara=F){

  library(ReporteRs)
  library(flextable)
  library(dplyr)

  if(template=="default"){
  doc<-docx(template = "c:/lhtemplate/styleapp.docx", empty_template = TRUE)}else{doc<-docx(template =template, empty_template = TRUE)}
if("default"%in%style.to.map){
  doc <- map_title(doc, stylenames =c("Heading1",
                                     "Heading2", "Heading2", "fnt"))}else{doc <- map_title(doc, stylenames =style.to.map)}
  if(logo.certara){
    doc<-addImage(doc,"c:/lhtemplate/logo.jpg", par.properties = parProperties(text.align = "center"),width = 3.35, height = 1.6)
  }
  if(TOC){
    doc <-addTOC(doc,level_max =toc.level)
  }
  doc
  }


#' lhrbind
#'
#' r bind 2 data frames regardless number of columns.
#' @param dat1,dat2 data frames.
#'
#' @keywords lhrbind
#' @export
#' @examples
#' lhrbind()

lhrbind<-function (dat1, dat2, na.replace = NA, all.character = T) 
{
  dat1[, setdiff(names(dat2), names(dat1))] <- na.replace
  dat2[, setdiff(names(dat1), names(dat2))] <- na.replace
  if (all.character) {
    dat <- rbind(chclass(dat1, names(dat1), "char"), chclass(dat2, 
                                                             names(dat2), "char"))
    print("Warning: all vectors in new dataset are character")
    dat
  }
  else (dat <- rbind(dat1, dat2))
}

#' lhcbind
#'
#' C bind 2 data frames regardless number of row length.
#' @param dat1,dat2 data frames.
#'
#' @keywords lhcbind
#' @export
#' @examples
#' lhcbind()

lhcbind<-function(dat1,dat2){
  dat1=as.data.frame(dat1)
  dat2=as.data.frame(dat2)
  r1<-nrow(dat1)
  r2<-nrow(dat2)
  if(r1>r2){
    r3=as.data.frame(matrix(ncol=ncol(dat2),nrow=r1-r2,data=""))
    names(r3)<-names(dat2)
    r3=rbind(dat2,r3)
    dat=cbind(dat1,r3)
  }
  if(r1<r2){
    r3=as.data.frame(matrix(ncol=ncol(dat1),nrow=r2-r1,data=""))
    names(r3)<-names(dat1)
    r3=rbind(dat1,r3)
    dat=cbind(r3,dat2)
  }
  if(r1==r2){dat=cbind(dat1,dat2)}
  dat
}

#' lhloess
#'
#' Compute the LOESS data for ploting.
#' @param data data.
#' @param x Independent variable
#' @param y Dependent variable
#' @param by Sort by. Only one sorting variabele is accepted. If more than 1 variables, create a unique sorting using paste(var1,var2,etc)
#' @param span LOESS stiffness
#' @keywords lhloess
#' @export
#' @examples
#' lhloess()


lhloess<-function(data,x,y,by,span=1){
  library(plyr)
  data$x=data[,x]
  data$y=data[,y]
  data$by=data[,by]
  head(data)
  dat=NULL
  for(i in unique(data$by)){
    tmp<-data[data$by==i,c(x,"x","y")]
    head(tmp)
    tmp1<-with(tmp,unlist(predict(loess(y~x,tmp,span=span),x)))
    tmp$loess<-tmp1
    dat<-rbind(dat,tmp)
  }
  #data$x<-data$y<-data$by<-NULL
  data<-join(data,dat)
}



#' load.pack1
#'
#' To install require packages. Use ipak function to install desired packages.
#' @param packages pre-define packages list.
#' @keywords load.pack1
#' @export
#' @examples
#' load.pack1()

loadpack<-function(...){
  library("ReporteRs")
  require(dplyr)
  library(plyr)
  require(stats)     # To format summary
  require(PCSmisc)     #
     #This package is needed to add a dataset label,it can only be used  with the 32bit version of R. If dataset label is not needed, the package SASxport can be used
  require(tidyr)
}


#' Ben's function
#'
#' internal used
#' @param packages pre-define packages list.
#' @keywords blk.locf
#' @export
#' @examples
#' blk.locf2()

blk.locf2<-function (x, id, na.action = c("fill", "carry.back"), fill = NA)
{
  .checkID(id)
  if (length(x) != length(id)) {
    stop("Lengths of x and id must match")
  }
  na.action <- match.arg(na.action)
  ii <- ifelse(is.na(x), NA, .myseqalong(x))
  ii <- unlist(tapply(ii, id, function(y, na.action) {
    if (all(is.na(y)))
      return(y)
    z <- cumsum(!is.na(y))
    if (na.action == "carry.back") {
      z[z == 0] <- 1
    }
    ifelse(z == 0, NA, y[!is.na(y)][ifelse(z > 0, z, 1)])
  }, na.action = na.action, simplify = FALSE))
  y <- x
  y[!is.na(ii)] <- x[ii[!is.na(ii)]]
  y[is.na(ii)] <- fill
  y
}



#########
#' TAD from ADDL
#'
#' This function allows you to derive time after dose from ADDL.
#' @param data data frame
#' @param id ID vector
#' @param ii dose interval vector
#' @param addl additional dose vector
#' @param rtime relative time after first dose vector
#' @param evid EVID vector
#' @param dose amount adminstered (ex: AMT) vector
#' @param dose.expand If "yes", all dosing rows in ADDL will be outputed
#' @keywords tad
#' @export
#' @examples
#' tad_addl()

tad_addl<-function (data, id, ii, addl, 
                    rtime, evid, dose.expand = "yes") 
{
  data <- chclass(data, c(rtime, evid, addl, ii), "num")
  data[, addl][is.na(data[, addl])] <- 0
  data[, ii][is.na(data[, ii])] <- 0
  data[, "TAD"] <- data[, "tad"] <- NULL
  nam <- names(data)
  data <- data[order(data[, id], data[, rtime]), ]
  dose <- data[data[, evid] == 1, ]
  dose[, "TAD"] <- 0
  datp <- data[data[, evid] != 1, ]
  dat0 <- data[data[, evid] == 1, ]
  datr <- NULL
  for (i in 1:nrow(dat0)) {
    dat1 <- dat0[i, ]
    if (dat1[, addl] == 0) {
      dat2 <- dat1
    }    else {
      dat2 <- as.data.frame(matrix(ncol = ncol(dat1), nrow = dat1[, 
                                                                  addl]+1))
      names(dat2) <- names(dat1)
      dat2[, names(dat2)] <- dat1
      dat2$dum <- seq(0, nrow(dat2) - 1, 1)
      dat2[, rtime] <- dat2[, rtime] + (dat2[, ii] * dat2$dum)
      dat2$dum <- NULL
    }
    datr <- rbind(datr, dat2)
  }
  dup1(datr, names(datr), "all")
  datr <- nodup(datr, names(datr), "all")
  datr[, addl] <- datr[, ii] <- 0
  setdiff(names(datr), names(datp))
  datr$loc1 <- datr[, rtime]
  datr$lhdose <- "yes"
  datp$loc1 <- NA
  datp$lhdose <- "no"
  datp1 <- rbind(datp, datr)
  datp1 <- datp1[order(datp1[, id], datp1[, rtime]), ]
  head(datp1)
  datp1 <- locf2(datp1, id, "loc1")
  datp1$TAD <- datp1[, rtime] - datp1$loc1
  datp1$TAD[datp1$TAD < 0] <- 0
  range(datp1$TAD)
  if (dose.expand != "yes") {
    d1 <- datp1[datp1$lhdose == "no", ]
    d1$loc1 <- d1$lhdose <- NULL
    data <- rbind(d1, dose)
    data <- data[order(data[, id], data[, rtime]), ]
  }  else {
    data <- datp1[, !names(datp1) %in% c("loc1", "lhdose")]
  }
  data
}


###########
#' BLQ M6 Method
#'
#' This function allows you to create data with BLQ using M6 method.
#' @param data data frame
#' @param id ID vector
#' @param evid EVID vector
#' @keywords m6
#' @export
#' @examples
#' tad_addl()
m6<-function(data,id,evid,mdv,blq.flag,time,dv,lloq){
  dat<-data
  #id="id";time="RTIME";mdv="mdv";evid="evid";blq.flag="blqf";dv="dv";lloq=0.01
  dat$cum<-cumsum(dat[,evid])
  dat$cum1<-cumsum(dat[,blq.flag])

  good<-addvar(dat[dat[,evid]==0&dat[,mdv]==0,],c(id,"cum"),time,"max(x)","no","good")
  good1<-addvar(dat[dat[,time]>0&dat[,blq.flag]==1,],c(id,"cum1"),time,"min(x)","no","good1")
  good1[,time]<-good1$good1
  good

  m4<-plyr::join(dat,good)
  m4<-plyr::join(m4,good1)
  good2<-addvar(m4[m4$good<=m4$good1,],c(id,"cum"),"good1","min(x)","no","good2")
  good2[,time]<-good2$good2

  good3<-addvar(m4[m4$good>=m4$good1,],c(id,"cum"),"good1","max(x)","no","good3")
  good3[,time]<-good3$good3

  m4<-plyr::join(m4,good2)
  m4<-plyr::join(m4,good3)
  m4$dvm6<-m4[,dv]
  m4$mdvm6<-m4[,mdv]

m4$dvm6[m4[,time]==m4$good2|m4[,time]==m4$good3]<-lloq/2
m4$mdvm6[m4[,time]==m4$good2|m4[,time]==m4$good3]<-0
m4$cum<-m4$cum1<- m4$good<-m4$good1<-m4$good2<-m4$good3<-NULL
  m4
}


#-------------------------
#' Reflag variables
#'
#' This function allows you to change variable name (ex: "M" to "Male").
#' @param dat data frame
#' @param var Vector to be changed
#' @param orignal.flag Original names (ex:c("M","F"))
#' @param new.flag New names (ex:c("Male","Female"))
#' @param newvar Create new vector
#' @keywords reflag
#' @export
#' @examples
#' reflag(dat,var="SEX",c("M","F"),c("Male","Female"),"SEXCH"))
reflag<-function (dat, var, orignal.flag, new.flag=NULL,newvar=NULL,to.factor=T,missing=c("",".","NA",NA)) 
{
  if(is.null(new.flag)){
    new.flag=orignal.flag
  }else{new.flag}
  forgot<-setdiff(dat[,var],orignal.flag)
  forgot<-forgot[!forgot%in%missing]
  print(paste("forgot:",forgot))
  stopifnot(length(forgot)==0)
  dat[,var]<-as.character(dat[,var])
  dat[dat[,var]%in%missing,var]<-"missing or unknown"
  orignal.flag<-as.character(orignal.flag)
  new.flag<-as.character(new.flag)
  dat[,var]<-as.character(dat[,var])
  if(!is.null(newvar)){
    dat[,newvar]<-factor(dat[,var],levels=c(orignal.flag,"missing or unknown"),
                         labels=c(new.flag,"missing or unknown"))
    if(to.factor==F){
      dat[,newvar]<-as.character(dat[,newvar])
    }}else{dat[,var]<-factor(dat[,var],levels=c(orignal.flag,"missing or unknown"),
                             labels=c(new.flag,"missing or unknown"))
    if(to.factor==F){
      dat[,var]<-as.character(dat[,var])
    }}
  dat
}
#-------------------------
#' Derived 1 variable and 1 function
#'
#' This function allows you to add derived variable (ex: add mean value by ID).
#' @param dat data frame
#' @param sort sort derived variable by (ex:c("ID","SEX"))
#' @param var variable to be derived
#' @param fun deriving funtion ex:"mean(x)")
#' @param add.to.data if "yes" result will be appended to dat
#' @param name column name of derived variable
#' @keywords addvar
#' @export
#' @examples
#' addvar()
addvar<-function(dat,sort,var,fun,add.to.data="yes",name=NULL){
  library(plyr)
  d<-dat
  a<-fun
  if(is.null(name)){name=paste0(gsub("(x)",var,fun))}
  b<-function(x){}
  body(b)<-parse(text=a)

  if(length(sort)>1){dd<-(aggregate(d[,var],d[,sort],b))}else{dd<-(aggregate(d[,var],list(d[,sort]),b))}
  names(dd)<-c(sort,name)
  if(add.to.data=="yes"){out<-plyr::join(dat,dd,type="left")}else{out<-dd}}

#-------------------------
#' Derived more variables and functions
#'
#' This function allows you to add derived variable (ex: add mean value by ID).
#' @param dat data frame
#' @param sort sort derived variable by (ex:c("ID","SEX"))
#' @param var variable to be derived
#' @param fun deriving funtion ex:c("mean(x)=mean","length(x[is.na(x)])")
#'
#' @keywords addvar
#' @export
#' @examples
#' addvar()

addvar2<-function (dat, sort, var, fun, rounding = "sigfig(x,3)") 
{
  tmp1 <- NULL
  stn <- NULL
  for (z in 1:length(fun)) {
    fy = gsub("=", "", sub(".*=", "=", fun[z]))
    fx <- gsub(sub(".*=", "=", fun[z]), "", fun[z])
    tmp <- NULL
    stn <- c(stn, fy)
    for (v in var) {
      t <- addvar(dat = dat, sort = sort, var = v, fun = fx, 
                  add.to.data = "no", name = fy)
      t$var <- v
      tmp <- rbind(tmp, t)
      tmp[, fy] <- as.numeric(as.character(tmp[, fy]))
      rounding1 <- "round(x,10)"
      if (!fy %in% c("N", "n")) {
        a <- gsub("x", "tmp[,fy]", rounding1)
        b <- function(x) {
        }
        body(b) <- parse(text = a)
        tmp[, fy] <- b()
      }
      else {
        tmp
      }
    }
    if (z == 1) {
      tmp1 <- tmp
    }
    else {
      tmp1 <- join(tmp1, tmp)
    }
  }
  a <- rounding
  b <- function(x) {
  }
  body(b) <- parse(text = a)
  for (z in stn[!stn %in% c("N", "n")]) {
    for(x in 1:nrow(tmp1)){
      tmp1[,z] <- b(as.numeric(tmp1[, z]))
    }
  }
  tmp1
}


#######ADD TIME########
#-------------------------
#' Add time in hour to calendar date/time
#'
#' This function allows you to add derived variable (ex: add mean value by ID).
#' @param datetime date/time vector to be computed
#' @param timehour Time to be added in hour
#' @param format date and time format
#' @param tz Time zone (default="GMT")
#' @param add.to.data if "yes" result will be appended to dat
#' @param name column name of derived variable
#' @keywords addtime
#' @export
#' @examples
#' addtime()

addtime<-function(datetime,timehour,format="%Y-%m-%d %H:%M",tz="GMT"){
  output<-substring(strptime(datetime,format=format,tz=tz)+timehour*60*60,1,16)
  output}


###TAD calculation using elapsed RTIME#########
#-------------------------
#' Derive TAD from RTIME
#'
#' This function allows you to derive Time after dose from Time after first dose.
#' @param data data frame
#' @param id subject id
#' @param time time after first dose
#' @param evid evid
#' @keywords rt2tad
#' @export
#' @examples
#' rt2tad()
rt2tad<-function(data,id,time,evid){
  #id="uid";time="T";evid="evid"
  data$cumsum<-unlist(tapply(data[,evid],list(data[,id]),cumsum))
  nrow(data)
  data$time<-data[,time]
  d<-data[data[,evid]==1,c(id,time,"cumsum")]
  names(d)<-c(id,"time1","cumsum")
  data$sort<-seq(1,nrow(data),1)
  d<-nodup(d,c(id,"cumsum"),"all")
  data1<-plyr::join(data,d,type="left")
  data1<-chclass(data1,c("time","time1"),"num")
  data1$tad<-with(data1,time-time1)
  data1$ndose<-data1$cumsum
  data1<-data1[order(data1$sort),];data1$sort<-data1$time1<-data1$time<-data1$cumsum<-NULL
  data1
}
####create NMEM UNIQUE SUBJECT####
#-------------------------
#' NMID
#'
#' This function allows you to create NMID.
#' @param data data frame
#' @param id subject id
#' @param varname column name
#' @keywords nmid
#' @export
#' @examples
#' nmid()
nmid<-function(data,id,varname="NMID"){
  id=id;varname=varname
  data<-data
  data$ord<-seq(1,nrow(data),1)
  idat<-data.frame(id=unique(data[,id]),varname=seq(1,length(unique(data[,id])),1))
  names(idat)<-c(id,varname)
  data<-merge(data,idat)
  data<-data[order(data[,varname],data$ord),]
  data$ord<-NULL
  data
}
###Elapse Time###
#-------------------------
#' Compute delta using calendar date and time
#'
#' This function allows you to compute the delta (time1-time2).
#' @param tm1 data frame
#' @param tm2 subject id
#' @param form1 date/time format 1
#' @param form2 date/time format 2
#' @keywords diftm
#' @export
#' @examples
#' diftm()
diftm<-function(tm1,tm2,unit="hour",form1="%Y-%m-%d %H:%M",form2="%Y-%m-%d %H:%M",tz="GMT"){
dat<- as.numeric(difftime(strptime(tm1,format=form1,tz=tz),strptime(tm2,format=form2,tz=tz), units=unit))
 dat}

######change date and time format
#-------------------------
#' Reformat calendar date/time
#'
#' This function allows you to change the date/time format.
#' @param dttm original date/time
#' @param tm2 subject id
#' @param form1 date/time format 1 to be changed
#' @param form2 new date/time format
#' @keywords format_time
#' @export
#' @examples
#' format_time()

format_time<-function(dttm,form1,form2="%Y-%m-%d %H:%M",tz="GMT"){
  strftime(strptime(dttm,format=form1,tz=tz),format=form2,tz=tz)
}
####SUMMARY table
#-------------------------
#' Quick table
#'
#' This function allows you to create quick and dirty table.
#' @param data original date/time
#' @param var1 var1
#' @param var2 var2
#' @keywords tab
#' @export
#' @examples
#' tab()
tab<-function(data,var1,var2){
  data[,"var1"]<-data[,var1]
  data[,"var2"]<-data[,var2]
  xtabs(~var1+var2,data)
}
##Change class
#-------------------------
#' Change variable class
#'
#' This function allows you to change variable class ("num" or "char").
#' @param data data
#' @param var variable (ex:c("DV","MDV"))
#' @param class class ("char" or "num")
#' @keywords chclass
#' @export
#' @examples
#' chclass()
chclass<-function(data,var,class="char"){
   for(i in var){
    if (class=="num"){
      data[,i]<-as.numeric(as.character(data[,i]))}
    else {data[,i]<-as.character(data[,i])}
  }
  data
}
#print unique variable only
#-------------------------
#' one
#'
#' one.
#' @param data data
#' @param var variable
#' @keywords one
#' @export
#' @examples
#' one()
one<-function(data,var){
  for(i in var){
    print(i)
    print(data[!duplicated(data[,i]),i])
  }
}
# keep unique only
#-------------------------
#' No duplicate
#'
#' This function allows you to remove duplicates.
#' @param data data
#' @param var variable (ex:c("DV","MDV"))
#' @param all if all="all", all columns in data will be kept (ex:all=c("ID","DV"))
#' @keywords nodup
#' @export
#' @examples
#' nodup()
nodup<-function(data,var,all,item){
  if(all=="all"){d1<-data[!duplicated(data[,var]),names(data)]}else{
    if(all=="var"){d1<-data[!duplicated(data[,var]),var]}else{
      d1<-data[!duplicated(data[,var]),c(var,item)]}}
  d1
}
#Output duplicated row for checking or remove duplicates if remove is set to non-NULL
#-------------------------
#' Check duplicates
#'
#' This function allows you to check duplicates.
#' @param data data
#' @param var variable (ex:c("DV","MDV"))
#' @param remove if remove="yes", duplicates will be removed)
#' @keywords duprow
#' @export
#' @examples
#' duprow()
duprow<-function(data,var=NULL,remove=NULL){
  flag="flag"
  data[,flag]<-""
  if(is.null(var)){
    var=names(data)}
  for(i in 1:length(var)){
    data[,flag]<-paste(data[,flag],data[,var[i]],sep="")
  }
  if(is.null(remove)){
    data[duplicated(data[,"flag"]),]}
  else{data1<-data[!duplicated(data[,"flag"]),]
       data1[,"flag"]<-NULL
       data1}
}


########Compute Rtime and tad#########
#-------------------------
#' Derive TAD and RTIME
#'
#' This function allows you to derive TAD and RTIME from calendar date/time.
#' @param data data
#' @param id subject id
#' @param date date variable "%Y-%m-%d"
#' @param time time variable  "%H:%M")
#' @param EVID evid variable (evid>0 for dose)
#' @keywords tadRT
#' @export
#' @examples
#' tadRT()
tadRT<-function (data, id, date, time, EVID, tz = "UTC") 
{
  locf <- function(x) {
    good <- !is.na(x)
    positions <- seq(length(x))
    good.positions <- good * positions
    last.good.position <- cummax(good.positions)
    last.good.position[last.good.position == 0] <- NA
    x[last.good.position]
  }
  data$DTTM <- data$TAD <- data$RTIME <- NULL
  data$DTTM <- as.character(paste(data[, date], data[, time], 
                                  sep = " "))
  data <- chclass(data, c(date, time), "char")
  data$tadtm <- NA
  data <- data[order(data[, id], as.Date(data[, date]), data[, 
                                                             time]), ]
  head(data)
  dtm <- data[data[, EVID] > 0, ]
  rtime <- dtm[!duplicated(dtm[, id]), c(id, "DTTM")]
  names(rtime)[2] <- "FDDTM"
  nodose <- data[data[, EVID] == 0, ]
  dose <- data[data[, EVID] > 0, ]
  dose$tadtm <- as.character(dose$DTTM)
  data <- rbind(dose, nodose)
  data$tadtm <- as.character(data$tadtm)
  head(data)
  data$DTTM <- strftime(strptime(data$DTTM, format = "%Y-%m-%d %H:%M", 
                                 tz = tz), format = "%Y-%m-%d %H:%M", tz = tz)
  data <- data[order(data[, id], data$DTTM), ]
  data$WT1 <- unlist(tapply(data$tadtm, data[, id], locf))
  data$tadtm <- rev(locf(rev(data$WT1)))
  data <- data[order(data[, id], as.Date(data[, date]), data[, 
                                                             time]), ]
  head(data)
  data$DTTM <- strftime(strptime(data$DTTM, format = "%Y-%m-%d %H:%M", 
                                 tz = tz), format = "%Y-%m-%d %H:%M", tz = tz)
  data$tadtm <- strftime(strptime(data$tadtm, format = "%Y-%m-%d %H:%M", 
                                  tz = tz), format = "%Y-%m-%d %H:%M", tz = tz)
  data$TAD <- as.numeric(difftime(strptime(data$tadtm, format = "%Y-%m-%d %H:%M", 
                                           tz = tz), strptime(data$DTTM, format = "%Y-%m-%d %H:%M", 
                                                              tz = tz), units = "hour")) * (-1)
  data <- merge(data, rtime, all.x = T)
  data$RTIME <- as.numeric(difftime(strptime(data$DTTM, format = "%Y-%m-%d %H:%M", 
                                             tz = tz), strptime(data$FDDTM, format = "%Y-%m-%d %H:%M", 
                                                                tz = tz), units = "hour"))
  data$WT1 <- NULL
  data$tadtm <- NULL
  data$FDDTM <- NULL
  data <- data[order(data[, id], as.Date(data[, date]), data[, 
                                                             time]), ]
  data$RTIME <- round(data$RTIME, 4)
  data$TAD <- round(data$TAD, 4)
  data
}


#' LOCF and LOCB
#'
#' LOCF LOCB function
#' @param data data
#' @param var variable to locf
#' @param by sort variable
#' @param locb carry backward
#' @keywords locb2
#' @export
#' @examples
#' locf2()
locf2<-function (data, by, var, locb = T) 
{
  locf <- function(x) {
    good <- !is.na(x)
    positions <- seq(length(x))
    good.positions <- good * positions
    last.good.position <- cummax(good.positions)
    last.good.position[last.good.position == 0] <- NA
    x[last.good.position]
  }
  data$dumy<-seq(1,nrow(data))
  data[, var] <- unlist(tapply(data[, var], data[, by], 
                               locf))
  
  if (locb) {
    data <- data[order(data[,by],rev(data$dumy)),]
    data[, var] <- unlist(tapply(data[, var], data[, by], 
                                 locf))
    data <- data[order(data[,by],data$dumy),]
  }
  data$dumy<-NULL
  data
}


########
# 1 cpt
#' One compartment micro constants and HL
#'
#' This function allows you to derive TAD and RTIME from calendar date/time.
#' @param data data
#' @keywords hl1cpt
#' @export
#' @examples
#' hl1cpt()
hl1cpt<-function(data,cl,v,output){
all<-c("HL","k")
  ifelse(output=="all",output<-all,output)

  k<-data[,cl]/data[,v]
  HL<-log(2)/k
  datf<-data.frame(k=k,HL=HL)
  data[,output]<-datf[,output]
  data
  }

#df<-data.frame(id=1:5,cla=1:5/2,v=2:6*4,cl2=3:7/20,v2=3:7*100,cl3=3:7/10,v3=3:7*50)
#df<-hl3cpt(df,"cla","cl2","cl3","v","v2","v3","all")


#Two-compartment
#' Two compartment micro constants and HL
#'
#' This function allows you to derive micro constants and HL.
#' @param data data
#' @keywords hl2cpt
#' @export
#' @examples
#' hl2cpt(pkdat1,"cl","cl2","v","v2","all")
#
hl2cpt<-function(data,cl,cl2,v,v2,output){
  all<-c("HLa","HLb","alfa","beta","k","k12","k21")
  ifelse(output=="all",output<-all,output)
df<-data
  k<-df[,cl]/df[,v]
  k12<-df[,cl2]/df[,v]
  k21<-df[,cl2]/df[,v2]
beta1<-(1/2)*(k12+k21+k-(sqrt((k12+k21+k)^2-(4*k21*k))))
alfa<-k21*k/beta1
alfaHL<-log(2)/alfa    # to be verify with excel
betaHL<-log(2)/beta1    # to be verified with excel
datf<-data.frame(k=k,k12=k12,k21=k21,alfa=alfa,beta=beta1,HLa=alfaHL,HLb=betaHL)
  data[,output]<-datf[,output]
  data
}

# Three CPT
#' Three compartment micro constants and HL
#'
#' This function allows you to derive micro constants and HL.
#' @param data data
#' @keywords hl3cpt
#' @export
#' @examples
#' hl3cpt(pkdat1,"cl","cl2",,"cl3","v","v2","v3","all")
#
hl3cpt<-function(data,Cl,Cl2,Cl3,V,V2,V3,output){
  all<-c("HLa","HLb","HLg","A","B","C","alpha","beta","gama")
  ifelse(output=="all",output<-all,output)
  df<-data
  k<-df[,Cl]/df[,V]
  k12<-df[,Cl2]/df[,V]
  k21<-df[,V]*k12/df[,V2]
  k13<-df[,Cl3]/df[,V]
  k31<-df[,V]*k13/df[,V3]
  a0<-k*k21*k31
  a1<-(k*k31) + (k21*k31) + (k21*k13) + (k*k21) + (k31*k12)
  a2<-k + k12 + k13 + k21 + k31
  p<-a1 - (a2^2)/3
  q<-2*(a2^3)/27 - a1*a2/3 + a0
  r1<-sqrt((-1)*p^3/27)
  r2<-2*(r1^(1/3))
  phi<-acos(-1*q/(2*r1))/3
  gama<-(-1)*((cos(phi)*r2)-(a2/3))             # gama instead of alpha: Formula error found in Ref. Dubois A. et al., "Mathematical Expressions of the Pharmacokinetic and Pharmacodynamic Models implemented in the PFIM"
  alpha<-(-1)*(cos(phi+(2*pi/3))*r2-a2/3)       # alpha instead of beta: Formula error found in Ref. Dubois A. et al., "Mathematical Expressions of the Pharmacokinetic and Pharmacodynamic Models implemented in the PFIM"
  beta<-(-1)*(cos(phi+(4*pi/3))*r2-a2/3)        # beta instead of gamma: Formula error found in Ref. Dubois A. et al., "Mathematical Expressions of the Pharmacokinetic and Pharmacodynamic Models implemented in the PFIM"
  alfaHL<-log(2)/alpha
  betaHL<-log(2)/beta
  gamaHL<-log(2)/gama
  A=(1/df[,V])*((k21-alpha)/(alpha-beta))*((k31-alpha)/(alpha-gama))
  B=(1/df[,V])*((k21-beta)/(beta-alpha))*((k31-beta)/(beta-gama))
  C=(1/df[,V])*((k21-gama)/(gama-beta))*((k31-gama)/(gama-alpha))
  datf<-data.frame(HLa=alfaHL,HLb=betaHL,HLg=gamaHL,alpha=alpha,beta=beta,gama=gama,A=A,B=B,C=C)
  data[,output]<-datf[,output]
  data
}


#Round old method
#' Rounding as per Excel Internal use
#'
#' This function allows you to round value as per Excel method.
#' @keywords cround
#' @export
#' @examples
#' cround1()
cround1= function(x,n,asnum=T){
  vorz = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
ifelse(is.na(x),output<-NA,
  output<-sprintf(paste("%.",n,"f",sep=""),z*vorz))
if(asnum){output<-as.numeric(as.character(output))}else{
output}
output
}

#' Round up 
#'
#' This function allows you to round value as in Excel.
#' @param z Vector or single value to be rounded
#' @param y number of significant figure 
#' @keywords rounding
#' @export
#' @examples
#' cround()
cround<-function (z, y) 
{
  if(length(z)>1){
    output<-NULL
    for(i in 1:length(z)){
      output1<-cround1(as.numeric(z[i]),y)
      output<-rbind(output,output1)}
    output
  }else{output<-cround1(as.numeric(z),y)
  output
  }}


#sigfig Internal Use
#' Significant figure
#'
#' This function allows you to round value in significant figure.
#' @keywords sigfig
#' @export
#' @examples
#' sigfig1()
sigfig1<-function (x, y) 
{
  sround = function(x, n) {
    vorz = sign(x)
    z = abs(x) * 10^n
    z = z + 0.5
    z = trunc(z)
    z = z/10^n
    ifelse(is.na(x), sro <- NA, sro <- z * vorz)
    sro
  }
  nround <- ifelse(x == 0, y - 1, y - 1 - floor(log10(abs(x))))
  if (!is.na(x) & ceiling(log10(abs(x))) >= 3) {
    output <- as.character(cround(x, 0))
  }else {
    if (!is.na(x) & ceiling(log10(abs(x)))<3) {
      output <- sprintf(paste("%.", nround, "f", sep = ""), 
                        sround(x, nround))
    }else{
      output <- NA
    }
  }
  output
}

#Sigfig 
#' Significant figure
#'
#' This function allows you to round value in significant figure.
#' @param z Vector or single value to be rounded
#' @param y number of significant figure 
#' @keywords sigfig
#' @export
#' @examples
#' sigfig()
sigfig<-function (z, y) 
{
  if(length(z)>1){
    output<-NULL
    for(i in 1:length(z)){
      output1<-sigfig1(as.numeric(z[i]),y)
      output<-rbind(output,output1)}
    output
  }else{output<-sigfig1(as.numeric(z),y)
  output
  }}

#Only output unique duplicate item
#' Filter unique duplicated row
#'
#' This function allows you to filter duplicated rows but only show unique row
#' @param data data
#' @param data data
#' @param all display all columns (all="all")
#' @param select display selected variables only
#' @keywords dup1
#' @export
#' @examples
#' dup1()
dup1<-function(data,var,all,select){
  d1<-data[duplicated(data[,var]),]
  if(all=="all"){d1<-d1}else{
    if(all=="var"){d1<-d1[,var]}else{
      d1<-d1[,c(var,select)]}}
  d1
}

#Output duplicated items with all or partial variabes
#' Filter all duplicated rows
#'
#' This function allows you to filter duplicated rows but only show unique row
#' @param data data
#' @param data data
#' @param all display all columns (all="all")
#' @param select display selected variables only
#' @keywords dup2
#' @export
#' @examples
#' dup2()
dup2<-function(data,var,all,select){
  d1<-data
  d1$dum<-""
  for(i in var){
    d1$dum<-paste(d1$dum,d1[,i],sep="-")
  }
  dup<-d1[duplicated(d1$dum),"dum"]
  d1<-d1[d1$dum%in%dup,]
  if(all=="all"){d1<-d1[,names(data)]}else{
    if(all=="var"){d1<-d1[,var]}else{
      d1<-d1[,c(var,select)]}}
  d1
}

#TABLE FUNCTIONS###############
#' bround Table function
#' @param data data
#' @keywords bround
#' @export
#' @examples
#' bround()
bround<-function(data,var,rtype="sigfig",dec=3){
  data<-chclass(data,var,"num")
  for(i in var){
    data[is.na(data[,i]),i]<-9999999999999
    if(rtype=="sigfig"){data[,i]<-sigfig(data[,i],dec)}else{data[,i]<-cround(data[,i],dec)}
    data[data[,i]=="9999999999999",i]<-"NA"
  }
  data
}

#' geom Table function
#'
#' @param x data
#' @keywords geom
#' @export
#' @examples
#' geom()


geom <- function(x) {
  exp(mean(log(x[x > 0]), na.rm=TRUE))
}

#' geocv Table function
#' @param x data
#' @keywords geocv
#' @export
#' @examples
#' geocv()

geocv <- function(x) {
  100*sqrt(exp(var(log(x[x > 0]), na.rm=TRUE)) - 1)
}

#' cv Table function
#' @param x data
#' @keywords cv
#' @export
#' @examples
#' cv()

cv <- function(x) {
  abs(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100)
}

#########
# mean<-"mean(x,na.rm=T)=mean"
# sd<-"sd(x,na.rm=T)=sd"
# cv<-"cv(x)=cv"
# qt05<-"quantile(x,0.05,na.rm=TRUE)=qt05"
# qt95<-"quantile(x,0.95,na.rm=TRUE)=qt05"

#per95<-function(x){quantile(x,percentile,na.rm=TRUE)}

#' se Table function
#' internal use.
#' @param x data
#' @keywords se
#' @export
#' @examples
#' se()

se<-function(x){sd(x,na.rm=TRUE)/(length(x))^0.5}

#' cilow Table function
#'
#' internal use
#' @param x data
#' @keywords generic
#' @export
#' @examples
#' cilow()

cilow<-function(x){mean(x,na.rm=TRUE)-((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}    #1.96)}

#' ciup Table function
#' internal use.
#' @param x data
#' @keywords ciup
#' @export
#' @examples
#' ciup()
ciup<-function(x){mean(x,na.rm=TRUE)+((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}

#' nmiss Table function
#' internal use.
#' @param x data
#' @keywords nmiss
#' @export
#' @examples
#' nmiss()
nmiss<-function(x){length(x[is.na(x)])}

#nmiss<-function(x){length(x[is.na(x)])}
#upci<-function(x){mean(x,na.rm=TRUE)+((sd(x,na.rm=TRUE)/(length(x#))^0.5)*qt(0.975,df=length(x)-1))}
#loci<-function(x){mean(x,na.rm=TRUE)-((sd(x,na.rm=TRUE)/(length(x#))^0.5)*qt(0.975,df=length(x)-1))}    #1.96)}
#se<-function(x){sd(x,na.rm=TRUE)/(length(x))^0.5}
#per95<-function(x){quantile(x,percentile,na.rm=TRUE)}
#per05<-function(x){quantile(x,1-percentile,na.rm=TRUE)}
#Gmean <- function(x) {
 # exp(mean(log(x[x > 0]), na.rm=TRUE))
#}
# Gcv <- function(x) {
#   100*sqrt(exp(var(log(x[x > 0]), na.rm=TRUE)) - 1)
# }
# 
# }
#####################TABLE STATS##################

#' conti<-function(input=input,var=var,by=by,round.type="sigfig",digit=3,quanti){
#'   statsfun()
#'   cv<-function(x){
#'     abs(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100)
#'   }
#'   nmiss<-function(x){length(x[is.na(x)])}
#'   upci<-function(x){mean(x,na.rm=TRUE)+((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}
#'   loci<-function(x){mean(x,na.rm=TRUE)-((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}    #1.96)}
#'   se<-function(x){sd(x,na.rm=TRUE)/(length(x))^0.5}
#'   per95<-function(x){quantile(x,percentile,na.rm=TRUE)}
#'   per05<-function(x){quantile(x,1-percentile,na.rm=TRUE)}
#'   Gmean <- function(x) {
#'     exp(mean(log(x[x > 0]), na.rm=TRUE))
#'   }
#'   Gcv <- function(x) {
#'     100*sqrt(exp(var(log(x[x > 0]), na.rm=TRUE)) - 1)
#'   }
#' 
#'   input<-input[,c(var,by)]
#'   l<-stats::reshape(input,
#'              varying = var,
#'              v.names = "score",
#'              timevar = "var",
#'              times = var,
#'              direction = "long")
#'   l1<-l[!is.na(l$score),]
#'   l2<-l[is.na(l$score),]
#'   l1$qt<-quanti
#'   sum<-plyr::ddply(l1,c(by,"var"),summarise,
#'              n=length(score),
#'              #nmiss=nmiss(score),
#'              mean=mean(score,na.rm=T),
#'              cv=cv(score),
#'              sd=sd(score,na.rm=T),
#'              se=se(score),
#'              median=median(score,na.rm=T),
#'              min=min(score,na.rm=T,na.rm=T),
#'              max=max(score),
#'              lo_qt975=quantile(score,0.025,na.rm=T),
#'              hi_qt975=quantile(score,0.975,na.rm=T),
#'              lo_qt95=quantile(score,0.05,na.rm=T),
#'              hi_qt95=quantile(score,0.95,na.rm=T),
#'              lo_qtxx=quantile(score,1-unique(qt),na.rm=T),
#'              hi_qtxx=quantile(score,unique(qt),na.rm=T),
#'              lowCI=loci(score),
#'              HiCI=upci(score),
#'              GeoMean=Gmean(score),
#'              GeoCV=Gcv(score)
#'   )
#' 
#'   variable=c("mean","cv","sd","se","median","min","GeoMean", "GeoCV",
#'              "max","lo_qt975", "hi_qt975", "lo_qt95",  "hi_qt95","lo_qtxx","hi_qtxx","lowCI","HiCI")
#' 
#'   l<-reshape(sum,
#'              varying = variable,
#'              v.names = "value",
#'              timevar = "toround",
#'              times = variable,
#'              direction = "long")
#'   l<-l[!is.na(l$value),]
#'   if(round.type=="sigfig"){
#'     l$value<-sigfig(l$value,digit)}else{l$value<-cround(l$value,digit)}
#'   l$id<-NULL
#'   keep<-names(l)[!names(l)%in%c("toround","value")]
#'   w <- stats::reshape(l,
#'                timevar = "toround",
#'                idvar = keep,
#'                direction = "wide")
#'   names(w)<-gsub("value.","",names(w))
#'   if(nrow(l2)>0){
#'     sum1<-plyr::ddply(l2,c(by,"var"),summarise,
#'                 nmiss=nmiss(score))
#'     w<-plyr::join(w,sum1)
#'     w$nmiss[is.na(w$nmiss)]<-0}else{w$nmiss=0}
#'   w$score<-w$id<-NULL
#'   w
#' }


#' Funtion for Descriptove stats of continuous covariate
#'
#'
#' Descriptove stats of continuous covariates
#' @param data datset or data frame (ex:data=PKdatat)
#' @param var List of continuous covariates (ex:c("CRCL","WT"))
#' @param by  Stratification variable (ex: by="study")
#' @param colby Specify sorting variable to be displayed vertically. (ex: colby=by or colby="var")
#' @param rowby Specidy sorting variable horizontally. If by > 1, set rowby=by.
#' @param sumry Result summary format to be displayed. Type summary.set to see different options of summary sets.
#' @param round.type rounding method, sigfig by default. (ex:"round" for rounding)
#' @param digit Number of round digits or significant figures
#' @keywords con.tab
#' @export
#' @examples
#' con.tab(data=dat,var=c("WT","crcl","ALT"),by=c("study"),colby="var",rowby=by,sumry=set1)

lhcontab<-function(dat,var,by,format="by.var",sumry=1,round.type="sigfig",digit=3,quant=0.90){
  statsfun()
  cv<-function(x){
    abs(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100)
  }
  nmiss<-function(x){length(x[is.na(x)])}
  upci<-function(x){mean(x,na.rm=TRUE)+((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}
  loci<-function(x){mean(x,na.rm=TRUE)-((sd(x,na.rm=TRUE)/(length(x))^0.5)*qt(0.975,df=length(x)-1))}    #1.96)}
  se<-function(x){sd(x,na.rm=TRUE)/(length(x))^0.5}
  per95<-function(x){quantile(x,percentile,na.rm=TRUE)}
  per05<-function(x){quantile(x,1-percentile,na.rm=TRUE)}
  Gmean <- function(x) {
    exp(mean(log(x[x > 0]), na.rm=TRUE))
  }
  Gcv <- function(x) {
    100*sqrt(exp(var(log(x[x > 0]), na.rm=TRUE)) - 1)
  }

  p1<-"(";p2<-")"
  b1<-"[";  b2<-"]"
  a<-"{"; a<-"}"
  d<-"-"; com<-","
  bsl<-"/";lb<-"\n"
  sp<-" "
  set=NULL
  set[[1]]=c("paste0(mean,sp,p1,cv,p2,lb,median,sp,b1,min,d,max,b2)","Mean(CV)Med[min-max]")
  set[[2]]=c("paste0(mean,sp,p1,sd,p2,lb,median,sp.b1,min,d,max,b2)","Mean(SD)Med[min-max]")
  set[[3]]=c("paste0(mean,sp,p1,cv,com,sp,n,p2,lb,median,sp,b1,min,d,max,b2)","Mean(CV,N) Med[min-max]")
  set[[4]]=c("paste0(mean,sp,p1,sd,com,sp,n,p2,lb,median,sp,b1,min,d,max,b2)","Mean(SD,N) Med[min-max]")
  set[[5]]=c("paste0(n,sp,p1,nmiss,p2,lb,mean,sp,p1,cv,p2,lb,median,sp,b1,min,d,max,b2)","N(Nmis)Mean(CV) Med[min-max]")
  set[[6]]=c("paste0(n,sp,p1,nmiss,p2,lb,mean,sp,p1,sd,p2,lb,median,sp,b1,min,d,max,b2)","N(Nmis)Mean(SD) Med[min-max]")
  set[[7]]=c("paste0(mean,sp,p1,cv,p2,lb,median,sp,b1,lowCI,d,HiCI,b2)","Mean(CV)Med[95CI]")
  set[[8]]= c("paste0(mean,sp,p1,cv,p2,lb,median,sp,b1,lo_qt95,d,hi_qt95,b2)","Mean(CV)Med[95PI]")
set[[9]]=c("paste0(mean,sp,p1,cv,p2,lb,median,sp,b1,lo_qt975,d,hi_qt975,b2)","Mean(CV)Med[97.5PI]")
set[[10]]=c("paste0(mean,sp,p1,cv,p2,lb,median,sp,b1,lo_qtxx,d,hi_qtxx,b2)","Mean(CV) Median[xxPI]")



  data<-dat[,c(var,by)]
    data<-data[,c(var,by)]
    data<-chclass(data,var,"num")
  #  lo_qtxx<-paste0("loqt",quant)
  #  hi_qtxx<-paste0("hiqt",quant)

    con1<-conti(data,var=var,by=by,round.type=round.type,digit=digit,quanti=quant)
    data1<-data
    head(data1)
    data1[,by]<-"Overall"
    con2<-conti(data1,var=var,by=by,round.type=round.type,digit=digit,quanti=quant)
    con1<-rbind(con1,con2)
   # names(con1)[names(con1)=="lo_qtxx"]<-lo_qtxx
  #  names(con1)[names(con1)=="hi_qtxx"]<-hi_qtxx
    grp<-sumry

    head(con1)
    if(is.null(grp)){w<-con1}else{
      groupstats<-set[[grp]][1]
      con1$summary<-with(con1,eval(parse(text=groupstats)))
      con1<-con1[,c(by,"var","summary")]
      if(format=="by.var"){
        w <- reshape(con1,
                            timevar = "var",
                            idvar = by,
                            direction = "wide")
        names(w)<-gsub("summary.","",names(w))
        nm<-names(w)
        w$stats<-set[[grp]][2]
        w<-w[,c("stats",nm)]
        }else{
            tx<-NULL
            for(i in var){
            ty<-t(con1[con1$var==i,c("var","summary")])
            tx<-rbind(tx,ty)}
            tx<-tx[row.names(tx)!="var",]
            row.names(tx)<-var
            dim(tx)
            con2<-chclass(con1,by,"char")
            byby<-nodup(con2,by,"var")
            t1<-t(byby)
            dim(t1)
            row.names(t1)<-by
            w<-as.data.frame(rbind(t1,tx))
            names(w)
            names(w)<-c(paste0(rep(set[[grp]][2],dim(w)[2]-1),1:dim(w)[2]-1))
            nm<-names(w)
           w$Covariate<-row.names(w)
           w<-w[,c("Covariate",nm)]
        }}
    row.names(w)<-NULL
    w
      }

################################################
#' roundbatch
#'
#' internal use
#' @keywords roundbatch
#' @export
#' @examples

roundbatch<-function(data,variable,toround,nb){
  head(data)
  data<-sum
  l<-stats::reshape(data,
             varying = variable,
             v.names = "value",
             timevar = "toround",
             times = variable,
             direction = "long")
  l<-l[!is.na(l$value),]
  if(toround=="sigfig"){
    l$value<-sigfig(l$value,nb)}else{l$value<-cround(l$value,nb)}
  l$id<-NULL
  keep<-names(l)[!names(l)%in%c("toround","value")]
  w <- stats::reshape(l,
               timevar = "toround",
               idvar = keep,
               direction = "wide")
  names(w)<-gsub("value.","",names(w))
  w
}


################COUNTS CATEGORICAL###############

#' Funtion for Descriptove Stats of Categorical Covariate
#'
#'
#' Descriptove stats of categorical covariates
#' cat.tab(data,var,by,colby="var",rowby=by)
#' @param data datset or data frame (ex:data=PKdatat)
#' @param var List of continuous covariates (ex:c("SEX","RACE"))
#' @param by  Stratification variable (ex: by="study")
#' @keywords cat.tab
#' @export
#' @examples
#' cat.tab(data=dat,var=c("SEX","RACE"),by=c("study"),colby="var",rowby=by)

lhcattab<-function (data, var, by)
  {
  rowby = by
  dat1 <- chclass(data[, c(var, by)], c(var, by), "char")

  tot <- stats::reshape(dat1, varying = var, v.names = "value",
                        timevar = "var", times = var, direction = "long")
  tot$id <- NULL
  tot1 <- addvar(tot, c(by, "var"), "var", "length(x)", "no",
                 "tot")
  tot2 <- addvar(tot, c(by, "var", "value"), "var", "length(x)",
                 "no", "subt")
  tot11 <- nodup(tot1, c(by), "all")
  names(tot11)[names(tot11) == "tot"] <- "N="
  tot12 <- addvar(tot, c("var", "value"), "var", "length(x)",
                  "no", "Overall")
  tot13 <- addvar(tot, c("var"), "var", "length(x)", "no",
                  "tot")
  tot12 <- plyr::join(tot12, tot13)
  tot12$Overall <- with(tot12, paste0(Overall, " (", sigfig(Overall/tot *
                                                              100, 3), "%)"))
  tot12$tot <- NULL
  tot11$"N=" <- paste0(tot11$"N=", " (", sigfig(tot11$"N="/max(tot13$tot) *
                                                  100, 3), "%)")
  tot4 <- plyr::join(tot2, tot1)
  tot4$summary <- with(tot4, paste0(subt, " (", sigfig(subt/tot *
                                                         100, 3), "%)"))
  tot3 <- tot4[, c(by, "var", "value", "summary")]

  tto<-addvar(tot4,c(rowby),"tot","max(x)","yes","all")
  tto[,c("var","value")]<-"all"
  tto$all<-paste0(tto$all," (100%)")

  tto0<-tto;tto0$tot<-tto0$subt<-tto0$summary<-NULL
  w0 <- stats::reshape(tto0, timevar =rowby, idvar = c("var",
                                                       "value"), direction = "wide")
  names(w0)<-gsub("all.","",names(w0))

  tto1<-tot4;tto1$tot<-tto1$subt<-NULL
  w <- stats::reshape(tto1, timevar =rowby, idvar = c("var",
                                                      "value"), direction = "wide")
  names(w)<-gsub("summary.","",names(w))

  w<-rbind(w,w0)

  w1<-addvar(tot4,c("var","value"),"subt","sum(x)","no","overall")
  w2<-addvar(tot4,c("var"),"subt","sum(x)","no","overall1")
  w1<-plyr::join(w1,w2)

  w1$overall<-paste0(w1$overall," (",sigfig(w1$overall/w1$overall1*100,3),"%)")
  w1a<-w1[1,]
  w1a$var<-w1a$value<-"all"
  w1a$overall<-paste0(w1a$overall1," (100%)")
  w1<-rbind(w1,w1a)

  w<-plyr::join(w,w1[,c("var",  "value","overall")])
  w<-w[order(w$var),]
  w
}



#' Individual table with descriptive statse
#'
#' Listing of individual data and descriptove stats
#' @param data datset or data frame (ex:data=PKdatat)
#' @param id unique identifier
#' @param by  Stratification variable (ex: by="study")
#' @param variables Specify sorting variable to be displayed vertically. (ex: colby=by or colby="var")
#' @param rtype rounding type. (sigfig by default)
#' @param dec round decimal or number of significant figures
#' @keywords ind.tab
#' @export
#' @examples
#' ind.tab(data=dat,id="NMID",by=c("study"))
indiv.tab<-function(data,id,by,variables,rtype="sigfig",dec=3){
  id<-id#
  data<-data[,c(id,by,variables)]#[!duplicated(data$id),]
  strat1<-by#c("phase")# mandatory
  convar<-variables #mandatory
  d1<-data[,c(id,strat1,convar)]
  d1<-chclass(d1,convar,"num")
  head(d1)

  t1<-NULL
  for(i in unique(d1[,strat1])){
    d0<-d1[d1[,strat1]%in%i,]
    l<-stats::reshape(d0,
               varying = c(convar),
               v.names = "score",
               timevar = "subj",
               times = c(convar),
               #new.row.names = 1:1000,
               direction = "long")
    head(l)
    l$id<-NULL
    str(l)
    st<-plyr::ddply(l,c(by,"subj"),summarise,
              N=round(length(score),0),
              Nmiss=round(length(score[is.na(score)]),0),
              Means=sigfig(mean(score,na.rm=T),3),
              SD=sigfig(sd(score,na.rm=T),3),
              cv=sigfig(cv(score),3),
              Median=sigfig(median(score,na.rm=T),3),
              Minimum=sigfig(min(score,na.rm=T),3),
              Maximum=sigfig(max(score,na.rm=T),3),
              GeoMean=sigfig(Gmean(score),3),
              GeoCV=sigfig(Gcv(score),3))
    keep<-names(st[,3:length(names(st))])
    l1<-stats::reshape(st,
                varying = c(keep),
                v.names = "Stats",
                timevar = "Results",
                times = c(keep),
                #new.row.names = 1:1000,
                direction = "long")
    l1$id<-NULL

    w<-stats::reshape(l1,
               timevar = "subj",
               idvar = c(strat1, "Results"),
               direction = "wide")
    names(w)<-gsub("Stats.","",names(w))
    head(d0)
    x1<-setdiff(names(d0),names(w))
    x2<-setdiff(names(w),names(d0))
    w[,x1]<-""
    d0[,x2]<-""
    d0<-d0[,c(id,strat1,x2,convar)]
    #d0<-chclass(d0,convar,"num")
    if(!is.null(rtype)){
      d0<-bround(d0,convar,rtype=rtype,dec=dec)}
    t<-rbind(d0,w)
    t<-t[,c(id,strat1,x2,convar)]
    t1<-rbind(t1,t)
  }
  t1
}

#' Calculate AUC Using the Trapezoidal Method
#'
#' Calculate the area under the curve (AUC) for each subject over the time interval for dv using the trapezoidal rule.
#' nca.analysis(data,n_lambda=3,id,time,dv,ss,partialAUC="no")
#' data
#' @param data.frame containing the data to use for the AUC calculation
#' @param time	chronologically ordered time variable present in data
#' @param id	variable in data defining subject level data
#' @param dv	dependent variable used to calculate AUC present in data
#' @keywords AUC
#' @export
#' @examples
#' AUC(data, time = 'TIME', id = 'ID', dv = 'DV')

AUC<-function (data, time = "TIME", id = "ID", dv = "DV")
{
  if (any(is.na(data[[id]])))
    warning("id contains NA")
  if (any(is.na(data[[time]])))
    warning("time contains NA")
  if (any(is.na(data[[dv]])))
    warning("dv contains NA")
  data <- data[order(data[[id]], -data[[time]]), ]
  nrec <- length(data[[time]])
  data$diff <- c(data[[time]][-nrec] - data[[time]][-1], 0)
  data$meanDV <- c((data[[dv]][-1] + data[[dv]][-nrec])/2,
                   0)
  data$dAUC <- data$diff * data$meanDV
  data <- data[order(data[[id]], data[[time]]), ]
  data <- data[duplicated(data[[id]]), ]
  AUC <- aggregate.data.frame(data$dAUC, by = list(data[[id]]),
                              FUN = sum)
  names(AUC) <- c(id, "AUC")
  return(AUC)
}

#' Derive Common NCA parameters using single and multiple profiles
#'
#' nca.cal()
#' @param data datset or data frame (ex:data=PKdatat)
#' @param id unique subject identifier
#' @param n_lambda  number of points for estimating the Lambda
#' @param time Sampling time after dose (TAD)
#' @param dv Concentration
#' @param dosing.regimen PK profile profile flag (ex: single, sd, md ,ss).As vector or numeric. Note that only one prefile per dose is to be analyzed.
#' @param label.for.sd Label to identify sd profile used in dosing.regimen. Need to compute the effective half-life
#' @param label.for.ss Label to identify ss profile used in dosing.regimen. Need to compute the effective half-life
#' @param tau Dose interval. As vector or numeric.
#' @param dose Dose administered. As vector or numeric. Required to estimate CL and Vss 
#' @param partialAUC Time interval for partial AUC. Ex: c(0,6,0,12,6,12) for AUC0-6, AUC0-12 and AUC6-12
#' @param partialConc Concentration at particular time (Ex:c(1,4) for concentration after 1 and 4 h)
#' @keywords nca.cal
#' @export
#' @examples 
#' p<-nca.cal(data=data, n_lambda = 3, id = "id", time = "time", dv = "dv", 
#' tau="ii",dose="dose",dosing.regimen = "ss",label.for.sd="single", partialAUC = NULL, partialConc = NULL)
#' 

nca.cal<-function (data, n_lambda = 3, id = "id", time = "time", dv = "dv",sort.by=NULL, ss.variable = "ss", sd_label_in_ss = NULL, ss_label_in_ss = NULL,tau = NULL, dose = NULL, partialAUC = NULL, partialConc = NULL) 
{
  dat <- data
  dat$time1 <- dat[, time]
  dat$time <- dat[, time]
  dat[, "tad"] <- NULL
  dat$id <- dat[, id]
  dat$dv <- dat[, dv]
  if (is.numeric(ss.variable)) {
    dat$ss <- ss.variable
  } else {
    dat$ss <- dat[,ss.variable]
  }
  if (is.numeric(dose)) {
    dat$dose <- dose
  } else {
    if (!is.null(dose)) {
      dat$dose <- dat[, dose]
    } else {
      dat$dose <- "dose required"
    }
  }
  if (is.numeric(tau) & !is.null(tau)) {
    dat$tau <- tau
  }else{
    if (!is.null(tau)) {
      dat$tau <- dat[, tau]
    }else {dat}
  }
  dat$idss <- paste(dat$id, dat$ss, sep = "-")
  if(!is.null(sort.by)){
    for(i in sort.by){
      dat$idss <- paste(dat$idss, dat[,i], sep = "-")  
    }}else{dat$idss<-dat$idss}
  
  dat <- dat[order(dat$idss, dat$time), ]
  dat <- addvar(dat, "idss", "time", "min(x)", "yes", "tad")
  dat$time <- dat$time - dat$tad
  idss <- nodup(dat, c("idss", "id", "ss", "dose",sort.by), "var")
  dat$dvtm <- dat$dv * dat$time
  datauc <- dat
  auclast <- AUC(datauc, time = time, id = "idss", dv = dv)
  names(auclast) <- c("idss", "AUClast")
  auclast <- plyr::join(auclast, idss)
  aucmlast <- AUC(datauc, time = time, id = "idss", dv = "dvtm")
  names(aucmlast) <- c("idss", "AUMClast")
  aucmlast <- plyr::join(aucmlast, idss)
  head(dat)
  dat$tad1 <- dat$time
  aucpart <- NULL
  if (!is.null(partialAUC)) {
    nauc <- length(partialAUC)/2
    for (z in seq(1, length(partialAUC), 2)) {
      tm1 <- partialAUC[z]
      tm2 <- partialAUC[z + 1]
      auc <- AUC(dat[dat[, "tad1"] >= tm1 & dat[, "tad1"] <= 
                       tm2, ], time = "tad1", id = "idss", dv = dv)
      names(auc) <- c("idss", paste0("AUC", tm1, "-", tm2))
      if (z == 1) {
        aucpart <- rbind(aucpart, auc)
      } else {
        aucpart[, paste0("AUC", tm1, "-", tm2)] <- auc[, 
                                                       2]
      }
    }
    aucpart <- join(aucpart, idss)
    aucpart$idss <- NULL
    aucpart
  } else {
    aucpart <- NULL
  }
  Cpart <- NULL
  if (!is.null(partialConc)) {
    nauc <- length(partialConc)
    for (z in 1:length(partialConc)) {
      tm1 <- partialConc[z]
      partc <- dat[dat[, "tad1"] == tm1, c("idss", "dv")]
      names(partc) <- c("idss", paste0("C", tm1))
      if (z == 1) {
        Cpart <- rbind(Cpart, partc)
      } else {
        Cpart[, paste0("C", tm1)] <- partc[, 2]
      }
    }
    Cpart
    Cpart <- join(Cpart, idss)
    Cpart$idss <- NULL
    Cpart
  }else {
    Cpart <- NULL
  }
  if (n_lambda != "no") {
    dat1 <- dat
    dat1$tmp <- seq(nrow(dat1))
    dat1 <- addvar(dat1, "idss", "tmp", "max(x)", "yes", 
                   "tmp2")
    head(dat1)
    dat1$tmp <- dat1$tmp2 - dat1$tmp
    dat1 <- dat1[dat1$tmp < n_lambda, ]
    str(dat1)
    dat1[, c("idss", "time", "dv")]
    test1 <- ddply(dat1[, c("idss", "time", "dv")], .(idss), 
                   summarize, interc = lm(log(dv) ~ time)$coef[1], Lambda = lm(log(dv) ~ time)$coef[2] * -1, R2 = summary(lm(log(dv) ~ time))$r.squared, HL = (log(2)/lm(log(dv) ~ time)$coef[2]) *  -1, that = max(time))
    test1$n_lambda <- n_lambda
    test1$Clast_hat <- with(test1, exp(-Lambda * that + interc))
    test1a <- ddply(dat1[, c("idss", "time", "dvtm")], .(idss), 
                    summarize, intercc = lm(log(dvtm) ~ time)$coef[1], 
                    Lambdac = lm(log(dvtm) ~ time)$coef[2] * -1, 
                    R2c = summary(lm(log(dvtm) ~ time))$r.squared, 
                    HLc = (log(2)/lm(log(dvtm) ~ time)$coef[2]) * -1, 
                    thatc = max(time))
    test1a$n_lambdac <- n_lambda
    test1a$Clast_hatc <- with(test1a, exp(-Lambdac * thatc + 
                                            intercc))
  }else {
    test1 <- NULL
  }
  if (TRUE %in% c(test1$HL < 0)) {
    test1$Warning.HL.Negative = ifelse(test1$HL, "yes", "")
  }
  max <- ddply(dat[, c("idss", "dv", "time", "time1")], .(idss), 
               summarize, Cmax = max(dv), Tmax = time1[dv == max(dv)], 
               Cmin = min(dv[time >= time[dv == max(dv)]]), Tlast = max(dat$time1), 
               Clast = dv[time == max(time)])
  maxa <- ddply(dat, .(idss), summarize, Clastc = dvtm[time == 
                                                         max(time)])
  head(dat)
  test <- plyr::join(max, idss)
  test <- plyr::join(test, maxa)
  test <- plyr::join(test, auclast)
  test <- plyr::join(test, aucmlast)
  if (n_lambda != "no") {
    test <- join(test, test1)
    test <- join(test, test1a)
    test$AUCinf_obs <- abs(as.numeric(as.character(test$AUClast)) + 
                             test$Clast/test$Lambda)
    test$AUMCinf_obs <- abs(as.numeric(as.character(test$AUMClast)) + 
                              test$Clastc/test$Lambdac)
    test$AUCinf_pred <- abs(as.numeric(as.character(test$AUClast)) + 
                              test$Clast_hat/test$Lambda)
    test$AUMCinf_pred <- abs(as.numeric(as.character(test$AUMClast)) + 
                               test$Clast_hatc/test$Lambdac)
    test$MRTlast <- test$AUMClast/test$AUClast
    test$MRTobs <- test$AUMCinf_obs/test$AUCinf_obs
    test$MRTpred <- test$AUMCinf_pred/test$AUCinf_pred
  }else {
    test$MRTlast <- test$AUMClast/test$AUClast
  }
  if (!is.null(Cpart)) {
    test <- plyr::join(test, Cpart)
  }
  if (!is.null(aucpart)) {
    test <- plyr::join(test, aucpart)
  }
  test$idss <- test$interc <- test$that <- NULL
  n <- names(test)
  n <- n[!n %in% c("id", "ss")]
  test <- test[, c("id", "ss", n)]
  if ("AUCinf_pred" %in% names(test) & is.numeric(test$dose)) {
    test$CLobs <- with(test, dose/AUCinf_obs)
    test$CLpred <- with(test, dose/AUCinf_pred)
    test$Vss_obs <- with(test, CLobs * AUMCinf_obs)
    test$Vss_pred <- with(test, CLpred * AUMCinf_pred)
    test <- test[, !names(test) %in% c("Lambdac", "R2c", 
                                       "HLc", "thatc", "n_lambdac", "Clast_hatc", "intercc")]
    names(test)[names(test) == "HL"] <- "HL_Lambda_z"
  }else {
    test$CLobs <- "Dose required"
    test$CLpred <- "Dose required"
    test$Vss_obs <- "Dose required"
    test$Vss_pred <- "Dose required"
  }
  if (!is.null(ss_label_in_ss) | !is.null(sd_label_in_ss)) {
    if (!is.null(ss_label_in_ss) & !is.null(sd_label_in_ss)) {
      head(dat)
      ss1 <- dat[dat$ss == ss_label_in_ss & dat$time <= dat$tau, 
                 ]
      single <- dat[dat$ss == sd_label_in_ss & dat$time <= 
                      dat$tau, ]
    }
    if (!is.null(sd_label_in_ss) & is.null(ss_label_in_ss)) {
      ss1 <- dat[dat$ss != sd_label_in_ss & dat$time <= dat$tau, 
                 ]
      single <- dat[dat$ss == sd_label_in_ss & dat$time <= 
                      dat$tau, ]
    }
    if (is.null(sd_label_in_ss) & !is.null(ss_label_in_ss)) {
      ss1 <- dat[dat$ss == ss_label_in_ss & dat$time <= dat$tau, 
                 ]
      single <- dat[dat$ss != ss_label_in_ss & dat$time <= 
                      dat$tau, ]
    }
    
    auctau <- AUC(ss1, time = "time", id = "idss", dv = "dv")
    names(auctau) <- c("idss", "AUCtau")
    aucsdtau <- AUC(single, time = "time", id = "idss", dv = "dv")
    names(aucsdtau) <- c("idss", "AUCsd_tau")
    test<-join(test,idss,type="left")
    #test$idss <- paste(test$id, test$ss, sep = "-")
    test <- join(test, aucsdtau, type = "left")
    test <- join(test, auctau, type = "left")
    ident <- nodup(test, c("id", "idss"), "var")
    a <- join(auctau, ident)
    a$idss <- NULL
    b <- join(aucsdtau, ident)
    b$idss <- NULL
    EHL <- join(a, b)
    EHL$Rc <- with(EHL, AUCtau/AUCsd_tau)
    tau1 <- nodup(single, c("id", "tau"), "var")
    EHL <- join(EHL, tau1)
    EHL$EHL <- with(EHL, log(2) * tau/(log(Rc/(Rc - 1))))
    test <- join(test, EHL[, c("id", "tau", "EHL")], type = "left")
  }
  if ("AUCinf_obs" %in% names(test)) {
    single <- dat[dat$time <= dat$tau, ]
    aucsdtau <- AUC(single, time = "time", id = "idss", dv = "dv")
    names(aucsdtau) <- c("idss", "AUCsd_tau")
    head(test)
    test$idss <- paste(test$id, test$ss, sep = "-")
    test <- join(test, aucsdtau, type = "left")
    test$Rc <- with(test, AUCinf_obs/AUCsd_tau)
    test$EHL <- with(test, log(2) * tau/(log(Rc/(Rc - 1))))
    test <- join(test, nodup(dat, c("idss", "tau"), "var"))
  } else {
    if (!c("tau") %in% names(dat)) {
      test$EHL <- "AUCinf or TAU is missing"
    }
  }
  keep <- names(test)[!names(test) %in% c("Lambdac", "R2c", 
                                          "HLc", "thatc", "n_lambdac", "Clast_hatc", "intercc", 
                                          "idss", "Rc")]
  test <- test[, keep]
}
###########################
