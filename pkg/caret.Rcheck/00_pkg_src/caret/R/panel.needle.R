"panel.needle" <-
  function (x, y, horizontal = TRUE, 
            pch = if (is.null(groups)) dot.symbol$pch else sup.symbol$pch, 
            col = if (is.null(groups)) dot.symbol$col else sup.symbol$col, 
            lty = dot.line$lty, lwd = dot.line$lwd, col.line = dot.line$col, 
            levels.fos = NULL, groups = NULL, ...) 
  {
    #adapted from panel.dotplot in lattice
    x <- as.numeric(x)
    y <- as.numeric(y)
    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")
    sup.symbol <- trellis.par.get("superpose.symbol")
    
    if (horizontal) {
      yscale <- extendrange(y, f = .2)   
      if (is.null(levels.fos)) levels.fos <- floor(yscale[2]) - ceiling(yscale[1]) + 1
      panel.abline(v = 0, col =1, lty = 1, lwd = 1)
      pch <- rep(pch, length(x))
      pch <- ifelse(x == 0, NA, pch)
      
      for(i in seq(along=x)) lsegments(x[i], y[i], 0, y[i])
      if (is.null(groups)) 
        panel.xyplot(x = x, y = y, col = col, pch = pch, ...)
      else panel.superpose(x = x, y = y, groups = groups, col = col, pch = pch, ...)        
    }
    else {
      xscale <- extendrange(x, f = .2)
      if (is.null(levels.fos)) levels.fos <- floor(xscale[2]) - ceiling(xscale[1]) + 1
      panel.abline(h = 0, col = col.line, lty = lty, lwd = lwd)
      pch <- rep(pch, length(x))
      pch <- ifelse(x == 0, NA, pch)
      if (is.null(groups))panel.xyplot(x = x, y = y, col = col, pch = pch, ...)
      else panel.superpose(x = x, y = y, groups = groups, col = col, pch = pch, ...)
    }
  }

