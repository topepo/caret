#  File src/library/stats/R/contrast.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## a fast version of diag(n, .) / sparse-Diagonal() + dimnames
## Originally .Diag in stats:contrast.R

.RDiag <- function (nms, sparse) {
  n <- as.integer(length(nms))
  d <- c(n, n)
  dn <- list(nms, nms)
  #   if (sparse) {
  #     requireNamespace("Matrix", quietly = TRUE)
  #     new("ddiMatrix", diag = "U", Dim = d, Dimnames = dn)
  #   }
  #   else
  array(c(rep.int(c(1, numeric(n)), n - 1L), 1), d, dn)
}