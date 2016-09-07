#' @export
caretTheme <- function() 
   list(
      plot.polygon = list(alpha = 1, col = "aliceblue", border = "black",
         lty = 1, lwd = 1),
      background = list(col = "transparent"), 
      bar.fill = list(col = "#cce6ff"), 
      box.rectangle = list(col = "black"), 
      box.umbrella = list(col = "black"), 
      dot.line = list(col = "#e8e8e8"), 
      dot.symbol = list(col = "black"), 
      plot.line = list(col = "black"), 
      plot.symbol = list(col = "black"), 
      regions = list(col = 
         c(
         "#FEF8FA", "#FDF6F9", "#FBF5F9", "#FAF3F8", 
         "#F8F2F7", "#F7F0F7", "#F5EEF6", "#F4EDF5", 
         "#F2EBF5", "#F1EAF4", "#EFE8F3", "#EDE7F2", 
         "#ECE5F1", "#EAE4F1", "#E8E2F0", "#E6E1EF", 
         "#E4DFEE", "#E2DEED", "#E0DCEC", "#DEDAEB", 
         "#DCD9EA", "#D9D7E9", "#D7D6E8", "#D4D4E7", 
         "#D1D2E6", "#CED1E5", "#CCCFE4", "#C8CEE3",
         "#C5CCE2", "#C2CAE1", "#BFC9E0", "#BBC7DF", 
         "#B8C5DF", "#B4C4DE", "#B1C2DD", "#ADC0DC", 
         "#A9BFDB", "#A6BDDA", "#A2BBD9", "#9EB9D9", 
         "#9BB8D8", "#97B6D7", "#93B4D6", "#8FB2D5", 
         "#8BB0D4", "#87AFD3", "#83ADD2", "#7FABD1", 
         "#7AA9D0", "#76A7CF", "#71A5CE", "#6CA3CC", 
         "#68A1CB", "#63A0CA", "#5D9EC9", "#589CC8",
         "#539AC6", "#4E98C5", "#4996C4", "#4493C3", 
         "#3F91C1", "#3A8FC0", "#358DBF", "#308BBE", 
         "#2C89BD", "#2887BC", "#2385BB", "#1F83BA", 
         "#1C80B9", "#187EB7", "#157CB6", "#127AB5", 
         "#0F78B3", "#0D76B2", "#0A73B0", "#0971AE", 
         "#076FAC", "#066DAA", "#056AA7", "#0568A5")
      ), 
      strip.shingle = list(col = c(
         "#ff7f00", "#00ff00", "#00ffff",
         "#ff00ff", "#ff0000", "#ffff00", "#0080ff")), 
      strip.background = list(col = c(
         "#ffe5cc", "#ccffcc", "#ccffff",
         "#ffccff", "#ffcccc", "#ffffcc", "#cce6ff")), 
      reference.line = list(col = "#e8e8e8"), 
      superpose.line = list(
         col = c(
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black", 
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black", 
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black",
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black", 
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black", 
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black"), 
         lty = rep(1:6, each = 6)), 
      superpose.symbol = list(
         pch = c(
            1, 4, 6, 0, 5, 17,
            4, 6, 0, 5, 17, 1,
            6, 0, 5, 17, 1, 4,
            0, 5, 17, 1, 4, 6,
            5, 17, 1, 4, 6, 0 ,
            17, 1, 4, 6, 0, 5), 
         cex = rep(0.7,  6 * 6), 
         col = c(
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black", 
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black", 
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black",
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black", 
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black", 
           "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black")))
         

