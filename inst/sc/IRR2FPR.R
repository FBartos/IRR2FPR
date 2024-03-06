library(SIAModuleIRR2FPR)

# use results based on Erosheva et. al (2021)
IRR      <- spearman_brown_formula(0.34, 2.79)
prop_sel <- 0.18

# compute the binary classification metrics
compute_true_positive_rate(IRR, prop_sel)
compute_false_positive_rate(IRR, prop_sel)
compute_false_negative_rate(IRR, prop_sel)

# visualize the metrics across the range of the proportion of selected candidates
par(mar=c(4,4,0.1, 0.1))
plot(NA, type = "n", axes = TRUE, bty = "n", xlab = "Proportion selected", ylab = "True positive rate", xlim = c(0, 1), ylim = c(0, 1), las = 1)
x_seq <- seq(0, 1, 0.01)

lines(x_seq, compute_true_positive_rate(IRR = IRR, proportion_selected = x_seq), lwd = 2)
points(prop_sel, compute_true_positive_rate(IRR = IRR, proportion_selected = prop_sel), pch = 16, cex = 1.5)


par(mar=c(4,4,0.1, 0.1))
plot(NA, type = "n", axes = TRUE, bty = "n", xlab = "Proportion selected", ylab = "False positive rate", xlim = c(0, 1), ylim = c(0, 1), las = 1)
x_seq <- seq(0, 1, 0.01)

lines(x_seq, compute_false_positive_rate(IRR = IRR, proportion_selected = x_seq), lwd = 2)
points(prop_sel, compute_false_positive_rate(IRR = IRR, proportion_selected = prop_sel), pch = 16, cex = 1.5)


par(mar=c(4,4,0.1, 0.1))
plot(NA, type = "n", axes = TRUE, bty = "n", xlab = "Proportion selected", ylab = "False negative rate", xlim = c(0, 1), ylim = c(0, 1), las = 1)
x_seq <- seq(0, 1, 0.01)

lines(x_seq, compute_false_negative_rate(IRR = IRR, proportion_selected = x_seq), lwd = 2)
points(prop_sel, compute_false_negative_rate(IRR = IRR, proportion_selected = prop_sel), pch = 16, cex = 1.5)
