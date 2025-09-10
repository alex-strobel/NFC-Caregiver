library(shape)   # for Arrows()

# load parameter estimates for rpe (Fig1), ee (Fig2, and de (Fig3)
load(here("Data", "NFC-Caregiver_RMarkdown-generated.RData"))

# FIGURE 1 --------------------------------------------------------------------

# --- helper functions ---
get_val <- function(lbl) {
  printnum(pe_rpe$std.all[pe_rpe$label == lbl], gt1 = FALSE)
}
get_sig <- function(lbl) {
  (pe_rpe$pvalue[pe_rpe$label == lbl] < .05) + 1
}

# --- layout parameters ---
x_left   <- 0
x_med    <- 8
x_right  <- 16
box_w    <- 4
box_h    <- 1
ind_offset <- .8

# y-coordinates of mediator boxes
y_mediators <- c(9.5, 7.75, 6, 4.25, 2.5)

mediator_labels <- c(
  "Self-Control",
  "Reappraisal",
  "Suppression",
  "Adaptive\nCoping",
  "Maladaptive\nCoping"
)

a_labels <- paste0("a", 1:5)
b_labels <- paste0("b", 1:5)
ind_labels <- paste0("ind", 1:5)

# --- prepare plot ---
par(mar = c(0,0,0,0))
plot(c(0,20), c(0,10.75), type="n", axes=FALSE, xlab="", ylab="")

# --- boxes ---
# predictor
rect(x_left, 5.5, x_left+box_w, 6.5)
text(x_left+box_w/2, 6, "Need for\nCognition", cex=.8)

# mediators
for (i in seq_along(y_mediators)) {
  rect(x_med, y_mediators[i]-box_h/2, x_med+box_w, y_mediators[i]+box_h/2)
  text(x_med+box_w/2, y_mediators[i], mediator_labels[i], cex=.8)
}

# outcome
rect(x_right, 5.5, x_right+box_w, 6.5)
text(x_right+box_w/2, 6, "Reduced\nPersonal Efficiency", cex=.8)

# --- arrows from predictor to mediators (a paths) ---
lines(rep(5,2), range(y_mediators))
lines(c(4,5), rep(6,2))
for (i in seq_along(y_mediators)) {
  lines(c(5,6), rep(y_mediators[i],2))
  Arrows(7, y_mediators[i], x_med, y_mediators[i], arr.type="triangle", arr.adj=1, arr.length=.2, arr.width=.2)
  text(6.5, y_mediators[i], get_val(a_labels[i]), font=get_sig(a_labels[i]), cex=.8)
}

# --- arrows from mediators to outcome (n paths) ---
lines(rep(x_right-1,2), range(y_mediators))
Arrows(x_right-1, 6, x_right, 6, arr.type="triangle", arr.adj=1, arr.length=.2, arr.width=.2)

for (i in seq_along(y_mediators)) {
  lines(c(x_med+4,x_med+5), rep(y_mediators[i], 2))
  text(13.5, y_mediators[i], get_val(b_labels[i]), font=get_sig(b_labels[i]), cex=.8)
  lines(c(x_med+6,x_med+7), rep(y_mediators[i], 2))
}

# --- indirect effects (a*b) ---
for (i in seq_along(y_mediators)) {
  text(x_med+box_w/2, y_mediators[i]+ind_offset, get_val(ind_labels[i]), 
       font=get_sig(ind_labels[i]), cex=.8)
}

# --- direct effect (c/c') ---
lines(rep(x_left+2,2),c(5.5, y_mediators[5]-1.75))
lines(c(x_left+2,x_right+2), rep(y_mediators[5]-1.75, 2))
Arrows(x_right+2, y_mediators[5]-1.75, x_right+2, y_mediators[3]-.5, arr.type="triangle", arr.adj=1, arr.length=.2, arr.width=.2)
text(x_med+2,y_mediators[5]-1.75+.3, get_val("c"), font=get_sig("c"), cex=.8)
text(x_med+2,y_mediators[5]-1.75-.3, paste0("[", get_val("total"), "]"), font=get_sig("total"), cex=.8)

# reset plot margins
par(mar=c(5,4,4,2))

# now save figure as Fig1.eps via Plots > Export > Save as Image ...

# FIGURE 2 --------------------------------------------------------------------

# --- helper functions ---
get_val <- function(lbl) {
  printnum(pe_ee$std.all[pe_ee$label == lbl], gt1 = FALSE)
}
get_sig <- function(lbl) {
  (pe_ee$pvalue[pe_ee$label == lbl] < .05) + 1
}

# --- layout parameters ---
x_left   <- 0
x_med    <- 8
x_right  <- 16
box_w    <- 4
box_h    <- 1
ind_offset <- .8

# y-coordinates of mediator boxes
y_mediators <- c(9.5, 7.75, 6, 4.25, 2.5)

mediator_labels <- c(
  "Self-Control",
  "Reappraisal",
  "Suppression",
  "Adaptive\nCoping",
  "Maladaptive\nCoping"
)

a_labels <- paste0("a", 1:5)
b_labels <- paste0("b", 1:5)
ind_labels <- paste0("ind", 1:5)

# --- prepare plot ---
par(mar = c(0,0,0,0))
plot(c(0,20), c(0,10.75), type="n", axes=FALSE, xlab="", ylab="")

# --- boxes ---
# predictor
rect(x_left, 5.5, x_left+box_w, 6.5)
text(x_left+box_w/2, 6, "Need for\nCognition", cex=.8)

# mediators
for (i in seq_along(y_mediators)) {
  rect(x_med, y_mediators[i]-box_h/2, x_med+box_w, y_mediators[i]+box_h/2)
  text(x_med+box_w/2, y_mediators[i], mediator_labels[i], cex=.8)
}

# outcome
rect(x_right, 5.5, x_right+box_w, 6.5)
text(x_right+box_w/2, 6, "Emotional\nExhaustion", cex=.8)

# --- arrows from predictor to mediators (a paths) ---
lines(rep(5,2), range(y_mediators))
lines(c(4,5), rep(6,2))
for (i in seq_along(y_mediators)) {
  lines(c(5,6), rep(y_mediators[i],2))
  Arrows(7, y_mediators[i], x_med, y_mediators[i], arr.type="triangle", arr.adj=1, arr.length=.2, arr.width=.2)
  text(6.5, y_mediators[i], get_val(a_labels[i]), font=get_sig(a_labels[i]), cex=.8)
}

# --- arrows from mediators to outcome (n paths) ---
lines(rep(x_right-1,2), range(y_mediators))
Arrows(x_right-1, 6, x_right, 6, arr.type="triangle", arr.adj=1, arr.length=.2, arr.width=.2)

for (i in seq_along(y_mediators)) {
  lines(c(x_med+4,x_med+5), rep(y_mediators[i], 2))
  text(13.5, y_mediators[i], get_val(b_labels[i]), font=get_sig(b_labels[i]), cex=.8)
  lines(c(x_med+6,x_med+7), rep(y_mediators[i], 2))
}

# --- indirect effects (a*b) ---
for (i in seq_along(y_mediators)) {
  text(x_med+box_w/2, y_mediators[i]+ind_offset, get_val(ind_labels[i]), 
       font=get_sig(ind_labels[i]), cex=.8)
}

# --- direct effect (c/c') ---
lines(rep(x_left+2,2),c(5.5, y_mediators[5]-1.75))
lines(c(x_left+2,x_right+2), rep(y_mediators[5]-1.75, 2))
Arrows(x_right+2, y_mediators[5]-1.75, x_right+2, y_mediators[3]-.5, arr.type="triangle", arr.adj=1, arr.length=.2, arr.width=.2)
text(x_med+2,y_mediators[5]-1.75+.3, get_val("c"), font=get_sig("c"), cex=.8)
text(x_med+2,y_mediators[5]-1.75-.3, paste0("[", get_val("total"), "]"), font=get_sig("total"), cex=.8)

# reset plot margins
par(mar=c(5,4,4,2))

# now save figure as Fig2.eps via Plots > Export > Save as Image ...

# FIGURE 3 --------------------------------------------------------------------

# --- helper functions ---
get_val <- function(lbl) {
  printnum(pe_de$std.all[pe_de$label == lbl], gt1 = FALSE)
}
get_sig <- function(lbl) {
  (pe_de$pvalue[pe_de$label == lbl] < .05) + 1
}

# --- layout parameters ---
x_left   <- 0
x_med    <- 8
x_right  <- 16
box_w    <- 4
box_h    <- 1
ind_offset <- .8

# y-coordinates of mediator boxes
y_mediators <- c(9.5, 7.75, 6, 4.25, 2.5)

mediator_labels <- c(
  "Self-Control",
  "Reappraisal",
  "Suppression",
  "Adaptive\nCoping",
  "Maladaptive\nCoping"
)

a_labels <- paste0("a", 1:5)
b_labels <- paste0("b", 1:5)
ind_labels <- paste0("ind", 1:5)

# --- prepare plot ---
par(mar = c(0,0,0,0))
plot(c(0,20), c(0,10.75), type="n", axes=FALSE, xlab="", ylab="")

# --- boxes ---
# predictor
rect(x_left, 5.5, x_left+box_w, 6.5)
text(x_left+box_w/2, 6, "Need for\nCognition", cex=.8)

# mediators
for (i in seq_along(y_mediators)) {
  rect(x_med, y_mediators[i]-box_h/2, x_med+box_w, y_mediators[i]+box_h/2)
  text(x_med+box_w/2, y_mediators[i], mediator_labels[i], cex=.8)
}

# outcome
rect(x_right, 5.5, x_right+box_w, 6.5)
text(x_right+box_w/2, 6, "Depersonalization", cex=.8)

# --- arrows from predictor to mediators (a paths) ---
lines(rep(5,2), range(y_mediators))
lines(c(4,5), rep(6,2))
for (i in seq_along(y_mediators)) {
  lines(c(5,6), rep(y_mediators[i],2))
  Arrows(7, y_mediators[i], x_med, y_mediators[i], arr.type="triangle", arr.adj=1, arr.length=.2, arr.width=.2)
  text(6.5, y_mediators[i], get_val(a_labels[i]), font=get_sig(a_labels[i]), cex=.8)
}

# --- arrows from mediators to outcome (n paths) ---
lines(rep(x_right-1,2), range(y_mediators))
Arrows(x_right-1, 6, x_right, 6, arr.type="triangle", arr.adj=1, arr.length=.2, arr.width=.2)

for (i in seq_along(y_mediators)) {
  lines(c(x_med+4,x_med+5), rep(y_mediators[i], 2))
  text(13.5, y_mediators[i], get_val(b_labels[i]), font=get_sig(b_labels[i]), cex=.8)
  lines(c(x_med+6,x_med+7), rep(y_mediators[i], 2))
}

# --- indirect effects (a*b) ---
for (i in seq_along(y_mediators)) {
  text(x_med+box_w/2, y_mediators[i]+ind_offset, get_val(ind_labels[i]), 
       font=get_sig(ind_labels[i]), cex=.8)
}

# --- direct effect (c/c') ---
lines(rep(x_left+2,2),c(5.5, y_mediators[5]-1.75))
lines(c(x_left+2,x_right+2), rep(y_mediators[5]-1.75, 2))
Arrows(x_right+2, y_mediators[5]-1.75, x_right+2, y_mediators[3]-.5, arr.type="triangle", arr.adj=1, arr.length=.2, arr.width=.2)
text(x_med+2,y_mediators[5]-1.75+.3, get_val("c"), font=get_sig("c"), cex=.8)
text(x_med+2,y_mediators[5]-1.75-.3, paste0("[", get_val("total"), "]"), font=get_sig("total"), cex=.8)

# reset plot margins
par(mar=c(5,4,4,2))

# now save figure as Fig3.eps via Plots > Export > Save as Image ...