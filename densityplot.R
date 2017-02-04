# library("data.table")
library("ggplot2")
library("scales")

dat <- read.csv2("sample_distribution.csv", header=TRUE, na="NA", check.names=FALSE)
# give names of headers
headers <- names(dat)
attach(dat)

d <- density(HE)
pk <- d$x[which.max(d$y)]
pk_y <- max(d$y)
pklabx <- pk + 3

# p15 = quantile(HE, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
p15 = quantile(HE, probs = 0.15, na.rm = FALSE, names = FALSE, type = 7)
p15_x = which.min(abs(d$x-p15))
p15_y = d$y[p15_x] 
p15labx = p15 + 4

# closure needed for FWHM 
xmax <- d$x[d$y==max(d$y)]

x1 <- d$x[d$x < xmax][which.min(abs(d$y[d$x < xmax]-max(d$y)/2))]
x2 <- d$x[d$x > xmax][which.min(abs(d$y[d$x > xmax]-max(d$y)/2))]
y1 <- d$y[d$x==x1]
y2 <- d$y[d$x==x2]


# points(c(x1, x2), c(d$y[d$x==x1], d$y[d$x==x2]), col="red")


## Create data
df <- with(density(HE), data.frame(x, y))
head(dat)

# ggplot(dat, aes(x=HE)) + geom_line(stat="density")

# helper for upper boundary space
yupper <- max(df$y) + 0.001

plot <- ggplot(data = df, mapping = aes(x = x, y = y)) +
  layer(geom = "line") +
  layer(geom = "area", mapping = aes(x = ifelse(x< -950 , x, -950)),
         geom_params = list(fill = "moccasin")) +
#         geom_params = list(fill = "coral2", alpha = 0.2)) +
  scale_y_continuous("Häufigkeit",limits = c(0, yupper), labels = percent, expand = c(0,0)) +
  scale_x_continuous("Dichte in HE", limits = c(-1025, -925), expand = c(0,0)) +
# geom_text(data = NULL, x=-990, y=0.002, label="LAV2", colour="coral2", alpha=0.1) +
  geom_text(data = NULL, x=-990, y=0.002, label="LAV2", colour="black") +
  annotate("segment", x=pk, xend=pk, y=pk_y, yend=0, colour="goldenrod2", size=1.5) +
  geom_text(x=pklabx, y=0.015, label="Pk", colour="goldenrod2") +
  annotate("segment", x=p15, xend=p15, y=p15_y, yend=0, colour="darkolivegreen3", size=1.5) +
  geom_text(x=p15labx, y=0.012, label="P15", colour="darkolivegreen3") +
  annotate("segment", x=x1, xend=x2, y=y2, yend=y2, colour="lightskyblue2", size=1.5) +
  geom_text(x=-938, y=y2, label="FWHM", colour="lightskyblue2") +
  labs(x=NULL, y=NULL) +
  theme_bw()

plot

