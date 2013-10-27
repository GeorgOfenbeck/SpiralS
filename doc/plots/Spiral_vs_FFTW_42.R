install.packages("ggplot2")
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)

# ====================================================================================== #
#									Results FFT3 - FFT63						         #
# ====================================================================================== #

order <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54)

adds_fftw <- c(12, 16, 32, 36, 60, 52, 80, 84, 140, 96, 176, 148, 156, 144, 296, 196, 414, 208, 264, 324, 594, 252, 352, 404, 380, 352, 760, 372, 804, 372, 552, 660, 524, 464, 1000, 932, 684, 516, 1112, 612, 1308, 736, 688, 1326, 2346, 624, 924, 830, 1092, 912, 2964, 894)
muls_fftw <- c(4, 0, 12, 8, 36, 4, 40, 24, 100, 16, 68, 72, 56, 24, 116, 80, 324, 48, 136, 200, 484, 44, 184, 136, 220, 144, 396, 112, 340, 84, 344, 232, 264, 160, 460, 456, 256, 116, 388, 272, 708, 400, 308, 1060, 2116, 136, 672, 420, 416, 272, 2704, 492)

adds_spiral <- c(12,16,32,36,84,52,80,84,188,96,216,196,156,144,312,196,430,208,336,420,886,252,352,484,380,448,952,372,806,372,696,692,644,464,1000,936,804,516,1112,756,1598,928,688,1864,3822,624,1248,804,1140,1072,2250,868)
muls_spiral <- c(4,0,12,8,36,4,40,24,84,16,76,72,56,24,108,80,230,48,136,168,422,44,184,152,220,144,396,112,342,84,296,216,264,160,460,460,280,116,388,272,710,336,308,844,1870,136,648,368,392,304,814,440)	
	

# ====================================================================================== #
#										Plot Results						             #
# ====================================================================================== #

fftw   <- adds_fftw   + muls_fftw
spiral <- adds_spiral + muls_spiral

d <- data.frame(order, adds_fftw, adds_spiral, muls_fftw, muls_spiral, fftw, spiral)

adds_plot <- ggplot(d, aes(order)) + geom_line(aes(y=adds_fftw, color="FFTW")) + geom_point(aes(y=adds_fftw, color="FFTW")) + geom_line(aes(y=adds_spiral, color="Spiral")) + geom_point(aes(y=adds_spiral, color="Spiral")) + ylab("Additions") + xlab("") + opts(title="Prototype vs FFTW") + geom_text(aes(label=adds_fftw, y=adds_fftw, color="FFTW"), hjust=0, vjust=0, size=2.5) + geom_text(aes(label=adds_spiral, y=adds_spiral, color="Spiral"), hjust=0, vjust=0, size=2.5)

muls_plot <- ggplot(d, aes(order)) + geom_line(aes(y=muls_fftw, color="FFTW")) + geom_point(aes(y=muls_fftw, color="FFTW")) + geom_line(aes(y=muls_spiral, color="Spiral")) + geom_point(aes(y=muls_spiral, color="Spiral")) + ylab("Miltiplications") + xlab("") + geom_text(aes(label=muls_fftw, y=muls_fftw, color="FFTW"), hjust=0, vjust=0, size=2.5) + geom_text(aes(label=muls_spiral, y=muls_spiral, color="Spiral"), hjust=0, vjust=0, size=2.5)

all_plot <- ggplot(d, aes(order)) + geom_line(aes(y=fftw, color="FFTW")) + geom_point(aes(y=fftw, color="FFTW")) + geom_line(aes(y=spiral, color="Spiral")) + geom_point(aes(y=spiral, color="Spiral")) + ylab("Add + Mul") + xlab("FFT Size") + geom_text(aes(label=fftw, y=fftw, color="FFTW"), hjust=0, vjust=0, size=2.5) + geom_text(aes(label=spiral, y=spiral, color="Spiral"), hjust=0, vjust=0, size=2.5)

grid.arrange(adds_plot, muls_plot, all_plot)



