install.packages("ggplot2")
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)

# ====================================================================================== #
#									Results FFT3 - FFT63						         #
# ====================================================================================== #

order <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63)

adds_fftw <- c(12, 16, 32, 36, 60, 52, 80, 84, 140, 96, 176, 148, 156, 144, 296, 196, 428, 208, 264, 324, 692, 252, 352, 404, 380, 352, 760, 372, 804, 372, 552, 660, 524, 464, 1000, 932, 684, 516, 1112, 612, 1308, 736, 688, 1522, 2346, 624, 912, 804, 1092, 912, 1928, 868, 1052, 844, 1512, 1636, 3654, 864, 1848, 1794, 1128)
muls_fftw <- c(4, 0, 12, 8, 36, 4, 40, 24, 100, 16, 68, 72, 56, 24, 116, 80, 228, 48, 136, 200, 484, 44, 184, 136, 220, 144, 396, 112, 340, 84, 344, 232, 264, 160, 460, 456, 256, 116, 388, 272, 708, 400, 308, 1060, 2116, 136, 648, 368, 416, 272, 748, 440, 632, 316, 760, 792, 3364, 224, 684, 804, 660)

adds_spiral <- c(12, 16, 40, 36, 86, 53, 81, 100, 220, 96, 218, 200, 180, 146, 322, 197, 431, 240, 342, 484, 1012, 253, 435, 488, 386, 456, 970, 420, 902, 377, 792, 710, 710, 465, 1003, 937, 810, 581, 1243, 768, 1622, 1056, 761, 2116, 4326, 626, 1283, 967, 1166, 1080, 2266, 874, 1540, 1053, 1519, 2056, 4230, 960, 2042, 1928, 1335)
muls_spiral <- c(4, 0, 12, 8, 38, 6, 40, 24, 84, 16, 78, 76, 56, 29, 117, 80, 230, 48, 142, 168, 420, 50, 185, 156, 222, 152, 414, 112, 342, 96, 296, 233, 274, 160, 462, 460, 286, 126, 410, 284, 734, 336, 308, 840, 1862, 149, 679, 369, 417, 312, 830, 442, 552, 346, 769, 828, 1886, 224, 686, 684, 625)	

# ====================================================================================== #
#										Plot Results						             #
# ====================================================================================== #

fftw   <- adds_fftw   + muls_fftw
spiral <- adds_spiral + muls_spiral

d <- data.frame(order, adds_fftw, adds_spiral, muls_fftw, muls_spiral, fftw, spiral)

adds_plot <- ggplot(d, aes(order)) + geom_line(aes(y=adds_fftw, color="FFTW")) + geom_point(aes(y=adds_fftw, color="FFTW")) + geom_line(aes(y=adds_spiral, color="Spiral")) + geom_point(aes(y=adds_spiral, color="Spiral")) + ylab("Additions") + xlab("") + opts(title="Spiral VS FFTW: ops count") + geom_text(aes(label=adds_fftw, y=adds_fftw, color="FFTW"), hjust=0, vjust=0, size=2.5) + geom_text(aes(label=adds_spiral, y=adds_spiral, color="Spiral"), hjust=0, vjust=0, size=2.5)

muls_plot <- ggplot(d, aes(order)) + geom_line(aes(y=muls_fftw, color="FFTW")) + geom_point(aes(y=muls_fftw, color="FFTW")) + geom_line(aes(y=muls_spiral, color="Spiral")) + geom_point(aes(y=muls_spiral, color="Spiral")) + ylab("Miltiplications") + xlab("") + geom_text(aes(label=muls_fftw, y=muls_fftw, color="FFTW"), hjust=0, vjust=0, size=2.5) + geom_text(aes(label=muls_spiral, y=muls_spiral, color="Spiral"), hjust=0, vjust=0, size=2.5)

all_plot <- ggplot(d, aes(order)) + geom_line(aes(y=fftw, color="FFTW")) + geom_point(aes(y=fftw, color="FFTW")) + geom_line(aes(y=spiral, color="Spiral")) + geom_point(aes(y=spiral, color="Spiral")) + ylab("Add + Mul") + xlab("FFT Size") + geom_text(aes(label=fftw, y=fftw, color="FFTW"), hjust=0, vjust=0, size=2.5) + geom_text(aes(label=spiral, y=spiral, color="Spiral"), hjust=0, vjust=0, size=2.5)

grid.arrange(adds_plot, muls_plot, all_plot)



