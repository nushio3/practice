require(ggplot2)
theme_set(theme_bw())

mips = read.table("mipsperbuck.dat", sep=" ", skip=2)[,1:2]
jit = position_jitter(width=0.5)

g = ggplot(data = mips, aes(x = V1, y = V2)) +
  scale_y_log10() +
  geom_jitter(colour=alpha("black", 1/2)) + 
  xlab("Year") + ylab("MIPS per die")
g
dev.copy2pdf(file="/tmp/figure1.pdf")

#######################################################
#Clock speed CPU
#######################################################
clock_files = c("clockfreq80x86.dat", "clockfreqP1.dat",
  "clockfreqPPro.dat", "clockfreqAtom.dat", "clockfreqP2.dat",
  "clockfreqXeonDC.dat", "clockfreq.dat", "clockfreqP3.dat",
  "clockfreqNehalem.dat", "clockfreqP4.dat")

df = read.table(clock_files[1], sep=" ")
for(i in 2:length(clock_files)) {
  filename = clock_files[i]
  df = rbind(df, read.table(filename, sep=" ")[,1:2])
}

jit = position_jitter(width=0.5)

g = ggplot(data = df, aes(x = df$V1, y = df$V2)) +
  scale_y_log10() +
  geom_jitter(position=jit, colour=alpha("black", 1/3)) + 
  xlab("Year") + ylab("MIPS/CPU clock speed") 
g
dev.copy2pdf(file="/tmp/figure2.pdf")


##################################################
###Flop speed
################################################

cpuflops = read.table("cpuflops.csv", sep=",")
gpuflops = read.table("gpuflops.csv", sep=",")

gpuflops$V3 = as.Date(strptime(gpuflops$V3, "%d/%m/%Y"))
cpuflops$V3 = as.Date(strptime(cpuflops$V3, "%d/%m/%Y"))
cpuflops$type = "CPU"; gpuflops$type = "GPU"
flops = rbind(gpuflops, cpuflops)

g = ggplot(data=flops, aes(x=V3, y=V2, group=type)) +
  geom_point(aes(colour=type) ) + geom_line(aes(colour=type)) +
  xlab("Year") + ylab("GFLOPS") +
  scale_y_log10(limits=c(1,1600)) 
g


dev.copy2pdf(file="/tmp/figure3.pdf")
