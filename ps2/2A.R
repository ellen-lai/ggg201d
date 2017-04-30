##### 2A #### all graphs on same graph
# NOTE: I know that this isn't the most efficient way to do this
# but I couldn't figure out how to do it. :(
# a = initial frequency of advantageous allele
# b = initial frequence of bad allele
# saa, sab, sbb = selection coeff for aa,ab,bb genotype

# function to calculate delta f(a)
delta_freq_a <- function(a,saa,sab,sbb) {
  abs(a-(a^2*(1-saa) + a*(1-a)*(1-sab))/(a^2*(1-saa) + 2*a*(1-a)*(1-sab) + (1-a)^2*(1-sbb))) 
}

# create vector of initial f(a) for x axis
freq_a_g1 <- seq(0,1,by=0.01)

# create a function that calculates other selection coeff
# based on MOI (doesn't work)
calc_sab <- function(moi,sbb) {
  if (moi == "Recessive"){
    sab <- sbb
  } else if (moi == "Dominant") {
      sab <- 0
    } else if (moi == "Additive") {
        sab <- sbb/2
      }
}
Dall = NULL

# 1 sbb = 0.1

# 1a recessive (a < b)
p = NULL
moi = "Recessive"
saa = 0
sab = 0.1
sbb = 0.1
for(i in freq_a_g1){
  z <- delta_freq_a(i, saa, sab, sbb)
  p <- append(p, values = z)
}

D <- data.frame(freq_a_g1,p)
D <- mutate(D, sbb, moi)
Dall <- D

# 1b a is dominant (a > b)
p = NULL
moi = "Dominant"
saa = 0
sab = 0
sbb = 0.1
for(i in freq_a_g1){
  z <- delta_freq_a(i, saa, sab, sbb)
  p <- append(p, values = z)
}

D <- data.frame(freq_a_g1,p)
D <- mutate(D, sbb, moi)
Dall <- bind_rows(Dall,D)

# 1c a and b are additive
p = NULL
moi = "Additive"
saa = 0
sab = 0.1/2
sbb = 0.1
for(i in freq_a_g1){
  z <- delta_freq_a(i, saa, sab, sbb)
  p <- append(p, values = z)
}

D <- data.frame(freq_a_g1,p)
D <- mutate(D, sbb, moi)
Dall <- bind_rows(Dall,D)

# 2 sbb = 0.25

# 2a recessive (a < b)
p = NULL
moi = "Recessive"
saa = 0
sab = 0.25
sbb = 0.25
for(i in freq_a_g1){
  z <- delta_freq_a(i, saa, sab, sbb)
  p <- append(p, values = z)
}

D <- data.frame(freq_a_g1,p)
D <- mutate(D, sbb, moi)
Dall <- bind_rows(Dall,D)

# 2b a is dominant (a > b)
p = NULL
moi = "Dominant"
saa = 0
sab = 0
sbb = 0.25
for(i in freq_a_g1){
  z <- delta_freq_a(i, saa, sab, sbb)
  p <- append(p, values = z)
}

D <- data.frame(freq_a_g1,p)
D <- mutate(D, sbb, moi)
Dall <- bind_rows(Dall,D)

# 2c a and b are additive
p = NULL
moi = "Additive"
saa = 0
sab = 0.25/2
sbb = 0.25
for(i in freq_a_g1){
  z <- delta_freq_a(i, saa, sab, sbb)
  p <- append(p, values = z)
}

D <- data.frame(freq_a_g1,p)
D <- mutate(D, sbb, moi)
Dall <- bind_rows(Dall,D)

# graph
ggplot(Dall, aes(freq_a_g1,p)) +
  geom_point(aes(color = as.factor(sbb), shape = moi), size = .8) +
  labs(x="Initial f(A)", y="Expected change in f(A)", color = "Selection coeff of BB", shape = "MOI") +
  scale_x_continuous(breaks = number_ticks(10)) +
  theme_bw() +
  ggsave("2A.pdf", width = 7, height = 4)
