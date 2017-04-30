#### 1B ####
pop <- seq(10, 100,by=10)
all_p_delta = NULL
f <- 0.5
for(n in pop) {
  # range of how many alleles change
  x <- n*f # initial number of A alleles
  range <- seq(0,x, by = 1)
  
  # create vector of possible number of alleles change by +/- i in range
  p <- NULL
  
  for(i in range) {
    p_delta <- dbinom((x + i), size = n, prob = f) + dbinom((x - i), size = n, prob = f) #NOTE: doubles p_delta for when delta f(A)=0
    p <- append(p, values = p_delta)
  }
  
  # calculate value of outcome
  v <- NULL
  for(i in range) {
    v <- append(v, values = i/n)
  }
  
  all_p_delta <- append(all_p_delta, sum(v*p))
}

D <- data.frame(pop,all_p_delta)

ggplot(D, aes(pop,all_p_delta)) +
  geom_point() +
  labs(x="Population size", y="Expected change in f(A)") +
  scale_x_continuous(breaks = number_ticks(10)) +
  theme_bw() +
  ggsave("1B.pdf", width = 5, height = 4)
