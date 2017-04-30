#### 1A ####
a <- seq(0,1, by=0.1)
n <- 20
all_p_delta = NULL
for(f in a) {
  # range of how many alleles change
  x <- n*f # initial number of A alleles
  range <- seq(0,x, by = 1)
  
  # create vector of possible number of alleles change by +/- i in range
  p <- NULL
  for(i in range) {
    #if(i==0) {
     # p_delta <- dbinom(x, size = n, prob = f)
      #p <- append(p, values = p_delta) # NOTE: Don't really need to include this calculation for when delta f(A) = 0 because the probability is multiplied by 0 in the expectation probability calculation
   # } else
      p_delta <- dbinom((x + i), size = n, prob = f) + dbinom((x - i), size = n, prob = f) 
      p <- append(p, values = p_delta)
    }
  
  # calculate value of outcome (excluding when delta = 0)
  v <- NULL
  for(i in range) {
    v <- append(v, values = i/n)
  }
  
  all_p_delta <- append(all_p_delta, sum(v*p))
}

D <- data.frame(a,all_p_delta)

# function for tick marks in graph
number_ticks <- function(n) {function(limits) pretty(limits, n)}

ggplot(D, aes(a,all_p_delta)) +
  geom_point() +
  labs(x="Initial f(A)", y="Expected change in f(A)") +
  scale_x_continuous(breaks = number_ticks(10)) +
  theme_bw() +
  ggsave("1A.pdf", width = 5, height = 4)
