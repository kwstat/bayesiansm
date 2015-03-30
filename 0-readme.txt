# 0-readme.txt

# Problem.  I had this error message:
# Failed to set trace monitor for mu.new
# Solution is to fully define mu.new by setting the first value to
# any value.  Then mu.new can be monitored.
# See: http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/ac5faa5d/
mu.new[1] <- 0
