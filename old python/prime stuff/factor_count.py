import primes
import matplotlib.pyplot as plt
X = range(2,1001)
Y = []
for x in X:
    Y.append(len(primes.primeFactors(x)))
plt.plot(Y,'x')
plt.xlabel('n')
plt.ylabel('number of factors of n')
plt.show()
