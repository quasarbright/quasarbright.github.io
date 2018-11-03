import primes
import matplotlib.pyplot as plt
P = primes.prange(1001)
Y = []
for i in range(len(P)-1):
    Y.append(P[i+1]-P[i])
plt.plot(Y,'x')
plt.xlabel('n')
plt.ylabel('p(n)-p(n-1)')
plt.show()
