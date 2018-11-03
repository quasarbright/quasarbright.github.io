from primes import *
import math
st = 'no da wae is dead'#input('your first name: ')
bn = ''.join(format(ord(x), 'b') for x in st)
num = math.floor(math.sqrt(int(bn,2)))
print(primeFactors(num))
