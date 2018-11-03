import math
def isPrime(n):
    '''returns whether a number is a prime number (boolean)'''
    assert isinstance(n, int)
    if n <= 1:
        return False
    for x in range(2,math.floor(math.sqrt(n))+1):
        if n % x == 0:
            return False
    return True
def  prange(a,b=None, step=1):
    '''returns the ath to bth prime numbers in order in a list, bth prime not included.
    b optional. If no b, then it will return 1st to ath prime, ath prime not included'''
    start = 0
    end = 0
    if b == None:
        if a <= 1:
            return None
        start = 1
        end = a
    elif a <= 0 or b <= 0 or a > b:
        return None
    else:
        start = a
        end = b
    assert isinstance(start, int)
    assert isinstance(end, int)
    assert isinstance(step, int)
    n = nthPrime(start)
    primes = [n]
    while len(primes) < end-start:
        n+=1
        if isPrime(n):
            primes.append(n)
    stepPrimes = []
    for i in range(len(primes)):
        if i % step == 0:
            stepPrimes.append(primes[i])
    return stepPrimes
def nthPrime(n):
    '''returns the nth prime number'''
    assert isinstance(n, int)
    if n > 0:
        m = 0
        i = 0
        while i < n:
            m+=1
            if isPrime(m):
                i+=1
        return m
def primesLessThan(n):
    '''returns all the prime numbers less than n'''
    if n <= 2:
        return None
    m = 2
    primes = [2]
    while m < n-1:
        m+=1
        if isPrime(m):
            primes.append(m)
    return primes
def primeFactors(n):
    '''returns the prime factorization of n as a list'''
    assert isinstance(n,int)
    if isPrime(n):
        return [n]
    factors = []
    primes = primesLessThan(math.sqrt(n)+1)
    i = 0
    current = primes[i]
    while n != 1 and i < len(primes):
        current = primes[i]
        while n % current == 0:
            factors.append(current)
            n = math.floor(n / current) #123/3 = 41.0 for some reason but it's ok bc % == 0
        i += 1
    if n != 1:
        factors += (primeFactors(n))
    return factors
def areCoprime(a,b):
    '''returns whether a and b share any factors greater than 1'''
    assert isinstance(a,int)
    assert isinstance(b,int)
    aps = set(primeFactors(a))
    bps = set(primeFactors(b))
    common = aps.intersection(bps)
    return len(common) == 0
def gcd(a,b):
    '''returns greatest common divisor of a and b'''
    assert isinstance(a,int)
    assert isinstance(b,int)
    aps = set(primeFactors(a))
    bps = set(primeFactors(b))
    common = aps.intersection(bps)
    ans = 1
    for x in common:
        ans = ans * x
    return ans
def condense(a):
    '''returns a hash of an array of positive integers using primes
    (dummy slow)'''
    for x in a:
        assert isinstance(x,int) and x > 0
    primes = [nthPrime(x) for x in a]
    ans = 1
    for x in primes:
        ans = x ** ans
    return ans
if __name__ == '__main__':
    print(condense([1,2,1]))
