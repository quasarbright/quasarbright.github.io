def is_prime(n):
    for x in range(2,n):
        if n % x == 0:
            return False
    return True
for n in range(10000):
    if not is_prime(n*n + n + 41):
        print(n, n*n + n + 41)
        break
