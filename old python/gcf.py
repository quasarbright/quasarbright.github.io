def gcf(a,b):
    r = a % b
    while r != 0:
        a = b
        b = r
        r = a % b
    return b
alf = "hey I'm alf"
raise Exception
print(gcf(12,6))
