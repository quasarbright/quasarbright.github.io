def f(x):
    if(x>4):
        return f(x-1)+2*x
    elif(x>1 and x<=4):
        return f(x-2)*x + x
    else:
        return x

print(f(6))
