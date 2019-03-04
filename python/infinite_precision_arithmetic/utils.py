def myzip(a, b):
    '''lists of digits, pads with 0s until both lists are empty'''
    a = list(a)
    b = list(b)
    while a != [] or b != []:
        ans = None
        if a == []:
            ans = (0, b[0])
            b = b[1:]
        elif b == []:
            ans = (a[0],0)
            a = a[1:]
        elif b != []:
            ans = (a[0], b[0])
            a = a[1:]
            b = b[1:]
        yield ans
