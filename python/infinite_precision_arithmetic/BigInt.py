class BigInt:
    '''
    immutable (unless you mess with digits externally)
    arbitrary precision and size
    '''
    def __init__(self, val, positive=True):
        '''
        val can be either an int, list of ints, or a string of ints, possibly with a preceeding "-"
        positive is an optional parameter only used when a list is passed in
        '''
        self.digits = []
        if isinstance(val, list) or isinstance(val, tuple):
            # empty case
            if val == []:
                raise ValueError(val)
            # validate contents
            for digit in val:
                if not isinstance(digit, int):
                    raise ValueError("list must contain only ints: {0} is not an int".format(repr(digit)))
                elif digit < 0:
                    raise ValueError("digits must be non-negative: {0}".format(repr(digit)))
            # handle leading zeroes
            for i in range(len(val)): # int dependence
                cur = val[i]
                if cur > 0:
                    self.digits = val[i:]
                    break
            if self.digits == [] and val[0] == 0:
                self.digits = [0]
            if self.digits == [0]:
                self.positive = True
            else:
                self.positive = positive
        elif isinstance(val, str):
            if val == '':
                raise ValueError(val)
            self.positive = True
            if val[0] == '-':
                self.positive = False
                if len(val) == 1: # int dependence
                    raise ValueError(val)
                else:
                    val = val[1:]
            for i in range(len(val)): # int dependence
                cur = int(val[i])
                if cur > 0:
                    self.digits = [int(digit) for digit in val[i:]]
                    break
            if self.digits == []:
                self.positive = True
                self.digits = [0]
            # let python raise the error
        elif(isinstance(val, int)):
            self.positive = True
            if val < 0:
                self.positive = False
                val = -val
            if val == 0:
                self.digits = (0,)
            while val > 0:
                self.digits.insert(0, val % 10)
                val = val // 10
        else:
            raise ValueError("bigint only accepts a list of digits, string, or int: {0}".format(repr(val)))

        # immutability
        self.digits = tuple(self.digits)


    def __str__(self):
        digit_str = ''
        for digit in self.digits:
            digit_str += str(digit)
        if self.positive:
            return 'BigInt({0})'.format(digit_str)
        else:
            return 'BigInt(-{0})'.format(digit_str)


    def __repr__(self):
        return str(self)


    def __format__(self):
        return str(self)


    def __eq__(self, other):
        '''
        false for equivalent ints
        '''
        # apparently, this is better than isinstance(other, BigInt) because of inheritance
        if type(other) is type(self):
            return self.digits == other.digits and self.positive == other.positive
        else:
            return False

    def __hash__(self):
        return hash((self.digits, self.positive))


    def __lt__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} and {1} are not comparable'.format(type(self), type(other)))
        # need to work with lengths as BigInts to avoid integer overflow
        selflen = self.length()
        otherlen = other.length()
        if selflen == otherlen:
            for pair in zip(self.digits, other.digits):
                sd, od = pair
                if sd < od:
                    return True
                if sd > od:
                    return False
            # getting here means equal
            return False
        elif selflen < otherlen:
            return True
        else:
            return False


    def __add__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        if self == BigInt(0):
            return other
        elif other == BigInt(0):
            return self
        if self.positive == other.positive:
            counter = BigInt(0)
            self_abs = self.abs()
            other_abs = other.abs()
            while counter != self_abs:
                counter = counter.add1()
                other_abs = other_abs.add1()
            return BigInt(other_abs.digits, self.positive)


    def x10(self):
        '''
        returns the number multiplied by 10
        '''
        return BigInt(list(self.digits) + [0], self.positive)


    def abs(self):
        '''
        absolute value
        '''
        return BigInt(self.digits, True)


    def length(self):
        '''
        length of integer representation
        '''
        ans = BigInt(0)
        for digit in self.digits:
            ans = ans.add1()
        return ans


    def add1(self):
        if self.positive:
            if self.digits[-1] < 9:
                return BigInt(self.digits[:-1] + (self.digits[-1] + 1,), True)
            else:
                if self.digits[:-1] == (): # tuple length 1 => self == BigInt(9)
                    return BigInt(10)
                else:
                    return BigInt(self.digits[:-1], True).add1().x10()
