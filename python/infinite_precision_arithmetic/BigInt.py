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
        if isinstance(val, list):
            # empty case
            if val == []:
                raise ValueError("[]")
            # validate contents
            for digit in val:
                if not isinstance(digit, int):
                    raise ValueError("list must contain only ints: {0} is not an int".format(repr(digit)))
                elif digit < 0:
                    raise ValueError("digits must be non-negative: {0}".format(repr(digit)))
            # handle leading zeroes
            for i in range(len(val)):
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
            self.positive = True
            if val[0] == '-':
                self.positive = False
                if len(val) == 1:
                    raise ValueError(val)
                else:
                    val = val[1:]
            for i in range(len(val)):
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
            while val > 0:
                self.digits.insert(0, val % 10)
                val = val // 10
        else:
            raise ValueError("bigint only accepts a list of digits, string, or int: {0}".format(repr(val)))

        # immutability
        self.digits = tuple(self.digits)


    def x10(self):
        '''
        returns the number multiplied by 10
        '''
        return BigInt(self.digits + [0], self.positive)


    def abs(self):
        '''
        absolute value
        '''
        return BigInt(self.digits, True)


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
