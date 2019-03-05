from utils import *

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
        if self == other:
            return False
        if self.positive and not other.positive:
            return False
        if other.positive and not self.positive:
            return True
        ans = None
        if selflen == otherlen:
            assert self != other and len(self.digits) == len(other.digits)
            for pair in zip(self.digits, other.digits):
                sd, od = pair
                if sd < od:
                    ans = True
                    break
                if sd > od:
                    ans = False
                    break
        elif selflen < otherlen:
            ans =  True
        else:
            ans = False
        if self.positive:
            return ans
        else:
            return not ans


    def __le__(self, other):
        return self == other or self < other


    def __abs__(self):
        return self.abs()


    def __neg__(self):
        return self.negate()


    def __add__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        if other == BigInt(0):
            return self
        elif self == BigInt(0):
            return other
        if self.positive == other.positive:
            sdigits = self.digits
            odigits = other.digits
            # int dependency for now. maybe make your own zip function which replaces empty with 0
            # pad with preceeding 0s to get equal length
            carry = 0
            reverse_digits = []
            for pair in myzip(self.digits[::-1], other.digits[::-1]):
                sd, od = pair
                sum = sd + od + carry
                reverse_digits.append(sum % 10)
                carry = sum // 10
            reverse_digits.append(carry)
            return BigInt(reverse_digits[::-1], self.positive)
        else:
            if self.positive:# other is negative
                return self - abs(other)
            else:# self is negative and other is positive
                return other - abs(self)

        '''
        elif other.positive:
            counter = BigInt(0)
            ans = self
            while counter != other:
                ans = ans.add1()
                counter = counter.add1()
            return ans
        else:
            counter = BigInt(0)
            # print('s, o', self, other)
            ans = self
            while counter != other:
                # print('c, o', counter, other)
                ans = ans.sub1()
                counter = counter.sub1()
            return ans
            '''


    def __sub__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        if other == BigInt(0):
            return self
        elif self == BigInt(0):
            return -other
        elif self.positive != other.positive:
            return self + -other
        else:
            # signs same
            max = None
            min = None
            negate = None
            if abs(self) > abs(other):
                max = abs(self)
                min = abs(other)
                negate = False
            else:
                max = abs(other)
                min = abs(self)
                negate = True
            decrement = 0
            reverse_digits = []
            # hand-written subtraction
            for pair in myzip(max.digits[::-1], min.digits[::-1]):
                maxd, mind = pair
                if maxd - decrement >= mind:
                    reverse_digits.append(maxd-decrement-mind)
                    decrement = 0
                else:
                    reverse_digits.append(10+maxd-decrement-mind)
                    decrement = 1
            if negate:
                return BigInt(reverse_digits[::-1], not self.positive)
            else:
                return BigInt(reverse_digits[::-1], self.positive)


    def __mul__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        if self == BigInt(1):
            return other
        elif other == BigInt(1):
            return self
        if self == BigInt(0) or other == BigInt(0):
            return BigInt(0)

        ans = BigInt(0)
        counter = BigInt(0)
        for digit in other.digits[::-1]:
            prod = self.mul_digit(digit)
            prod = prod.x10(counter)
            ans = ans + prod
            counter = counter.add1()
        if self.positive == other.positive:
            return ans
        else:
            ans.positive = False
            return ans


    def __pow__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        if not other.positive:
            raise ValueError("exponent must not be negative: {0}".format(other))
        if other == BigInt(0):
            return BigInt(1)
        elif self == BigInt(0) or self == BigInt(1):
            return self
        elif self == BigInt(10):
            return self.x10(other)
        elif other <= BigInt(10):
            ans = self
            counter = BigInt(1)
            while counter < other:
                ans = ans * self
                counter = counter.add1()
            return ans
        else:
            ans = BigInt(1)
            pow = self
            for digit in other.digits[::-1]:
                ans = ans * (pow ** BigInt(digit))
                pow = pow ** BigInt(10)
            return ans


    def __floordiv__(self, other):
        quotient, remainder = self.divide(other)
        return quotient


    def __mod__(self, other):
        quotient, remainder = self.divide(other)
        return remainder


    def mul_digit(self, digit):
        '''
        multiplies self by the given digit, treats all as positive
        digit int
        return BigInt
        '''
        carry = 0
        reverse_digits = [] # answer digits in reverse order
        for sd in self.digits[::-1]:
            prod = digit * sd + carry
            assert prod < 100
            reverse_digits.append(prod % 10)
            carry = prod // 10
        reverse_digits.append(carry)
        return BigInt(reverse_digits[::-1], True)


    def divide(self, other):
        '''
        returns (quotient, remainder)
        a bit wacky with negatives
        '''
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        if other == BigInt(0):
            raise ZeroDivisionError('division by zero')
        if other == BigInt(1):
            return self, BigInt(0)
        if other == BigInt(-1):
            return -self, BigInt(0)
        if self == BigInt(0):
            return BigInt(0), BigInt(0)
        abss = abs(self)
        abso = abs(other)
        n = abso
        counter = BigInt(0)
        while n <= abss:
            n += abso
            counter = counter.add1()
        quotient = counter
        remainder = abss - abso * quotient
        if self.positive == other.positive:
            if remainder != BigInt(0):
                remainder.positive = other.positive
        else:
            quotient.positive = False
            if remainder != BigInt(0):
                remainder = abs(other) - remainder
                remainder.positive = other.positive
        return quotient, remainder


    def x10(self, n=None):
        '''
        returns the number multiplied by 10 n times
        '''
        if n is None:
            n = BigInt(1)
        if type(self) is not type(n):
            raise TypeError('{0} {1}'.format(type(self), type(n)))
        digits = []
        counter = BigInt(0)
        while counter < n:
            digits.append(0)
            counter = counter.add1()
        digits = tuple(digits)
        return BigInt(self.digits + digits, self.positive)


    def abs(self):
        '''
        absolute value
        '''
        return BigInt(self.digits, True)


    def negate(self):
        return BigInt(self.digits, not self.positive)


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
                digits = self.digits[:-1] + (self.digits[-1] + 1,)
                return BigInt(digits, True)
            else:
                if self == BigInt(9):
                    return BigInt(10)
                else:
                    return BigInt(self.digits[:-1], True).add1().x10()
        else:
            ans = abs(self)
            ans = ans.sub1()
            if ans != BigInt(0):
                ans.positive = False
            return ans


    def sub1(self):
        if self.positive:
            if self.digits[-1] > 0:
                digits = self.digits[:-1] + (self.digits[-1] - 1,)
                return BigInt(digits, True)
            else:
                if self == BigInt(0):
                    return BigInt(-1)
                else:
                    digits = self.digits[:-1]
                    ans = BigInt(digits, True)
                    ans = ans.sub1()
                    ans = ans.x10()
                    digits = ans.digits[:-1] + (9,)
                    return BigInt(digits, True)
        else:
            # we're negative
            ans = abs(self)
            ans = ans.add1()
            ans.positive = False
            return ans


    def gcd(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        a = max(abs(self), abs(other))
        b = min(abs(self), abs(other))
        while b != BigInt(0):
            temp = a % b
            a = b
            b = temp
        return a
