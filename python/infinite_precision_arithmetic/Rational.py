from BigInt import *
class Rational:
    '''
    constructs a fraction, doesn't simplify
    immutable
    '''
    def __init__(self, numerator, denominator=BigInt(1)):
        if type(numerator) is not type(BigInt(0)) or type(denominator) is not type(BigInt(0)):
            raise TypeError('{0}, {1}'.format(type(numerator), type(denominator)))
        if denominator == BigInt(0):
            raise ZeroDivisionError('Rational({0}, {1})'.format(numerator, denominator))
        else:
            self.numerator = abs(numerator)
            self.denominator = abs(denominator)
            self.positive = numerator.positive == denominator.positive
            if numerator == BigInt(0):
                self.positive = True


    def __str__(self):
        if self.positive:
            return '{0} / {1}'.format(self.numerator, self.denominator)
        else:
            return '-{0} / {1}'.format(self.numerator, self.denominator)


    def __repr__(self):
        if self.positive:
            return 'Rational({0}, {1})'.format(self.numerator, self.denominator)
        else:
            return 'Rational(-{0}, {1})'.format(self.numerator, self.denominator)


    def __eq__(self, other):
        if type(self) is not type(other):
            return False
        if self.numerator == BigInt(0):
            return other.numerator == BigInt(0)
        else:
            # check signs and cross multiply
            return self.positive == other.positive and self.numerator * other.denominator == self.denominator * other.numerator


    def __lt__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        if self.positive and not other.positive:
            return False
        if other.positive and not self.positive:
            return True
        if self == other:
            return False
        # same sign and unequal
        s, o = self.lcd(other)
        if self.positive:
            return s.numerator < o.numerator
        else:
            return s.numerator > o.numerator


    def __le__(self, other):
        return self < other or self == other


    def __hash__(self):
        s = self.simplify()
        return hash((s.numerator, s.denominator, s.positive))


    def __abs__(self):
        ans = Rational(self.numerator, self.denominator)
        ans.positive = True
        return ans


    def __neg__(self):
        if self.numerator == BigInt(0):
            return self
        else:
            ans = Rational(self.numerator, self.denominator)
            ans.positive = not self.positive
            return ans


    def __add__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        if self.numerator == BigInt(0):
            return other
        elif other.numerator == BigInt(0):
            return self
        elif self.positive == other.positive:
            s, o = self.lcd(other)
            numerator = s.numerator + o.numerator
            denominator = s.denominator
            ans = Rational(numerator, denominator)
            ans.positive = s.positive
            return ans
        else:
            return self - -other


    def __sub__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        if self.numerator == BigInt(0):
            return -other
        elif other.numerator == BigInt(0):
            return self
        elif self.positive == other.positive:
            s, o = self.lcd(other)
            numerator = s.numerator - o.numerator
            denominator = s.denominator
            ans = Rational(numerator, denominator)
            if numerator.positive:
                ans.positive = self.positive
            else:
                ans.positive = not self.positive
            return ans
        else:
            return self + -other


    def __mul__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        ans = Rational(self.numerator * other.numerator, self.denominator * other.denominator)
        ans.positive = self.positive == other.positive
        if ans.numerator == BigInt(0):
            ans.positive = True
        return ans


    def __truediv__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        return self * other.inverse()


    def __pow__(self, other):
        if type(other) is not type(BigInt(1)):
            raise TypeError('exponent must be a BigInt, got {1}'.format(type(other)))
        if not other.positive:
            return self.inverse() ** abs(other) # may divide by zero
        if other == BigInt(0) or self == Rational(BigInt(1)):
            return Rational(BigInt(1))
        elif self.numerator == BigInt(0):
            return Rational(BigInt(0))
        elif other <= BigInt(10):
            ans = self
            counter = BigInt(1)
            while counter < other:
                ans = ans * self
                counter = counter.add1()
            return ans
        else:
            ans = Rational(BigInt(1))
            pow = self
            for digit in other.digits[::-1]:
                ans = ans * (pow ** BigInt(digit))
                pow = pow ** BigInt(10)
            return ans


    def simplify(self):
        if self.numerator == BigInt(0):
            return Rational(BigInt(0), BigInt(1))
        else:
            gcd = self.numerator.gcd(self.denominator)
            numerator = self.numerator // gcd
            denominator = self.denominator // gcd
            ans =  Rational(numerator, denominator)
            ans.positive = self.positive
            return ans


    def lcd(self, other):
        '''
        returns two rationals with the same, lowest common denominator (simplifies first)
        in order (self, other)
        maintains signs and numerical value
        1/2.lcd(-3/5) => (5/10, -6/10)
        '''
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        s = self.simplify()
        o = other.simplify()
        denominator = s.denominator.lcm(o.denominator)
        sn = s.numerator * (denominator // s.denominator)
        on = o.numerator * (denominator // o.denominator)
        s = Rational(sn, denominator)
        s.positive = self.positive
        o = Rational(on, denominator)
        o.positive = other.positive
        return s, o


    def inverse(self):
        ans = Rational(self.denominator, self.numerator)
        ans.positive = self.positive
        return ans
