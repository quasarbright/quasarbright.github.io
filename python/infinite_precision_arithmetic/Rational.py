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
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        if self.numerator == BigInt(0):
            return other.numerator == BigInt(0)
        else:
            # check signs and cross multiply
            return self.positive == other.positive and self.numerator * other.denominator == self.denominator * other.numerator


    def __lt__(self, other):
        if self.positive and not other.positive:
            return False
        if other.positive and not self.positive:
            return True
        if self == other:
            return False
        # same sign and unequal
        s, o = self.common_denominator(other)
        if self.positive:
            return s.numerator < o.numerator
        else:
            return s.numerator > o.numerator


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


    def common_denominator(self, other):
        '''
        returns two rationals with the same denominator (simplifies first)
        in order (self, other)
        maintains signs
        '''
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

# left off about to implement hash, le, abs, inv, and arithmetic
