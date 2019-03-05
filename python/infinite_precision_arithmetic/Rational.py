from BigInt import *
class Rational:
    '''
    constructs a fraction in simplest form
    '''
    def __init__(self, numerator, denominator=BigInt(1)):
        if type(numerator) is not type(BigInt(0)) or type(denominator) is not type(BigInt(0)):
            raise TypeError('{0}, {1}'.format(type(numerator), type(denominator)))
        if denominator == BigInt(0):
            raise ZeroDivisionError('Rational({0}, {1})'.format(numerator, denominator))
        if numerator == BigInt(0):
            self.numerator = numerator
            self.denominator = BigInt(1)
            self.positive = True
        else:
            self.numerator = abs(numerator)
            self.denominator = abs(denominator)
            gcd = self.numerator.gcd(self.denominator)
            self.numerator = self.numerator // gcd
            self.denominator = self.denominator // gcd
            self.positive = numerator.positive == denominator.positive

    def __str__(self):
        if self.positive:
            return 'Rational({0} / {1})'.format(''.join(self.numerator.digits), ''.join(self.denominator.digits))
        else:
            return 'Rational(-{0} / {1})'.format(''.join(self.numerator.digits), ''.join(self.denominator.digits))


    def __eq__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        return self.numerator == other.numerator and self.denominator == other.denominator and self.positive == other.positive
