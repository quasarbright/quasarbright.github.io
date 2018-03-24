from django.db import models
from django.utils import timezone
from .reference import TYPE_CHOICES

# Create your models here.
class Transaction(models.Model):
    date = models.DateField()
    amount = models.FloatField()
    type = models.CharField(
        max_length=2,
        choices=TYPE_CHOICES,
        default='CA'
    )
    description = models.CharField(max_length=255)
    def __str__(self):
        return '\"{0}\" for ${1}'.format(self.description,self.amount)
# class BalanceEntry(models.Model):
#     transaction = models.ForeignKey(Transaction,on_delete=models.CASCADE)
#     balance =
