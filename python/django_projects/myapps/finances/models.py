from django.db import models
from django.utils import timezone
from .reference import TYPE_CHOICES
from django.contrib.auth.models import User
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
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    def __str__(self):
        return '\"{0}\" for ${1} by {2}'.format(self.description, self.amount, self.user)
# class BalanceEntry(models.Model):
#     transaction = models.ForeignKey(Transaction,on_delete=models.CASCADE)
#     balance =
