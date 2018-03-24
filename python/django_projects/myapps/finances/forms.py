from django import forms
from .reference import TYPE_CHOICES

class TransactionForm(forms.Form):
    # date = models.DateField()
    amount = forms.FloatField()
    type = forms.ChoiceField(
        choices=TYPE_CHOICES,
    )
    description = forms.CharField(max_length=255)
