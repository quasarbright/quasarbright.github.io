from django.shortcuts import render
from django.http import HttpResponse
from .forms import TransactionForm
from .models import Transaction
from django.utils import timezone
from .reference import TYPE_CHOICES
# Create your views here.
def index(request):
    return HttpResponse('this is the finances app')
def insert(request):
    return render(request, 'finances/insert.html',context={'form':TransactionForm})
def insertResult(request):
    amount = request.POST['amount']
    type = request.POST['type']
    for pair in TYPE_CHOICES:
        if pair[0] == type:
            type = pair[1]
            break
    description = request.POST['description']
    context = {'amount':amount,'type':type,'description':description}
    Transaction.objects.create(date=timezone.now(),amount=amount,type=type,description=description)
    return render(request,'finances/insertResult.html',context=context)
