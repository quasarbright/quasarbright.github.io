from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render
from django.urls import reverse
from django.utils import timezone
from django.views import generic
from django.contrib.auth.decorators import login_required
from django.contrib.auth.mixins import LoginRequiredMixin
from .forms import TransactionForm
from .models import Transaction
from .reference import TYPE_CHOICES

# Create your views here.


class TransactionView(LoginRequiredMixin, generic.detail.DetailView):
    model = Transaction
    context_object_name = 'transaction'
    template_name = 'finances/viewTransaction.html'


class TransactionsView(LoginRequiredMixin, generic.ListView):
    model = Transaction
    context_object_name = 'transactions'
    template_name = 'finances/view.html'


@login_required
def index(request):
    return render(request, 'finances/index.html')


class TransactionFormView(LoginRequiredMixin, generic.edit.FormView):
    template_name = 'finances/insert.html'
    context_object_name = 'form'
    form_class = TransactionForm


@login_required
def insertResult(request):
    amount = request.POST['amount']
    type = request.POST['type']
    context_type = ''
    for pair in TYPE_CHOICES:
        if pair[0] == type:
            context_type = pair[1]
            break
    description = request.POST['description']
    context = {'amount': amount, 'type': context_type,
               'description': description}
    transaction = Transaction.objects.create(
        date=timezone.now(), amount=amount, type=type, description=description)
    pk = transaction.id
    return HttpResponseRedirect(reverse('finances:view_transaction', args=[pk]))
