from django.http import HttpResponse, HttpResponseRedirect, HttpResponseForbidden
from django.shortcuts import render
from django.urls import reverse
from django.utils import timezone
from django.views import generic
from django.contrib.auth.decorators import login_required
from django.contrib.auth.mixins import LoginRequiredMixin
# from .forms import TransactionForm
from .models import Transaction
from .reference import TYPE_CHOICES

# Create your views here.


# view a single transaction
class TransactionView(LoginRequiredMixin, generic.detail.DetailView):
    model = Transaction
    context_object_name = 'transaction'
    template_name = 'finances/viewTransaction.html'
    def dispatch(self,request,*args,**kwargs):
        transaction_user = Transaction.objects.get(pk=kwargs['pk']).user
        if request.user != transaction_user:
            return HttpResponseForbidden('FORBIDDEN: This is not a transaction made by you. You cannot view it')
        else:
            return super().dispatch(request,*args,**kwargs)


# view all of the transactions
class TransactionsView(LoginRequiredMixin, generic.ListView):
    model = Transaction
    context_object_name = 'transactions'
    template_name = 'finances/view.html'
    def get_queryset(self):
        return Transaction.objects.filter(user=self.request.user)


@login_required
def index(request):
    return render(request, 'finances/index.html')


# class TransactionFormView(LoginRequiredMixin, generic.edit.FormView):
#     template_name = 'finances/insert.html'
#     context_object_name = 'form'
#     form_class = TransactionForm
class TransactionFormView(LoginRequiredMixin, generic.edit.CreateView):
    model = Transaction
    fields = ['amount', 'type', 'description']
    template_name = 'finances/insert.html'


@login_required
def insertResult(request):
    amount = request.POST['amount']
    type = request.POST['type']
    user = request.user
    context_type = ''
    for pair in TYPE_CHOICES:
        if pair[0] == type:
            context_type = pair[1]
            break
    description = request.POST['description']
    context = {'amount': amount, 'type': context_type,
               'description': description}
    transaction = Transaction.objects.create(
        date=timezone.now(), amount=amount, type=type, description=description, user=user)
    pk = transaction.id
    return HttpResponseRedirect(reverse('finances:view_transaction', args=[pk]))
