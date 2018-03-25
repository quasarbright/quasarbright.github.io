from django.urls import path

from . import views

app_name = 'finances'
urlpatterns = [
    path('', views.index, name='index'),
    path('transactions/view/<int:pk>',
         views.TransactionView.as_view(), name="view_transaction"),
    path('transactions/view', views.TransactionsView.as_view(), name="view"),
    path('transactions/insert', views.TransactionFormView.as_view(), name='insert'),
    path('transactions/insertResult', views.insertResult, name='insertResult'),
]
