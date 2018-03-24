from django.urls import path

from . import views

app_name = 'finances'
urlpatterns = [
    path('', views.index, name='index'),
    path('transactions/view',views.TransactionView.as_view(),name="view"),
    path('transactions/insert',views.insert,name='insert'),
    path('transactions/insertResult',views.insertResult,name='insertResult'),
]
