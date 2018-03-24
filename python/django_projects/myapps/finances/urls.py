from django.urls import path

from . import views

app_name = 'finances'
urlpatterns = [
    path('', views.index, name='index'),
    path('insert',views.insert,name='insert'),
    path('insertResult',views.insertResult,name='insertResult'),
]
