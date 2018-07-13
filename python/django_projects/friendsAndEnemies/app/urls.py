from django.urls import path,include
from . import views
app_name = 'app'
urlpatterns = [
    path('',views.index,name='index'),
    path('get_input/',views.get_input,name='get_input')
]
