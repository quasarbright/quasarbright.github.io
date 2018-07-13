from django.shortcuts import render

# Create your views here.
from django.http import HttpResponse

import os
here = os.path.dirname(os.path.abspath(__file__))
def welcome(request):
    with open(os.path.join(here,'index.html'),'r') as html:
        return HttpResponse(html.read())
