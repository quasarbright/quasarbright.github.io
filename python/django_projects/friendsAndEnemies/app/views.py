from django.shortcuts import render
from django.http import HttpResponse, HttpResponseRedirect
from .process import generate_teams
# Create your views here.
def index(request):
    return render(request,'app/input.html')
def get_input(request):
    obj = request.POST
    players, friends, enemies = obj['players'],obj['friends'],obj['enemies']
    output = generate_teams(players,enemies,friends)
    return HttpResponse(output)
