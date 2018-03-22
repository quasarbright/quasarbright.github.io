from django.shortcuts import render, get_object_or_404
from .models import Choice, Question
from django.http import HttpResponse, Http404, HttpResponseRedirect
from django.template import loader
from django.urls import reverse
from django.db.models import F
from django.views import generic
from django.utils import timezone

class IndexView(generic.ListView):
    template_name = 'polls/index.html'
    context_object_name = 'latest_question_list'#for reference in template file

    def get_queryset(self):
        """Return the last five published questions.
        Not including future ones"""
        return Question.objects.filter(pub_date__lte=timezone.now()).order_by('-pub_date')[:5]

class DetailView(generic.DetailView):
    model = Question
    template_name = 'polls/details.html'
    def get_queryset(self):
        '''
        exclude unpublished questions
        '''
        return Question.objects.filter(pub_date__lte=timezone.now())

class ResultsView(generic.DetailView):
    model = Question
    template_name = 'polls/results.html'

def vote(request, question_id):
    question = get_object_or_404(Question, pk=question_id)
    try:
        selected_choice = question.choice_set.get(pk=request.POST['choice'])
        #so request.POST is a dict with the request details. see templates/polls/details.html
    except (KeyError, Choice.DoesNotExist):
        return render(request,'polls/detail.html',{
        'question':question,
        'error_message':'you didn\'t select a choice'
        })
    else:
        #update db
        selected_choice.votes = F('votes') + 1
        #use F to move the incrementing to the database where it is more robust. See "race conditions"
        selected_choice.save()
        #always return an HttpResponseRedirect after successfully dealing with POST
        #this prevents double posting if user hits back button
        #this redirects to results
        return HttpResponseRedirect(reverse('polls:results', args=(question.id,)))
        #reverse gets the url from the "name"
        '''
        note: vote is the action of a form on details. It is not a page that appears. In fact, after vote is executed,
        the user is redirected to a results page. This would usually be done by php. Now you can do forms like a real person.
        request.POST values are all strings
        '''

######################## old ###################################################
"""
def index(request):
    #display 5 latest question (texts) sorted by pub date
    latest_question_list = Question.objects.order_by('-pub_date')[:5]
    context = {
        "latest_question_list":latest_question_list
    }
    return render(request,'polls/index.html',context)

def detail(request, question_id):
    # try:
    #     question = Question.objects.get(pk=question_id)
    # except Question.DoesNotExist:
    #     raise Http404('question does not exist')
    question = get_object_or_404(Question, pk=question_id)
    return render(request, 'polls/details.html',{'question':question})

def results(request, question_id):
    question = get_object_or_404(Question,pk=question_id)
    return render(request,'polls/results.html',{'question':question})

"""
