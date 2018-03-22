from django.test import TestCase

# Create your tests here.
import datetime
from django.utils import timezone
from django.urls import reverse
from .models import Question, Choice

def create_question(question_text,days):
    '''
    create question with given question_text and in given days (as timedelta)
    days + for future - for past
    '''
    time = timezone.now() + datetime.timedelta(days=days)
    return Question.objects.create(question_text=question_text,pub_date=time)
class QuestionIndexViewTests(TestCase):
    def test_no_questions(self):
        '''
        if no questions, appropriate error displayed
        '''
        response = self.client.get(reverse('polls:index'))
        self.assertEqual(response.status_code,200)
        self.assertContains(response,'No polls are available.')
        self.assertQuerysetEqual(response.context['latest_question_list'],[])
    def test_past_question(self):
        '''
        questions with past pub_date displayed
        '''
        create_question('past question.',days=-30)
        response = self.client.get(reverse('polls:index'))
        self.assertQuerysetEqual(response.context['latest_question_list'],['<Question: past question.>'])
    def test_future_question(self):
        '''
        questions with date in future aren't displayed
        '''
        create_question(question_text="Future question.", days=30)
        response = self.client.get(reverse('polls:index'))
        self.assertContains(response, "No polls are available.")
        self.assertQuerysetEqual(response.context['latest_question_list'], [])
    def test_future_question_and_past_question(self):
        """
        Even if both past and future questions exist, only past questions
        are displayed.
        """
        create_question(question_text="Past question.", days=-30)
        create_question(question_text="Future question.", days=30)
        response = self.client.get(reverse('polls:index'))
        self.assertQuerysetEqual(
            response.context['latest_question_list'],
            ['<Question: Past question.>']
        )
    def test_two_past_questions(self):
        """
        The questions index page may display multiple questions.
        """
        create_question(question_text="Past question 1.", days=-30)
        create_question(question_text="Past question 2.", days=-5)
        response = self.client.get(reverse('polls:index'))
        self.assertQuerysetEqual(
            response.context['latest_question_list'],
            ['<Question: Past question 2.>', '<Question: Past question 1.>']
        )
def QuestionDetailViewTests(TestCase):
    def test_future_question(self):
        """
        The detail view of a question with a pub_date in the future
        returns a 404 not found.
        """
        future_question = create_question(question_text='Future question.', days=5)
        url = reverse('polls:detail', args=(future_question.id,))
        response = self.client.get(url)
        self.assertEqual(response.status_code, 404)
    def test_past_question(self):
        """
        The detail view of a question with a pub_date in the past
        displays the question's text.
        """
        past_question = create_question(question_text='Past Question.', days=-5)
        url = reverse('polls:detail', args=(past_question.id,))
        response = self.client.get(url)
        self.assertContains(response, past_question.question_text)
class QuestionModelTests(TestCase):
    def test_pub_recent_with_future_question(self):
        """
        pub_recent() returns False for questions whose pub_date
        is in the future.
        """
        time = timezone.now() + datetime.timedelta(days=30)
        future_question = Question(pub_date=time)
        self.assertIs(future_question.pub_recent(),False)
    def test_pub_recent_with_old_question(self):
        """
        pub_recent() returns False for questions whose pub_date
        is more than 1 day in the past.
        """
        def test_time_delta(**timedelta):
            now = timezone.now()
            time = now - datetime.timedelta(**timedelta)
            future_question = Question(pub_date=time)
            self.assertIs(future_question.pub_recent(),False)
        test_time_delta(days=1,hours=1)
        test_time_delta(days=2)
        test_time_delta(days=100)
    def test_pub_recent_with_recent_question(self):
        """
        pub_recent() returns True for questions whose pub_date
        is less than 1 day in the past.
        """
        def test_time_delta(**timedelta):
            now = timezone.now()
            time = now - datetime.timedelta(**timedelta)
            future_question = Question(pub_date=time)
            self.assertIs(future_question.pub_recent(),True)
        test_time_delta(hours=1)
        test_time_delta(hours=23)
    def test_pub_recent_with_edge_cases(self):
        """
        pub_recent() returns True for questions whose pub_date
        is exactly 1 or 0 days in the past.
        """
        def test_time_delta(**timedelta):
            now = timezone.now()
            time = now - datetime.timedelta(**timedelta)
            future_question = Question(pub_date=time)
            self.assertIs(future_question.pub_recent(),True)
        test_time_delta(seconds=0)
        test_time_delta(days=1)
