from django.contrib import admin
from .models import Event, EventTag, EventLink

# Register your models here.
admin.site.register(Event)
admin.site.register(EventTag)
admin.site.register(EventLink)
