from django.contrib import admin

from .models import Transaction

# Register your models here.

class TransactionAdmin(admin.ModelAdmin):
    list_display = ('description','amount','type','date','user')
    list_filter = ['date','amount','type']
    search_fields = ['description']
admin.site.register(Transaction,TransactionAdmin)
