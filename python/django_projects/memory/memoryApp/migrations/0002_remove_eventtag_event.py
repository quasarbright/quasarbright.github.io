# Generated by Django 2.0.3 on 2018-06-07 12:52

from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ('memoryApp', '0001_initial'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='eventtag',
            name='event',
        ),
    ]