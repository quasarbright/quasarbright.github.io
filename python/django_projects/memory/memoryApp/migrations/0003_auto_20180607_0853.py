# Generated by Django 2.0.3 on 2018-06-07 12:53

from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ('memoryApp', '0002_remove_eventtag_event'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='event',
            name='date',
        ),
        migrations.RemoveField(
            model_name='event',
            name='description',
        ),
        migrations.RemoveField(
            model_name='event',
            name='importance',
        ),
        migrations.RemoveField(
            model_name='event',
            name='location',
        ),
        migrations.RemoveField(
            model_name='event',
            name='name',
        ),
        migrations.RemoveField(
            model_name='eventlink',
            name='event',
        ),
    ]