# Generated by Django 2.0.3 on 2018-06-07 12:49

from django.db import migrations, models
import django.db.models.deletion
import memoryApp.models


class Migration(migrations.Migration):

    initial = True

    dependencies = [
    ]

    operations = [
        migrations.CreateModel(
            name='Event',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(db_index=True, max_length=511)),
                ('date', models.DateField(blank=True)),
                ('description', models.TextField(blank=True)),
                ('importance', models.FloatField(help_text='Should be a number from 0 to 10 (inclusive), not necessarily an integer. 10 is extremely important.', validators=[memoryApp.models.validateImportance])),
                ('location', models.CharField(blank=True, help_text="Should be the address or name of the place where the event occurred. Don't worry too much about formatting.", max_length=511)),
            ],
        ),
        migrations.CreateModel(
            name='EventLink',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('urlText', models.URLField(db_column='url', db_index=True, max_length=511)),
                ('type', models.CharField(choices=[('IM', 'image link'), ('OT', 'other link')], default='OT', max_length=2)),
                ('event', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to='memoryApp.Event')),
            ],
            options={
                'abstract': False,
            },
        ),
        migrations.CreateModel(
            name='EventTag',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(db_index=True, max_length=511, validators=[memoryApp.models.validateLowercase])),
                ('event', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to='memoryApp.Event')),
            ],
            options={
                'abstract': False,
            },
        ),
    ]
