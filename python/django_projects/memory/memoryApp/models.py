from django.db import models
from django.core.exceptions import ValidationError
from django.utils.translation import gettext_lazy as _
import django.utils.timezone


#validators
def validateImportance(value):
    if value < 0 or value > 10:
        raise ValidationError(
            _('%(value)s is not between 0 and 10'),
            params={'value': value},
        )


def validateLowercase(value):
    if value.lower() != value:
        raise ValidationError(
            _('%(value)s is not all lowercase'),
            params={'value': value},
        )


# models
class Event(models.Model):
    name = models.CharField(max_length=511, default='placeholder', db_index=True)
    date = models.DateField(blank=True)
    description = models.TextField(blank=True)
    IMPORTANCE_HELP_TEXT = 'Should be a number from 0 to 10 (inclusive), not necessarily an integer. 10 is extremely important.'
    importance = models.FloatField(help_text=IMPORTANCE_HELP_TEXT, validators=[validateImportance])
    LOCATION_HELP_TEXT = 'Should be the address or name of the place where the event occurred. Don\'t worry too much about formatting.'
    location = models.CharField(max_length=511, blank=True, help_text=LOCATION_HELP_TEXT)

    def __str__(self):
        return self.name
    # pass



class EventTag(models.Model):
    event = models.ForeignKey(Event, on_delete=models.CASCADE)
    name = models.CharField(max_length=511, db_index=True, validators=[validateLowercase])

    def __str__(self):
        return self.name
    # pass


class EventLink(models.Model):
    event = models.ForeignKey(Event, on_delete=models.CASCADE)
    urlText = models.URLField(max_length=511, db_column='url', db_index=True)
    IMAGE_LINK = 'IM'
    OTHER_LINK = 'OT'
    TYPE_CHOICES = (
        (IMAGE_LINK, 'image link'),
        (OTHER_LINK, 'other link')
    )
    type = models.CharField(max_length=2, choices=TYPE_CHOICES, default=OTHER_LINK)

    def __str__(self):
        return self.urlText
    # pass
