# -*- encoding: utf-8 -*-

from behave import *

import requests

from internal import siren

@given(u'a {property} of {value},')
def step_impl(context, property, value):
    setattr(context, property, value)

@given(u'an {property} of {value},')
def step_impl(context, property, value):
    setattr(context, property, value)

@when(u'I create a character')
def step_impl(context):
    assert context.discipline is not None, u'missing: Given a discipline of …'
    assert context.race is not None, u'missing: Given a race of …'

    r = requests.get('http://localhost:45753/earthdawn/4e/characters')
    
    assert r.headers['content-type'] == 'application/vnd.siren+json', 'Incorrect Content-Type'

    e = siren.Entity(r.json())
    r = e.create_character(discipline = context.discipline, race = context.race)

    assert r.status_code == 201
    assert r.headers['content-type'] == 'application/vnd.siren+json', 'Incorrect Content-Type'

    context.character = siren.Entity(r.json())

@then(u'I should receive a character')
def step_impl(context):
    assert 'Character' in context.character.class_

@then(u'the {object}\'s {property} is {value},')
def step_impl(context, object, property, value):
    assert getattr(context, object).properties[property] == value

@then(u'the available actions should be')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then the available actions should be')
