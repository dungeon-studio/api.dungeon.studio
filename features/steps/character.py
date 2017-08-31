# -*- encoding: utf-8 -*-

from behave import *

from collections import namedtuple

CharacterRequest = namedtuple(u'CharacterRequest', [u'discipline', u'race'])


@given(u'a discipline of {discipline},')
def step_impl(context, discipline):
    if 'character_request' not in context:
        context.character_request = CharacterRequest(None, None)

    context.character_request = context.character_request._replace(discipline = discipline)

@given(u'a race of {race},')
def step_impl(context, race):
    if 'character_request' not in context:
        context.character_request = CharacterRequest(None, None)

    context.character_request = context.character_request._replace(race = race)

@when(u'I create a character')
def step_impl(context):
    assert 'character_request' in context, u'missing: Given a discipline of …'

    assert context.character_request.discipline is not None, u'missing: Given a discipline of …'
    assert context.character_request.race is not None, u'missing: Given a race of …'

    assert False, u'TODO create a character'

@then(u'I should receive a character')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then I should receive a character')

@then(u'the character\'s discipline is wizard,')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then the character\'s discipline is wizard,')

@then(u'the character\'s race is dwarf,')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then the character\'s race is dwarf,')

@then(u'the available actions should be')
def step_impl(context):
    raise NotImplementedError(u'STEP: Then the available actions should be')
