@wip @dungeon-studio @character
Feature: Character Creation

  Background: Services are Running
    Given dungeon-studio is running

  Scanario Outline: Minimal Successful Character Creation
    Given a discipline URL of <discipline>,
    And a race URL of <race>,
    When I create a character
    Then I should receive the character
    And the available actions should be:
      | TODO

    Examples: Character Attributes
      | discipline                                          | race                                          |
      | http://localhost:45753/earthdawn/4e/disciplines/... | http://localhost:45753/earthdawn/4e/races/... |
