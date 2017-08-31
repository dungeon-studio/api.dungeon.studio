@wip
Feature: Character Creation

  @dungeon-studio
  Scenario: Minimal Successful Character Creation
    Given a discipline of wizard,
      And a race of dwarf,
     When I create a character
     Then I should receive a character
      And the character's discipline is wizard,
      And the character's race is dwarf,
      And the available actions should be:
            | name              | title               |
            | change-race       | "Change Race"       |
            | change-discipline | "Change Discipline" |
            | change-attributes | "Change Attributes" |
