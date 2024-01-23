## Shortlist
- [ ] Abbot 
    - collect resources menu
    - tax richest menu
    - fixup magician, change format to not require "Select<>"

- [ ] Custom extractor for form url encoded data:
    https://github.com/tokio-rs/axum/blob/main/examples/customize-extractor-error/src/custom_extractor.rs

- [ ] Rework warlord menu as a modal
- [ ] Blackmailer
    - Bribe menu

- [ ] Spy
    - Spy menu

- [ ] Diplomat 
- [ ] Marshal 

## Ideas
- [ ] Keyboard based warrant assignment
- [ ] Multi room support
- [ ] Make log format and tenses consistent
- [ ] Rework menus to use drag and drop.
- [ ] Rework menus as modals.
- [ ] Detect when building is impossible.
- [ ] Make it possible to save a set of roles to use, for config.
- [ ] Restore game from sqlite

## Four+ player support
- [ ] Need to show faceup discard.

## Playtest 1 feedback
- [ ] Notification bell for start of turn.
    - [x] audio tag, with ws event listener
    - [ ] Server needs to send an html element with data-ring-bell, but only once when the turn begins
    - [ ] Server shuffles audio files to send.
- [ ] role and district description should have dedicated info icon for bringing up their tooltips.
- [ ] Warlord menu should make it obvious the cost to destroy from the great wallled city is higher.

## Playtest 2
- [x] Role config menu is clipped on smaller screens
- [x] flex-wrap my-roles. Basically, they should stack vertically if I narrow my window
- [x] museum auto tooltip hides the artifacts
- [x] Ending game can crashes the game
- [x] Limit width and apply text-wrap to right sidebar usernames. Long usernames shouldn't distort the whole page.
- [x] "Players' turn" could have more context. show role + name. Put word draft if they are drafting.
- [x] Roles left sidebar, italics say "hover for details". We should restore the tooltip or remove that phrase.
- [ ] Emoji warrant on the build button so you know the dangers
- [ ] Destroy cost should be disaplyed prominently on the warlord menu. 
- [ ] dismissible popup when your district is detroyed, just shows inline a picture of the destroyed district.
- [ ] Allow names with some punctuation. Will need to url encode names in city request

- [ ] Logan couldn't see his city districts without scrolling.
- [ ] Logan couldn't easily see enemy districts without scrolling. Warlord menu
- [ ] William found the button highlighting behavior in the config menu to be confusing. Didn't realize I was going for tab like interface. How can I make it more tab like?
- [ ] Save the dragged position of districts in a city.

## Feature Ideas
- Optional Timers
    - Basic
    - Chess Clock (Pokemon showdown)
- kick bots / spammers
- auth?
- multi room
- spectator support
- [ ] Resizable windows, dimensions are saved per user and kept persistently between sessions
- Custom Card editor
- Custom Role editor

## Custom Roles
- [ ] Nerfed Asssassin: Kill the role and all its abilities, but the player can still take normal actions. Gather, build, any district abilities

## Cleo's easter eggs
- [x] Dragging the dragon out of his section play's Mr. Brightside. Putting him back pauses it.
    - [ ] Need to send ws updates as oob swaps so as to not disrupt the video player dom state.

## Tech Debt
### Deserialization
Json encoding with htmx still doesn't handle arrays well. I am using a kludge of the Select<> type to handle this

Form url encoding would be better, because no htmx extension required and more web standardsy. Problem is Axum doesn't extract arrays or duplicate form fields. I'm also finding out axum doesn't deserialize strings to numbers. Pretty frustrating. I can write a custom extra for all this.
Deserializing CityDistrictTarget requires custom handlers, and leaves the struct without Serialiable/Deserializable instances.
Also neither handles parsing strings to numbers

- I can use `#[serde_as(as = "DisplayFromStr")]` to handle the int parsing.
- I can use serde_html_form to handle arrays in forms.
    - but this requires giving up internally tagged enums. I have switch to externally tagged, if I do this.
    - this is because the feature in serde is half baked. There are rough edges when deserializing to non json formats while using internally tagged enums. It's up to the deserializer library to figure it out, and this doesn't handle it.
- with json encoding:
    - The main pain point is around arrays. I have to use my Select<> type to handle it.
- I can go back `serde_urlencoded`, which handled internally tagged enums, but not arrays.
- I can try to port code between form deserializers to get both features.


Three packages, and they are all incomplete:
- https://github.com/nox/serde_urlencoded
    - handles untagged/internally tagged enums, doesn't handle nesting or sequences

https://github.com/jplatte/serde_html_form
    - handles arrays
    - breaks handling of untagged/internally tagged enums
https://github.com/samscott89/serde_qs
    - handles nesting





### Game Engine
The game engine is all pretty hard coded. Steps of a turn are coupled to specific roles and actions that may occur. Metadata tied to specific roles is embededded in the root game state. All of this is easy from a standpoint of building a game with a small set of roles. But this style of programming would not scale to building other types of games, card games, or board games with lots of moving pieces and systems and expansion content. This is not a huge problem per se, there's lots of specific rulings in the rulebook for how different roles and districts interact with each other. By doing everything in line, I can ensure all the interactions hold up.

This just wouldn't work if I wanted to add a custom card editor. Or support something like mtg / pokemon etc. with a bajillion interactions. It's a way of development is alright for the game I built, but It makes me curious about refactoring this system to be one of abstract triggers, and more explicit steps in a turn, and modifiers and so on. 

The big one for me is metadata. The tax collector's money stash is on the game struct. The alchemist refund is on the game struct. The museum tucked cards are defined on the game struct. 
At least I made the city districts hold the beautified status instead of the game struct.
