## Shortlist
- Finish Game engine    
    - [ ] Killed Emperor end of round action to grant crown
    - [ ] Bewitch
    - [ ] Wizard build

## Longer list
- Notifications
    - Seer: card is taken. 
    - Seer: card is given.
    - Magician
    - Spy
    - Warlord 
    - Marshal
    - Diplomat
    - Theater 
    - Wizard 

- Fixup janky UI
    - [ ] Warlord Menu:
        - Destroy cost should be displayed prominently 
        - should disable & grey districts that cost too much.

    - [ ] Build Menu:
        - Reminder that you have a warrant; emoji + text
        - Cost to build should be displayed
        - Use tabs instead of radio buttons

    - [ ] Marshal menu should disable & grey districts that cost too much.
- [ ] Max height for hand panel, with horizontal scroll
- [ ] Make log format and tenses consistent
- [ ] Random audio notification

## Ideas
- [ ] Randomizer mode: any role can be any rank
- [ ] Persist game config to sqlite db
- [ ] Keyboard based warrant assignment
- [ ] Multi room support
- [ ] Detect when building is impossible.
- [ ] Pesist the dragged position of districts in a city.
- [ ] Resizable windows, dimensions are saved per user and kept persistently between sessions

## Features if I wanted to make this bigger
- [ ] A way for me to kick bots / spammers
- [ ] Integrate w/ Google OAuth
- [ ] Multi room support
- [ ] Backup games and Restore from sqlite


## Playtest 1 
- [x] Notification bell for start of turn.
- [ ] role and district description should have dedicated info icon for bringing up their tooltips.

## Playtest 2
- [x] Role config menu is clipped on smaller screens
- [x] flex-wrap my-roles. Basically, they should stack vertically if I narrow my window
- [x] museum auto tooltip hides the artifacts
- [x] Ending game can crashes the game
- [x] Limit width and apply text-wrap to right sidebar usernames. Long usernames shouldn't distort the whole page.
- [x] "Players' turn" could have more context. show role + name. Put word draft if they are drafting.
- [x] Roles left sidebar, italics say "hover for details". We should restore the tooltip or remove that phrase.
- [ ] Allow names with some punctuation. Will need to url encode names in city request
- [ ] Logan couldn't see his city districts without scrolling.
- [ ] Logan couldn't easily see enemy districts without scrolling. Warlord menu
- [ ] William found the button highlighting behavior in the config menu to be confusing. Didn't realize I was going for tab like interface. How can I make it more tab like?

## Playtest 3
- [ ] public/index.css is being cached across multiple browsers. I need to look into how axum serves directories, and invalidate the cache on this file. Because its ridiculous william had to disable his cache and reload.
- [ ] All Cities in 1 panel, scroll.
- [ ] Timer:  2 minute timer, +20secs every turn, capped at 2 minutes.
- [ ] Logan's audio notification didn't work.
    - [ ] some way of grabbing attention of the user. 
    - [ ] Does the WS connection die, when logan was switching apps/safari tabs?
    - [ ] Ask logan to use Firefox, Chrome, or Brave.
- [ ] Lock down game room permissions to Room host. Make myself room host, so I can configure the game.
- [ ] Highlight active player in sidebar. It's too subtle to notice the active player.
- [ ] Tooltips or faq links on every card. Logan didn't know what the seer did, and I can't blame him.
- [ ] Seer names overflow the badge size

## Custom Roles
- [ ] Nerfed Asssassin: Kill the role and all its abilities, but the player can still take normal actions. Gather, build, any district abilities

## Cleo's easter eggs
- [x] Dragging the dragon out of his section play's Mr. Brightside. Putting him back pauses it.
- [x] "Pause" menu button

## Tech Debt

### Anyhow
Use anyhow instead of coercing all errors to a string type.

### ActionTag
Incomplete action submission conflates two scenarios:
(1) the intent to open the relevant menu
(2) the intent to submit a menu, but you are missing data in the form.


### Unwraps
Just eliminate all sources of unwrap. It's not the worth the risks that panics pose to the app.

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
