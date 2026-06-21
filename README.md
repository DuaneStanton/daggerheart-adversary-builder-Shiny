# daggerheart-adversary-builder-Shiny
A locally-hostable Shiny app to set up and run adversaries for the Daggerheart TTRPG

The app has the following structure, organized by tab:


- Start : start here - specify the party size (if following the 'Battle Points' guidance from the core rules), the challenge type, and the count for each adversary type (including Colossi!)

- Customize : fill in details for your adversaries (name, motives & tactics, weapon name, experiences, features) and tweak the default values for HP/stress/damage (following RightKnighttoFight's guidelines - see "Credit" tab). Optionally, populate adversaries with randomly-generated Passive/Action/Reaction features that are sensible for that adversary type (also listed in the 'Feature Reference' tab)
  - Caution: If you decide to go back to the 'Start' tab and change details, the 'Customize' tab will reset, potentially losing your previously-entered details. 

- Run : optionally run your encounter from here! Basic conditions and messages for emptied-Stress and -HP adversaries help you keep track of things.

- Obsidian - Daggerforge : export a JSON file to your Downloads folder to upload and run your custom adversaries using the Daggerforge plugin in Obsidian.

- Obsidian - ITS Theme Markdown : export a .txt file to your Downloads folder to copy-paste into your Obsidian vault with the ITS Theme plugin installed. While it _can_ also work in Obsidian without the plugin, the end result doesn't look great.

- Feature Reference : see a listing of the available features per adversary, plus some 'general use' features you might consider copying into the 'Feature' fields for your adversaries. 'General use' features do _not_ automatically populate the Feature listings as combinations of these can create overly-tough adversaries (e.g. resistance to both physical AND magic damage).

- MORE HERE

Users can customize specific adversaries on one tab, potentially run an encounter (e.g. adjust HP/Stress/conditions) in another tab, and output the customized stat blocks in a template that will work with the Obsidian (ITS Theme) to run encounters there {note: this last is a work in progress; the rest of the app is as well, but has minimal functional components needed for actual use}
