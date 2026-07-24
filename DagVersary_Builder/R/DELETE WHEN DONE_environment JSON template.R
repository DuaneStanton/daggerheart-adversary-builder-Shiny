# template JSON for Environments for Daggerforge

"environments": [
  {
    "id": "CUE_1784148591206_e2hd7fqz",
    "name": "OUTPOST TOWN (mod)",
    "tier": "1",
    "type": "Social",
    "desc": "A small town on the outskirts of a nation or region, close to a dungeon, tombs, or other adventuring destinations.",
    "impulse": "Drive the desperate to certain doom, profit off of ragged hope",
    "difficulty": "12",
    "potentialAdversaries": "Jagged Knife Bandits (Hexer, Kneebreaker, Lackey, Lieutenant, Shadow, Sniper), Masked Thief, Merchant",
    "source": "custom",
    "features": [
      {
        "name": "Rumors Abound",
        "type": "Passive",
        "richContent": "<div class=\"df-p\">Gossip is the fastest-traveling currency in the realm. A PC can inquire about major events by making a Presence Roll. What they learn depends on the outcome of their roll, based on the following criteria:</div><div class=\"df-ul\"><div class=\"df-li\"><div class=\"df-p\">Critical Success: Learn about two major events. The PC can ask one follow-up question about one of the rumors and get atruthful (if not always complete) answer.</div></div><div class=\"df-li\"><div class=\"df-p\">Success with Hope: Learn about two events, at least one of which is relevant to the character’s background.</div></div><div class=\"df-li\"><div class=\"df-p\">Success with Fear: Learn an alarming rumor related to the character’s background.</div></div><div class=\"df-li\"><div class=\"df-p\">Any Failure: The locals respond poorly to their inquiries. The PC must mark a Stress to learn one relevant rumor.</div></div></div>",
        "questions": [
          "What news do the PCs have that they could pass along to curious travelers?",
          "What do the locals think about these events?"
        ]
      },
      {
        "name": "Rival Party",
        "type": "Passive",
        "richContent": "<div class=\"df-p\">Another adventuring party is here, seeking the same treasure or leads as the PCs.</div>",
        "questions": [
          "Which PC has a connection to one of the rival party members?",
          "Do they approach the PC first or do they wait for the PC to move?"
        ]
      },
      {
        "name": "It’d Be a Shame If Something Happened to Your Store",
        "type": "Action",
        "richContent": "<div class=\"df-p\">The PCs witness as agents of a local crime boss shake down a general goods store.</div>",
        "questions": [
          "What trouble does it cause if the PCs intervene?"
        ]
      },
      {
        "name": "Wrong Place, Wrong Time",
        "type": "Reaction",
        "richContent": "<div class=\"df-p\">At night, or when the party is alone in a back alley, you can spend a Fear to introduce a group of thieves who try to rob them. The thieves appear at Close range of a chosen PC and include a Jagged Knife Kneebreaker, as many Lackeys as there are PCs, and a Lieutenant. For a larger party, add a Hexer or Sniper.</div>",
        "questions": [
          "What details show the party that these people are desperate former adventurers?"
        ]
      }
    ]
  }
]
