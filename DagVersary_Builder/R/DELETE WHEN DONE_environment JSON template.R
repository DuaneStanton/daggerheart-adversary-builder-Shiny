# template JSON for Environments for Daggerforge
# 
# "environments": [
# {
#   "id": "CUE_1785019111621_0cpgevox",
#   "name": "CLIFFSIDE ASCENT (mod TEST)",
#   "tier": "1",
#   "type": "Traversal",
#   "desc": "A steep, rocky cliff side tall enough to make traversal dangerous.",
#   "impulse": "Cast the unready down to a rocky doom, draw people in with promise of what lies at the top",
#   "difficulty": "12",
#   "potentialAdversaries": "Construct, Giant Scorpion, Glass Snake",
#   "source": "custom",
#   "features": [
#     {
#       "name": "The Climb",
#       "type": "Passive",
#       "richContent": "<div class=\"df-p\">Climbing up the cliff side uses a Progress Countdown (12). It ticks down according to the following criteria when the PCs make an action roll to climb:</div><div class=\"df-ul\"><div class=\"df-li\"><div class=\"df-p\">Critical Success: Tick down 3</div></div><div class=\"df-li\"><div class=\"df-p\">Success with Hope: Tick down 2</div></div><div class=\"df-li\"><div class=\"df-p\">Success with Fear: Tick down 1</div></div><div class=\"df-li\"><div class=\"df-p\">Failure with Hope: No advancement</div></div><div class=\"df-li\"><div class=\"df-p\">Failure with Fear: Tick up 1</div></div></div><div class=\"df-p\">When the countdown triggers, the party has made it to the top of the cliff .</div>",
#       "questions": [
#         "What strange formations are the stones arranged in?",
#         "What ominous warnings did previous adventurers leave?"
#       ]
#     },
#     {
#       "name": "Pitons Left Behind",
#       "type": "Passive",
#       "richContent": "<div class=\"df-p\">Previous climbers left behind large metal rods that climbers can use to aid their ascent. If a PC using the pitons fails an action roll to climb, they can mark a Stress instead of ticking the countdown up.</div>",
#       "questions": [
#         "What do the shape and material of these pitons tell you about the previous climbers?",
#         "How far apart are they from one another?",
#         "Are all the pitons real, or are some illusory?"
#       ]
#     },
#     {
#       "name": "Fall",
#       "type": "Action",
#       "cost": "Spend a Fear",
#       "richContent": "<div class=\"df-p\">to have a PC’s handhold fail, plummeting them toward the ground. If they aren’t saved on the next action, they hit the ground and tick up the countdown by 2. The PC takes 1d12 physical damage if the countdown is between 8 and 12, 2d12 between 4 and 7, and 3d12 at 3 or lower.</div>",
#       "questions": [
#         "How can you tell many others have fallen here before?",
#         "What lives in these walls that might try to scare adventurers into falling for an easy meal?"
#       ]
#     },
#     {
#       "name": "test Loop Countdown",
#       "type": "Passive",
#       "richContent": "<div class=\"df-p\">Begin a Countdown (loop 4)</div>",
#       "questions": []
#     },
#     {
#       "name": "test Decreasing Countdown",
#       "type": "Passive",
#       "richContent": "<div class=\"df-p\">Begin a Countdown (decreasing 5)</div>",
#       "questions": []
#     },
#     {
#       "name": "test Increasing Countdown",
#       "type": "Passive",
#       "richContent": "<div class=\"df-p\">Begin a Countdown (increasing 5)</div>",
#       "questions": []
#     },
#     {
#       "name": "test Dynamic Loop Countdown",
#       "type": "Passive",
#       "richContent": "<div class=\"df-p\">Begin a Countdown (loop d4)</div>",
#       "questions": []
#     },
#     {
#       "name": "test Dynamic Countdown",
#       "type": "Passive",
#       "richContent": "<div class=\"df-p\">Begin a Countdown (d6)</div>",
#       "questions": []
#     }
#   ],
#   "countdowns": [
#     {
#       "name": "The Climb",
#       "max": 12
#     }
#   ]
# }
# ]
