{-|
Module      : Earthdawn.FourthEdition.Disciplines.Values
Description : Earthdawn 4th Edition Discipline Values
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Hard-coded values for ability resources.

If this resource becomes more dynamic we'll figure out a datastore.
-}
module Earthdawn.FourthEdition.Disciplines.Values
  ( disciplinesMap
  ) where

import qualified Data.Map.Strict as Map (fromList, Map)

import Earthdawn.FourthEdition.Disciplines.Types
import Earthdawn.FourthEdition.Talents.Values

-- | Static 'Map.Map' of name to 'Discipline'.
disciplinesMap :: Map.Map String Discipline
disciplinesMap = Map.fromList [(name d, d) | d <- [airSailor, archer, beastmaster, cavalryman, elementalist, illusionist, nethermancer, scout, skyRaider, swordmaster, thief, troubadour, warrior, weaponsmith, wizard]]

airSailor :: Discipline
airSailor = Discipline
  { name    = "air-sailor"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [avoidBlow, climbing, meleeWeapons, threadWeaving, windCatcher])
             , (2, [awareness])
             , (3, [empathic sense])
             , (4, [woundBalance])
             , (5, [hearteningLaugh])
             , (6, [airDance])
             , (7, [inspireOthers])
             , (8, [lionHeart])
             ]

archer :: Discipline
archer = Discipline
  { name    = "archer"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [avoidBlow, missileWeapons, mysticAim, threadWeaving, trueShot])
             , (2, [mysticPursuit])
             , (3, [anticipateBlow])
             , (4, [longShot])
             , (5, [spotArmorFlaw])
             , (6, [bankShot])
             , (7, [flameArrow])
             , (8, [secondShot])
             ]

beastmaster :: Discipline
beastmaster = Discipline
  { name    = "beastmaster"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [avoidBlow, clawShape, threadWeaving, unarmedCombat, wildernessSurvival])
             , (2, [awareness])
             , (3, [dominateBeast])
             , (4, [greatLeap])
             , (5, [bloodShare])
             , (6, [animalTalk])
             , (7, [downStrike])
             , (8, [clawFrenzy])
             ]

cavalryman :: Discipline
cavalryman = Discipline
  { name    = "cavalryman"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [animalBond, charge, meleeWeapons, threadWeaving, trickRiding])
             , (2, [animalTraining])
             , (3, [enhanceAnimalCompanion])
             , (4, [callAnimalCompanion])
             , (5, [armorMount])
             , (6, [wheelingAttack])
             , (7, [wheelingDefense])
             , (8, [doubleCharge])
             ]

elementalist :: Discipline
elementalist = Discipline
  { name    = "elementalist"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [awareness, patterncraft, spellcasting, threadWeaving, woodSkin])
             , (2, [fireHeal])
             , (3, [elementalTongues])
             , (4, [elementalHold])
             , (5, [summon])
             , (6, [willforce])
             , (7, [earthSkin])
             , (8, [holdThread])
             ]

illusionist :: Discipline
illusionist = Discipline
  { name    = "illusionist"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [falseSight, firstImpression, patterncraft, spellcasting, threadWeaving])
             , (2, [trueSight])
             , (3, [conversation])
             , (4, [disguiseSelf])
             , (5, [powerMask])
             , (6, [willforce])
             , (7, [hypnotize])
             , (8, [holdThread])
             ]

nethermancer :: Discipline
nethermancer = Discipline
  { name    = "nethermancer"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [astralSight, frighten, patterncraft, spellcasting, threadWeaving])
             , (2, [steelThought])
             , (3, [spiritTalk])
             , (4, [spiritHold])
             , (5, [summon])
             , (6, [willforce])
             , (7, [orbitingSpy])
             , (8, [holdThread])
             ]

scout :: Discipline
scout = Discipline
  { name    = "scout"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [awareness, climbing, threadWeaving, tracking, wildernessSurvival])
             , (2, [stealthyStride])
             , (3, [mysticPursuit])
             , (4, [dangerSense])
             , (5, [evidenceAnalysis])
             , (6, [astralSight])
             , (7, [safePath])
             , (8, [orbitingSpy])
             ]

skyRaider :: Discipline
skyRaider = Discipline
  { name    = "sky-raider"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [battleShout, climbing, fireblood, meleeWeapons, threadWeaving])
             , (2, [greatLeap])
             , (3, [woundBalance])
             , (4, [fireHeal])
             , (5, [battleBellow])
             , (6, [steelyStare])
             , (7, [downStrike])
             , (8, [momentumAttack])
             ]

swordmaster :: Discipline
swordmaster = Discipline
  { name    = "swordmaster"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [avoidBlow, maneuver, meleeWeapons, taunt, threadWeaving])
             , (2, [firstImpression])
             , (3, [riposte])
             , (4, [hearteningLaugh])
             , (5, [secondWeapon])
             , (6, [disarm])
             , (7, [resistTaunt])
             , (8, [secondAttack])
             ]

thief :: Discipline
thief = Discipline
  { name    = "thief"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [awareness, lockPicking, pickingPockets, stealthyStride, threadWeaving])
             , (2, [disarmTrap])
             , (3, [haggle])
             , (4, [concealObject])
             , (5, [engagingBanter])
             , (6, [sloughBlame])
             , (7, [fastHand])
             , (8, [falseSight])
             ]

troubadour :: Discipline
troubadour = Discipline
  { name    = "troubadour"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [emotionSong, firstImpression, hearteningLaugh, itemHistory, threadWeaving])
             , (2, [etiquette])
             , (3, [empathicSense])
             , (4, [research])
             , (5, [inspireOthers])
             , (6, [lastingImpression])
             , (7, [resistTaunt])
             , (8, [sloughBlame])
             ]

warrior :: Discipline
warrior = Discipline
  { name    = "warrior"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [avoidBlow, meleeWeapons, threadWeaving, tigerSpring, woodSkin])
             , (2, [woundBalance])
             , (3, [airDance])
             , (4, [waterfallSlam])
             , (5, [earthSkin])
             , (6, [temperFlesh])
             , (7, [crushingBlow])
             , (8, [secondAttack])
             ]

weaponsmith :: Discipline
weaponsmith = Discipline
  { name    = "weaponsmith"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [forgeWeapon, itemHistory, meleeWeapons, steelThought, threadWeaving])
             , (2, [conversation])
             , (3, [suppressCurse])
             , (4, [woundBalance])
             , (5, [forgeArmor])
             , (6, [temperFlesh])
             , (7, [spotArmorFlaw])
             , (8, [lionHeart])
             ]

wizard :: Discipline
wizard = Discipline
  { name    = "wizard"
  , circles = Map.fromList cs
  }
  where cs = [ (1, [dispelMagic, patterncraft, research, spellcasting, threadWeaving])
             , (2, [astralSight])
             , (3, [tenaciousWeave])
             , (4, [steelThought])
             , (5, [astralInterference])
             , (6, [willforce])
             , (7, [holdThread])
             , (8, [suppressCurse])
             ]
