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

-- | Static 'Map.Map' of name to 'Discipline'.
disciplinesMap :: Map.Map String Discipline
disciplinesMap = Map.fromList [(name d, d) | d <- [airSailor, archer, beastmaster, cavalryman, elementalist, illusionist, nethermancer, scout, skyRaider, swordmaster, thief, troubadour, warrior, weaponsmith, wizard]]

airSailor :: Discipline
airSailor = Discipline
  { name = "air-sailor"
  }

archer :: Discipline
archer = Discipline
  { name = "archer"
  }

beastmaster :: Discipline
beastmaster = Discipline
  { name = "beastmaster"
  }

cavalryman :: Discipline
cavalryman = Discipline
  { name = "cavalryman"
  }

elementalist :: Discipline
elementalist = Discipline
  { name = "elementalist"
  }

illusionist :: Discipline
illusionist = Discipline
  { name = "illusionist"
  }

nethermancer :: Discipline
nethermancer = Discipline
  { name = "nethermancer"
  }

scout :: Discipline
scout = Discipline
  { name = "scout"
  }

skyRaider :: Discipline
skyRaider = Discipline
  { name = "sky-raider"
  }

swordmaster :: Discipline
swordmaster = Discipline
  { name = "swordmaster"
  }

thief :: Discipline
thief = Discipline
  { name = "thief"
  }

troubadour :: Discipline
troubadour = Discipline
  { name = "troubadour"
  }

warrior :: Discipline
warrior = Discipline
  { name = "warrior"
  }

weaponsmith :: Discipline
weaponsmith = Discipline
  { name = "weaponsmith"
  }

wizard :: Discipline
wizard = Discipline
  { name = "wizard"
  }
