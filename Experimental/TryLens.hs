{-#LANGUAGE TemplateHaskell #-}
module TryLens where

import Control.Lens
import Experimental.Lens

-- This is a TH splice, it just creates some functions for us automatically based on the record functions in 'Location'. We'll describe them in more detail below.
$(makeLenses ''Location)

--latitude  :: Lens' Location Arc
--longitude :: Lens' Location Arc

a1 = Arc{_degree=10,_minute =30,_second=0}
a2 = Arc{_degree=0,_minute =20,_second=5}
p  = Location{_latitude =a1, _longitude =a2}


getLatitude :: Location -> Arc
getLatitude = view latitude 

setLatitude :: Arc -> Location -> Location
setLatitude = set latitude

