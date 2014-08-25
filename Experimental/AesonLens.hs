-- # LANGUAGE OverloadedStrings #

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

-- this code is intended for ghci experimentation, so we use "let" to bind a value to a name
jsonBlob = "[{\"someObject\": {\"version\": [1, 0, 3]}}]"

-- example from above
myVal = jsonBlob ^? nth 0 . key "someObject" . key "version" . nth 1

-- What progressively composing the prisms looks like, note the composition order:
-- λ> jsonBlob ^? nth 0
Just (Object fromList
       [("someObject",
         Object fromList
           [("version",Array
             (fromList [Number 1.0, Number 0.0, Number 3.0]))])])

-- λ> jsonBlob ^? nth 0 . key "someObject"
Just (Object fromList
       [("version",
         Array (fromList [Number 1.0, Number 0.0, Number 3.0]))])

-- λ> jsonBlob ^? nth 0 . key "someObject" . key "version"
Just (Array (fromList [Number 1.0, Number 0.0, Number 3.0]))

-- λ> jsonBlob ^? nth 0 . key "someObject" . key "version" . nth 1
Just (Number 0.0)