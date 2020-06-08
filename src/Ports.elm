port module Ports exposing (saveScores)

import Json.Encode as E


port saveScores : E.Value -> Cmd msg
