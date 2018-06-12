module OppAPI exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias User =
    { userFirstName : String
    , userLastName : String
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "userFirstName" string
        |> required "userLastName" string

type alias Contract =
    { contractUserId : Int
    , contractAddress : String
    }

decodeContract : Decoder Contract
decodeContract =
    decode Contract
        |> required "contractUserId" int
        |> required "contractAddress" string

getUsers : Http.Request (List (User))
getUsers =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "users"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeUser)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getUserById : Int -> Http.Request (User)
getUserById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeUser
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getContracts : Http.Request (List (Contract))
getContracts =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "contracts"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeContract)
        , timeout =
            Nothing
        , withCredentials =
            False
        }