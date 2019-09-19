module Api.Endpoint exposing (Endpoint, article, articles, comment, comments, favorite, feed, follow, login, profiles, request, tags, user, users, asset_pairs, kraken_meta_info)

import Article.Slug as Slug exposing (Slug)
import CommentId exposing (CommentId)
import Http
import Url.Builder exposing (QueryParameter)
import Username exposing (Username)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , withCredentials : Bool
    }
    -> Http.Request a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , withCredentials = config.withCredentials
        }



-- TYPES


{-| Get a URL to the Conduit API.

This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.

-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin "https://conduit.productionready.io"
        ("api" :: paths)
        queryParams
        |> Endpoint


url2 : List String -> List QueryParameter -> Endpoint
url2 paths queryParams = 
    Url.Builder.crossOrigin "http://192.168.1.8:4000"
    ("api" :: paths)
    queryParams
    |> Endpoint

craken_public_url : List String -> List QueryParameter -> Endpoint
craken_public_url  paths queryParams =
    Url.Builder.crossOrigin "https://cors-anywhere.herokuapp.com/https://api.kraken.com"
    ("0" :: paths)
    queryParams
    |> Endpoint

-- ENDPOINTS


login : Endpoint
login =
    url2 [ "users", "login" ] []


user : Endpoint
user =
    url [ "user" ] []


users : Endpoint
users =
    url [ "users" ] []


follow : Username -> Endpoint
follow uname =
    url [ "profiles", Username.toString uname, "follow" ] []



-- ARTICLE ENDPOINTS


asset_pairs : Endpoint
asset_pairs = 
   craken_public_url["public", "AssetPairs"] [] 

kraken_meta_info : List QueryParameter -> Endpoint
kraken_meta_info params= 
    craken_public_url["public", "Ticker"] params

article : Slug -> Endpoint
article slug =
    url [ "articles", Slug.toString slug ] []

comments : Slug -> Endpoint
comments slug =
    url [ "articles", Slug.toString slug, "comments" ] []


comment : Slug -> CommentId -> Endpoint
comment slug commentId =
    url [ "articles", Slug.toString slug, "comments", CommentId.toString commentId ] []


favorite : Slug -> Endpoint
favorite slug =
    url [ "articles", Slug.toString slug, "favorite" ] []


articles : List QueryParameter -> Endpoint
articles params =
    url [ "articles" ] params


profiles : Username -> Endpoint
profiles uname =
    url [ "profiles", Username.toString uname ] []


feed : List QueryParameter -> Endpoint
feed params =
    url [ "articles", "feed" ] params


tags : Endpoint
tags =
    url [ "tags" ] []
