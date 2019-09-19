module AssetPair exposing (AssetPair, Full, Preview, exchange, body, favorite, favoriteButton, fetch, fromPreview, fullDecoder, mapAuthor, metadata, previewDecoder, slug, unfavorite, unfavoriteButton)

{-| The interface to the Article data structure.

This includes:

  - The Article type itself
  - Ways to make HTTP requests to retrieve and modify articles
  - Ways to access information about an article
  - Converting between various types

-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article.Body as Body exposing (Body)
import Article.Slug as Slug exposing (Slug)
import Article.Tag as Tag exposing (Tag)
import Author exposing (Author)
import Html exposing (Attribute, Html, i)
import Html.Attributes exposing (class)
import Html.Events exposing (stopPropagationOn)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode
import Markdown
import Profile exposing (Profile)
import Time
import Username as Username exposing (Username)
import Viewer exposing (Viewer)



-- TYPES
type AssetPair a
    = AssetPair Internals a


type alias Metadata =
    { 
      opening_price : Float
    }


type alias Internals =
    { pari_name : String
    , alnternate_name : String
    , ws_name : String
    }


type Preview
    = Preview


type Full
    = Full Body

-- INFO

author : Article a -> Author
author (Article internals _) =
    internals.author


metadata : Article a -> Metadata
metadata (Article internals _) =
    internals.metadata


slug : Article a -> Slug
slug (Article internals _) =
    internals.slug


body : Article Full -> Body
body (Article _ (Full extraInfo)) =
    extraInfo



-- TRANSFORM


mapAuthor : (Author -> Author) -> Article a -> Article a
mapAuthor transform (Article info extras) =
    


fromPreview : Body -> Article Preview -> Article Full
fromPreview newBody (Article info Preview) =
    Article info (Full newBody)



-- SINGLE


fetch : Maybe Cred -> Slug -> Http.Request (Article Full)
fetch maybeCred articleSlug =
    Decode.field "article" (fullDecoder maybeCred)
        |> Api.get (Endpoint.article articleSlug) maybeCred


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))
