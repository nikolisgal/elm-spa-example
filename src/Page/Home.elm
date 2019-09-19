module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Preview)
import Article.Feed as Feed
import Article.Tag as Tag exposing (Tag)
import Browser.Dom as Dom
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Http
import Loading
import Log
import Page
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)
import Json.Decode exposing (dict, list, string, Decoder, field, map5, decodeString, errorToString, nullable, float, int)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Dict exposing (Dict)
import Paginate exposing (..)


-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , feedTab : FeedTab
    , feedPage : Int
    , tags : Status (List Tag)
    , feed : Status KrakenResponse
    , feedPair : Maybe Pagination
    , feedMeta : Maybe (Dict String AssetMetaInfo)
    }


type alias Pagination = 
    { pairs : PaginatedList AssetPair
    , reversed : Bool
    , query : String
    , globalId : Int
    }

type alias AssetPair = 
    { name : String
    , info : AssetPairInfo
    }

type alias  AssetPairInfo = 
    { alternate_name : String
    , ws_name : String
    , base : String
    , quote : String
    }

type alias AssetMetaInfo = 
    { a : List String
    , b : List String
    , c : List String
    , v : List String
    , p : List String
    , t : List Int
    , l : List String
    , h : List String
    , o : String
    }

type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type FeedTab
    = YourFeed Cred
    | GlobalFeed
    | TagFeed Tag


type alias KrakenResponse =
   { errors : List String
   , assetPairs : Dict String AssetPairInfo
   }

type alias KrakenMetaResponse = 
    { errors : List String
    , assetInfo : Dict String AssetMetaInfo
    }

decoderAssetMetaInfo : Decoder AssetMetaInfo 
decoderAssetMetaInfo =
    Json.Decode.succeed AssetMetaInfo
    |> required "a" (list string) 
    |> required "b" (list string)
    |> required "c" (list string)
    |> required "v" (list string)
    |> required "p" (list string)
    |> required "t" (list int)
    |> required "l" (list string)
    |> required "h" (list string)
    |> required "o" string



decoderKrakenMeta : Decoder KrakenMetaResponse 
decoderKrakenMeta = 
    Json.Decode.succeed KrakenMetaResponse
      |> required "error" (list string)
      |> required "result" (dict decoderAssetMetaInfo)


decoder : Decoder AssetPairInfo
decoder = 
    Json.Decode.succeed AssetPairInfo 
      |> required "altname" string 
      |> optional "wsname" string ""
      |> required "base" string
      |> required "quote" string


decoderKraken : Decoder KrakenResponse
decoderKraken = 
    Json.Decode.succeed KrakenResponse
      |> required "error" (list string)
      |> required "result" (dict decoder)  


init : Session -> ( Model, Cmd Msg )
init session =
    let
        _ = Debug.log "init method" "session home "
        feedTab =
            case Session.cred session of
                Just cred ->
                    YourFeed cred

                Nothing ->
                    GlobalFeed

        loadTags =
            Http.toTask Tag.list
    in
    ( { session = session
      , timeZone = Time.utc
      , feedTab = feedTab
      , feedPair = Nothing
      , feedPage = 1
      , tags = Loading
      , feed = Loading
      , feedMeta = Nothing
      }
    , Cmd.batch
        [ fetchFeed session 1
            |> Task.attempt CompletedFeedLoad
        , Tag.list
            |> Http.send CompletedTagsLoad
        , Task.perform GotTimeZone Time.here
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Conduit"
    , content =
        div [ class "home-page" ]
            [ viewBanner
            , div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] <|
                        case model.feed of
                            Loaded feed ->
                                case model.feedPair of
                                    Just paginat ->
                                        case model.feedMeta of
                                            Just feedMeta ->
                                                [viewPaginated paginat.pairs feedMeta]
                                            Nothing ->
                                                []
                                    Nothing ->
                                        []
                            Loading ->
                                [Loading.icon]

                            LoadingSlowly ->
                                [ Loading.icon ]

                            Failed ->
                                [ Loading.error "feed kokino fws" ]
                    , div [ class "col-md-3" ] <|
                        case model.tags of
                            Loaded tags ->
                                [ div [ class "sidebar" ] <|
                                    [ p [] [ text "Popular Tags" ]
                                    , viewTags tags
                                    ]
                                ]

                            Loading ->
                                []

                            LoadingSlowly ->
                                [ Loading.icon ]

                            Failed ->
                                [ Loading.error "tags" ]
                    ]
                ]
            ]
    }



viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
        ]


toTableRow:  Dict String AssetMetaInfo -> AssetPair-> Html Msg
toTableRow metaInfo assetPair =
   case Dict.get assetPair.info.alternate_name metaInfo of
       Nothing ->
           case Dict.get assetPair.name metaInfo of
               Nothing ->
                 tr []
                 [ td[][text assetPair.info.alternate_name]
                 , td[][text assetPair.info.base]
                 ]

               Just assetMetaInf ->
                  tr []
                  [ td[][text assetPair.info.alternate_name]
                  , td[][text assetPair.info.base]
                  , td[][text assetMetaInf.o]
                  ]

       Just assetMetaInfo ->
         tr []
         [ td[][text assetPair.info.alternate_name]
         , td[][text assetPair.info.base]
         , td[][text assetMetaInfo.o]
         ]

-- TABS
viewPaginated : PaginatedList AssetPair -> Dict String AssetMetaInfo -> Html Msg
viewPaginated assetPairs metaInfo=
    let
        
        displayInfoView =
            div []
                [ text <|
                    String.join " "
                        [ "page"
                        , String.fromInt <| Paginate.currentPage assetPairs
                        , "of"
                        , String.fromInt <| Paginate.totalPages assetPairs
                        ]
                , div[] [ table [class "table"]
                  ( [
                       thead [class "thead-dark"]
                       [ 
                          th [scope "col"] [ text "Name" ]
                       ,  th [scope "col"] [ text "Base" ]
                       ]
                   ] ++  
                   (List.map (toTableRow metaInfo) (page assetPairs)))
                     
                ]
                ]


        
        prevButtons =
            [ button [ onClick First, disabled <| Paginate.isFirst assetPairs ] [ text "<<" ]
            , button [ onClick Prev, disabled <| Paginate.isFirst assetPairs ] [ text "<" ]
            ]

        nextButtons =
            [ button [ onClick Next, disabled <| Paginate.isLast assetPairs ] [ text ">" ]
            , button [ onClick Last, disabled <| Paginate.isLast assetPairs ] [ text ">>" ]
            ]

        pagerButtonView index isActive =
            button
                [ style "font-weight"
                    (if isActive then
                        "bold"

                     else
                        "normal"
                    )
                , onClick <| GoTo index
                ]
                [ text <| String.fromInt index ]

        pagerOptions =
            { innerWindow = 1
            , outerWindow = 1
            , pageNumberView = pagerButtonView
            , gapView = text "..."
            }
    in
    div [] <|
        [ displayInfoView
        ]
            ++ prevButtons
            ++ [ span [] <| Paginate.pager pagerButtonView assetPairs ]
            ++ nextButtons
            ++ [ p [] [ ]
               , span [] <| Paginate.elidedPager pagerOptions assetPairs
               ]



viewTabs : Maybe Cred -> FeedTab -> Html Msg
viewTabs maybeCred tab =
    case tab of
        YourFeed cred ->
            Feed.viewTabs [] (yourFeed cred) [ globalFeed ]

        GlobalFeed ->
            let
                otherTabs =
                    case maybeCred of
                        Just cred ->
                            [ yourFeed cred ]

                        Nothing ->
                            []
            in
            Feed.viewTabs otherTabs globalFeed []

        TagFeed tag ->
            let
                otherTabs =
                    case maybeCred of
                        Just cred ->
                            [ yourFeed cred, globalFeed ]

                        Nothing ->
                            [ globalFeed ]
            in
            Feed.viewTabs otherTabs (tagFeed tag) []


yourFeed : Cred -> ( String, Msg )
yourFeed cred =
    ( "Your Feed", ClickedTab (YourFeed cred) )


globalFeed : ( String, Msg )
globalFeed =
    ( "Global Feed", ClickedTab GlobalFeed )


tagFeed : Tag -> ( String, Msg )
tagFeed tag =
    ( "#" ++ Tag.toString tag, ClickedTab (TagFeed tag) )



-- TAGS


viewTags : List Tag -> Html Msg
viewTags tags =
    div [ class "tag-list" ] (List.map viewTag tags)


viewTag : Tag -> Html Msg
viewTag tagName =
    a
        [ class "tag-pill tag-default"
        , onClick (ClickedTag tagName)

        -- The RealWorld CSS requires an href to work properly.
        , href ""
        ]
        [ text (Tag.toString tagName) ]



-- UPDATE


type Msg =
     Next
    | Prev
    | First
    | Last
    | GoTo Int
    | ClickedTag Tag
    | ClickedTab FeedTab
    | ClickedFeedPage Int
    | CompletedFeedLoad (Result Http.Error (KrakenResponse))
    | GetMetaInfo (Result Http.Error (KrakenMetaResponse))
    | CompletedTagsLoad (Result Http.Error (List Tag))
    | GotTimeZone Time.Zone
    | GotSession Session
    | PassedSlowLoadThreshold


mapperOfAssets : Dict String AssetPairInfo -> List AssetPair
mapperOfAssets dict =
    let 
       dict_trans =  Dict.map (\k a -> {name = k, info = a}) dict
    in
    Dict.values dict_trans

    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTag tag ->
            let
                feedTab =
                    TagFeed tag
            in
            ( { model | feedTab = feedTab }
            , fetchFeed model.session  1
                |> Task.attempt CompletedFeedLoad
            )

        ClickedTab tab ->
            ( { model | feedTab = tab }
            , fetchFeed model.session  1
                |> Task.attempt CompletedFeedLoad
            )

        ClickedFeedPage page ->
            ( { model | feedPage = page }
            , fetchFeed model.session  page
                |> Task.andThen (\feed -> Task.map (\_ -> feed) scrollToTop)
                |> Task.attempt CompletedFeedLoad
            )
        GetMetaInfo (Ok krakenResponse) ->
            ({model | feedMeta = Just krakenResponse.assetInfo}, Cmd.none)
        GetMetaInfo (Err error) ->
            let
                _ = Debug.log "error when geting meta" (Debug.toString error)
            in
            ( model, Cmd.none)
        CompletedFeedLoad (Ok feed) ->
            let
                list = mapperOfAssets feed.assetPairs
                pagination = { pairs = Paginate.fromList 10 list, reversed = False, query = "", globalId = (Dict.size (feed.assetPairs))}

            in
            ( { model | feed = Loaded feed, feedPair = Just pagination }
              , fetchMetaInfo (Paginate.page pagination.pairs) 
              |> Task.attempt GetMetaInfo
            )

        CompletedFeedLoad (Err error) ->
            let 
               _= Debug.log "error in feed" (Debug.toString error)
            in
            ( { model | feed = Failed }, Cmd.none )

        CompletedTagsLoad (Ok tags) ->
            ( { model | tags = Loaded tags }, Cmd.none )

        CompletedTagsLoad (Err error) ->
            ( { model | tags = Failed }
            , Log.error
            )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )

        GoTo index ->
            case model.feedPair of
                Just pgnation -> 
                    let 
                        old_pagination = pgnation
                        new_pagination = { old_pagination | pairs = Paginate.goTo index old_pagination.pairs }
                    in
                    ({ model | feedPair = Just new_pagination}
                    , fetchMetaInfo (Paginate.page new_pagination.pairs) 
                      |> Task.attempt GetMetaInfo
                    )
                Nothing ->
                    (model, Cmd.none)

        Next ->
            case model.feedPair of
                Just pgnation -> 
                    let 
                        old_pagination = pgnation
                        new_pagination = { old_pagination | pairs = Paginate.next old_pagination.pairs }
                    in
                    ({ model | feedPair = Just new_pagination}
                     , fetchMetaInfo (Paginate.page new_pagination.pairs) 
                      |> Task.attempt GetMetaInfo
                     ) 
                Nothing ->
                    (model, Cmd.none)
        Prev ->
            case model.feedPair of
                Just pgnation -> 
                    let 
                        old_pagination = pgnation
                        new_pagination = { old_pagination | pairs = Paginate.prev  old_pagination.pairs }
                    in
                    ({ model | feedPair = Just new_pagination}, Cmd.none)
                Nothing ->
                    (model, Cmd.none)


        First ->
            case model.feedPair of
                Just pgnation -> 
                    let 
                        old_pagination = pgnation
                        new_pagination = { old_pagination | pairs = Paginate.first old_pagination.pairs }
                    in
                    ({ model | feedPair = Just new_pagination}, Cmd.none)
                Nothing ->
                    (model, Cmd.none)


        Last ->
             case model.feedPair of
                Just pgnation -> 
                    let 
                        old_pagination = pgnation
                        new_pagination = { old_pagination | pairs = Paginate.last old_pagination.pairs }
                    in
                    ({ model | feedPair = Just new_pagination}, Cmd.none)
                Nothing ->
                    (model, Cmd.none)


        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                feed =
                    case model.feed of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other

                tags =
                    case model.tags of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | feed = feed, tags = tags }, Cmd.none )



-- HTTP

assetsToNamesCsv : List AssetPair -> String
assetsToNamesCsv assetList = 
    let
       
       cnct = 
          \original pair -> 
            let
               new_info_2= {alternate_name = original.info.alternate_name ++ ","++pair.info.alternate_name} 
            in  
            { info = new_info_2}
       new_info = {alternate_name = "ZECUSD"}
       result = 
           List.foldr (cnct) {info = new_info} assetList
       _ = Debug.log "csv: " result.info.alternate_name
    in
    result.info.alternate_name
    

fetchMetaInfo : List AssetPair -> Task Http.Error (KrakenMetaResponse)
fetchMetaInfo assetPair =
    let
        request =
            Api.get (Endpoint.kraken_meta_info ([Url.Builder.string "pair" (assetsToNamesCsv assetPair)])) Nothing decoderKrakenMeta
        _ = Debug.log "the Request" (Debug.toString request)
    in
    Http.toTask request



fetchFeed : Session -> Int -> Task Http.Error (KrakenResponse)
fetchFeed session  page =
    let
        maybeCred =
            Session.cred session

        request =
            Api.get Endpoint.asset_pairs Nothing  decoderKraken
        _ = Debug.log "the Request" (Debug.toString request)
    in
    Http.toTask request


articlesPerPage : Int
articlesPerPage =
    10


scrollToTop : Task x ()
scrollToTop =
    Dom.setViewport 0 0
        -- It's not worth showing the user anything special if scrolling fails.
        -- If anything, we'd log this to an error recording service.
        |> Task.onError (\_ -> Task.succeed ())



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

-- EXPORT

toSession : Model -> Session
toSession model =
    model.session
