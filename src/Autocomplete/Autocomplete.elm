module Autocomplete.Autocomplete
    exposing
        ( HtmlDetails
        , KeySelected
        , MouseSelected
        , Msg
        , SectionConfig
        , SectionNode
        , State
        , UpdateConfig
        , ViewConfig
        , ViewWithSectionsConfig
        , empty
        , reset
        , resetToFirstItem
        , resetToLastItem
        , sectionConfig
        , subscription
        , update
        , updateConfig
        , view
        , viewConfig
        , viewWithSections
        , viewWithSectionsConfig
        )

import Char exposing (KeyCode)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode
import Keyboard


-- MODEL


type alias State data =
    { key : Maybe data
    , mouse : Maybe data
    }


type alias KeySelected =
    Bool


type alias MouseSelected =
    Bool


empty : State data
empty =
    { key = Nothing, mouse = Nothing }


reset : UpdateConfig msg data -> State data -> State data
reset { separateSelections } { key, mouse } =
    if separateSelections then
        { key = Nothing, mouse = mouse }
    else
        empty


resetToFirstItem : UpdateConfig msg data -> List data -> Int -> State data -> State data
resetToFirstItem config data howManyToShow state =
    resetToFirst config (List.take howManyToShow data) state


resetToFirst : UpdateConfig msg data -> List data -> State data -> State data
resetToFirst config data state =
    let
        { toKeyedItemId, separateSelections } =
            config

        setFirstItem datum newState =
            { newState | key = Just datum }
    in
    case List.head data of
        Nothing ->
            empty

        Just datum ->
            if separateSelections then
                reset config state
                    |> setFirstItem datum
            else
                empty
                    |> setFirstItem datum


resetToLastItem : UpdateConfig msg data -> List data -> Int -> State data -> State data
resetToLastItem config data howManyToShow state =
    let
        reversedData =
            List.reverse <| List.take howManyToShow data
    in
    resetToFirst config reversedData state



-- UPDATE


{-| Add this to your `program`s subscriptions to animate the spinner.
-}
subscription : Sub (Msg data)
subscription =
    Keyboard.downs KeyDown


type Msg data
    = KeyDown KeyCode
    | WentTooLow
    | WentTooHigh
    | MouseEnter data
    | MouseLeave data
    | MouseClick data
    | NoOp


type alias UpdateConfig msg data =
    { onKeyDown : KeyCode -> Maybe data -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : data -> Maybe msg
    , onMouseLeave : data -> Maybe msg
    , onMouseClick : data -> Maybe msg
    , toKeyedItemId : data -> String
    , separateSelections : Bool
    }


updateConfig :
    { toKeyedItemId : data -> String
    , onKeyDown : KeyCode -> Maybe data -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : data -> Maybe msg
    , onMouseLeave : data -> Maybe msg
    , onMouseClick : data -> Maybe msg
    , separateSelections : Bool
    }
    -> UpdateConfig msg data
updateConfig { toKeyedItemId, onKeyDown, onTooLow, onTooHigh, onMouseEnter, onMouseLeave, onMouseClick, separateSelections } =
    { toKeyedItemId = toKeyedItemId
    , onKeyDown = onKeyDown
    , onTooLow = onTooLow
    , onTooHigh = onTooHigh
    , onMouseEnter = onMouseEnter
    , onMouseLeave = onMouseLeave
    , onMouseClick = onMouseClick
    , separateSelections = separateSelections
    }


update : UpdateConfig msg data -> Msg data -> Int -> State data -> List data -> ( State data, Maybe msg )
update config msg howManyToShow state items =
    case msg of
        KeyDown keyCode ->
            let
                boundedList =
                    items
                        |> List.take howManyToShow

                newKey =
                    navigateWithKey keyCode boundedList state.key
            in
            if newKey == state.key && keyCode == 38 then
                update config WentTooHigh howManyToShow state items
            else if newKey == state.key && keyCode == 40 then
                update config WentTooLow howManyToShow state items
            else if config.separateSelections then
                ( { state | key = newKey }
                , config.onKeyDown keyCode newKey
                )
            else
                ( { key = newKey, mouse = newKey }
                , config.onKeyDown keyCode newKey
                )

        WentTooLow ->
            ( state
            , config.onTooLow
            )

        WentTooHigh ->
            ( state
            , config.onTooHigh
            )

        MouseEnter item ->
            ( resetMouseStateWithId config.separateSelections item state
            , config.onMouseEnter item
            )

        MouseLeave item ->
            ( resetMouseStateWithId config.separateSelections item state
            , config.onMouseLeave item
            )

        MouseClick item ->
            ( resetMouseStateWithId config.separateSelections item state
            , config.onMouseClick item
            )

        NoOp ->
            ( state, Nothing )


resetMouseStateWithId : Bool -> data -> State data -> State data
resetMouseStateWithId separateSelections item state =
    if separateSelections then
        { key = state.key, mouse = Just item }
    else
        { key = Just item, mouse = Just item }


getPreviousItem : List data -> data -> data
getPreviousItem items selectedItem =
    Maybe.withDefault selectedItem <| List.foldr (getPrevious selectedItem) Nothing items


getPrevious : data -> data -> Maybe data -> Maybe data
getPrevious item selectedItem resultItem =
    if selectedItem == item then
        Just item
    else if resultItem == Just item then
        Just selectedItem
    else
        resultItem


getNextItem : List data -> data -> data
getNextItem items selectedItem =
    Maybe.withDefault selectedItem <| List.foldl (getPrevious selectedItem) Nothing items


navigateWithKey : Int -> List data -> Maybe data -> Maybe data
navigateWithKey code items maybeItem =
    case code of
        38 ->
            Maybe.map (getPreviousItem items) maybeItem

        40 ->
            Maybe.map (getNextItem items) maybeItem

        _ ->
            maybeItem


view : ViewConfig data -> Int -> State data -> List data -> Html (Msg data)
view config howManyToShow state data =
    viewList config howManyToShow state data


viewWithSections : ViewWithSectionsConfig data sectionData -> Int -> State data -> List sectionData -> Html (Msg data)
viewWithSections config howManyToShow state sections =
    let
        getKeyedItems section =
            ( config.section.toKeyedItemId section, viewSection config state section )
    in
    Html.Keyed.ul (List.map mapNeverToMsg config.section.ul)
        (List.map getKeyedItems sections)


viewSection : ViewWithSectionsConfig data sectionData -> State data -> sectionData -> Html (Msg data)
viewSection config state section =
    let
        sectionNode =
            config.section.li section

        attributes =
            List.map mapNeverToMsg sectionNode.attributes

        customChildren =
            List.map (Html.map (\html -> NoOp)) sectionNode.children

        getKeyedItems datum =
            ( config.toKeyedItemId datum, viewData config state datum )

        viewItemList =
            Html.Keyed.ul (List.map mapNeverToMsg config.ul)
                (config.section.getData section
                    |> List.map getKeyedItems
                )

        children =
            List.append customChildren [ viewItemList ]
    in
    Html.li attributes
        [ Html.node sectionNode.nodeType attributes children ]


viewData : ViewWithSectionsConfig data sectionData -> State data -> data -> Html (Msg data)
viewData { toKeyedItemId, li } { key, mouse } item =
    let
        listItemData =
            li (isSelected key) (isSelected mouse) item

        customAttributes =
            List.map mapNeverToMsg listItemData.attributes

        customLiAttr =
            List.append customAttributes
                [ Html.Events.onMouseEnter (MouseEnter item)
                , Html.Events.onMouseLeave (MouseLeave item)
                , Html.Events.onClick (MouseClick item)
                ]

        isSelected maybeItem =
            case maybeItem of
                Just someItem ->
                    someItem == item

                Nothing ->
                    False
    in
    Html.li customLiAttr
        (List.map (Html.map (\html -> NoOp)) listItemData.children)


viewList : ViewConfig data -> Int -> State data -> List data -> Html (Msg data)
viewList config howManyToShow state data =
    let
        customUlAttr =
            List.map mapNeverToMsg config.ul

        getKeyedItems datum =
            ( config.toKeyedItemId datum, viewItem config state datum )
    in
    Html.Keyed.ul customUlAttr
        (List.take howManyToShow data
            |> List.map getKeyedItems
        )


viewItem : ViewConfig data -> State data -> data -> Html (Msg data)
viewItem { toKeyedItemId, li } { key, mouse } item =
    let
        listItemData =
            li (isSelected key) (isSelected mouse) item

        customAttributes =
            List.map mapNeverToMsg listItemData.attributes

        customLiAttr =
            List.append customAttributes
                [ Html.Events.onMouseEnter (MouseEnter item)
                , Html.Events.onMouseLeave (MouseLeave item)

                -- NoOp mousedown so it does not trigger a blur event on the input
                , Html.Events.onWithOptions "mousedown" { stopPropagation = False, preventDefault = True } (Json.Decode.succeed NoOp)
                , Html.Events.onClick (MouseClick item)
                ]

        isSelected maybeItem =
            case maybeItem of
                Just someItem ->
                    someItem == item

                Nothing ->
                    False
    in
    Html.li customLiAttr
        (List.map (Html.map (\html -> NoOp)) listItemData.children)


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


type alias ViewConfig data =
    { toKeyedItemId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    }


type alias ViewWithSectionsConfig data sectionData =
    { toKeyedItemId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , section : SectionConfig data sectionData
    }


type alias SectionConfig data sectionData =
    { toKeyedItemId : sectionData -> String
    , getData : sectionData -> List data
    , ul : List (Attribute Never)
    , li : sectionData -> SectionNode Never
    }


type alias SectionNode msg =
    { nodeType : String
    , attributes : List (Attribute msg)
    , children : List (Html msg)
    }


viewConfig :
    { toKeyedItemId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    }
    -> ViewConfig data
viewConfig { toKeyedItemId, ul, li } =
    { toKeyedItemId = toKeyedItemId
    , ul = ul
    , li = li
    }


viewWithSectionsConfig :
    { toKeyedItemId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , section : SectionConfig data sectionData
    }
    -> ViewWithSectionsConfig data sectionData
viewWithSectionsConfig { toKeyedItemId, ul, li, section } =
    { toKeyedItemId = toKeyedItemId
    , ul = ul
    , li = li
    , section = section
    }


sectionConfig :
    { toKeyedItemId : sectionData -> String
    , getData : sectionData -> List data
    , ul : List (Attribute Never)
    , li : sectionData -> SectionNode Never
    }
    -> SectionConfig data sectionData
sectionConfig { toKeyedItemId, getData, ul, li } =
    { toKeyedItemId = toKeyedItemId
    , getData = getData
    , ul = ul
    , li = li
    }



-- HELPERS


mapNeverToMsg : Attribute Never -> Attribute (Msg data)
mapNeverToMsg msg =
    Html.Attributes.map (\_ -> NoOp) msg
