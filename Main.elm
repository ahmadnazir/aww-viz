module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { boxes : List Box
    , drag : Maybe Drag
    }


type alias Box =
    { position : Position
    , id : Int
    }


type alias BoxId =
    Int


type alias Drag =
    { start : Position
    , current : Position
    , id : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model [ { id = 1, position = (Position 200 200) }, { id = 2, position = (Position 500 500) } ] Nothing, Cmd.none )



-- UPDATE


type Msg
    = DragStart BoxId Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({ boxes, drag } as model) =
    case msg of
        DragStart id xy ->
            { model | drag = Just (Drag xy xy id) }

        DragAt xy ->
            { model | drag = Maybe.map (\{ start, id } -> Drag start xy id) drag }

        DragEnd _ ->
            { model | boxes = List.map (dragEnd model.drag) model.boxes, drag = Nothing }


dragEnd : Maybe Drag -> Box -> Box
dragEnd drag box =
    case drag of
        Nothing ->
            box

        Just { id, current} ->
            if id == box.id then
                { box | position = getPosition drag box }
            else
                box



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


(=>) =
    (,)


viewBox : Maybe Drag -> Box -> Html Msg
viewBox drag box =
    let
        realPosition =
            getPosition drag box
    in
        div
            [ onMouseDown box.id
            , style
                [ "background-color" => "#3C8D2F"
                , "cursor" => "move"
                , "width" => "100px"
                , "height" => "100px"
                , "border-radius" => "4px"
                , "position" => "absolute"
                , "left" => px realPosition.x
                , "top" => px realPosition.y
                , "color" => "white"
                , "display" => "flex"
                , "align-items" => "center"
                , "justify-content" => "center"
                ]
            ]
            [ text "Drag Me!"
            ]


view : Model -> Html Msg
view model =
    div []
        (List.map (viewBox model.drag)
            model.boxes
        )


px : Int -> String
px number =
    toString number ++ "px"


getPosition : Maybe Drag -> Box -> Position
getPosition drag box =
    case drag of
        Nothing ->
            box.position

        Just { start, current, id } ->
            if id == box.id then
                Position
                    (box.position.x + current.x - start.x)
                    (box.position.y + current.y - start.y)
            else
                box.position


onMouseDown : BoxId -> Attribute Msg
onMouseDown id =
    on "mousedown" (Decode.map (DragStart id) Mouse.position)
