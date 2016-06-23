module Main exposing (main)

import AnimationFrame
import Html exposing (Html, div, text, button, input)
import Html.App as App
import Html.Attributes as Attrs
import Html.Events exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector2 as Vec2 exposing (Vec2)
import Mouse
import Time exposing (Time, inMilliseconds)
import WebGL exposing (..)
import List exposing (map, map2)

type alias Position =
    { x : Int, y : Int }

vecPosition : Position -> Vec2
vecPosition pos =
    Vec2.vec2 (toFloat pos.x) (toFloat pos.y)

dim = Position 400 300

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { time : Time
    , position : Position
    , subscribeMouse : Bool
    }

init : (Model, Cmd Msg)
init =
    (Model 0 (Position 0 0) False, Cmd.none)


-- UPDATE

type Msg
  = Tick Time
  | MousePos Position
  | SubMousePos Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            ({model | time = newTime}, Cmd.none)
        MousePos pos ->
            ({model | position = pos}, Cmd.none)
        SubMousePos active ->
            ({model | subscribeMouse = active}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subscriptions = [ AnimationFrame.times Tick ]
    in
        if model.subscribeMouse then
            Sub.batch
                ((Mouse.moves MousePos) :: subscriptions)
        else
            Sub.batch subscriptions



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [ onMouseEnter <| SubMousePos True
          , onMouseLeave <| SubMousePos False
          , Attrs.style [("width", (toString dim.x) ++ "px")]
          ]
          [webglView model]
    , div [] [text <| toString model.position ]
    ]


webglView : Model -> Html Msg
webglView model =
    model |>
    scene |>
    toHtml [Attrs.width dim.x, Attrs.height dim.y]

scene : Model -> List Renderable
scene model =
  [ render vertexShader fragmentShader scratch (uniforms model.time) ]

type alias Vertex =
  { pos : Vec3 }

scratch : Drawable Vertex
scratch =
  Triangle
    [ ( { pos = Vec3.vec3 1 1 0 }
      , { pos = Vec3.vec3 -1 -1 0 }
      , { pos = Vec3.vec3 -1 1 0 }
      )
    , ( { pos = Vec3.vec3 1 1 0 }
      , { pos = Vec3.vec3 -1 -1 0 }
      , { pos = Vec3.vec3 1 -1 0 }
      )
    ]

vertexShader : Shader Vertex { u | u_time:Float, u_resolution: Vec2 } {}

vertexShader = [glsl|
  precision mediump float;
  attribute vec3 pos;
  void main() {
    gl_Position = vec4(pos, 1);
  }
|]

fragmentShader : Shader {} { u | u_time:Float, u_resolution: Vec2 } {}
fragmentShader = [glsl|
  precision mediump float;
  uniform float u_time;
  uniform vec2 u_resolution;

  // Plot a line on Y using a value between 0.0-1.0
  float plot(vec2 st, float pct) {
  return smoothstep( pct-0.02, pct, st.y) -
         smoothstep( pct, pct+0.02, st.y);
  }

  void main() {
  vec2 st = gl_FragCoord.xy/u_resolution;

  float y = st.x;

  vec3 color = vec3(y);

  // Plot a line
  float pct = plot(st,y);
  color = (1.0-pct)*color+pct*vec3(0.0,1.0,0.0);
  gl_FragColor = vec4(color,1.0);
}
|]

uniforms: Time -> {u_time: Float, u_resolution: Vec2}
uniforms time =
    { u_time = (milli time)
    , u_resolution = vecPosition dim
    }

mod1000 : Int -> Int
mod1000 n =
    n % 1000

milli : Time -> Float
milli =
    inMilliseconds >> truncate >> mod1000 >> toFloat
