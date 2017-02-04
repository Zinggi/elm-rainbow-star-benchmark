module Main exposing (..)

import WebGLTypes as GL
import Matrix4 as M4 exposing (Float4x4)
import WebGL exposing (..)
import Html exposing (Html, div, button, text)
import AnimationFrame
import Html.Attributes exposing (width, height)
import Plot exposing (..)
import Plot.Grid as Grid
import Plot.Line as Line
import Plot.Axis as Axis


type alias Model =
    { time : Float
    , cubes : Int
    , dt : Float
    , frames : Int
    , results : List Plot.Point
    }


type Msg
    = Tick Float


main : Program Never Model Msg
main =
    Html.program
        { init = ( Model 0 100 0 0 [], Cmd.none )
        , view = view
        , subscriptions = (\model -> AnimationFrame.diffs Tick)
        , update = update
        }


maxCubes : Int
maxCubes =
    2000


maxFrames : Int
maxFrames =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                time =
                    model.time + dt / 1000

                ( dt_, cubes, frames, results ) =
                    if model.frames == maxFrames && model.cubes <= maxCubes then
                        ( 0, model.cubes + 100, 0, ( toFloat model.cubes, toFloat maxFrames / model.dt * 1000 ) :: model.results )
                    else
                        ( model.dt + dt, model.cubes, model.frames + 1, model.results )
            in
                { model
                    | time = time
                    , cubes = cubes
                    , frames = frames
                    , dt = dt_
                    , results = results
                }
                    ! []


view : Model -> Html Msg
view { time, cubes, results } =
    if cubes <= maxCubes then
        WebGL.toHtml [ width 900, height 900 ] (scene cubes time)
    else
        plot
            [ size ( 800, 400 )
            , margin ( 20, 20, 40, 40 )
            ]
            [ verticalGrid
                [ Grid.lines
                    [ Line.stroke "lightgray" ]
                ]
            , horizontalGrid
                [ Grid.lines
                    [ Line.stroke "lightgray" ]
                ]
            , xAxis
                [ Axis.line [ Line.stroke "gray" ]
                , Axis.tickDelta 200
                ]
            , yAxis
                [ Axis.line [ Line.stroke "gray" ]
                , Axis.tickDelta 5
                ]
            , line
                [ Line.stroke "blue"
                , Line.strokeWidth 2
                ]
                (List.reverse results)
            ]



-- VIEW


scene : Int -> Float -> List Entity
scene cubes time =
    List.range 1 cubes
        |> List.map (\i -> entity vertexShader fragmentShader cube (uniforms time i))


uniforms : Float -> Int -> { transform : GL.Mat4, perspective : GL.Mat4, camera : GL.Mat4, shade : Float, color : Float }
uniforms t index =
    let
        i =
            toFloat index

        rotation =
            M4.mul (M4.makeRotate (1.5 * t + i * 0.1) ( 0, 1, 0 )) (M4.makeRotate (t + i * 0.3) ( 1, 0, 0 ))

        r =
            4 + 0.5 * sin (5 * t + (0.02 * i) * 5)

        transform =
            M4.mul (M4.makeTranslate ( r * cos (0.02 * i), r * sin (0.02 * i), 0 )) rotation

        perspective =
            M4.makePerspective 45 1 0.01 100

        camera =
            M4.makeLookAt ( 20 * sin t, 0, 20 * cos t ) ( 0, 0, 0 ) ( 0, 1, 0 )
    in
        { transform = GL.fromFloat4x4 <| transform
        , perspective = GL.fromFloat4x4 <| perspective
        , camera = GL.fromFloat4x4 <| camera
        , shade = 0.8
        , color = i * 0.01213
        }



-- SHADERS


vertexShader : Shader { attr | position : GL.Vec3 } { unif | transform : GL.Mat4, perspective : GL.Mat4, camera : GL.Mat4, color : Float } { vcolor : GL.Vec3 }
vertexShader =
    [glsl|

attribute vec3 position;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 transform;
//attribute vec3 color;
uniform float color;

varying vec3 vcolor;

vec3 hsv2rgb(float h, float s, float v) {
    vec3 c = vec3(h,s,v);
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

void main () {
    gl_Position = perspective * camera * transform *  vec4(position, 1.0);
    //vcolor = color;
    vcolor = hsv2rgb(color, 0.8, 0.8);
}

|]


fragmentShader : Shader {} { u | shade : Float } { vcolor : GL.Vec3 }
fragmentShader =
    [glsl|

precision mediump float;

uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]



-- MESHES


type alias Vertex =
    { position : GL.Vec3
    }


cube : Mesh Vertex
cube =
    let
        rft =
            GL.fromFloat3 ( 1, 1, 1 )

        -- right, front, top
        lft =
            GL.fromFloat3 ( -1, 1, 1 )

        -- left,  front, top
        lbt =
            GL.fromFloat3 ( -1, -1, 1 )

        rbt =
            GL.fromFloat3 ( 1, -1, 1 )

        rbb =
            GL.fromFloat3 ( 1, -1, -1 )

        rfb =
            GL.fromFloat3 ( 1, 1, -1 )

        lfb =
            GL.fromFloat3 ( -1, 1, -1 )

        lbb =
            GL.fromFloat3 ( -1, -1, -1 )
    in
        triangles
            << List.concat
        <|
            [ face rft rfb rbb rbt
              -- right
            , face rft rfb lfb lft
              -- front
            , face rft lft lbt rbt
              -- top
            , face rfb lfb lbb rbb
              -- bottom
            , face lft lfb lbb lbt
              -- left
            , face rbt rbb lbb lbt
              -- back
            ]


face : GL.Vec3 -> GL.Vec3 -> GL.Vec3 -> GL.Vec3 -> List ( Vertex, Vertex, Vertex )
face a b c d =
    [ ( Vertex a, Vertex b, Vertex c )
    , ( Vertex c, Vertex d, Vertex a )
    ]
