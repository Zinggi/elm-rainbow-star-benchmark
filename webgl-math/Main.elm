module Main exposing (..)

import Color exposing (..)
import WebGLTypes as GL
import Vector3 as V3 exposing (Float3)
import Matrix4 as M4 exposing (Float4x4)
import WebGL exposing (..)
import Html.Events exposing (onClick)
import Html exposing (Html, div, button, text)
import AnimationFrame
import String
import Html.Attributes exposing (width, height)


type alias Model =
    { time : Float, cubes : Int, dt : Float, dtDebounce : Float }


type Msg
    = Tick Float
    | AddCubes Int


main : Program Never Model Msg
main =
    Html.program
        { init = ( Model 0 1 0 0, Cmd.none )
        , view = view
        , subscriptions = (\model -> AnimationFrame.diffs Tick)
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                dtDebounce =
                    model.dtDebounce + dt

                time =
                    model.time + dt / 1000
            in
                if dtDebounce > 200 then
                    { model | time = time, dtDebounce = 0, dt = dt } ! []
                else
                    { model | time = time, dtDebounce = dtDebounce } ! []

        AddCubes n ->
            { model | cubes = max 1 (model.cubes + n) } ! []


view : Model -> Html Msg
view { time, cubes, dt } =
    div []
        [ button [ onClick (AddCubes 10) ] [ text "10 more cubes" ]
        , button [ onClick (AddCubes 100) ] [ text "100 more" ]
        , button [ onClick (AddCubes -10) ] [ text "10 less cubes" ]
        , button [ onClick (AddCubes -100) ] [ text "100 less cubes" ]
        , div [] [ text ("Number of cubes: " ++ toString cubes) ]
        , div [] [ text ("dt: " ++ formatFloat dt ++ "ms") ]
        , div [] [ text ("fps: " ++ formatFloat (1000 / dt)) ]
        , WebGL.toHtml [ width 900, height 900 ] (scene cubes time)
        ]


formatFloat a =
    toString (toFloat (round (1000 * a)) / 1000)
        |> String.padRight 6 ' '



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
