module Art exposing (..)

import Random exposing (..)
import Time exposing (..)
import Html exposing (Html, text, div, img)
import Html.Attributes as Hattr exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as Lex exposing (..)
import Task exposing (..)

--MODEL -- 

type alias Model = 
    { eraseableText : List EraseableWord 
    , seed : Random.Seed 
    , status : Status
    }

type alias EraseableWord =
    { text: String
    , erased: Bool
    , position: Int
    }

type Status 
    = Erasing 
    | BringingBack

initModel: Model 
initModel = 
    { eraseableText = (textToEraseableWords theText)
    , seed = Random.initialSeed 1
    , status = Erasing
    }

init : ( Model, Cmd Msg )
init =
    ( initModel, now )


theText: String
theText = 
    """
    In dreams (Coleridge writes), images take the shape of the effects we believe they cause. We are not terrified because some sphinx 
    is threatening us but rather dream of a sphinx in order to explain the terror we are feeling. If this is the case, how can a simple 
    account of such imaginings communicate the dread and the thrills, the adventure, anxieties, and joys conjured by last night's dream? 
    I am going to attempt to do this all the same. Perhaps the fact that the entire dream consisted of a single scene will erase or 
    ease this fundamental difficulty. \n \n 
    It took place in the Humanities Building, at dusk. As often happens in dreams, everything was somehow different: everything had been 
    affected by a slight enlargement. We were electing people to committees. I was chatting with Pedro Henriquez Urena, who in reality 
    has been dead for many years. Suddenly we were assaulted by the racket of a street band or a demonstration. The cries of people and 
    animals reached us from the Lower City. A voice cried: "Here they come!" then: "It's the Gods!" Four or five individuals emerged from 
    the mob and took their places on the stage of the lecture hall. We all cheered, weeping: it was the Gods, coming back after centuries 
    of exile. The stage made them taller: they threw their heads back and thrust their chests forward in haughty acceptance of our homage. \n \n 
    One of them was holding a bough of the kind no doubt required by the simplistic botany of dreams; another made a broad gesture 
    with his hand, which was a claw; one of Janus's faces looked apprehensively at the curving beak of Thoth. Stirred perhaps by our 
    cheers, another one–I'm no longer sure which one–broke out in triumphant but incredibly harsh clacking, complete with gargles and 
    whistles. From that point on, things began to change. \n \n 
    It was all due to our perhaps precipitous suspicion that the Gods did not know how to talk. Hundreds of years of living like animals 
    on the run had atrophied their human dimension. The moon of Islam and the Roman Cross had been merciless with these fugitives. The 
    decadence of the Olympic bloodline was evident in their beetling brows, yellowed teeth, patchy half-breed or Chinese whiskers, and bestial 
    protruding lips. Their clothing spoke not of genteel poverty but of the flashy bad taste of the Lower City's back rooms and bordellos.  \n \n 
    A carnation bled from one buttonhole; we detected the outline of a dagger under a tight-fitting jacket. All at once we sensed that 
    they were playing their last card, that they had grown sly, stultified and cruel like aging beasts of prey, and that they would destroy 
    us if we allowed ourselves to be swayed by fear or pity.  \n \n 
    We drew our heavy pistols (in the dream, they just appeared) and cheerfully put the Gods to death.
    """

textToEraseableWords: String -> List EraseableWord
textToEraseableWords inputText = 
    let 
        rawWordsArray = String.split " " inputText
    in 
        List.map2 createWord rawWordsArray (List.range 1 <| List.length <| rawWordsArray)



createWord: String -> Int -> EraseableWord
createWord string int = 
    EraseableWord string False int 

--TODO create seed based on time in init 


---- UPDATE ----

eraseOrBringBack: EraseableWord -> EraseableWord 
eraseOrBringBack word = 
    case word.erased of 
        True -> 
            EraseableWord word.text False word.position
        False -> 
            EraseableWord word.text True word.position

hasPosition: Int -> EraseableWord -> Bool 
hasPosition int word = 
    if word.position == int then 
        True 
    else 
        False 

now : Cmd Msg
now = 
  Task.perform (Just >> GetSeed) Time.now

type Msg
    = GetSeed (Maybe Time)
    | ToggleWord

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 

        GetSeed (Just time) -> 
            let 
                timeSeed = round (Time.inSeconds time) |> Random.initialSeed
            in 
                ({model | seed = timeSeed}, Cmd.none)

        GetSeed Nothing -> --This would be an error, but I am not too concerned with it... I have a fallback seed
            model ! [ ]

        ToggleWord -> 
            model ! [ ]

currentErasedWords: Model -> List EraseableWord 
currentErasedWords model = List.filter (.erased) model.eraseableText

currentAmountErased: Model -> Int 
currentAmountErased model = List.length (currentErasedWords model) 

totalNumberOfWords: Model -> Int 
totalNumberOfWords model = List.length model.eraseableText 

randomIndex: Model -> Int
randomIndex model = randomIndexAndSeed model |> Tuple.first

newSeed: Model -> Random.Seed 
newSeed model = randomIndexAndSeed model |> Tuple.second

eraseAWord: Model -> Model 
eraseAWord model =
    let 
        erasedWord = eraseAtIndex (randomIndex model) model.eraseableText
    in 
        case erasedWord of 
            Just newWords -> 
                { model | 
                    eraseableText = newWords, 
                    seed = (newSeed model)
                }
            _ -> 
                { model | seed = (newSeed model) } --DEBUG?) 


bringBackAWord: Model -> Model
bringBackAWord model = 
            let 
                broughtBackWord = bringBackAtIndex (randomIndex model)  model.eraseableText
            in 
                case broughtBackWord of 
                    Just newWords -> 
                        { model | 
                            eraseableText = newWords, 
                            seed = (newSeed model)
                        }
                    _ -> 
                        { model | seed = (newSeed model) } --DEBUG?


randomIndexAndSeed: Model -> (Int, Seed) 
randomIndexAndSeed model = 
    let 
        total: Int 
        total = totalNumberOfWords model

        seed: Random.Seed
        seed = model.seed
    in 
        Random.step (Random.int 0 ((totalNumberOfWords model)-1)) seed

eraseAtIndex: Int -> List EraseableWord -> Maybe (List EraseableWord)
eraseAtIndex index words = 
    let 
        wordAtIndex = Lex.getAt index words

    in 
        case wordAtIndex of  
            Just word -> 
                case word.erased of 
                    True -> 
                        Just words 
                    False -> 
                        Lex.setAt index (eraseOrBringBack word) words
            Nothing -> 
                Debug.crash "No word at that index."

bringBackAtIndex: Int -> List EraseableWord -> Maybe (List EraseableWord)
bringBackAtIndex index words = 
    let 
        wordAtIndex = Lex.getAt index words

    in 
        case wordAtIndex of  
            Just word -> 
                case word.erased of 
                    True -> 
                        Lex.setAt index (eraseOrBringBack word) words
                    False -> 
                        Just words
            Nothing -> 
                Debug.crash "No word at that index."


isErased: EraseableWord -> Bool 
isErased word = 
    word.erased == True

isNotErased: EraseableWord -> Bool 
isNotErased word = 
    word.erased == False


--VIEW --

myStyles: List (Html.Attribute Msg) 
myStyles = 
    List.singleton 
        (style 
            [ ("font-family", "Georgia")
            , ("font-size", "20px")
            , ("display", "inline-block")
            , ("margin", "auto")
            ] )


allTextDisplayed: Model -> Html Msg
allTextDisplayed model = 
    div myStyles (List.map displayEraseableWord model.eraseableText) 

view: Model -> Html Msg 
view model = 
    div [] ([allTextDisplayed model])


displayEraseableWord: EraseableWord -> Html Msg 
displayEraseableWord word = 
    Html.span 
        ([onClick (ToggleWord),  (style [("color", (wordColor word))]) ])
        [Html.text (word.text ++ " ") ]


wordColor: EraseableWord -> String
wordColor word = 
    case word.erased of 
        True -> 
            "whitesmoke"
        False -> 
            "black"
-- MAIN -- 

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

