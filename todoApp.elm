module MyTodoApp where

import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Signal          exposing (..)
import StartApp.Simple as StartApp

type alias Model =
  { title: String, tasks: List Task, uid : Int}

type alias Task =
  { title: String, content: String, isDone: Bool , id: Int}

initialModel : Model
initialModel =
  { title="my tasklist"
    ,tasks = [
      {title = "tache1", content = "content tache 1", isDone= True, id=1},
      {title = "tache2", content = "content tache 2", isDone= False, id=2}
      ]
    ,uid = 2
  }

type Action = Add String String | Delete Int | MarkDone Int Bool

{-
createNewTask : String -> String -> Bool -> Int -> Task
createNewTask ttle content compStatus current_index
    {title = ttle, content = content, isDone = compStatus, id = (current_index +1)}
-}

update : Action -> Model -> Model
update action updateList =
  case action of
    Add title content ->
        {updateList | tasks = (updateList.tasks ++ [{title= title, content= content, isDone = False, id= updateList.uid + 1}])}
    Delete t_id ->
        {updateList | tasks = List.filter (\t -> t.id /= t_id) updateList.tasks}
    MarkDone id completionStatus ->
        let markTask t = if t.id == id then { t | isDone = completionStatus } else t
        in
            {updateList | tasks = List.map markTask updateList.tasks}


view : Address Action -> Model -> Html
view address model =
  div []
    [ --button [ onClick address (Mark "•") ] [ text "Mark" ],
      h2 [] [ text(model.title ++ " – "), text (model.tasks |> List.length |> toString), text " tâches" ],
      ul [] (List.map (\el -> taskItem el address) model.tasks),
      label[for "ajout-titre"][
      text "Titre",
      input[type' "text", id "ajout-titre", placeholder "titre"][]
      ],
      label[for "ajout-contenu"][
      text "Contenu",
      input[type' "text", id "ajout-contenu", placeholder "contenu"][]
      ],
      button [type' "button"][text "➕"]
    ]

taskItem taskElt address =
    li[] [
        p [][text taskElt.title],
        p [][text taskElt.content],
        p [][text (taskElt.isDone |> toString) ],
        button [type' "button"
                ,onClick address (MarkDone taskElt.id True)][text "✅"],
        button [type' "button"
                ,onClick address (Delete taskElt.id)][text "❌"]
        ]

main : Signal Html
main =
  StartApp.start
    { model = initialModel,
      view = view,
      update = update }
