import "./main.css"
import Elm from "./App.elm"

const root = document.getElementById("root")
const app = Elm.App.embed(root)

app.ports.sendFile.subscribe(function() {
  const result = "aaaaaaaaaaa"
  app.ports.fileSended.send(result)
})
