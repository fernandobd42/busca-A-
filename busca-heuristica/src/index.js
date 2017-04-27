import "./main.css"
import Elm from "./App.elm"

const root = document.getElementById("root")
const app = Elm.App.embed(root)

app.ports.sendFile.subscribe(function() {
    const reader = new FileReader()
    const filePath = document.getElementById("idFilePath")  
    reader.readAsText(filePath.files[0])

    reader.onload = function(e) {
      console.log(reader.result)
      app.ports.fileSended.send(reader.result)
    }

})
