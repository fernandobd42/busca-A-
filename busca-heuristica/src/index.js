import "./main.css"
import Elm from "./App.elm"
import wallImg from './img/wall.png'
import groundImg from './img/ground.png'
import mouseImg from './img/mouse.png'
import cheeseImg from './img/cheese.png'
import doorImg from './img/door.png'
import walkedGround from './img/walkedGround.png'

const root = document.getElementById("root")
const app = Elm.App.embed(root, {wallImg, groundImg, mouseImg, cheeseImg, doorImg, walkedGround})

app.ports.sendFile.subscribe( function() {
    const reader = new FileReader()
    const filePath = document.getElementById("idFilePath")  
    reader.readAsText(filePath.files[0])

    reader.onload = function(e) {
      console.log(reader.result)
      app.ports.fileSended.send(reader.result)
    }

})
