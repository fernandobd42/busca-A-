window.onload = function() {
  var fileInput = document.getElementById("input")

  fileInput.addEventListener("change", function(e) {
    var file = fileInput.files[0]
    var textType = /text.*/

    if (file.type.match(textType)) {
      var reader = new FileReader()

      reader.onload = function(e) {
        console.log(reader.result)
      }

      reader.readAsText(file)
    } else {
      console.log("File not supported!")
    }
  })
}
