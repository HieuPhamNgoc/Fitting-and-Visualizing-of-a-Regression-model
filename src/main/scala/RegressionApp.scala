import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.embed.swing.SwingFXUtils
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._
import scalafx.scene.text.Font
import scalafx.stage.{FileChooser, Stage}

import javax.imageio.ImageIO
import scala.util.Random

object RegressionApp extends JFXApp{
  // Primary stage
  stage = new JFXApp.PrimaryStage{
    title.value = "Regression Model"
    width = 1000
    height = 850
  }

  val root = new Pane
  val scene = new Scene(root)
  stage.scene = scene

  stage.setResizable(false)

  //Primary stage drawing

  val canvas = new Canvas(1000, 850)
  val g = canvas.graphicsContext2D
  g.fill = Black
  g.font = new Font(35)
  g.fillText("FITTING REGRESSION MODEL", 270, 330.5)

  root.children += canvas

  //Alert for unsupported file
  val alert = new Alert(AlertType.Error) {
    initOwner(stage)
    title = "Error Reading File"
    headerText = "Not a supported file"
    contentText = "Please choose another type of file (CSV or JSON)"
  }


  // Importing a file
  val importButton = new Button("Import a file")
   // Customizing "Import File" button
  importButton.minHeight = 50
  importButton.minWidth = 400
  importButton.layoutX = 290
  importButton.layoutY = 400
  importButton.underline = true
  root.children += importButton
  importButton.onAction = (event) => {
    val fileChooser = new FileChooser
    val selectedText = fileChooser.showOpenDialog(stage)
    if (selectedText != null) {
    val fileName = selectedText.getCanonicalPath

    // Reading csv file
    if (fileName.toLowerCase.endsWith(".csv")) {
      val reader = new CSVReader(fileName)
      val data = reader.data
      val readWindow = new Stage{
        title = "Read Data and Table"
        width = 250
        height = 600
      }
      val root = new Pane
      val scene = new Scene(root)
      readWindow.scene = scene

      readWindow.setResizable(false)
      readWindow.show()


      // Drawing table from csv
      val input = ObservableBuffer(data)
      val table = new TableView[Seq[Double]](input)
      val coll1  = new TableColumn[Seq[Double], Double]("x")
      coll1.cellValueFactory = cdf => ObjectProperty(cdf.value.head)
      val coll2 = new TableColumn[Seq[Double], Double]("y")
      coll2.cellValueFactory = cdf => ObjectProperty(cdf.value.last)
      table.columns ++= List(coll1, coll2)
      table.layoutY = 25

      root.children += table
      // Creating choices for regression
      val menuBar = new MenuBar
      val regressionMenu = new Menu("Regression")
      val linearRegressionMenu = new MenuItem("Linear Regression")
      val quadraticRegressionMenu = new MenuItem("Quadratic Regression")

      regressionMenu.items = List(linearRegressionMenu, quadraticRegressionMenu)
      menuBar.menus = List(regressionMenu)

      root.children += menuBar

      // Create options for graph layout
      val label = new Label("Graph layout: Data points - Regression line")
      label.layoutX = 5
      label.layoutY = 430
      root.children += label

      val options = ObservableBuffer("Black - Blue", "Black - Green", "Black - Red", "Blue - Black", "Blue - Green", "Blue - Red", "Green - Black", "Green - Blue",
      "Green - Red", "Red - Black", "Red - Blue", "Red - Green")
      val layoutChoice = new ChoiceBox(options)
      layoutChoice.layoutX = 5
      layoutChoice.layoutY = 450
      root.children += layoutChoice

      // Graphing Linear Regression
      linearRegressionMenu.onAction = (event) => {

        // Regression line data
        val regression = new LinearRegression(data)
        val a = regression.a
        val b = regression.b
        val regressionData = IndexedSeq(Seq(data.minBy(_.head).head - 10, (data.minBy(_.head).head - 10) * a + b), Seq(data.maxBy(_.last).head + 10, (data.maxBy(_.last).head + 10) *a + b))

        val linearData = XYChart.Series[Number, Number]("Linear Regression", ObservableBuffer(regressionData.map(seq => XYChart.Data[Number, Number](seq.head, seq.last))))

        // Graphing
        val plotWindow = new Stage {
          title.value = "Plotting Linear Regression"
          width = 1000
          height = 850
        }

        val xAxis = NumberAxis()
        val yAxis = NumberAxis()
        val dataPointsData = XYChart.Series[Number, Number]("Data Points", ObservableBuffer(data.map(seq => XYChart.Data[Number, Number](seq.head, seq.last))))

        val plot = new LineChart(xAxis, yAxis)
        plot.getData.add(dataPointsData)
        plot.getData.add(linearData)


        val root = new Pane
        root.children += plot
        plot.minHeight = 800
        plot.minWidth = 975

        val scene = new Scene(root)
        plotWindow.scene = scene

        // Setting graph layout
        scene.stylesheets.add("RedBlue.css")

        if (layoutChoice.getValue == "Black - Blue"){
          scene.stylesheets.add("BlackBlue.css")
        }
        else if (layoutChoice.getValue == "Black - Green") {
          scene.stylesheets.add("BlackGreen.css")
        }
        else if (layoutChoice.getValue == "Black - Red") {
          scene.stylesheets.add("BlackRed.css")
        }
        else if (layoutChoice.getValue == "Blue - Black") {
          scene.stylesheets.add("BlueBlack.css")
        }
        else if (layoutChoice.getValue == "Blue - Green") {
          scene.stylesheets.add("BlueGreen.css")
        }
        else if (layoutChoice.getValue == "Blue - Red") {
          scene.stylesheets.add("BlueRed.css")
        }
        else if (layoutChoice.getValue == "Green - Black") {
          scene.stylesheets.add("GreenBlack.css")
        }
        else if (layoutChoice.getValue == "Green - Blue") {
          scene.stylesheets.add("GreenBlue.css")
        }
        else if (layoutChoice.getValue == "Green - Red") {
          scene.stylesheets.add("GreenRed.css")
        }
        else if (layoutChoice.getValue == "Red - Black") {
          scene.stylesheets.add("RedBlack.css")
        }
        else if (layoutChoice.getValue == "Red - Blue") {
          scene.stylesheets.add("RedBlue.css")
        }
        else if (layoutChoice.getValue == "Red - Green") {
          scene.stylesheets.add("RedGreen.css")
        }


        // Showing calculated factors
        val canvas = new Canvas(1000, 850)
        val g = canvas.graphicsContext2D
        g.fill = White
        g.fillRect(100, 80, 0, 0)
        g.fill = Black
        g.font = new Font(20)
        g.fillText(s" a: ${a}\n b: ${b}\n R^2: ${regression.rSquared}", 55, 40)

        root.children += canvas

        // Add save button
        val saveButton = new Button("Save")
        saveButton.minHeight = 30
        saveButton.minWidth = 50
        saveButton.layoutX = 870
        saveButton.layoutY = 768

        saveButton.onAction = (event) => {
          val image = plot.snapshot(null, null)
          val fileChooser = new FileChooser
          fileChooser.setTitle("Save graph")
          fileChooser.setInitialFileName("MyRegressionGraph")
          fileChooser.getExtensionFilters.addAll( new FileChooser.ExtensionFilter("PNG file", "*.png"))
          val file = fileChooser.showSaveDialog(plotWindow)
          if (file != null){
            ImageIO.write(SwingFXUtils.fromFXImage(image, null), "png", file)
          }
        }

        root.children += saveButton

        plotWindow.setResizable(false)
        plotWindow.show()

      }

      // Graphing Quadratic Regression
      quadraticRegressionMenu.onAction = (event) => {
        val regression = new QuadraticRegression(data)
        val a = regression.a
        val b = regression.b
        val c = regression.c
        val rand = Random
        var regressionData = IndexedSeq[Seq[Double]]()
        for (i <- 0 to 10000) {
          regressionData = regressionData :+ Seq(rand.between(data.minBy(_.head).head - 15, data.maxBy(_.head).head) + 10)
        }
        regressionData = regressionData.map(seq => Seq(seq.head, a* seq.head * seq.head + b* seq.head + c))
        val quadraticData = XYChart.Series[Number, Number]("Quadratic Regression", ObservableBuffer(regressionData.map(seq => XYChart.Data[Number, Number](seq.head, seq.last))))

        val plotWindow = new Stage {
          title.value = "Plotting Quadratic Regression"
          width = 1000
          height = 850
        }
        val xAxis = NumberAxis()
        val yAxis = NumberAxis()
        val dataPointsData = XYChart.Series[Number, Number]("Data points", ObservableBuffer(data.map(seq => XYChart.Data[Number, Number](seq.head, seq.last))))

        val plot = new LineChart(xAxis, yAxis)

        val root = new Pane
        root.children += plot
        plot.minHeight = 800
        plot.minWidth = 975
        plot.getData.add(dataPointsData)
        plot.getData.add(quadraticData)


        val scene = new Scene(root)
        plotWindow.scene = scene

        // Setting graph layout
        scene.stylesheets.add("RedBlue.css")

        if (layoutChoice.getValue == "Black - Blue"){
          scene.stylesheets.add("BlackBlue.css")
        }
        else if (layoutChoice.getValue == "Black - Green") {
          scene.stylesheets.add("BlackGreen.css")
        }
        else if (layoutChoice.getValue == "Black - Red") {
          scene.stylesheets.add("BlackRed.css")
        }
        else if (layoutChoice.getValue == "Blue - Black") {
          scene.stylesheets.add("BlueBlack.css")
        }
        else if (layoutChoice.getValue == "Blue - Green") {
          scene.stylesheets.add("BlueGreen.css")
        }
        else if (layoutChoice.getValue == "Blue - Red") {
          scene.stylesheets.add("BlueRed.css")
        }
        else if (layoutChoice.getValue == "Green - Black") {
          scene.stylesheets.add("GreenBlack.css")
        }
        else if (layoutChoice.getValue == "Green - Blue") {
          scene.stylesheets.add("GreenBlue.css")
        }
        else if (layoutChoice.getValue == "Green - Red") {
          scene.stylesheets.add("GreenRed.css")
        }
        else if (layoutChoice.getValue == "Red - Black") {
          scene.stylesheets.add("RedBlack.css")
        }
        else if (layoutChoice.getValue == "Red - Blue") {
          scene.stylesheets.add("RedBlue.css")
        }
        else if (layoutChoice.getValue == "Red - Green") {
          scene.stylesheets.add("RedGreen.css")
        }

        // Showing calculated factors
        val canvas = new Canvas(1000, 850)
        val g = canvas.graphicsContext2D
        g.fill = White
        g.fillRect(100, 80, 0, 0)
        g.fill = Black
        g.font = new Font(20)
        g.fillText(s" a: ${a}\n b: ${b}\n c: ${c}\n R^2: ${regression.rSquared}", 55, 40)

        root.children += canvas

         // Add save button
        val saveButton = new Button("Save")
        saveButton.minHeight = 30
        saveButton.minWidth = 50
        saveButton.layoutX = 870
        saveButton.layoutY = 768

       saveButton.onAction = (event) => {
          val image = plot.snapshot(null, null)
          val fileChooser = new FileChooser
          fileChooser.setTitle("Save graph")
          fileChooser.setInitialFileName("MyRegressionGraph")
          fileChooser.getExtensionFilters.addAll( new FileChooser.ExtensionFilter("PNG file", "*.png"))
          val file = fileChooser.showSaveDialog(plotWindow)
         if (file != null) {
          ImageIO.write(SwingFXUtils.fromFXImage(image, null), "png", file)
         }
       }

        root.children += saveButton

        plotWindow.setResizable(false)
        plotWindow.show()
      }
    }


    // Reading JSON file

    else if (fileName.toLowerCase.endsWith(".json")) {
      val reader = new JsonReader(fileName)
      val data = reader.data

      val readWindow = new Stage{
        title = "Read Data and Table"
        width = 250
        height = 600
      }

      val root = new Pane
      val scene = new Scene(root)
      readWindow.scene = scene

      readWindow.setResizable(false)
      readWindow.show()

      // Drawing table from json
      val input = ObservableBuffer(data)
      val table = new TableView[Seq[Double]](input)
      val coll1  = new TableColumn[Seq[Double], Double]("x")
      coll1.cellValueFactory = cdf => ObjectProperty(cdf.value.head)
      val coll2 = new TableColumn[Seq[Double], Double]("y")
      coll2.cellValueFactory = cdf => ObjectProperty(cdf.value.last)
      table.columns ++= List(coll1, coll2)
      table.layoutY = 25

      root.children += table

      // Creating choices for regression
      val menuBar = new MenuBar
      val regressionMenu = new Menu("Regression")
      val linearRegressionMenu = new MenuItem("Linear Regression")
      val quadraticRegressionMenu = new MenuItem("Quadratic Regression")

      regressionMenu.items = List(linearRegressionMenu, quadraticRegressionMenu)
      menuBar.menus = List(regressionMenu)

      root.children += menuBar

      // Create options for graph layout
      val label = new Label("Graph layout: Data points - Regression line")
      label.layoutX = 5
      label.layoutY = 430
      root.children += label

      val options = ObservableBuffer("Black - Blue", "Black - Green", "Black - Red", "Blue - Black", "Blue - Green", "Blue - Red", "Green - Black", "Green - Blue",
      "Green - Red", "Red - Black", "Red - Blue", "Red - Green")
      val layoutChoice = new ChoiceBox(options)
      layoutChoice.layoutX = 5
      layoutChoice.layoutY = 450
      root.children += layoutChoice

      // Graphing Linear Regression
      linearRegressionMenu.onAction = (event) => {

        // Regression line data
        val regression = new LinearRegression(data)
        val a = regression.a
        val b = regression.b
        val regressionData = IndexedSeq(Seq(data.minBy(_.head).head - 10, (data.minBy(_.head).head - 10) * a + b), Seq(data.maxBy(_.last).head + 10, (data.maxBy(_.last).head + 10) *a + b))

        val lineGraph = XYChart.Series[Number, Number]("Linear Regression", ObservableBuffer(regressionData.map(seq => XYChart.Data[Number, Number](seq.head, seq.last))))

        // Graphing
        val plotWindow = new Stage {
          title.value = "Plotting Linear Regression"
          width = 1000
          height = 850
        }

        val xAxis = NumberAxis()
        val yAxis = NumberAxis()
        val dataPointsData = XYChart.Series[Number, Number]("Data Points", ObservableBuffer(data.map(seq => XYChart.Data[Number, Number](seq.head, seq.last))))

        val plot = new LineChart(xAxis, yAxis)
        plot.getData.add(dataPointsData)
        plot.getData.add(lineGraph)


        val root = new Pane
        root.children += plot
        plot.minHeight = 800
        plot.minWidth = 975

        val scene = new Scene(root)
        plotWindow.scene = scene

        // Setting graph layout
        scene.stylesheets.add("RedBlue.css")

        if (layoutChoice.getValue == "Black - Blue"){
          scene.stylesheets.add("BlackBlue.css")
        }
        else if (layoutChoice.getValue == "Black - Green") {
          scene.stylesheets.add("BlackGreen.css")
        }
        else if (layoutChoice.getValue == "Black - Red") {
          scene.stylesheets.add("BlackRed.css")
        }
        else if (layoutChoice.getValue == "Blue - Black") {
          scene.stylesheets.add("BlueBlack.css")
        }
        else if (layoutChoice.getValue == "Blue - Green") {
          scene.stylesheets.add("BlueGreen.css")
        }
        else if (layoutChoice.getValue == "Blue - Red") {
          scene.stylesheets.add("BlueRed.css")
        }
        else if (layoutChoice.getValue == "Green - Black") {
          scene.stylesheets.add("GreenBlack.css")
        }
        else if (layoutChoice.getValue == "Green - Blue") {
          scene.stylesheets.add("GreenBlue.css")
        }
        else if (layoutChoice.getValue == "Green - Red") {
          scene.stylesheets.add("GreenRed.css")
        }
        else if (layoutChoice.getValue == "Red - Black") {
          scene.stylesheets.add("RedBlack.css")
        }
        else if (layoutChoice.getValue == "Red - Blue") {
          scene.stylesheets.add("RedBlue.css")
        }
        else if (layoutChoice.getValue == "Red - Green") {
          scene.stylesheets.add("RedGreen.css")
        }

        // Showing calculated factors
        val canvas = new Canvas(1000, 850)
        val g = canvas.graphicsContext2D
        g.fill = White
        g.fillRect(100, 80, 0, 0)
        g.fill = Black
        g.font = new Font(20)
        g.fillText(s" a: ${a}\n b: ${b}\n R^2: ${regression.rSquared}", 55, 40)

        root.children += canvas

         // Add save button
        val saveButton = new Button("Save")
        saveButton.minHeight = 30
        saveButton.minWidth = 50
        saveButton.layoutX = 870
        saveButton.layoutY = 768

        saveButton.onAction = (event) => {
          val image = plot.snapshot(null, null)
          val fileChooser = new FileChooser
          fileChooser.setTitle("Save graph")
          fileChooser.setInitialFileName("MyRegressionGraph")
          fileChooser.getExtensionFilters.addAll( new FileChooser.ExtensionFilter("PNG file", "*.png"))
          val file = fileChooser.showSaveDialog(plotWindow)
          if (file != null){
            ImageIO.write(SwingFXUtils.fromFXImage(image, null), "png", file)
          }
        }

        root.children += saveButton

        plotWindow.setResizable(false)
        plotWindow.show()
      }

      // Graphing Quadratic Regression
      quadraticRegressionMenu.onAction = (event) => {
        val regression = new QuadraticRegression(data)
        val a = regression.a
        val b = regression.b
        val c = regression.c
        val rand = Random
        var regressionData = IndexedSeq[Seq[Double]]()
        for (i <- 0 to 10000) {
          regressionData = regressionData :+ Seq(rand.between(data.minBy(_.head).head - 15, data.maxBy(_.head).head) + 10)
        }
        regressionData = regressionData.map(seq => Seq(seq.head, a* seq.head * seq.head + b* seq.head + c))
        val quadraticData = XYChart.Series[Number, Number]("Quadratic Regression", ObservableBuffer(regressionData.map(seq => XYChart.Data[Number, Number](seq.head, seq.last))))

        val plotWindow = new Stage {
          title.value = "Plotting Quadratic Regression"
          width = 1000
          height = 850
        }
        val xAxis = NumberAxis()
        val yAxis = NumberAxis()
        val dataPointsData = XYChart.Series[Number, Number]("Data points", ObservableBuffer(data.map(seq => XYChart.Data[Number, Number](seq.head, seq.last))))

        val plot = new LineChart(xAxis, yAxis)

        val root = new Pane
        root.children += plot
        plot.minHeight = 800
        plot.minWidth = 975
        plot.getData.add(dataPointsData)
        plot.getData.add(quadraticData)


        val scene = new Scene(root)
        plotWindow.scene = scene

        // Setting graph layout
        scene.stylesheets.add("RedBlue.css")

        if (layoutChoice.getValue == "Black - Blue"){
          scene.stylesheets.add("BlackBlue.css")
        }
        else if (layoutChoice.getValue == "Black - Green") {
          scene.stylesheets.add("BlackGreen.css")
        }
        else if (layoutChoice.getValue == "Black - Red") {
          scene.stylesheets.add("BlackRed.css")
        }
        else if (layoutChoice.getValue == "Blue - Black") {
          scene.stylesheets.add("BlueBlack.css")
        }
        else if (layoutChoice.getValue == "Blue - Green") {
          scene.stylesheets.add("BlueGreen.css")
        }
        else if (layoutChoice.getValue == "Blue - Red") {
          scene.stylesheets.add("BlueRed.css")
        }
        else if (layoutChoice.getValue == "Green - Black") {
          scene.stylesheets.add("GreenBlack.css")
        }
        else if (layoutChoice.getValue == "Green - Blue") {
          scene.stylesheets.add("GreenBlue.css")
        }
        else if (layoutChoice.getValue == "Green - Red") {
          scene.stylesheets.add("GreenRed.css")
        }
        else if (layoutChoice.getValue == "Red - Black") {
          scene.stylesheets.add("RedBlack.css")
        }
        else if (layoutChoice.getValue == "Red - Blue") {
          scene.stylesheets.add("RedBlue.css")
        }
        else if (layoutChoice.getValue == "Red - Green") {
          scene.stylesheets.add("RedGreen.css")
        }

        // Showing calculated factors
        val canvas = new Canvas(1000, 850)
        val g = canvas.graphicsContext2D
        g.fill = White
        g.fillRect(100, 80, 0, 0)
        g.fill = Black
        g.font = new Font(20)
        g.fillText(s" a: ${a}\n b: ${b}\n c: ${c}\n R^2: ${regression.rSquared}", 55, 40)

        root.children += canvas

         // Add save button
        val saveButton = new Button("Save")
        saveButton.minHeight = 30
        saveButton.minWidth = 50
        saveButton.layoutX = 870
        saveButton.layoutY = 768

        saveButton.onAction = (event) => {
          val image = plot.snapshot(null, null)
          val fileChooser = new FileChooser
          fileChooser.setTitle("Save graph")
          fileChooser.setInitialFileName("MyRegressionGraph")
          fileChooser.getExtensionFilters.addAll( new FileChooser.ExtensionFilter("PNG file", "*.png"))
          val file = fileChooser.showSaveDialog(plotWindow)
          if (file != null) {
            ImageIO.write(SwingFXUtils.fromFXImage(image, null), "png", file)
          }
        }

        root.children += saveButton

        plotWindow.setResizable(false)
        plotWindow.show()
      }
    }

    // Unsupported kinds of files
    else {
      alert.showAndWait()
    }
    }
  }
  // Instruction Scene
  val instructionButton = new Button("Instruction")
  instructionButton.minHeight = 50
  instructionButton.minWidth = 400
  instructionButton.layoutX = 290
  instructionButton.layoutY = 460
  instructionButton.underline = true
  root.children += instructionButton
  instructionButton.onAction = (event) => {
    val instructionScene = new Stage {
      title = "Intruction"
      width = 1000
      height = 850
    }
    val root = new Pane
    val scene = new Scene(root)
    instructionScene.scene = scene
    instructionScene.show()

    val canvas = new Canvas(1000, 850)
    val g = canvas.graphicsContext2D
    g.fill = White
    g.fillRect(1000, 850, 0, 0)
    g.fill = Black
    g.font = new Font(30)
    g.fillText("INTRUCTION", 407.5, 40)
    g.font = new Font(14.33)
    g.fillText(
      "I. File format:\n" +
      "CSV file: The first line should be the 'x, y', and the other lines should be made up of two numbers respectively to x and y.\n" +
      "Example:\n" +
      "x, y\n" +
      "3.63, 8.23\n" +
      "5.345, 10.423\n" +
      "4.52, 13.313 \n" +
      "414.21, 9124.1438394\n" +
      "12.314, 313\n" +
      "JSON file: The file should be an array consisting of n values, with n being the number of data points (x, y). Each value should contain two variables x,y\nand their values respectively.\n" +
      "Example:\n" +
      "[\n  {\n    \"x\": 1,\n    \"y\": 4\n  },\n  {\n    \"x\": 5,\n    \"y\": 8\n  },\n  {\n    \"x\": 3,\n    \"y\": 12\n  },\n]\n" +
      "Other files are not supported.\n\n" +
      "II. Usage:\n" +
      "Simply choose the dataset you wish to find the regression from your computer, if the file is formatted correctly, the data will be read correctly and\n" +
      "recorded into a table. From this table, you can choose which kind of regressions being applied to your dataset from the Regression menu.\n" +
      "The graph is then drawn in the new window.", 40, 100)

    root.children += canvas
    instructionScene.show()
  }

}
