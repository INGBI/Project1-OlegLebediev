package edu.towson.cis.cosc455.olebed1.project1

/**
  * Created by olebed1 on 10/11/2017.
  */
object Compiler {
  var currentToken: String = ""
  var fileContents: String = ""

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticsAnalyzer
  var pos : Int = -1
  var filename:String = ""

  def main(args: Array[String]): Unit = {
    //checkFile(args)
    //filename = args(0)
    readFile("C:\\Users\\olebed1\\IdeaProjects\\Project1-OlegLebediev\\out\\artifacts\\project1_oleglebediev_jar\\Test2.gtx")

    Scanner.getNextToken()
    Parser.gittex()
    SemanticAnalyzer.toHTML()
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (!args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}