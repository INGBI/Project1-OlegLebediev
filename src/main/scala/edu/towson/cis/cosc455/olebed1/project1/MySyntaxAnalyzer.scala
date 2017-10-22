package edu.towson.cis.cosc455.olebed1.project1

/**
  * Created by olebed1 on 10/11/2017.
  */
class MySyntaxAnalyzer extends SyntaxAnalyzer{
  var pars = new scala.collection.mutable.Stack[String]
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      title()
      variableDefine()
      body()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        pars.push(Compiler.currentToken)
      }
      else {
        println("Syntax error. Expected: '" + CONSTANTS.DOCE + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
    else {
      println("Syntax error. Expected '" + CONSTANTS.DOCB + "'. Received '" + Compiler.currentToken + "'")
      System.exit(1)
    }
  }

  override def title(): Unit = ???

  override def body(): Unit = ???

  override def paragraph(): Unit = ???

  override def heading(): Unit = ???

  override def variableDefine(): Unit = ???

  override def variableUse(): Unit = ???

  override def bold(): Unit = ???

  override def listItem(): Unit = ???

  override def link(): Unit = ???

  override def image(): Unit = ???

  override def newline(): Unit = ???
}
