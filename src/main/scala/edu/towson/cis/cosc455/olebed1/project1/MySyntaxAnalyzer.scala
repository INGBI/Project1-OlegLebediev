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

  override def title(): Unit = 
  {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //calls for the regular text check
      regText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        pars.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax error. Expected '" + CONSTANTS.BRACKETE + "'. Received '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
    else {
      println("Syntax error. Expected: '" + CONSTANTS.TITLEB + "'. Received: '" + Compiler.currentToken + "'")
      System.exit(1)
    }
  }
  //Added the reg text needed in the title, and everywhere else where we have text
  def regText(): Unit =
  {
    if (!(CONSTANTS.SPECIALCHAR contains Compiler.currentToken)) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      regText()
    }
  }

  override def body(): Unit = ???

  override def paragraph(): Unit = ???

  override def heading(): Unit = ???

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      regText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)) {
        pars.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        regText()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          pars.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          variableDefine()
        }
        else {
          println("Syntax error. Expected: '" + CONSTANTS.BRACKETE + "'. Received: '" + Compiler.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("Syntax error. Expected: '" + CONSTANTS.EQSIGN + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      regText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        pars.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax error. Expected: '" + CONSTANTS.BRACKETE + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  override def bold(): Unit = ???

  override def listItem(): Unit = ???

  override def link(): Unit = ???

  override def image(): Unit = ???

  override def newline(): Unit = ???
}