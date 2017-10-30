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
    if (textCheck()) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      regText()
    }
    else if (Compiler.pos == Compiler.Scanner.fileL) {}
  }

  def textCheck(): Boolean = {
    if (Compiler.currentToken.contains(':') || Compiler.currentToken.contains('.') || Compiler.currentToken.contains(',')) {return true}
    if (Compiler.currentToken.contains("\n")) return Compiler.currentToken.length == Compiler.currentToken.filter(_.isLetterOrDigit).length+1
    Compiler.currentToken.length == Compiler.currentToken.filter(_.isLetterOrDigit).length
  }
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
  override def body(): Unit = {
    if (Compiler.pos == Compiler.Scanner.fileL) {}
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      paragraph()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      body()
    }
    else {
      innerText()
      body()
    }
  }

  def innerText(): Unit = {


    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      heading()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      listItem()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      image()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      innerText()
    }
    else if (textCheck()) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerText()
    }
  }

  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        variableDefine()
      }
      innerText()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        pars.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax error. Expected: '" + CONSTANTS.PARAE + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
    else {
      println("Syntax error. Expected: '" + CONSTANTS.PARAB + "'. Received: '" + Compiler.currentToken + "'")
      System.exit(1)
    }
  }

  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      regText()
    }
  }

  override def bold(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase((CONSTANTS.BOLD))) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      regText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        pars.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax error. Expected: '" + CONSTANTS.BOLD + "'. Received: '" + Compiler.currentToken + "'")
        System.exit(1)
      }
    }
  }

  override def listItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }

  override def link(): Unit = ???

  override def image(): Unit = ???

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      pars.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }
}