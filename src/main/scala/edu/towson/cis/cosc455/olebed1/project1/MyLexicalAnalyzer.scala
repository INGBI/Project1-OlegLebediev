package edu.towson.cis.cosc455.olebed1.project1

/**
  * Created by olebed1 on 10/11/2017.
  */
class MyLexicalAnalyzer extends LexicalAnalyzer{
  var nextC : Char = ' '
  var tokenS : String =""
  var fileL : Int = Compiler.fileContents.length

  override def addChar(): Unit = {
    //Adds chars to token string one at a time
    tokenS += nextC
  }

  override def getChar(): Unit = {
    Compiler.pos += 1
    while(Compiler.pos < fileL && CONSTANTS.ENDOFLINE.contains(nextC))
      {
        nextC = Compiler.fileContents.charAt(Compiler.pos)
        getChar()
      }
  }

  override def getNextToken(): Unit ={
    tokenS = ""
    getChar()
    if(CONSTANTS.SPECIALCHAR.contains(nextC))
      if (CONSTANTS.BOLD.contains(nextC)) {
        addChar()
        getChar()
      }
      else if (CONSTANTS.LISTITEM.contains(nextC)) {
          addChar()
        tokenS += readFully()
      }
      else if (CONSTANTS.SPECIALCHAR(3)==nextC) {
        addChar()
        tokenS += readFully()
        if (CONSTANTS.BRACKETE.contains(nextC)) {
          addChar()
        }
        if (tokenS.equalsIgnoreCase(CONSTANTS.DOCE)) {
          getChar()
          if (Compiler.pos - fileL != 0) {
            Compiler.pos  -= 1
            getNextToken()
            println("Syntax error. There cannot be any symbols after \\END")
            System.exit(1)
          }
        }
      }
      else if (CONSTANTS.HEADING.contains(nextC)) {
        addChar()
        tokenS += readFully()
      }
      else if (CONSTANTS.IMAGEB.charAt(0) == nextC) {
        addChar()
        getChar()
        if (CONSTANTS.BRACKETE.contains(nextC)) {
          addChar()
        }
        else {
          println("Lexical error. Illegal character after '!'. Received: '" + nextC + "'")
          System.exit(1)
        }
      }
      else if (CONSTANTS.SPECIALCHAR.contains(nextC)) {
        addChar()
      }
  }
  
  def readFully() : String = { //Reads in text until end of word, line or token
    var text: String = ""
    getChar()

    while (Compiler.pos < fileL && !CONSTANTS.ENDOFLINE.contains(nextC)) {
      text += nextC
      getChar()
    }
    if (CONSTANTS.ENDOFLINE(1) == nextC) {
      text += nextC
    }
    if (CONSTANTS.ENDOFLINE(2) == nextC) {
      getChar()
      if (CONSTANTS.ENDOFLINE(3) == nextC) {
        text += nextC
      }
    }
    return text
  }


  override def lookup(): Boolean = ???
}
