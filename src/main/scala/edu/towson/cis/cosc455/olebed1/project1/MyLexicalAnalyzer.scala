package edu.towson.cis.cosc455.olebed1.project1

/**
  * Created by olebed1 on 10/11/2017.
  */
class MyLexicalAnalyzer extends LexicalAnalyzer{
  var nextC : Char = ' '
  var tokenS : String =""
  var fileL : Int = 0

  override def addChar(): Unit = {
    //Adds chars to token string one at a time
    tokenS += nextC
  }

  override def getChar(): Unit = {
    if (Compiler.pos < fileL) {
      Compiler.pos += 1
      nextC = Compiler.fileContents.charAt(Compiler.pos)
    }
  }

  def nonSpace() : Unit = {   //Calls get char until a non space character is found
    while (CONSTANTS.ENDOFLINE.contains(nextC) && Compiler.pos < fileL) {
      getChar()
    }
  }

  override def getNextToken(): Unit = {
    fileL = Compiler.fileContents.length -1
    tokenS = ""

    getChar()
    nonSpace()
    if (fileL != Compiler.pos) {}
    if (CONSTANTS.SPECIALCHAR.contains(nextC)) {
      if (CONSTANTS.BOLD.contains(nextC)) {
        addChar()
        getChar()
      }
      else if (CONSTANTS.LISTITEM.contains(nextC)) {
        addChar()
        tokenS += readFully()
      }
      else if (CONSTANTS.ADDRESSE.contains(nextC)) {
        addChar()
        getChar()
      }
      else if (CONSTANTS.SPECIALCHAR(3) == nextC) {
        addChar()
        tokenS += readFully()
        if (CONSTANTS.SPECIALCHAR(3) == nextC) {
          addChar()
          Compiler.currentToken = tokenS
          Compiler.pos += 1
          return
        }
        if (CONSTANTS.SPECIALCHAR(6) == nextC) {
          addChar()
          Compiler.currentToken = tokenS
          return
        }
        if (CONSTANTS.BRACKETE.contains(nextC)) {
          addChar()
        }
        if (CONSTANTS.DOCE == tokenS.toUpperCase) {
          nonSpace()
          if (Compiler.pos - fileL != 0) {
            Compiler.pos -= 1
            getNextToken()
            println("Lexical error. There cannot be any symbols after \\END")
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
        if (CONSTANTS.IMAGEB.charAt(1) == nextC) {
          addChar()
          if (lookup()) {
            if (tokenS.substring(tokenS.length - 1, tokenS.length) == "\n" || tokenS.substring(tokenS.length - 1, tokenS.length) == "\r" || tokenS.substring(tokenS.length - 1, tokenS.length) == "\t") {
              Compiler.currentToken = tokenS.substring(0, tokenS.length - 1)
              return
            }
            else {
              Compiler.currentToken = tokenS
              return
            }
          }
        }
        else {
          println("Lexical error. Illegal character after '!'. Received: '" + nextC + "'")
          System.exit(1)
        }
      }
      else if (CONSTANTS.SPECIALCHAR.contains(nextC)) {
        addChar()
      }
      if (tokenS.length > 0 && lookup()) {
        if (tokenS.substring(tokenS.length - 1, tokenS.length) == "\n" || tokenS.substring(tokenS.length - 1, tokenS.length) == "\r" || tokenS.substring(tokenS.length - 1, tokenS.length) == "\t") {
          Compiler.currentToken = tokenS.substring(0, tokenS.length - 1)
        }
        else Compiler.currentToken = tokenS
      }
      else {
        println("Lexical error: Illegal token received: '" + tokenS + "'")
        System.exit(1)
      }
    }
    else if (nextC.isLetterOrDigit ||nextC==':' || nextC=='.' || nextC==',') {
        addChar()
        tokenS += readFully()
        if (nextC.toString.equals(CONSTANTS.ADDRESSE) || nextC.toString.equals(CONSTANTS.BRACKETE) || nextC.toString.equals(CONSTANTS.PARAE) || nextC.toString.equals(CONSTANTS.EQSIGN) || nextC.equals('\\')) {
          //Will decrement index so special characters aren't skipped
          Compiler.pos -= 1
        }
        Compiler.currentToken = tokenS
      }
  }

  def readFully() : String = { //Reads in text until end of word, line or token
    var text: String = ""
    getChar()

    while (Compiler.pos < fileL && !CONSTANTS.ENDOFLINE.contains(nextC) && !CONSTANTS.SPECIALCHAR.contains(nextC)) {
      text += nextC
      getChar()
    }
    if (CONSTANTS.ENDOFLINE(0) == nextC) {
      text += nextC
    }
    if (CONSTANTS.ENDOFLINE(1) == nextC) {
      getChar()
      if (CONSTANTS.ENDOFLINE(2) == nextC) {
        text += nextC
      }
    }
    return text
  }

  override def lookup(): Boolean = {
    var temp: String = ""
    if (tokenS.substring(tokenS.length-1,tokenS.length) == "\n" || tokenS.substring(tokenS.length-1,tokenS.length) == "\r" || tokenS.substring(tokenS.length-1,tokenS.length) == "\t"){
      temp = tokenS.toUpperCase.substring(0,tokenS.length-1)
      return CONSTANTS.KEYWORDS.contains(temp)
    }
    else {
      return CONSTANTS.KEYWORDS.contains(tokenS.toUpperCase)
    }
  }
}
