package edu.towson.cis.cosc455.olebed1.project1

import java.io._
import java.awt.Desktop
import java.io.{File, IOException}
/**
  * Created by olebed1 on 10/11/2017.
  */
class MySemanticsAnalyzer {
  var output : String = ""
  var outStack = new scala.collection.mutable.Stack[String]
  var semPars = new scala.collection.mutable.Stack[String]
  var varName = new scala.collection.mutable.Stack[String]
  var varMean = new scala.collection.mutable.Stack[String]
  var token : String = ""
  var countVarPar = 0
  def toHTML() =
  {
    semPars = Compiler.Parser.pars.reverse
    token = semPars.pop()
    while (semPars.nonEmpty)
    {
      if (token.equalsIgnoreCase(CONSTANTS.DOCB)) {
        outStack.push("<html>\n")
        token = semPars.pop()
      }
      else if (token.equalsIgnoreCase(CONSTANTS.TITLEB)) {
        outStack.push("<head>\n")
        outStack.push("<title>\n")
        token = semPars.pop()
        while (!CONSTANTS.KEYWORDS.contains(token)) {
          outStack.push(token +" ")
          token = semPars.pop()
        }
        outStack.push("</title>\n")
        outStack.push("</head>\n")
        token = semPars.pop()
        while (token == CONSTANTS.DEFB)
        {
          if (token.equalsIgnoreCase(CONSTANTS.DEFB)) {
            varName.push(semPars.pop())
            semPars.pop()
            varMean.push(semPars.pop())
            semPars.pop()
            token = semPars.pop()
          }
        }
      }
      else if (token.equalsIgnoreCase(CONSTANTS.HEADING)) {
        outStack.push("<h1>")
        outStack.push(semPars.pop()+" ")
        token = semPars.pop()
        while (!CONSTANTS.KEYWORDS.contains(token)) {
          outStack.push(token+" ")
          token = semPars.pop()
        }
        outStack.push("</h1>\n")
      }
      else if (token.equalsIgnoreCase(CONSTANTS.PARAB)) {
        outStack.push("<p>")
        token = semPars.pop()
        while (token == CONSTANTS.DEFB)
        {
          if (token.equalsIgnoreCase(CONSTANTS.DEFB)) {
            countVarPar +=1
            varName.push(semPars.pop())
            semPars.pop()
            varMean.push(semPars.pop())
            semPars.pop()
            token = semPars.pop()
          }
        }
      }
      else if (token.equalsIgnoreCase(CONSTANTS.PARAE)) {
        outStack.push("</p>\n")
        token = semPars.pop()
        for (i <- 0 until countVarPar)
        {
          varName.pop()
          varMean.pop()
        }
      }
      else if (token.equalsIgnoreCase(CONSTANTS.BOLD)) {
        outStack.push("<b>")
        token = semPars.pop()
        while (!CONSTANTS.KEYWORDS.contains(token)) {
          outStack.push(token+" ")
          token = semPars.pop()
        }
        outStack.push("</b>\n")
        token = semPars.pop()
      }
      else if (token.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
        outStack.push("<li>")
        token = semPars.pop()
        while (!CONSTANTS.KEYWORDS.contains(token) && !CONSTANTS.SPECIALCHAR.contains(token) && token!="\n" ) {
          outStack.push(token+" ")
          token = semPars.pop()
        }
        outStack.push("</li>\n")
      }
      else if (token.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
        outStack.push("<br>\n")
        token = semPars.pop()
      }
      else if (token.equalsIgnoreCase(CONSTANTS.LINKB)) {
        var temp: String = ""
        token = semPars.pop()
        while (!CONSTANTS.KEYWORDS.contains(token)) {
          temp += token + " "
          token = semPars.pop()
        }
        semPars.pop()
        outStack.push("<a href = \"")
        outStack.push(semPars.pop())
        outStack.push("\">")
        outStack.push(temp)
        outStack.push("</a> ")
        semPars.pop()
        token = semPars.pop()
      }
      else if (token.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
        var temp: String = ""
        token = semPars.pop()
        while (!CONSTANTS.KEYWORDS.contains(token)) {
          temp += token + " "
          token = semPars.pop()
        }
        semPars.pop()
        token =semPars.pop()

        outStack.push("<img src =\"")
        outStack.push(token)
        outStack.push("\" alt=\"")
        outStack.push(temp)
        outStack.push("\">\n")
        semPars.pop()
        token = semPars.pop()
      }
      else if (token.equalsIgnoreCase(CONSTANTS.USEB)) {
        var name: String = semPars.pop()
        semPars.pop()
        for (i <- 0 until varName.length)
        {
          if(varName.equals(name))
          {
            outStack.push(varMean(i))
          }
          else if (i == varName.length-1)
          {
            println("Static Semantic Error: Variable by that name has not been defined")
            System.exit(1)
          }
        }
        token = semPars.pop()
      }
      else if (token.equalsIgnoreCase(CONSTANTS.DOCE)) {
        outStack.push("</html>\n")
      }
      else {
        outStack.push(token+" ")
        token = semPars.pop()
      }
      if (token.equalsIgnoreCase(CONSTANTS.DOCE)) {
        outStack.push("</html>\n")
      }
    }

    val output = outStack.reverse.mkString
    val print = new PrintWriter(new File(Compiler.filename + ".html"))
    print.write(output)
    print.close
    openHTMLFileInBrowser(Compiler.filename + ".html")
  }

  def openHTMLFileInBrowser(htmlFileStr: String) = {
    val file: File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
    }
  }
}
