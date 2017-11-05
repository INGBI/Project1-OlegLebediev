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
  var varParName = new scala.collection.mutable.Stack[String]
  var varParMean = new scala.collection.mutable.Stack[String]
  var token : String = ""
  var ifPar = 0
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
        ifPar = 1
      }
      else if (token.equalsIgnoreCase(CONSTANTS.PARAE)) {
        outStack.push("</p>\n")
        token = semPars.pop()
        ifPar = 0
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
        while (!CONSTANTS.KEYWORDS.contains(token) && !CONSTANTS.SPECIALCHAR.contains(token)) {
          outStack.push(token+" ")
          token = semPars.pop()
        }
        while(token.equalsIgnoreCase(CONSTANTS.USEB)) {
            var name: String = semPars.pop()
            semPars.pop()
          if (ifPar==0) {
            if (varName.contains(name)) {
              outStack.push(" " + varMean(varName.indexOf(name, 0)) + " ")
            }
            else {
              println("Static Semantic Error: Variable with name: [" +name+ "] has not been defined")
              System.exit(1)
            }
          }
          else
          {
            if (varParName.contains(name)) {
              outStack.push(" " + varParMean(varParName.indexOf(name, 0)) + " ")
            }
            else if (varName.contains(name)) {
              outStack.push(" " + varMean(varName.indexOf(name, 0)) + " ")
            }
            else if (!varParName.contains(name) || !varName.contains(name)) {
              println("Static Semantic Error: Variable with name: [" +name+ "] has not been defined")
              System.exit(1)
            }
          }
          token = semPars.pop()
        }
        outStack.push("</li>")
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
      else if (token.equalsIgnoreCase(CONSTANTS.DEFB)) {
          if (ifPar == 0) {
            varName.push(semPars.pop())
            semPars.pop()
            varMean.push(semPars.pop())
            semPars.pop()
            token = semPars.pop()
          }
          else {
            varParName.push(semPars.pop())
            semPars.pop()
            varParMean.push(semPars.pop())
            semPars.pop()
            token = semPars.pop()
          }
        }
      else if (token.equalsIgnoreCase(CONSTANTS.USEB)) {
        var name: String = semPars.pop()
        semPars.pop()
          if (ifPar==0) {
            if (varName.contains(name)) {
              outStack.push(" " + varMean(varName.indexOf(name, 0)) + " ")
            }
            else {
              println("Static Semantic Error: Variable with name: [" +name+ "] has not been defined")
              System.exit(1)
            }
          }
          else
          {
            if (varParName.contains(name)) {
              outStack.push(" " + varParMean(varParName.indexOf(name, 0)) + " ")
            }
            else if (varName.contains(name)) {
              outStack.push(" " + varMean(varName.indexOf(name, 0)) + " ")
            }
            else if (!varParName.contains(name) || !varName.contains(name)) {
              println("Static Semantic Error: Variable with name: [" +name+ "] has not been defined")
              System.exit(1)
            }
          }
        token = semPars.pop()
      }
      else if (token.equalsIgnoreCase(CONSTANTS.DOCE)) {
        outStack.push("</html>\n")
      }
      else if (!CONSTANTS.KEYWORDS.contains(token)) {
        outStack.push(token+" ")
        token = semPars.pop()
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
