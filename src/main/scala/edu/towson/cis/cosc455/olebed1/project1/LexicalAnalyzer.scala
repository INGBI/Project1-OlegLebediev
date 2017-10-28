package edu.towson.cis.cosc455.olebed1.project1

/**
  * Created by olebed1 on 10/11/2017.
  */

trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Unit
  def getNextToken() : Unit
  def lookup() : Boolean
}