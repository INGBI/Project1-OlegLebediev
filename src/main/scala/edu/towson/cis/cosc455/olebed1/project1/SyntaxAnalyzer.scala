package edu.towson.cis.cosc455.olebed1.project1

/**
  * Created by olebed1 on 10/11/2017.
  */
trait SyntaxAnalyzer {
  def gittex() : Unit
  def title() : Unit
  def body() : Unit
  def paragraph() : Unit
  def heading() : Unit
  def variableDefine() : Unit
  def variableUse() : Unit
  def bold() : Unit
  def listItem() : Unit
  def link() : Unit
  def image() : Unit
  def newline() : Unit
}
