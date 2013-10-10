package main.scala.eu.swdev.xml.token

/**
 * Created with IntelliJ IDEA.
 * User: swachter
 * Date: 08.10.13
 * Time: 17:58
 * To change this template use File | Settings | File Templates.
 */
trait TokenHandler {

  def startXmlDecl()
  def endXmlDecl()

  def attr(name: String, value: String)



}
