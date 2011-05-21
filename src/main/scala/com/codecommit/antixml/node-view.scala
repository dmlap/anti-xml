/*
 * Copyright (c) 2011, Daniel Spiewak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer. 
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of the <ORGANIZATION> nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.codecommit.antixml

import java.io.{ByteArrayInputStream, InputStream}
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants, XMLStreamReader}

sealed trait XmlEvent
case class StartElement(ns: Option[String],
                        name: String,
                        namespaces: Map[String, String],
                        attrs: Map[String, String]) extends XmlEvent
object EndElement extends XmlEvent
case class Characters(text: String) extends XmlEvent
object EndDocument extends XmlEvent

sealed trait NodeView {
  private[antixml] def parse(): Stream[XmlEvent]
}
object NodeView {
  def apply(xmlReader: XMLStreamReader): ElemView = {
    def parseEvent(): Stream[XmlEvent] =
      xmlReader.next match {
        case XMLStreamConstants.START_ELEMENT => {
          val uri = xmlReader.getNamespaceURI
          val startElement =
            StartElement(if (uri == null || uri == "") None else Some(uri),
                         xmlReader.getLocalName,
                         Map((0 until xmlReader.getNamespaceCount) map { i =>
                           xmlReader.getNamespacePrefix(i) -> xmlReader.getNamespaceURI(i)
                         }: _*),
                         Map((0 until xmlReader.getAttributeCount) map { i =>
                           (xmlReader.getAttributeNamespace(i) match {
                             case ns if ns == null || ns == "" => xmlReader.getAttributeLocalName(i)
                             case namespace => xmlReader.getAttributePrefix(i) + ":" + xmlReader.getAttributeLocalName(i)
                           }) -> xmlReader.getAttributeValue(i)
                         }: _*))
          Stream.cons(startElement, parseEvent())
        }
        case XMLStreamConstants.CHARACTERS =>
          Stream.cons(Characters(new String(xmlReader.getTextCharacters,
                                            xmlReader.getTextStart,
                                            xmlReader.getTextLength)), parseEvent())
        case XMLStreamConstants.END_ELEMENT =>
          Stream.cons(EndElement, parseEvent())
        case XMLStreamConstants.END_DOCUMENT =>
          Stream.cons(EndDocument, Stream.empty)
      }
    new ElemView(parseEvent())
  }
  def fromString(xml: String): ElemView =
    fromInputStream(new ByteArrayInputStream(xml.getBytes))
  def fromInputStream(xml: InputStream) =
    apply(XMLInputFactory.newInstance().createXMLStreamReader(xml))
}
class ElemView private[antixml](events: Stream[XmlEvent]) extends NodeView {
  val (ns: Option[String],
       name: String,
       namespaces: Map[String, String],
       attrs: Map[String, String]) = {
    val event: StartElement = events.head.asInstanceOf[StartElement]
    (event.ns,
     event.name,
     event.namespaces,
     event.attrs)
  }
  private[antixml] lazy val (_children, remaining) = {
    val children = new GroupNodeView(events.tail)
    (children, children.parse())
  }
  private[antixml] def parse() = remaining.tail
  lazy val children: GroupNodeView = _children

  val qName = ns map (_ + ":" + name) getOrElse name
  override def toString: String = {
    val namespaces = ("" /: this.namespaces) { (result, namespace) =>
        result + " xmlns:" + namespace._1 + "='" + namespace._2 + "'"
      }
    val attributes = ("" /: this.attrs) { (result, attribute) =>
      result + " " + attribute._1 + "='" + attribute._2 + "'"
    }
    val tail = (children.length match {
        case 0 =>  " /"
        case _ =>
          ">" + (children.mkString("")) + "</" + qName
      })
    "<" + qName + namespaces + attributes + tail + ">"
  }
}

class TextView private[antixml](events: Stream[XmlEvent]) extends NodeView {
  val text: String = events.head.asInstanceOf[Characters].text
  private[antixml] def parse() = events.tail
  override val toString: String = text
}
