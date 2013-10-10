package main.scala.eu.swdev.xml.token

import java.nio.{CharBuffer, ByteBuffer}
import java.nio.charset.{CharsetDecoder, Charset}

/**
 * Created with IntelliJ IDEA.
 * User: swachter
 * Date: 08.10.13
 * Time: 15:16
 * To change this template use File | Settings | File Templates.
 */
class Decoder(charConsumer: CharBuffer => Unit) {

  private val byteBuffer = ByteBuffer.allocate(1024)
  private val charBuffer = CharBuffer.allocate(1024)

  private var state: () => Unit = () => {
    val detection = detectPreliminaryEncoding
    byteBuffer.position(detection._1)
    byteBuffer.mark()
    val charsetDecoder = detection._2.newDecoder()
    charsetDecoder.decode(byteBuffer, charBuffer, false)

    if (charBuffer.limit() < 4) {
      throw new RuntimeException("CharBuffer must contain at least 5 characters for xml declaration detection")
    }
    val chars = new Array[Char](5);
    charBuffer.get(chars, 0, 5)
    val string = new String(chars)
    val decoder = if (string.equals("<?xml")) {
      val charset = detectEncoding
      val dec = charset.newDecoder()
      byteBuffer.reset()
      charBuffer.clear()
      dec.decode(byteBuffer, charBuffer, false)
      dec
    } else {
      charsetDecoder
    }

    charConsumer(charBuffer)

    state = () => {
      decoder.decode(byteBuffer, charBuffer, false)
      charConsumer(charBuffer)
    }

  }

  def consumeBytes(): Unit = {
    state()
  }

  def detectPreliminaryEncoding = {
    if (byteBuffer.limit() < 3) throw new RuntimeException("ByteBuffer must contain at least 4 bytes for encoding detection")
    val head4 = byteBuffer.getLong(0)
    val head2 = byteBuffer.getInt(0)
    val byte = byteBuffer.get(3)
    if (head4 == 0x0000feffl) {
      (4, Charset.forName("UTF-32BE"))
    } else if (head4 == 0xfffe0000l) {
      (4, Charset.forName("UTF-32LE"))
    } else if (head4 == 0x0000fffel) {
      throw new RuntimeException("unsupported byte order 2143")
    } else if (head4 == 0xfeff0000l) {
      throw new RuntimeException("unsupported byte order 3412")
    } else if (head2 == 0xfeff) {
      (2, Charset.forName("UTF-16BE"))
    } else if (head2 == 0xfffe) {
      (2, Charset.forName("UTF-16LE"))
    } else if (head2 == 0xefbb && byte == 0xbf) {
      (3, Charset.forName("UTF-8"))
    } else if (head4 == 0x0000003cl) {
      (0, Charset.forName("UTF-32BE"))
    } else if (head4 == 0x3c000000l) {
      (0, Charset.forName("UTF-32LE"))
    } else if (head4 == 0x00003c00l) {
      throw new RuntimeException("unsupported byte order 2143")
    } else if (head4 == 0x003c0000l) {
      throw new RuntimeException("unsupported byte order 3412")
    } else if (head4 == 0x003c003fl) {
      (0, Charset.forName("UTF-16BE"))
    } else if (head4 == 0x3c003f00l) {
      (0, Charset.forName("UTF-16LE"))
    } else if (head4 == 0x3c3f786dl) {
      (0, Charset.forName("UTF-8"))
    } else if (head4 == 0x4c6fa794) {
      (0, Charset.forName("Cp500"))
    } else {
      (0, Charset.forName("UTF-8"))
    }
  }

  def detectEncoding: Charset = ???

}
