package gay.menkissing.engine.util

import java.nio.ByteBuffer
import java.io.OutputStream
import java.nio.BufferOverflowException

class ByteBufferOutputStream(private val wrappedBuffer: ByteBuffer) extends OutputStream {

  override def write(b: Int): Unit =
    try
      wrappedBuffer.put(b.toByte)
    catch
      // not implementing enlargement for i highly doubt that will work for mapped buffers anyway
      case ex: BufferOverflowException =>
        throw ex

  override def write(b: Array[Byte]): Unit =
    wrappedBuffer.put(b)
  
  override def write(bytes: Array[Byte], off: Int, len: Int): Unit =
    wrappedBuffer.put(bytes, off, len)
}
