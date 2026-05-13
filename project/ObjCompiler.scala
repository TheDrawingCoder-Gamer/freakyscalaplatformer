import java.io.InputStream
import java.io.FileOutputStream
import scala.io.Source
import sbt.util.Logger
import scala.collection.mutable
import java.io.DataOutputStream
import java.io.File
import upickle.default._
import scala.util.matching.Regex

object ObjCompiler {
  final case class SavedMesh(vertices: Array[Float], indices: Array[Int])
  object SavedMesh {
    implicit val rw: ReadWriter[SavedMesh] = macroRW
  }

  final case class PointF(x: Float, y: Float)

  val vertexPosPattern = raw"v (-?[0-9.]+) [0-9.]+ (-?[0-9.]+)".r
  val vertexTexPattern = raw"vt (-?[0-9.]+) (-?[0-9.]+)".r
  val facePattern = raw"f ([0-9]+)/([0-9]+) ([0-9]+)/([0-9]+) ([0-9]+)/([0-9]+)".r

  def compileObj(source: File, dest: File): File = {
    val data = Source.fromFile(source)

    val verts = mutable.Buffer.empty[PointF]
    val texCoords = mutable.Buffer.empty[PointF]
    val indices = mutable.Buffer.empty[Int]
    val texIndices = mutable.Buffer.empty[Int]

    data.getLines().foreach {
      case vertexPosPattern(x, z) =>
        verts.append(PointF(x.toFloat, z.toFloat))
      case vertexTexPattern(u, v) =>
        texCoords.append(PointF(u.toFloat, v.toFloat))
      
      case facePattern(p1, t1, p2, t2, p3, t3) =>
        indices.append(p1.toInt - 1, p2.toInt - 1, p3.toInt - 1)
        texIndices.append(t1.toInt - 1, t2.toInt - 1, t3.toInt - 1)
      case _ => ()
    }

    val minX = verts.minBy(_.x).x
    val maxX = verts.maxBy(_.x).x
    val minY = verts.minBy(_.y).y
    val maxY = verts.maxBy(_.y).y

    val editedVerts =
      verts.map { vec =>
        PointF(
          (vec.x - minX) / (maxX - minX),
          (vec.y - minY) / (maxY - minY)
        )
      }
    
    val vertToTexPairing = mutable.Map.empty[Int, Int]
    


    indices.zip(texIndices).zipWithIndex.foreach {
      case ((p, t), idx) =>
        if (vertToTexPairing.contains(p) && vertToTexPairing(p) != t) {
          editedVerts.append(editedVerts(p))
          vertToTexPairing(editedVerts.length - 1) = t
          indices(idx) = editedVerts.length - 1
        } else {
          vertToTexPairing(p) = t
        }

        
    }

    val vertexArray = mutable.Buffer.empty[Float]

    editedVerts.zipWithIndex.foreach { case (p, idx) =>
      vertexArray.append(p.x, p.y)
      val tex = texCoords(vertToTexPairing(idx))
      vertexArray.append(tex.x, tex.y)
    }

    val outStream = new FileOutputStream(dest)
    val mesh = SavedMesh(vertexArray.toArray, indices.toArray)
    writeBinaryTo(mesh, outStream)
    outStream.close()

    dest
  }
}
