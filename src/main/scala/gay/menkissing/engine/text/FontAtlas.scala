package gay.menkissing.engine.text

import upickle.default.*
import gay.menkissing.common.math.RectD

import collection.mutable


object RawMSDFAtlas {
  final case class RawTLBR(left: Double, bottom: Double, right: Double, top: Double)
    derives ReadWriter {
      def asRect: RectD =
        RectD(left, top, right - left, bottom - top)
    }

  final case class RawGlyph(unicode: Int, advance: Double, planeBounds: Option[RawTLBR] = None, atlasBounds: Option[RawTLBR] = None)
    derives ReadWriter

  final case class RawAtlas(
    `type`: Font.FontKind,
    distanceRange: Double = 0.0,
    distanceRangeMiddle: Double = 0.0,
    size: Int,
    width: Int,
    height: Int
  ) derives ReadWriter




  final case class RawFontAtlas(atlas: RawAtlas, metrics: Metrics, glyphs: Vector[RawGlyph]) derives ReadWriter
}

final case class Metrics(emSize: Double, lineHeight: Double, ascender: Double, descender: Double, underlineY: Double, underlineThickness: Double)
  derives ReadWriter

final case class AemRange(low: Double, high: Double)

final case class Atlas(
  kind: Font.FontKind,
  size: Int,
  width: Int,
  height: Int,
  aemRange: Option[AemRange]
)

final case class GlyphTexInfo(planeBounds: RectD, atlasBounds: RectD)

final case class Glyph(advance: Double, texInfo: Option[GlyphTexInfo])

final case class FontAtlas(glyphs: Map[Char, Glyph], metrics: Metrics, atlas: Atlas)

object FontAtlas {
  given Reader[FontAtlas] = 
    summon[Reader[RawMSDFAtlas.RawFontAtlas]].map { it =>
      val map = mutable.Map.empty[Char, Glyph]
      it.glyphs.foreach {
        case RawMSDFAtlas.RawGlyph(c, adv, Some(pb), Some(ab)) =>
          val info = GlyphTexInfo(pb.asRect, ab.asRect)
          val glyph = Glyph(adv, Some(info))
          map(c.toChar) = glyph
        case RawMSDFAtlas.RawGlyph(c, adv, _, _) =>
          val glyph = Glyph(adv, None)
          map(c.toChar) = glyph
      }
      val aemRange =
        Option.when(it.atlas.`type`.isSDF) {
          AemRange(
            (it.atlas.distanceRangeMiddle - it.atlas.distanceRange / 2) / it.atlas.size,
            (it.atlas.distanceRangeMiddle + it.atlas.distanceRange / 2) / it.atlas.size
          )
        }
      val atlas =
        Atlas(
          it.atlas.`type`,
          it.atlas.size,
          it.atlas.width,
          it.atlas.height,
          aemRange
        )
      FontAtlas(map.toMap, it.metrics, atlas)
    }
}

