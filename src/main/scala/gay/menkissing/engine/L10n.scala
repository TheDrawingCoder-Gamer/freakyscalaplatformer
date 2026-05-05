package gay.menkissing.engine

import gay.menkissing.LDtkMap
import gay.menkissing.World.getClass
import upickle.default.*

final case class L10n(entries: Map[String, String]) {
  def named(name: String): String =
    entries.getOrElse(name, name)
}

object L10n {
  given readWriter: ReadWriter[L10n] = readwriter[Map[String, String]].bimap(_.entries, L10n.apply)
  
  val instance = ujson.InputStreamParser.transform[L10n](getClass.getResourceAsStream("/en_us.json"), upickle.default.reader[L10n])
}