package chess.entities

sealed trait Move {
  def from: Int
  def to: Int
}

case class NormalMove(from: Int, to: Int) extends Move
case class Capture(from: Int, to: Int, captured: Int) extends Move
case class Castle(from: Int, to: Int, right: Boolean) extends Move
case class EnPassant(from: Int, to: Int, capturedPos: Int, captured: Int) extends Move
case class Promotion(from: Int, to: Int, promoteTo: Int, captured: Int) extends Move
