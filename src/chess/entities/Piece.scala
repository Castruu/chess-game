package chess.entities

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object Piece {
  val None = 0;
  val Pawn = 1;
  val Bishop = 2;
  val Knight = 3;
  val Rook = 4;
  val Queen = 5;
  val King = 6;

  val White = 8;
  val Black = 16;

  val Moved = 32;

  def generateMoves(board: Board, from: Int): Set[Move] = {
    val piece = board.getPiece(from);
    piece match {
      case p if Piece.isDirectional(p) => MoveGenerator.generateMoveByDirection(board, from)
      case p if Piece.isOffset(p) => MoveGenerator.generateMoveByOffset(board, from)
      case p if getPieceBits(p) == Pawn => MoveGenerator.generatePawnMove(board, from)
      case _ => Set.empty
    }
  }

  def isWhite(piece: Int): Boolean = (piece & White) == White

  def hasMoved(piece: Int): Boolean = (piece & Moved) == Moved


  def getPieceBits(piece: Int): Int = piece & 7

  def isValidPiece(piece: Int): Boolean = {
    val pieceRange = 1 to 6
    pieceRange.contains(getPieceBits(piece))
  }

  def isDirectional(piece: Int): Boolean = {
    val pieceBits = getPieceBits(piece)
    (pieceBits == Queen) || (pieceBits == Rook) || (pieceBits == Bishop)
  }

  def isOffset(piece: Int): Boolean = {
    val pieceBits = getPieceBits(piece)
    (pieceBits == King) || (pieceBits == Knight)
  }

  def movesHorizontal(piece: Int): Boolean = {
    val pieceBits = getPieceBits(piece)
    (pieceBits == Queen) || (pieceBits == Rook)
  }

  def movesDiagonal(piece: Int): Boolean = {
    val pieceBits = getPieceBits(piece)
    (pieceBits == Queen) || (pieceBits == Bishop)
  }

}

object MoveGenerator {
  private val directions: Vector[Int] = Vector(1, -1, 8, -8, 7, 9, -7, -9)
  private val offsets: Map[Int, Vector[(Int, Int)]] = Map(
    (Piece.King, Vector((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))),
    (Piece.Knight, Vector((-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)))
  )

  def generateMoveByDirection(board: Board, from: Int): Set[Move] = {
    val pieceToMove = board.getPiece(from)
    if (!Piece.isDirectional(pieceToMove)) return Set.empty
    val startIndex = if (Piece.movesHorizontal(pieceToMove)) 0 else 4
    val endIndex = if (Piece.movesDiagonal(pieceToMove)) 8 else 4
    val pieceDirections = directions.slice(startIndex, endIndex)
    val distances = Board.squareDistanceToEdge(from).slice(startIndex, endIndex)
    pieceDirections.zipWithIndex.flatMap { case (offset, i) =>
      val moves = ListBuffer[Move]()
      var posOnBoard = from + offset

      breakable {
        for (i <- 0 until distances(i)) {
          board.getPiece(posOnBoard) match {
            case Piece.None => moves += NormalMove(from, posOnBoard)
            case piece if !Piece.isValidPiece(piece) =>
            case piece if Piece.isWhite(piece) != Piece.isWhite(pieceToMove) =>
              moves += Capture(from, posOnBoard, piece)
              break()
            case _ => break();
          }

          posOnBoard += offset
        }
      }
      moves.toSet
    }.toSet
  }

  def generateMoveByOffset(board: Board, from: Int): Set[Move] = {
    val pieceToMove = board.getPiece(from);
    val pieceBits = Piece.getPieceBits(pieceToMove);
    if (!Piece.isOffset(pieceToMove)) return Set.empty
    val moves = ListBuffer[Move]()
    val fromSquare = Board.boardPositionToSquare(from)
    val squareRange = 0 until 8
    offsets(pieceBits).foreach { offset =>
      val squareOnBoard = Square(fromSquare.file + offset._1, fromSquare.rank + offset._2)
      if (squareRange.contains(squareOnBoard.rank) && squareRange.contains(squareOnBoard.file)) {
        val posOnBoard = Board.squareToBoardPosition(squareOnBoard)
        board.getPiece(posOnBoard) match {
          case Piece.None => moves += NormalMove(from, posOnBoard)
          case piece if !Piece.isValidPiece(piece) =>
          case piece if Piece.isWhite(piece) != Piece.isWhite(pieceToMove) =>
            moves += Capture(from, posOnBoard, piece);
          case _ =>
        }
      }
    }

    moves.toSet
  }

  def generatePawnMove(board: Board, from: Int): Set[Move] = {
    val moves = ListBuffer[Move]()
    val pieceToMove = board.getPiece(from);
    val colorFactor = if (Piece.isWhite(pieceToMove)) 8 else -8;

    val captureOffset = Vector(-1 + colorFactor, 1 + colorFactor)
    captureOffset.foreach { offset =>
      val posOnBoard = from + offset
      if (board.isInBounds(posOnBoard)) {
        board.getPiece(posOnBoard) match {
          case Piece.None =>
          case piece if !Piece.isValidPiece(piece) =>
          case piece if Piece.isWhite(piece) != Piece.isWhite(pieceToMove) =>
            if (board.isPromotionRank(posOnBoard)) {
              val queenToPromote = Piece.Moved | (if (Piece.isWhite(pieceToMove)) Piece.White else Piece.Black) | Piece.Queen;
              moves += Promotion(from, posOnBoard, queenToPromote, piece)
            } else {
              moves += Capture(from, posOnBoard, piece)
            }
          case _ => None
        }
      }
    }

    val forwardRange = if (Piece.hasMoved(pieceToMove)) 1 to 1 else 1 to 2
    breakable {
      for (forwardOffset <- forwardRange) {
        val offset = forwardOffset * colorFactor;
        val posOnBoard = from + offset
        if (board.isInBounds(posOnBoard)) {
          board.getPiece(posOnBoard) match {
            case Piece.None =>
              if (board.isPromotionRank(posOnBoard)) {
                val queenToPromote = Piece.Moved | (if (Piece.isWhite(pieceToMove)) Piece.White else Piece.Black) | Piece.Queen;
                moves += Promotion(from, posOnBoard, queenToPromote, Piece.None)
              } else {
                moves += NormalMove(from, posOnBoard)
              }
            case piece if !Piece.isValidPiece(piece) =>
            case _ => break()
          }
        }
      }
    }

    moves.toSet
  }
}
