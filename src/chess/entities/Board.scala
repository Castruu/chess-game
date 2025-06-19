package chess.entities

import chess.entities.Board.{getInitialBoardArray, squareToBoardPosition}

import scala.collection.mutable

case class Square(file: Int, rank: Int) {
  def ==(rhs: Square): Boolean = rhs.file == file && rhs.rank == rank;
  def +(rhs: Square): Square = Square(rhs.file + file, rhs.rank + rank);
  def -(rhs: Square): Square = Square(file - rhs.file, rank - rhs.rank);
}

case class Board(
                  board: Vector[Int],
                ) {

  def this() = {
    this(
      Vector.from(getInitialBoardArray)
    )
  }

  def makeMove(move: Move): Board = {
    if(move.from == move.to) return this;
    val pieceToMove = getPiece(move.from);
    var updatedBoard = updateWithPiece(move.from, Piece.None);
    move match {
      case Promotion(_, _, promoteTo, _) => updatedBoard.updateWithPiece(move.to, promoteTo)
      case Castle(from, to, right) =>
        if(Piece.hasMoved(pieceToMove)) return this;
        updatedBoard = updatedBoard.updateWithPiece(to , Piece.Moved | pieceToMove);
        val rookFile = if (right) 7 else 0;
        val rookRank = move.to / 8;
        val rookPos = rookFile + rookRank * 8;
        val rook = getPiece(rookPos)
        if(Piece.hasMoved(rook)) return this;
        updatedBoard = updatedBoard.updateWithPiece(move.to + (if (right) -1 else 1), Piece.Moved | rook);
        updatedBoard.updateWithPiece(rookPos, Piece.None)
      case EnPassant(_, to, capturedPos, _) =>
        updatedBoard = updatedBoard.updateWithPiece(capturedPos, Piece.None)
        updatedBoard.updateWithPiece(to, Piece.Moved | pieceToMove)
      case _ => updatedBoard.updateWithPiece(move.to, Piece.Moved | pieceToMove);
    }
  }

  def getPiece(coordinate: Int): Int = board(coordinate)
  def getPieceFromSquare(square: Square): Int = board(squareToBoardPosition(square))

  def isInBounds(position: Int): Boolean = {
    position >= 0 && position < board.length
  }

  def isSquareInBounds(square: Square): Boolean = {
    val range = 0 to 7
    range.contains(square.file) && range.contains(square.rank)
  }

  def isPromotionRank(coordinates: Int): Boolean = coordinates <= 7 || coordinates >= 56

  private def updateWithPiece(pos: Int, piece: Int): Board = {
    copy(
      board = board.updated(pos, piece)
    )
  }


  def getKingSquare(white: Boolean): Option[Int] = {
    board.zipWithIndex.find {
      case (it, index) => Piece.isValidPiece(it) && (Piece.isWhite(it) == white) && (Piece.getPieceBits(it) == Piece.King)
    }.map(it => it._2)
  }
}

object Board {
  lazy val squareDistanceToEdge: Map[Int, Vector[Int]] = computeSquareDistanceToEdge();

  private def getInitialBoardArray: Array[Int] = {
    val initBoard = Array.fill(64)(0);
    initBoard(0) = Piece.White | Piece.Rook
    initBoard(7) = Piece.White | Piece.Rook
    initBoard(56) = Piece.Black | Piece.Rook
    initBoard(63) = Piece.Black | Piece.Rook

    initBoard(1) = Piece.White | Piece.Knight
    initBoard(6) = Piece.White | Piece.Knight
    initBoard(57) = Piece.Black | Piece.Knight
    initBoard(62) = Piece.Black | Piece.Knight

    initBoard(2) = Piece.White | Piece.Bishop
    initBoard(5) = Piece.White | Piece.Bishop
    initBoard(58) = Piece.Black | Piece.Bishop
    initBoard(61) = Piece.Black | Piece.Bishop

    initBoard(3) = Piece.White | Piece.Queen
    initBoard(59) = Piece.Black | Piece.Queen

    initBoard(4) = Piece.White | Piece.King
    initBoard(60) = Piece.Black | Piece.King

    for (i <- 0 to 7) {
      initBoard(i + 8) = Piece.White | Piece.Pawn
      initBoard(i + 48) = Piece.Black | Piece.Pawn
    }

    initBoard
  }

  private def computeSquareDistanceToEdge(): Map[Int, Vector[Int]] = {
    val distances: mutable.Map[Int, Vector[Int]] = mutable.Map();
    for(file <- 0 until 8;
        rank <- 0 until 8) {
      val pos = file + rank * 8;

      val distanceNorth = 7 - rank
      val distanceEast = 7 - file
      val distanceSouth = rank
      val distanceWest = file

      distances(pos) = Vector(
        distanceEast,
        distanceWest,
        distanceNorth,
        distanceSouth,
        math.min(distanceNorth, distanceWest),
        math.min(distanceNorth, distanceEast),
        math.min(distanceSouth, distanceEast),
        math.min(distanceSouth, distanceWest)
      )
    }

    distances.toMap
  }

  def squareToBoardPosition(square: Square): Int = {
    square.file + square.rank * 8;
  }

  def boardPositionToSquare(pos: Int): Square = {
    Square(pos % 8, (pos / 8))
  }

}
