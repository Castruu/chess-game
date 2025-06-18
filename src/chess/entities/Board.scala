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

//  def getEnPassantIfEnabled(attackerPos: Square, attacker: Piece): Option[EnPassant] = {
//    if (!(attacker.isInstanceOf[Pawn])) return None
//    if (moveStack.isEmpty) return None
//    val lastMove = moveStack.top
//    if (abs(attackerPos.x - lastMove.to.x) != 1) return None
//    if (lastMove.to.y != attackerPos.y) return None
//    if (abs(lastMove.to.y - lastMove.from.y) != 2) return None
//
//    val piece = getPiece(lastMove.to)
//    if (piece.isEmpty) return None
//    if (!(piece.get.isInstanceOf[Pawn])) return None
//
//    if (!((piece.get.white && lastMove.to.y == 3) || (!piece.get.white && lastMove.to.y == 4))) return None;
//
//    val colorOffset = if (attacker.white) 1 else -1
//    Some(EnPassant(attackerPos, Square(lastMove.to.x, lastMove.to.y + colorOffset), lastMove.to, piece.get.asInstanceOf[Pawn]));
//  }
//
//  def getCastleMove(king: King, right: Boolean): Option[Castle] = {
//    if (king.moved) return None
//    val rookFile = if (right) 7 else 0;
//    val kingRank = if (king.white) 0 else 7;
//    val rookSquare = Square(rookFile, kingRank)
//    val piece = getPiece(rookSquare)
//    if (piece.isEmpty) return None
//    if (!piece.get.isInstanceOf[Rook]) return None
//    val rook = piece.get.asInstanceOf[Rook]
//    if (rook.moved) return None
//    val range = if (right) 5 to 6 else 1 to 3
//    for (i <- range) {
//      val square = Square(i, kingRank)
//      val squareHasPiece = getPiece(square).isDefined
//      if (squareHasPiece) return None
//      if (attackedSquares(!king.white).contains(square)) return None
//    }
//
//    Some(Castle(Square(4, kingRank), Square(if (right) 6 else 2, kingRank), right))
//  }
//
//
//  def isSquareAttacked(byWhite: Boolean, square: Square): Boolean = {
//    attackedSquares(byWhite).contains(square)
//  }
//
//  private def updateMoveCache(): Unit = {
//    whiteMoves ++= generateMovesByColor(white = true);
//    blackMoves ++= generateMovesByColor(white = false);
//  }
//
//  private def generateMovesByColor(white: Boolean): Map[Square, List[Move]] = {
//    val moves = for {
//      (files, x) <- board.zipWithIndex
//      (piece, y) <- files.zipWithIndex
//      if piece.exists(_.white == white)
//    } yield {
//      val square = Square(x, y)
//      val pieceMoves = piece.get.generateMoves(square, this)
//      piece.get match {
//        case _: Pawn =>
//          val colorFactor = if (white) 1 else -1
//          val squares = Array(Square(x - 1, y + colorFactor), Square(x + 1, y + colorFactor))
//          squares.foreach(sq => attackedSquares(white) += sq)
//        case _ => pieceMoves.foreach(move => attackedSquares(white) += move.to)
//      }
//      square -> pieceMoves
//    }
//
//    moves.toMap
//  }
//
//  private def isCheck(white: Boolean): Boolean = {
//    val kingSquare = board.zipWithIndex.view.flatMap { case (files, x) =>
//      files.zipWithIndex.collectFirst {
//        case (Some(king: King), y) if king.white == white => Square(x, y)
//      }
//    }.headOption
//
//    kingSquare.exists(attackedSquares(!white).contains)
//  }

  def updateWithPiece(pos: Int, piece: Int): Board = {
    copy(
      board = board.updated(pos, piece)
    )
  }


  def getKingSquare(white: Boolean): Int = {
    board.zipWithIndex.find {
      case (it, index) => Piece.isValidPiece(it) && (Piece.isWhite(it) == white) && (Piece.getPieceBits(it) == Piece.King)
    }.get._2
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
