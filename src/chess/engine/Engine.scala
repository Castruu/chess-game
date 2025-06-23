package chess.engine

import chess.ChessState
import chess.entities.{Board, Move, Piece, Promotion}

object Engine {

  private val pawnPositionValuation = Vector[Int](
    0, 0, 0, 0, 0, 0, 0, 0,
    50, 50, 50, 50, 50, 50, 50, 50,
    10, 10, 20, 30, 30, 20, 10, 10,
    5, 5, 10, 25, 25, 10, 5, 5,
    0, 0, 0, 20, 20, 0, 0, 0,
    5, -5, -10, 0, 0, -10, -5, 5,
    5, 10, 10, -20, -20, 10, 10, 5,
    0, 0, 0, 0, 0, 0, 0, 0
  )

  private val knightPositionValuation = Vector[Int](
    -50, -40, -30, -30, -30, -30, -40, -50,
    -40, -20, 0, 0, 0, 0, -20, -40,
    -30, 0, 10, 15, 15, 10, 0, -30,
    -30, 5, 15, 20, 20, 15, 5, -30,
    -30, 0, 15, 20, 20, 15, 0, -30,
    -30, 5, 10, 15, 15, 10, 5, -30,
    -40, -20, 0, 5, 5, 0, -20, -40,
    -50, -40, -30, -30, -30, -30, -40, -50
  )

  private val bishopPositionValuation = Vector[Int](
    -20, -10, -10, -10, -10, -10, -10, -20,
    -10, 0, 0, 0, 0, 0, 0, -10,
    -10, 0, 5, 10, 10, 5, 0, -10,
    -10, 5, 5, 10, 10, 5, 5, -10,
    -10, 0, 10, 10, 10, 10, 0, -10,
    -10, 10, 10, 10, 10, 10, 10, -10,
    -10, 5, 0, 0, 0, 0, 5, -10,
    -20, -10, -10, -10, -10, -10, -10, -20
  )

  private val rookPositionValuation = Vector[Int](
    0, 0, 0, 0, 0, 0, 0, 0,
    5, 10, 10, 10, 10, 10, 10, 5,
    -5, 0, 0, 0, 0, 0, 0, -5,
    -5, 0, 0, 0, 0, 0, 0, -5,
    -5, 0, 0, 0, 0, 0, 0, -5,
    -5, 0, 0, 0, 0, 0, 0, -5,
    -5, 0, 0, 0, 0, 0, 0, -5,
    0, 0, 0, 5, 5, 0, 0, 0
  )

  private val queenPositionValuation = Vector[Int](
    -20, -10, -10, -5, -5, -10, -10, -20,
    -10, 0, 0, 0, 0, 0, 0, -10,
    -10, 0, 5, 5, 5, 5, 0, -10,
    -5, 0, 5, 5, 5, 5, 0, -5,
    0, 0, 5, 5, 5, 5, 0, -5,
    -10, 5, 5, 5, 5, 5, 0, -10,
    -10, 0, 5, 0, 0, 0, 0, -10,
    -20, -10, -10, -5, -5, -10, -10, -20
  )

  private val kingMiddleGamePositionValuation = Vector[Int](
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -20, -30, -30, -40, -40, -30, -30, -20,
    -10, -20, -20, -20, -20, -20, -20, -10,
    20, 20, 0, 0, 0, 0, 20, 20,
    20, 30, 10, 0, 0, 10, 30, 20
  )

  private val kingEndGamePositionValuation = Vector[Int](
    -50, -40, -30, -20, -20, -30, -40, -50,
    -30, -20, -10, 0, 0, -10, -20, -30,
    -30, -10, 20, 30, 30, 20, -10, -30,
    -30, -10, 30, 40, 40, 30, -10, -30,
    -30, -10, 30, 40, 40, 30, -10, -30,
    -30, -10, 20, 30, 30, 20, -10, -30,
    -30, -30, 0, 0, 0, 0, -30, -30,
    -50, -30, -30, -30, -30, -30, -30, -50
  )

  private val piecePositionValuation = Map[Int, Vector[Int]](
    (Piece.Pawn, pawnPositionValuation),
    (Piece.Knight, knightPositionValuation),
    (Piece.Bishop, bishopPositionValuation),
    (Piece.Rook, rookPositionValuation),
    (Piece.Queen, queenPositionValuation),
  )

  def testMoveGeneration(state: ChessState, depth: Int): Int = {
    val result = perftHelper(state, depth)
    println(s"Depth: $depth   Count: $result")
    result
  }

  private def perftHelper(state: ChessState, depth: Int): Int = {
    if (depth == 0) return 1

    val moves = state.generateLegalMoves().values.flatMap(_.toList).toList

    moves.map { move =>
      val squareFrom = Board.boardPositionToSquare(move.from)
      val squareTo = Board.boardPositionToSquare(move.to)
      val piece = state.board.getPiece(move.from)
      val promotionBit = move match {
        case promotion: Promotion =>
          Option(promotion.promoteTo)
        case _ => None
      }
      val updatedState = state.movePiece(squareFrom, squareTo, piece, promotionBit)
      perftHelper(updatedState, depth - 1)
    }.sum

  }

  def search(state: ChessState, depth: Int = 1, alpha: Int = Int.MinValue, beta: Int = Int.MaxValue): (Int, Option[Move]) = {
    val moves = state.generateLegalMoves().values.flatMap(_.toList).toList
    if(state.isCheckmate) {
      return if (state.whiteTurn) (Int.MinValue + 1, None) else (Int.MaxValue - 1, None)
    }

    if (state.isStalemate) {
      return (0, None)
    }

    if (depth == 0) {
      return (evaluatePosition(state.board.board), None)
    }

    var bestMove: Option[Move] = None

    if (state.whiteTurn) {
      var maxEval = Int.MinValue
      var currentAlpha = alpha

      for (move <- moves) {
        val promotionBits = move match {
          case promotion: Promotion =>
            Some(promotion.promoteTo)
          case _ => None
        }
        val updatedState = state.movePiece(
          Board.boardPositionToSquare(move.from),
          Board.boardPositionToSquare(move.to),
          state.board.getPiece(move.from),
          promotionBits);
        val eval = search(updatedState, depth - 1, currentAlpha, beta)._1

        if (eval > maxEval) {
          bestMove = Some(move)
          maxEval = eval
        }
        currentAlpha = math.max(currentAlpha, eval)
        if (beta <= currentAlpha) return (maxEval, bestMove)
      }
      (maxEval, bestMove)
    } else {
      var minEval = Int.MaxValue
      var currentBeta = beta
      for (move <- moves) {
        val promotionBits = move match {
          case promotion: Promotion =>
            Some(promotion.promoteTo)
          case _ => None
        }
        val updatedState = state.movePiece(
          Board.boardPositionToSquare(move.from),
          Board.boardPositionToSquare(move.to),
          state.board.getPiece(move.from),
          promotionBits);
        val eval = search(updatedState, depth - 1, alpha, currentBeta)._1
        if (eval < minEval) {
          bestMove = Some(move)
          minEval = eval
        }
        currentBeta = math.min(currentBeta, eval)
        if (currentBeta <= alpha) return (minEval, bestMove)
      }
      (minEval, bestMove)
    }
  }

  private def evaluatePosition(board: Vector[Int]): Int = {
    var value = 0;
    for ((piece, idx) <- board.zipWithIndex) {
      if (Piece.isValidPiece(piece)) {
        val colorFactor = if (Piece.isWhite(piece)) 1 else -1
        val pieceBits = Piece.getPieceBits(piece)
        val baseValue = pieceValue(pieceBits)
        val posValueIdx = if (Piece.isWhite(piece)) idx else board.size - 1 - idx
        val positionValue = pieceBits match {
          case Piece.King => if (!board.exists(it => Piece.getPieceBits(it) == Piece.Queen)) {
            kingEndGamePositionValuation(posValueIdx)
          } else kingMiddleGamePositionValuation(posValueIdx)
          case _ => piecePositionValuation(pieceBits)(posValueIdx)
        }
        value += ((baseValue + positionValue) * colorFactor)
      }
    }
    value
  }

  private val pieceValue = Map[Int, Int](
    (Piece.Pawn, 100),
    (Piece.Knight, 320),
    (Piece.Bishop, 330),
    (Piece.Rook, 500),
    (Piece.Queen, 900),
    (Piece.King, Short.MaxValue)
  )


}
