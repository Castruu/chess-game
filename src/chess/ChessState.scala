package chess

import chess.engine.Engine
import chess.entities._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class ChessState(
                       board: Board = new Board(),
                       whiteTurn: Boolean = true,
                       playerWhite: Boolean = false,
                       playWithEngine: Boolean = true,
                       moveStack: Vector[Move] = Vector(),
                     ) {

  private lazy val pseudoLegalMoves = generatePseudoLegalMoves();

  lazy val legalMoves: Map[Int, Set[Move]] = generateLegalMoves()

  def generateLegalMoves(): Map[Int, Set[Move]] = {
    val legalMoves: mutable.Map[Int, Set[Move]] = mutable.Map()

    for ((pos, moves) <- pseudoLegalMoves.filter { case (pos, _) => Piece.isWhite(board.getPiece(pos)) == whiteTurn }) {
      val pieceMoves: ListBuffer[Move] = ListBuffer()
      for (move <- moves) {
        val updatedBoard = board.makeMove(move)
        val updatedState = copy(
          board = updatedBoard,
          whiteTurn = !whiteTurn,
          moveStack = moveStack.appended(move)
        )

        val kingSquare = updatedState.board.getKingSquare(whiteTurn);
        if (kingSquare.isDefined) {
          val attacksKing = updatedState.pseudoLegalMoves.values.exists(moves =>
            moves.exists(it => it.to == kingSquare.get)
          )
          if (!attacksKing) {
            pieceMoves += move;
          }
        }
      }

      legalMoves(pos) = pieceMoves.toSet
    }

    legalMoves.toMap
  }

  private def generatePseudoLegalMoves(): Map[Int, Set[Move]] = {
    val pseudoMovesMap: mutable.Map[Int, Set[Move]] = mutable.Map()

    board.board
      .zipWithIndex.foreach {
        case (piece, pos) if Piece.isValidPiece(piece) =>
          val extraMoves: Set[Move] = Piece.getPieceBits(piece) match {
            case Piece.Pawn => getEnPassantIfEnabled(pos).toSet
            case _ => Set.empty
          }
          pseudoMovesMap(pos) = (Piece.generateMoves(board, pos) ++ extraMoves)
        case _ =>
      }
    val kingSquare = board.getKingSquare(whiteTurn);
    if (kingSquare.isDefined) {
      val kingMoves = pseudoMovesMap(kingSquare.get) ++ getCastleRightAndLeftMove(board.getPiece(kingSquare.get),
        pseudoMovesMap.filter(it => Piece.isWhite(board.getPiece(it._1)) != whiteTurn)
          .values.flatten.toSet)
        .filter(it => it.isDefined).map(it => it.get)
      pseudoMovesMap(kingSquare.get) = kingMoves
    }


    pseudoMovesMap.toMap
  }

  def movePiece(from: Square, to: Square, piece: Int, promotionBits: Option[Int] = None): ChessState = {
    if (isCheckmate || isStalemate) return this;
    val fromPos = Board.squareToBoardPosition(from);
    val toPos = Board.squareToBoardPosition(to);
    if (piece != board.getPiece(fromPos)) return this;
    val possibleMoves = legalMoves.getOrElse(fromPos, Set.empty).filter(it => it.to == toPos)
    if (possibleMoves.isEmpty) return this;
    val move = if (possibleMoves.size > 1) {
      possibleMoves.map(it => it.asInstanceOf[Promotion])
        .find(it => Piece.getPieceBits(it.promoteTo) == Piece.getPieceBits(promotionBits.get))
    } else possibleMoves.find(_ => true)


    if (move.isEmpty) return this;

    val updatedBoard = board.makeMove(move.get)
    if (updatedBoard == board) return this;

    copy(
      board = updatedBoard,
      whiteTurn = !whiteTurn,
      moveStack = moveStack.appended(move.get)
    )
  }

  private def getEnPassantIfEnabled(attackerPos: Int): Option[EnPassant] = {
    val pieceToAttack = board.getPiece(attackerPos)
    val attackerIsWhite = Piece.isWhite(pieceToAttack);
    if (Piece.getPieceBits(pieceToAttack) != Piece.Pawn) return None
    if (moveStack.isEmpty) return None
    val lastMove = moveStack.last
    val differenceAttackerTarget = attackerPos - lastMove.to;
    if (math.abs(differenceAttackerTarget) != 1) return None
    if (math.abs(lastMove.from - lastMove.to) != 16) return None
    val piece = board.getPiece(lastMove.to)
    if (Piece.isWhite(piece) == attackerIsWhite) return None
    val pieceBits = Piece.getPieceBits(piece)
    if (pieceBits == Piece.None) return None
    if (pieceBits != Piece.Pawn) return None
    val enPassantRank = if (attackerIsWhite) 4 else 3
    val moveToRank = attackerPos / 8
    if (moveToRank != enPassantRank) return None;
    val colorOffset = if (attackerIsWhite) 8 else -8
    val toPos = attackerPos + colorOffset - differenceAttackerTarget
    Some(EnPassant(attackerPos, toPos, lastMove.to, piece));
  }

  private def getCastleRightAndLeftMove(king: Int, attackerPseudoMoves: Set[Move]): Set[Option[Castle]] = {
    Set(getCastleMove(king, right = false, attackerPseudoMoves), (getCastleMove(king, right = true, attackerPseudoMoves)))
  }

  private def getCastleMove(king: Int, right: Boolean, attackerPseudoMoves: Set[Move]): Option[Castle] = {
    if (Piece.hasMoved(king)) return None
    val rookFile = if (right) 7 else 0;
    val kingRank = if (Piece.isWhite(king)) 0 else 56;
    if (attackerPseudoMoves.exists(it => it.to == (kingRank + 4))) return None
    val rookPos = rookFile + kingRank
    val piece = board.getPiece(rookPos)
    val pieceBits = Piece.getPieceBits(piece)
    if (pieceBits != Piece.Rook) return None
    if (Piece.hasMoved(piece)) return None
    val range = if (right) 5 to 6 else 1 to 3
    for (i <- range) {
      val pos = i + kingRank
      val posPiece = board.getPiece(pos)
      if (Piece.getPieceBits(posPiece) != Piece.None) return None
      if (attackerPseudoMoves.exists(it => it.to == pos)) return None
    }

    Some(Castle(4 + kingRank, (if (right) 6 else 2) + kingRank, right))
  }

  def playEngineMove(): ChessState = {
    val (eval, bestMove) = Engine.search(this, 4);
    if (bestMove.isDefined) {
      return movePiece(
        Board.boardPositionToSquare(bestMove.get.from),
        Board.boardPositionToSquare(bestMove.get.to),
        board.getPiece(bestMove.get.from),
        bestMove.get match {
          case promotion: Promotion =>
            Some(promotion.promoteTo)
          case _ => None
        }
      )
    }
    this
  }

  private def isKingAttacked: Boolean = {
    val kingSquare = board.getKingSquare(whiteTurn)
    if(kingSquare.isDefined) {
      return pseudoLegalMoves.values.exists(moves =>
        moves.exists(it => it.to == kingSquare.get)
      )
    }

    true
  }

  def isStalemate: Boolean = {
    legalMoves.forall { case (_, moves) => moves.isEmpty } && !isKingAttacked
  }

  def isCheckmate: Boolean = {
    legalMoves.forall { case (_, moves) => moves.isEmpty } && isKingAttacked
  }


}
