package chess

import chess.entities.{Board, Castle, EnPassant, Move, NormalMove, Piece, Square}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class ChessState(
                       board: Board = new Board(),
                       whiteTurn: Boolean = true,
                       moveStack: Vector[Move] = Vector(),
                     ) {

  private lazy val pseudoLegalMoves = generatePseudoLegalMoves();

  lazy val legalMoves: Map[Int, Set[Move]] = generateLegalMoves()

  private def generateLegalMoves(): Map[Int, Set[Move]] = {
    val legalMoves: mutable.Map[Int, Set[Move]] = mutable.Map()

    for ((pos, moves) <- pseudoLegalMoves) {
      val pieceMoves: ListBuffer[Move] = ListBuffer()
      for (move <- moves) {
        val attacksKing = checkPseudoMoveIsValid(move)
        if (!attacksKing) {
          pieceMoves += move
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
        case (piece, pos) if Piece.isValidPiece(piece) && (Piece.isWhite(piece) == whiteTurn) =>
          val extraMoves: Set[Move] = Piece.getPieceBits(piece) match {
            case Piece.Pawn => getEnPassantIfEnabled(pos).toSet
            case Piece.King => getCastleRightAndLeftMove(piece).filter(it => it.isDefined).map(it => it.get)
            case _ => Set.empty
          }
          pseudoMovesMap(pos) = (Piece.generateMoves(board, pos) ++ extraMoves)
        case _ =>
      }

    pseudoMovesMap.toMap
  }

  def movePiece(from: Square, to: Square, piece: Int): ChessState = {
    if(isCheckmate) return this;
    val fromPos = Board.squareToBoardPosition(from);
    val toPos = Board.squareToBoardPosition(to);
    if (piece != board.getPiece(fromPos)) return this;
    val move = legalMoves(fromPos).find(it => it.to == toPos);
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
    val moveToRank = lastMove.to / 8
    if (moveToRank != enPassantRank) return None;
    val colorOffset = if (attackerIsWhite) 8 else -8
    val toPos = attackerPos + colorOffset - differenceAttackerTarget
    Some(EnPassant(attackerPos, toPos, lastMove.to, piece));
  }

  private def getCastleRightAndLeftMove(king: Int): Set[Option[Castle]] = {
    Set(getCastleMove(king, right = false), (getCastleMove(king, right = true)))
  }

  private def getCastleMove(king: Int, right: Boolean): Option[Castle] = {
    if (Piece.hasMoved(king)) return None
    val rookFile = if (right) 7 else 0;
    val kingRank = if (Piece.isWhite(king)) 0 else 56;
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
      if(!checkPseudoMoveIsValid(
        NormalMove(from = kingRank + 4, to = pos)
      )) return None
    }

    Some(Castle(4 + kingRank, (if (right) 6 else 2) + kingRank, right))
  }

  private def checkPseudoMoveIsValid(move: Move): Boolean = {
    val updatedBoard = board.makeMove(move)
    val updatedState = copy(
      board = updatedBoard,
      whiteTurn = !whiteTurn,
      moveStack = moveStack.appended(move)
    )
    updatedState.pseudoLegalMoves.values.exists(moves =>
      moves.exists(it => it.to == updatedState.board.getKingSquare(whiteTurn))
    )
  }

  def isCheckmate: Boolean = {
    legalMoves.forall { case (_, moves) => moves.isEmpty }
  }


}
