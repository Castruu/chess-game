package chess

import processing.core.{PApplet, PFont, PShape}
import ChessGame._
import chess.entities.{Board, Piece, Square}
import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}

class ChessGame extends GameBase {

  var dragState: Option[DragState] = None
  var gameState: ChessState = ChessState()
  val gridHeight = 8
  val gridWidth = 8
  val topMarginPixels: Int = 50
  val totalWidthInPixels: Int = WidthCellInPixels * gridWidth
  val totalHeightInPixels: Int = HeightCellInPixels * gridHeight
  val screenArea: Rectangle = Rectangle(Point(0, 0), totalWidthInPixels.toFloat, totalHeightInPixels.toFloat)
  private var bishopBlack, bishopWhite, kingBlack, kingWhite, knightBlack, knightWhite,
  pawnBlack, pawnWhite, queenBlack, queenWhite, rookBlack, rookWhite, squareAttacked, squareCapture: Option[PShape] = None;

  override def draw(): Unit = {
    setFillColor(Color.White)
    drawGrid(screenArea)
  }

  def drawCellOutline(area: Rectangle, color: Color): Unit = {
    setFillColor(color)
    drawRectangle(area)
  }

  private def drawGrid(gameArea: Rectangle): Unit = {
    for (x <- 0 until gridWidth; y <- (0 until gridHeight).reverse) {
      val rect = getCell(gameArea, (x, y))
      drawCellOutline(rect, if ((x + y) % 2 == 0) Color(119, 153, 84) else Color(233, 237, 204))
      val square = Square(x, y)
      val pieceShape = getShapeFromSquare(square)
      if (dragState.isDefined && dragState.get.startSquare == square) {}
      else {
        pieceShape.foreach(it => shape(it, rect.left, rect.top));
      }
    }

    if (dragState.isDefined) {
      val square: Square = dragState.get.startSquare
      val posOnBoard = Board.squareToBoardPosition(square)
      gameState.legalMoves(posOnBoard).foreach { move =>
        val squareShapeOpt = if (Piece.isValidPiece(gameState.board.getPiece(move.to))) squareCapture else squareAttacked
        val toSquare = Board.boardPositionToSquare(move.to)
        squareShapeOpt.foreach(squareShape => shape(squareShape,
          toSquare.file * 100 + WidthCellInPixels / 2 - squareShape.width / 2,
          (gridHeight - toSquare.rank - 1) * 100 + HeightCellInPixels / 2 - squareShape.height / 2))
      }
      getShapeFromSquare(square).foreach(it => shape(it, emouseX - it.width / 2, emouseY - it.height / 2))
    }

    if(gameState.isCheckmate) {
      val winnerText = if(gameState.whiteTurn) "BLACK" else "WIN"
      drawTextCentered(s"Checkmate -> $winnerText", 50, Point((totalWidthInPixels / 2).toFloat, (totalHeightInPixels / 2).toFloat))
    }

  }


  def getCell(gameArea: Rectangle, p: (Int, Int)): Rectangle = {
    val leftUp = Point(gameArea.left + p._1 * WidthCellInPixels,
      gameArea.top + ((gridHeight - 1) - p._2) * HeightCellInPixels)
    Rectangle(leftUp, WidthCellInPixels.toFloat, HeightCellInPixels.toFloat)
  }

  private def getShapeFromSquare(square: Square): Option[PShape] = {
    val piece: Int = gameState.board.getPieceFromSquare(square);
    piece match {
      case Piece.None => None
      case piece if Piece.isValidPiece(piece) => getShapeFromPiece(piece)
      case _ => None
    }
  }

  private def getShapeFromPiece(piece: Int): Option[PShape] = {
    val white = Piece.isWhite(piece);
    val pieceBits = Piece.getPieceBits(piece)
    pieceBits match {
      case Piece.King => if (white) kingWhite else kingBlack
      case Piece.Queen => if (white) queenWhite else queenBlack
      case Piece.Rook => if (white) rookWhite else rookBlack
      case Piece.Bishop => if (white) bishopWhite else bishopBlack
      case Piece.Knight => if (white) knightWhite else knightBlack
      case Piece.Pawn => if (white) pawnWhite else pawnBlack
      case _ => None
    }
  }

  override def mousePressed(): Unit = {
    val squareCoordinates = (emouseX, emouseY);
    val square = getSquareFromCoordinates(squareCoordinates);
    dragState = gameState.board.getPieceFromSquare(square) match {
      case Piece.None => None
      case piece if !Piece.isValidPiece(piece) => None
      case piece
        if (Piece.isWhite(piece) == gameState.whiteTurn) => Some(
        DragState(
          piece,
          square,
        )
      )
      case _ => None
    }
  }

  override def mouseDragged(): Unit = {
    dragState = dragState.map(_.copy(isDragging = true))
    dragState match {
      case Some(state) => {
        dragState = dragState.map(_.copy(isDragging = true))
      }
      case None =>
    }
  }

  override def mouseReleased(): Unit = {
    dragState match {
      case Some(state) if dragState.get.isDragging => {
        val draggedSquare = (emouseX, emouseY);
        val square = getSquareFromCoordinates(draggedSquare);
        gameState = gameState.movePiece(state.startSquare, square, state.piece)
      }
      case Some(_) =>
      case None =>
    }

    dragState = None
  }


  //
  //  override def keyPressed(event: KeyEvent): Unit = {
  //
  //    event.getKeyCode match {
  //      case VK_LEFT => gameLogic.move(Left)
  //      case VK_RIGHT => gameLogic.move(Right)
  //      case VK_DOWN => gameLogic.move(Down)
  //      case VK_UP   => gameLogic.move(Up)
  //      case VK_SPACE    => gameLogic.setTarget()
  //      case _        => ()
  //    }
  //
  //  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    // If line below gives errors try size(totalWidthInPixels, totalHeightInPixels, PConstants.P2D)
    size(totalWidthInPixels, totalHeightInPixels)
  }

  override def setup(): Unit = {
    // Fonts are loaded lazily, so when we call text()
    // for the first time, there is significant lag.
    // This prevents it from happening during gameplay.
    text("", 0, 0)
    pawnWhite = Some(loadShape("pieces/pawn-w.svg"));
    pawnBlack = Some(loadShape("pieces/pawn-b.svg"));
    bishopWhite = Some(loadShape("pieces/bishop-w.svg"));
    bishopBlack = Some(loadShape("pieces/bishop-b.svg"));
    knightWhite = Some(loadShape("pieces/knight-w.svg"));
    knightBlack = Some(loadShape("pieces/knight-b.svg"));
    rookWhite = Some(loadShape("pieces/rook-w.svg"));
    rookBlack = Some(loadShape("pieces/rook-b.svg"));
    queenWhite = Some(loadShape("pieces/queen-w.svg"));
    queenBlack = Some(loadShape("pieces/queen-b.svg"));
    kingWhite = Some(loadShape("pieces/king-w.svg"));
    kingBlack = Some(loadShape("pieces/king-b.svg"));
    squareAttacked = Some(loadShape("square-attacked.svg"))
    squareCapture = Some(loadShape("square-capture.svg"))
  }

  private def getSquareFromCoordinates(coordinates: (Int, Int)): Square = {
    Square(coordinates._1 / 100, (totalHeightInPixels - coordinates._2) / 100);
  }

}


object ChessGame {

  val WidthCellInPixels: Int = 100
  val HeightCellInPixels: Int = WidthCellInPixels

  def main(args: Array[String]): Unit = {
    PApplet.main("chess.ChessGame")
  }

}

case class DragState(
                      piece: Int,
                      startSquare: Square,
                      isDragging: Boolean = false
                    )
