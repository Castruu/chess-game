package chess

import chess.ChessGame._
import chess.entities.{Board, Piece, Square}
import gameengine.GameBase
import gameengine.graphics.{Color, Point, Rectangle}
import processing.core.{PApplet, PShape}
import processing.event.KeyEvent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ChessGame extends GameBase {

  var dragState: Option[DragState] = None
  var pieceToPromote: Int = Piece.Queen
  var gameStateOpt: Option[ChessState] = None

  def gameState: ChessState = gameStateOpt.get

  var whiteOver, blackOver: Boolean = false
  val whiteX = 100;
  val whiteY = 400;
  val blackX = 500;
  val blackY = 400;
  val buttonSize = 200;
  var waitingForEngine = gameStateOpt.isDefined && gameState.playerWhite == gameState.whiteTurn
  val gridHeight = 8
  val gridWidth = 8
  val topMarginPixels: Int = 50
  val totalWidthInPixels: Int = WidthCellInPixels * gridWidth
  val totalHeightInPixels: Int = HeightCellInPixels * gridHeight
  val screenArea: Rectangle = Rectangle(Point(0, 0), totalWidthInPixels.toFloat, totalHeightInPixels.toFloat)
  private var bishopBlack, bishopWhite, kingBlack, kingWhite, knightBlack, knightWhite,
  pawnBlack, pawnWhite, queenBlack, queenWhite, rookBlack, rookWhite, squareAttacked, squareCapture: Option[PShape] = None;

  override def draw(): Unit = {
    if (gameStateOpt.isDefined) {
      setFillColor(Color.White)
      drawGrid(screenArea)
      drawInfoArea()
      if (!waitingForEngine) {
        if (gameState.playerWhite != gameState.whiteTurn) {
          if (gameState.playWithEngine) {
            waitingForEngine = true;
            Future {
              val updatedState = gameState.playEngineMove();
              gameStateOpt = Option(updatedState)
              waitingForEngine = false;
            }
          }
        }
      }
    } else {
      update(mouseX, mouseY);
      background(30, 30, 30);
      drawTextCentered(s"Welcome to Chess", 50, Point(400, 150))
      drawTextCentered(s"made by vfcastro", 20, Point(400, 180))

      drawTextCentered(s"Select your color: ", 40, Point(400, 350))
      pushStyle();

      if (whiteOver) {
        fill(140, 140, 140);
      } else {
        setFillColor(Color.White);
      }
      stroke(0);
      rect(whiteX.toFloat, whiteY.toFloat, buttonSize.toFloat, buttonSize.toFloat);

      popStyle()
      pushStyle();

      if (blackOver) {
        fill(70, 70, 70);
      } else {
        setFillColor(Color.Black);
      }
      stroke(255);
      rect(blackX.toFloat, blackY.toFloat, buttonSize.toFloat, buttonSize.toFloat);
      popStyle()
    }

  }

  private def update(x: Int, y: Int): Unit = {
    if (overButton(whiteX, whiteY, buttonSize, buttonSize)) {
      whiteOver = true;
      blackOver = false;
    } else if (overButton(blackX, blackY, buttonSize, buttonSize)) {
      blackOver = true;
      whiteOver = false;
    } else {
      blackOver = false;
      whiteOver = false;
    }
  }


  private def drawInfoArea(): Unit = {
    pushStyle()
    fill(255, 255, 255)
    val rect: Rectangle = Rectangle(Point(0, 800), totalWidthInPixels.toFloat, 100)
    drawRectangle(rect)
    popStyle()

    pushStyle()
    fill(40, 40, 40)
    drawTextCentered(s"Selected piece to promote -> ${Piece.getPieceChar(pieceToPromote)}", 30, Point(400, 850))
    drawTextCentered(s"Change pressing B, N, R or Q (defaults to Q)", 20, Point(400, 880))
    popStyle()

  }

  private def drawCellOutline(area: Rectangle, color: Color): Unit = {
    setFillColor(color)
    drawRectangle(area)
  }

  private def drawGrid(gameArea: Rectangle): Unit = {
    for (x <- 0 until gridWidth; y <- 0 until gridHeight) {
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
      gameState.legalMoves.getOrElse(posOnBoard, Set.empty).foreach { move =>
        val squareShapeOpt = if (Piece.isValidPiece(gameState.board.getPiece(move.to))) squareCapture else squareAttacked
        val toSquare = Board.boardPositionToSquare(move.to)
        squareShapeOpt.foreach(squareShape =>
          if (gameState.playerWhite) {
            shape(squareShape,
              toSquare.file * 100 + WidthCellInPixels / 2 - squareShape.width / 2,
              (gridHeight - toSquare.rank - 1) * 100 + HeightCellInPixels / 2 - squareShape.height / 2)
          } else {
            shape(squareShape,
              (gridWidth - toSquare.file - 1) * 100 + WidthCellInPixels / 2 - squareShape.width / 2,
              toSquare.rank * 100 + HeightCellInPixels / 2 - squareShape.height / 2)
          }
        )
      }
      getShapeFromSquare(square).foreach(it => shape(it, emouseX - it.width / 2, emouseY - it.height / 2))

    }

    if (gameState.isCheckmate) {
      val winnerText = if (gameState.whiteTurn) "BLACK WINS" else "WHITE WINS"
      pushStyle()
      fill(0, 0, 0)
      drawTextCentered(s"Checkmate -> $winnerText", 50, Point((totalWidthInPixels / 2).toFloat, (totalHeightInPixels / 2).toFloat))
      popStyle()
      return
    }

    if (gameState.isStalemate) {
      pushStyle()
      fill(0, 0, 0)
      drawTextCentered(s"Stalemate!", 50, Point((totalWidthInPixels / 2).toFloat, (totalHeightInPixels / 2).toFloat))
      popStyle()
      return
    }

  }


  def getCell(gameArea: Rectangle, p: (Int, Int)): Rectangle = {
    val leftUp = if (gameState.playerWhite) {
      Point(gameArea.left + p._1 * WidthCellInPixels,
        gameArea.top + ((gridHeight - 1) - p._2) * HeightCellInPixels)
    } else {
      Point(gameArea.left + ((gridWidth - 1) - p._1) * WidthCellInPixels,
        gameArea.top + p._2 * HeightCellInPixels)
    }
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
    if (gameStateOpt.isEmpty) {
      if (whiteOver) {
        gameStateOpt = Option(
          ChessState(
            playerWhite = true
          )
        );
      }
      if (blackOver) {
        gameStateOpt = Option(
          ChessState(
            playerWhite = false
          )
        );
      }
      return;
    }
    if (emouseY > 800) return;
    if (gameState.whiteTurn != gameState.playerWhite) return
    val squareCoordinates = (emouseX, emouseY);
    val square = getSquareFromCoordinates(squareCoordinates);
    dragState = gameState.board.getPieceFromSquare(square) match {
      case Piece.None => None
      case piece if !Piece.isValidPiece(piece) => None
      case piece
        if (Piece.isWhite(piece) == gameState.playerWhite) => Some(
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
      case Some(_) => {
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
        gameStateOpt = Option(gameState.movePiece(state.startSquare, square, state.piece, Option(pieceToPromote)))
      }
      case Some(_) =>
      case None =>
    }


    dragState = None
  }

  override def keyPressed(event: KeyEvent): Unit = {
    pieceToPromote = event.getKey.toUpper match {
      case 'Q' => Piece.Queen
      case 'R' => Piece.Rook
      case 'B' => Piece.Bishop
      case 'N' => Piece.Knight
      case _ => Piece.Queen
    }
  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    // If line below gives errors try size(totalWidthInPixels, totalHeightInPixels, PConstants.P2D)
    size(totalWidthInPixels, totalHeightInPixels + 100)
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
    if (gameState.playerWhite) {
      Square(coordinates._1 / 100, (totalHeightInPixels - coordinates._2) / 100);
    } else {
      Square((totalWidthInPixels - coordinates._1) / 100, coordinates._2 / 100);
    }
  }

  def overButton(x: Int, y: Int, width: Int, height: Int): Boolean = {
    mouseX >= x && mouseX <= x + width &&
      mouseY >= y && mouseY <= y + height
  }

}


object ChessGame {

  private val WidthCellInPixels: Int = 100
  private val HeightCellInPixels: Int = WidthCellInPixels

  def main(args: Array[String]): Unit = {
    PApplet.main("chess.ChessGame")
  }

}

case class DragState(
                      piece: Int,
                      startSquare: Square,
                      isDragging: Boolean = false
                    )
