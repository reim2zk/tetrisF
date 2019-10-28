package tetris.domain

case class Position(x: Int, y: Int)
case class Rectangle(p0: Position, p1: Position) {
  require(p0.x < p1.x && p0.y < p1.y)
}

case class TetrisBoard(rectangle: Rectangle)

trait PieceType
case object Square extends PieceType
case object Bar extends PieceType

case class Piece(filled: Seq[Position],
                 center: Position,
                 directionIndex: Int,
                 pieceType: PieceType)
case class Pieces(value: Seq[Piece])
case class Tetris(pieces: Pieces, board: TetrisBoard)

object TetrisService {

  def contain(rectangle: Rectangle, p: Position): Boolean =
    rectangle.p0.x <= p.x && p.x <= rectangle.p1.x &&
      rectangle.p0.y <= p.y && p.y <= rectangle.p1.y
  def isSubset(self: Rectangle, other: Rectangle): Boolean = {
    self.p0.x <= other.p0.x && other.p1.x <= self.p1.x &&
    self.p0.y <= other.p0.y && other.p1.y <= self.p1.y
  }
  def intersect(self: Rectangle, other: Rectangle): Boolean = {
    import scala.math.{min, max}
    (max(self.p0.x, other.p0.x) < min(self.p1.x, other.p1.x)) &&
    (max(self.p0.y, other.p0.y) < min(self.p1.y, other.p1.y))
  }

  def rectangle(pieces: Pieces): Rectangle = {
    val ps = for {
      piece <- pieces.value
      p <- piece.filled
    } yield p
    val xs = ps.map(_.x)
    val ys = ps.map(_.y)
    Rectangle(Position(xs.min, ys.min), Position(xs.max, ys.max))
  }
  def rectangle(piece: Piece): Rectangle = {
    val p0 = Position(piece.filled.map(_.x).min, piece.filled.map(_.y).min)
    val p1 = Position(piece.filled.map(_.x).max, piece.filled.map(_.y).max)
    Rectangle(p0, p1)
  }
  def turnRight(piece: Piece): Piece = ???
  def turnLeft(piece: Piece): Piece = ???
  def down(piece: Piece): Piece = ???
  def left(piece: Piece): Piece = ???
  def right(piece: Piece): Piece = ???

  def TetrisRule(tetris: Tetris): Boolean = {
    isSubset(tetris.board.rectangle, rectangle(tetris.pieces))
  }
}
