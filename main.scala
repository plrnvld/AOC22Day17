import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("Example.txt")
    val moves = source.getLines().next.toList
      .map(c => if (c == '<') Move.Left else Move.Right)
      
    for (move <- moves) {
        println(move)
    }
  }
}

class Chamber {
    // Needs a better way to store existing shapes
    
    def freezeShape(shape: Shape) {
        
    }    
}

class Shape(var topLeft: Point, shapeType: ShapeType.Value) {
    def canMove(move: Move.Value, chamber: Chamber) {
        if (move == Move.Left) {
            
        } else {
            
        }
    }
}

class Point(val x: Int, val y: Int) {
    
}

object ShapeType extends Enumeration {
    val Flat, Cross, Angle, Vertical, Block = Value
}

object Move extends Enumeration {
    type Move = Value
    val Left, Right = Value
}