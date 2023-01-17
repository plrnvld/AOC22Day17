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
    val chamberContents = List[ChamberRow]()
    var lastRow = -1
    
    def freezeShape(shape: Shape) {
        
    }    
}

class ChamberRow(val height: Int) {
    val width: Int = 7
    
}

class Shape(var topLeft: Point, shapeType: ShapeType.Value) {
    def flatPoints(topLeft: Point = Point(0,0)) = Array(Point(topLeft.x,0), Point(topLeft.x+1,0), Point(topLeft.x+2,0), Point(topLeft.x+3,0))

    // ############ Add more points
    
    def canMove(move: Move.Value, chamber: Chamber): Boolean = {
        if (move == Move.Left) {
            true // ##########
        } else {
            true // ##########
        }
    }

    def freezeToRows(rowsToAddTo: List[ChamberRow]) = {
        val level3 = rowsToAddTo(3)
        val level2 = rowsToAddTo(2)
        val level1 = rowsToAddTo(1)
        val level0 = rowsToAddTo(0)
        
        
    }
}

case class Point(val x: Int, val y: Int) {    
}

object ShapeType extends Enumeration {
    val Flat, Cross, Angle, Vertical, Block = Value
}

object Move extends Enumeration {
    val Left, Right = Value
}