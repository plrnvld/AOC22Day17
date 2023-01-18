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
    val chamberRows = List[ChamberRow]() // Lowest one first
    var lastRow = -1
    
    def freezeShape(shape: Shape) {
        
    }

    def printChamber(currentShape: Option[Shape]) = {    
        for (row <- chamberRows.reverse) {
            row.printRow(currentShape)        
        }
        
        println("+-------+")
    }
}

class ChamberRow(val height: Int) {
    var rowValues: List[Boolean] = List.fill(7)(false)

    def printRow(currentShape: Option[Shape]) = {
        val shapePoints = currentShape match {
            case None => List[Point]()
            case Some(shape) => shape.pointsOnHeight(height)
        }

        print('|')
        for((hasValue, x) <- rowValues.zipWithIndex) {
          if (shapePoints.exists(p => p.x == x)) 
            print('@')
          else print(if (hasValue) '#' else '.')
        } 
        print('|')
        
        println()
    }
}

class Shape(var topLeft: Point, shapeType: ShapeType.Value) {
    def flatPoints(topLeft: Point = Point(0,0)) = {
        val bl = topLeft
        List(Point(bl.x,bl.y), Point(bl.x+1,bl.y), Point(bl.x+2,bl.y), Point(bl.x+3,bl.y))
    }
    
    def crossPoints(topLeft: Point = Point(0,0)) = {
        val bl = topLeft.addY(-2)
        List(Point(bl.x+1,bl.y), Point(bl.x,bl.y+1), Point(bl.x+1,bl.y+1), Point(bl.x+2,bl.y+1),Point(bl.x+1,bl.y+2))
    }
    
    def anglePoints(topLeft: Point = Point(0,0)) = {
        val bl = topLeft.addY(-2)
        List(Point(bl.x,bl.y), Point(bl.x+1,bl.y), Point(bl.x+2,bl.y), Point(bl.x+2,bl.y+1),Point(bl.x+2,bl.y+2))
    }

    def verticalPoints(topLeft: Point = Point(0,0)) = {
        val bl = topLeft.addY(-3)
        List(Point(bl.x,bl.y), Point(bl.x,bl.y+1), Point(bl.x,bl.y+2), Point(bl.x,bl.y+3))
    }

    def blockPoints(topLeft: Point = Point(0,0)) = {
        val bl = topLeft.addY(-1)
        List(Point(bl.x,bl.y), Point(bl.x+1,bl.y), Point(bl.x,bl.y+1), Point(bl.x+1,bl.y+1))
    }

    def points(tl: Point): List[Point] = 
        shapeType match {
            case ShapeType.Flat => flatPoints(tl)
            case ShapeType.Cross => crossPoints(tl)
            case ShapeType.Angle => anglePoints(tl)
            case ShapeType.Vertical => verticalPoints(tl)
            case ShapeType.Block => blockPoints(tl)
        }

    def pointsOnHeight(height: Int): List[Point] = points(topLeft).filter(p => p.y == height)

    def canMove(move: Move.Value, chamber: Chamber): Boolean = {
        if (move == Move.Left) {
            // ### Add collision detection
            val nextTopLeft = topLeft.addX(-1)
            points(nextTopLeft).exists(p => p.x < 0)
        } else {
            // ### Add collision detection
            val nextTopLeft = topLeft.addX(1)
            points(nextTopLeft).exists(p => p.x > 6)
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
    def add(other: Point): Point = Point(x + other.x, y + other.y)
    def addX(deltaX: Int): Point = Point(x + deltaX, y)
    def addY(deltaY: Int): Point = Point(x, y + deltaY)
}

object ShapeType extends Enumeration {
    val Flat, Cross, Angle, Vertical, Block = Value
}

object Move extends Enumeration {
    val Left, Right = Value
}