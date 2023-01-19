import scala.io.Source
import scala.collection.mutable.ListBuffer

object Main {
    def main(args: Array[String]): Unit = {
        val source = Source.fromFile("Example.txt")
        val moves = source.getLines().next.toList
            .map(c => if (c == '<') Move.Left else Move.Right)
      
        val chamber = new Chamber()
        chamber.chamberRows += new ChamberRow(0)
        chamber.chamberRows += new ChamberRow(1)
        chamber.chamberRows += new ChamberRow(2)
        chamber.chamberRows += new ChamberRow(3)
        chamber.chamberRows += new ChamberRow(4)

        val cross = new Shape(Point(1,2), ShapeType.Cross)
        chamber.freeze(cross)
        val flat = new Shape(Point(1,4), ShapeType.Flat)
        chamber.printChamber(Some(flat))
    }
}

class Chamber {
    val chamberRows = ListBuffer[ChamberRow]() // Lowest one first
    var lastRow = -1
    
    def freeze(shape: Shape) {
        for (point <- shape.points()) {
            freezePoint(point)
        }
    }

    def printChamber(currentShape: Option[Shape]) = {    
        for (row <- chamberRows.reverse) {
            row.printRow(currentShape)        
        }
        
        println("+-------+")
    }

    def rowForHeight(height: Int): ChamberRow = {
        chamberRows.find(r => r.height == height).get
    }

    def freezePoint(point: Point) {
        val row = rowForHeight(point.y)
        row.rowValues(point.x) = true
    }
}

class ChamberRow(val height: Int) {
    var rowValues: ListBuffer[Boolean] = ListBuffer.fill(7)(false)

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

class Shape(var topLeft: Point, val shapeType: ShapeType.Value) {
    private def flatPoints(topLeft: Point) = {
        val bl = topLeft
        List(Point(bl.x,bl.y), Point(bl.x+1,bl.y), Point(bl.x+2,bl.y), Point(bl.x+3,bl.y))
    }
    
    private def crossPoints(topLeft: Point) = {
        val bl = topLeft.addY(-2)
        List(Point(bl.x+1,bl.y), Point(bl.x,bl.y+1), Point(bl.x+1,bl.y+1), Point(bl.x+2,bl.y+1),Point(bl.x+1,bl.y+2))
    }
    
    private def anglePoints(topLeft: Point) = {
        val bl = topLeft.addY(-2)
        List(Point(bl.x,bl.y), Point(bl.x+1,bl.y), Point(bl.x+2,bl.y), Point(bl.x+2,bl.y+1),Point(bl.x+2,bl.y+2))
    }

    private def verticalPoints(topLeft: Point) = {
        val bl = topLeft.addY(-3)
        List(Point(bl.x,bl.y), Point(bl.x,bl.y+1), Point(bl.x,bl.y+2), Point(bl.x,bl.y+3))
    }

    private def blockPoints(topLeft: Point) = {
        val bl = topLeft.addY(-1)
        List(Point(bl.x,bl.y), Point(bl.x+1,bl.y), Point(bl.x,bl.y+1), Point(bl.x+1,bl.y+1))
    }

    def points(tl: Point = topLeft): List[Point] = 
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