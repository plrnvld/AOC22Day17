import scala.io.Source
import scala.collection.mutable.ListBuffer

object Main {
    def main(args: Array[String]): Unit = {
        val source = Source.fromFile("Example.txt")
        val moves = source.getLines().next.toList
            .map(c => if (c == '<') Move.Left else Move.Right)

        val shapeOrder = List[ShapeType.Value](ShapeType.Flat, ShapeType.Cross, ShapeType.Angle, ShapeType.Vertical, ShapeType.Block)
      
        val chamber = new Chamber()

        for (i <- 1 to 10) {
            val newShapeType = shapeOrder((i - 1) % shapeOrder.length)
            println()
            println(s"Shape $i ($newShapeType):")
            
            val start = chamber.addRowsForNewRoundAndGetStart(newShapeType)
            val shape = new Shape(start, newShapeType)

            var frozen = false
            while (!frozen) {
                if (shape.canMoveDown(chamber))
                    shape.moveDown()
                else {
                    chamber.freeze(shape)
                    frozen = true
                }
            }

            chamber.printChamber(Some(shape))
        }        
    }
}

class Chamber {
    val chamberRows = ListBuffer[ChamberRow]() // Lowest one first
    var lastRow = -1
    var highestRock = -1

    private def shapeHeight(shapeType: ShapeType.Value): Int = 
        shapeType match {
            case ShapeType.Flat => 1
            case ShapeType.Cross => 3
            case ShapeType.Angle => 3
            case ShapeType.Vertical => 4
            case ShapeType.Block => 2
        }

    def addRowsForNewRoundAndGetStart(shapeType: ShapeType.Value): Point = {
        val newStartHeight = highestRock + 3 + shapeHeight(shapeType)
        
        for (height <- (lastRow + 1) to newStartHeight)
            chamberRows += new ChamberRow(height)

        lastRow = newStartHeight

        Point(2, newStartHeight)
    }

    def freeze(shape: Shape) {
        for (point <- shape.points()) {
            freezePoint(point)
        }
    }

    def isOccupied(point: Point): Boolean ={
        if (point.y > lastRow)
            return false;

        chamberRows(point.y).isOccupied(point.x)
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

        if (point.y > highestRock)
            highestRock = point.y
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

    def isOccupied(x: Int): Boolean = rowValues(x)
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

    def bottomRight(): Point = 
        shapeType match {
            case ShapeType.Flat => topLeft.addX(3)
            case ShapeType.Cross => topLeft.add(Point(2, -2))
            case ShapeType.Angle => topLeft.add(Point(2, -2))
            case ShapeType.Vertical => topLeft.addY(-3)
            case ShapeType.Block => topLeft.add(Point(1, -1))
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

    def canMoveLeft(chamber: Chamber): Boolean = {
        val nextTopLeft = topLeft.addX(-1)
        val pts = points(nextTopLeft)

        if (pts.exists(p => p.x < 0))
            return false;

        !givesCollision(pts, chamber)
    }

    def canMoveRight(chamber: Chamber): Boolean = {
        val nextTopLeft = topLeft.addX(1)
        val pts = points(nextTopLeft)

        if (pts.exists(p => p.x > 6))
            return false;

        !givesCollision(pts, chamber)
    }

    def canMoveDown(chamber: Chamber): Boolean = {
        val nextTopLeft = topLeft.addY(-1)
        val pts = points(nextTopLeft)

        if (pts.exists(p => p.y < 0))
            return false;

        !givesCollision(pts, chamber)
    }

    def moveLeft() = topLeft = topLeft.addX(-1)

    def moveRight() = topLeft = topLeft.addX(1)

    def moveDown() = topLeft = topLeft.addY(-1)

    def givesCollision(points: List[Point], chamber: Chamber): Boolean = 
        points.exists(p => chamber.isOccupied(p))
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