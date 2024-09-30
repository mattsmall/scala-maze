package maze

import scala.xml.Elem

class Svg {

}

object Svg {

  def toSvg(maze: Maze): Elem = {
    val path = toSvgPath(maze)//"M0,0 L100,100"
    <svg xmlns:svg="http://www.w3.org/2000/svg" xmlns="http://www.w3.org/2000/svg" version="1.0" width="100" height="100">
      <g>
        <path d={ path } style="stroke: rgb(0, 0, 0); stroke-width: 0.985156; stroke-linecap: square; stroke-miterlimit: 4; stroke-dasharray: none; stroke-opacity: 1;"/>
      </g>
    </svg>
  }

  def toSvgPath(maze: Maze): String = {
    val sb = new StringBuilder
    val C = 10;

    val imgWidth = maze.width * C;
    val imgHeight = maze.height * C;
    for (col <- maze.grid) {
      for (cell <- col) {

        val y = imgHeight - (cell.y * C) - 1
        val x = cell.x * C

        if (!cell.isOpen(W())) {
          sb append " M" + x + "," + (y - C)
          sb append " L" + x + "," + (y)
        }
        if (!cell.isOpen(S())) {
          sb append " M" + x + "," + y
          sb append " L" + (x + C) + "," + y
        }
        if (!cell.isOpen(E())) {
          sb append " M" + (x + C - 1) + "," + (y - C)
          sb append " L" + (x + C - 1) + "," + (y)
        }
        if (!cell.isOpen(N())) {
          sb append " M" + (x) + "," + (y - (C - 1))
          sb append " L" + (x + C) + "," + (y - (C - 1))
        }

        //println(  ((cell.x * C) + 1) + ", " + ((cell.y * C) + 1))

      }
    }
    sb.toString

  }

}