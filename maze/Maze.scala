package maze

import scala.util.Random

import java.io.File
import java.awt.Graphics2D
import java.awt.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

class Maze(mazeWidth: Int, mazeHeight: Int) {

  require(mazeWidth > 1)
  require(mazeHeight > 1)

  val width = mazeWidth;
  val height = mazeHeight;

  // Initialize the grid to a two-dim array of cells.
  var grid = Array.tabulate(width, height)((x, y) => new Cell(x, y))

  // Eventually, abstract this out to accept a generation algorith.
  def generate() {
    // Recreate the grid to reset visited flag.
    grid = Array.tabulate(width, height)((x, y) => new Cell(x, y))
    recursiveBacktrack(0, 0)
    //binaryTree()

    print
  }

  // http://weblog.jamisbuck.org/2011/2/1/maze-generation-binary-tree-algorithm#article_body
  def binaryTree() {
    grid = Array.tabulate(width, height)((x, y) => new Cell(x, y))
    val dirs = List(N(), E())
    for (col <- grid) {
      for (cell <- col) {
        println(cell)
        val rdir = Random.shuffle(dirs)
        if (has(cell, rdir.head))
          open(cell, rdir.head)
        else if (has(cell, rdir.tail.head))
          open(cell, rdir.tail.head)
      }
    }
  }

  def recursiveBacktrack(x: Int, y: Int) {
    if (x < 0 || y < 0 || x > width || y > height)
      Unit
    else
      recursiveBacktrack(grid(x)(y))
  }

  // http://weblog.jamisbuck.org/2010/12/27/maze-generation-recursive-backtracking
  def recursiveBacktrack(cell: Cell) {
    cell.visited = true
    for (dir <- Random.shuffle(Cell.dirs)) {
      if (has(cell, dir)) {
        val nx = get(cell, dir)
        if (!nx.visited) {
          cell.open(dir)
          nx.open(dir.opposite)
          recursiveBacktrack(nx)
        }
      }
    }
  }

  def get(cell: Cell, side: Side): Cell = {
    //println("get: " + side + " of " + cell)
    side match {
      case N() => grid(cell.x)(cell.y + 1)
      case S() => grid(cell.x)(cell.y - 1)
      case E() => grid(cell.x + 1)(cell.y)
      case W() => grid(cell.x - 1)(cell.y)
      case _ => throw new IllegalArgumentException
    }

  }

  def has(cell: Cell, side: Side): Boolean = {
    //println("has: " + side + " of " + cell)
    side match {
      case N() => cell.y < height - 1
      case S() => cell.y > 0
      case E() => cell.x < width - 1
      case W() => cell.x > 0
      case _ => throw new IllegalArgumentException
    }
  }

  def open(cell: Cell, side: Side): Cell = {
    if (has(cell, side)) {
      cell.open(side)
      get(cell, side).open(side.opposite)
    }
    cell
  }

  // So that some Maze m1 can be used like: m1(3,2)
  def apply(x: Int, y: Int): Cell = {
    grid(x)(y)
  }

  def print {
    println(toString)
  }

  // http://www.exampledepot.com/egs/javax.imageio/Graphic2File.html
  override def toString = {
    val sb = new StringBuilder
    sb append " " append ("_" * (width * 2 - 1)) append "\n"
    for (y <- (0 to (height - 1)).reverse) {
      sb append "|"
      for (x <- 0 to (width - 1)) {
        sb append (if (grid(x)(y).isOpen(S())) " " else "_")
        sb append (if (grid(x)(y).isOpen(E())) { " " } else "|")
      }
      sb append "\n"
    }
    sb.toString
  }

  def toImage(filename: String): Unit = {
    val file = new File(filename + ".png");
    file.delete
    ImageIO.write(toImage, "png", file);
  }

  // SVG Path: http://tutorials.jenkov.com/svg/path-element.html
  def toImage(): BufferedImage = {
    val C = 20;

    val imgWidth = width * C;
    val imgHeight = height * C;

    // Create a buffered image in which to draw
    val img = new BufferedImage(imgWidth, imgHeight, BufferedImage.TYPE_INT_RGB);

    // Create a graphics contents on the buffered image
    val gfx: Graphics2D = img.createGraphics();

    // Draw graphics
    gfx.setColor(Color.black);
    gfx.fillRect(0, 0, imgWidth, imgHeight);

    for (col <- grid) {
      for (cell <- col) {
        //gfx.setColor(Random.shuffle(List(Color.green, Color.blue, Color.yellow, Color.red)).head);
        gfx.setColor(Color.green)
        //gfx.drawRect((cell.x * C),(cell.y * C), C , C );

        val y = imgHeight - (cell.y * C) - 1
        val x = cell.x * C
        println(y)

        if (!cell.isOpen(W()))
          gfx.drawRect(x, y - C, 1, C);
        if (!cell.isOpen(S()))
          gfx.drawRect(x, y, C, 1);
        if (!cell.isOpen(E()))
          gfx.drawRect((x) + (C - 1), y - C, 1, C);
        if (!cell.isOpen(N()))
          gfx.drawRect(x, y - (C - 1), C, 1);

        //println(  ((cell.x * C) + 1) + ", " + ((cell.y * C) + 1))

      }
    }
    // Graphics context no longer needed so dispose it
    gfx.dispose()
    img
  }

  def toSvgPath(): String = {
    val sb = new StringBuilder
    val C = 10;

    val imgWidth = width * C;
    val imgHeight = height * C;
    for (col <- grid) {
      for (cell <- col) {

        val y = imgHeight - (cell.y * C) - 1
        val x = cell.x * C

        if (!cell.isOpen(W())){
        	sb append " M" + x + "," + (y - C)
        	sb append " L" + x + "," + (y)
        }
        if (!cell.isOpen(S())){
        	sb append " M" + x + "," + y
        	sb append " L" + (x+C) + "," + y
        }
        if (!cell.isOpen(E())){
        	sb append " M" + (x+C-1) + "," + (y - C)
        	sb append " L" + (x+C-1) + "," + (y)
        }
        if (!cell.isOpen(N())){
        	sb append " M" + (x) + "," + (y - (C - 1))
        	sb append " L" + (x+C) + "," + (y - (C - 1))
        }

        //println(  ((cell.x * C) + 1) + ", " + ((cell.y * C) + 1))

      }
    }
    sb.toString

  }
}

object Maze {
  def apply(width: Int, height: Int): Maze = {
    val m = new Maze(width, height)
    m.generate()
    m
  }
}
