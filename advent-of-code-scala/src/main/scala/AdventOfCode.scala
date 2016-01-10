import scala.util.matching.Regex
import scala.io.Source
import scala.collection.mutable.Map
import scalaz._
import Scalaz._

object AdventOfCode {

	// Day 5: Doesn't He Have Intern-Elves For This?

	def isNice(line: String): Boolean = {
		val pattern1 = ".*[aeiou].*[aeiou].*[aeiou].*".r
		val pattern2 = ".*([a-z])\\1.*".r
		val pattern3 = ".*(ab|cd|pq|xy).*".r

		(pattern1 findFirstIn line) match {
			case Some(_) => (pattern2 findFirstIn line) match {
				case Some(_) => (pattern3 findFirstIn line) match {
					case Some(_) => false
					case _ => true
				}
				case _ => false
			}
			case _ => false
		}
	}

	def isNice2(line: String): Boolean = {
		val pattern1 = ".*([a-z]{2}).*\\1.*".r
		val pattern2 = ".*([a-z])[a-z]\\1.*".r

		(pattern1 findFirstIn line) match {
			case Some(_) => (pattern2 findFirstIn line) match {
				case Some(_) => true
				case _ => false
			}
			case _ => false
		}
	}

	def numNice(filename: String): Int = {
		var numNice = 0
		Source.fromFile(filename).getLines().foreach{ line => 
			if(isNice2(line)){ numNice += 1 }
		}
		numNice
	}

	// Day 6: Probably a Fire Hazard

	def range(xs: List[Int], ys: List[Int]): List[List[Int]] = {
    	(xs, ys).zipped.map((x, y) => List.range(x, y + 1)).sequence
    }

    def turnOn(g: Map[Int,Set[Int]], x: Int, y: Int): Map[Int,Set[Int]] = {
    	g + (x -> ((g getOrElse (x,Set[Int]())) + y))
    }

    def turnOff(g: Map[Int,Set[Int]], x: Int, y: Int): Map[Int,Set[Int]] = {
    	g + (x -> ((g getOrElse (x,Set[Int]())) - y))
    }

	def turnOnLights(grid: Map[Int,Set[Int]], coordinates: List[List[Int]]): Map[Int,Set[Int]] = {
		coordinates.foldLeft(grid){ case (g, (x :: y :: _)) => turnOn(g, x, y) }
	}

	def toggleLights(grid: Map[Int,Set[Int]], coordinates: List[List[Int]]): Map[Int,Set[Int]] = {
		coordinates.foldLeft(grid){ case (g, (x :: y :: _)) => 
			(g get x) match {
				case Some(ys) => if(ys contains y){ turnOff(g, x, y) } else{ turnOn(g, x, y) }
				case None => turnOn(g, x, y)
			}
		}
	}

	def turnOffLights(grid: Map[Int,Set[Int]], coordinates: List[List[Int]]): Map[Int,Set[Int]] = {
		coordinates.foldLeft(grid){ case (g, (x :: y :: _)) => turnOff(g, x, y) }
	}

	def numLit(filename: String): Int = {
		var grid:Map[Int,Set[Int]] = Map()
		val onpattern = "turn on (\\d+),(\\d+) through (\\d+),(\\d+)".r
		val togglepattern = "toggle (\\d+),(\\d+) through (\\d+),(\\d+)".r
		val offpattern = "turn off (\\d+),(\\d+) through (\\d+),(\\d+)".r

		Source.fromFile(filename).getLines().foreach{ line => 
			grid = line match {
				case onpattern(x1,y1,x2,y2) => turnOnLights(grid,range(List(x1.toInt,y1.toInt),List(x2.toInt,y2.toInt)))
				case togglepattern(x1,y1,x2,y2) => toggleLights(grid,range(List(x1.toInt,y1.toInt),List(x2.toInt,y2.toInt)))
				case offpattern(x1,y1,x2,y2) => turnOffLights(grid,range(List(x1.toInt,y1.toInt),List(x2.toInt,y2.toInt)))
			}
		}
		grid.values.foldLeft(0)(_ + _.size)
	}

	def turnOnLightsBetter(grid: Set[List[Int]], coordinates: Set[List[Int]]): Set[List[Int]] = {
		grid | coordinates
	}

	def toggleLightsBetter(grid: Set[List[Int]], coordinates: Set[List[Int]]): Set[List[Int]] = {
		(grid &~ coordinates) | (coordinates &~ grid)
	}

	def turnOffLightsBetter(grid: Set[List[Int]], coordinates: Set[List[Int]]): Set[List[Int]] = {
		grid &~ coordinates
	}

	def numLitBetter(filename: String): Int = {
		var grid:Set[List[Int]] = Set()
		val onpattern = "turn on (\\d+),(\\d+) through (\\d+),(\\d+)".r
		val togglepattern = "toggle (\\d+),(\\d+) through (\\d+),(\\d+)".r
		val offpattern = "turn off (\\d+),(\\d+) through (\\d+),(\\d+)".r

		Source.fromFile(filename).getLines().foreach{ line => 
			grid = line match {
				case onpattern(x1,y1,x2,y2) => turnOnLightsBetter(grid,range(List(x1.toInt,y1.toInt),List(x2.toInt,y2.toInt)).toSet)
				case togglepattern(x1,y1,x2,y2) => toggleLightsBetter(grid,range(List(x1.toInt,y1.toInt),List(x2.toInt,y2.toInt)).toSet)
				case offpattern(x1,y1,x2,y2) => turnOffLightsBetter(grid,range(List(x1.toInt,y1.toInt),List(x2.toInt,y2.toInt)).toSet)
			}
		}
		grid.size
	}


  
	def main(args: Array[String]): Unit = {
		//println("Nice: " + numNice(args(0)))
		println("Lit: " + numLitBetter(args(0)))
	}
}
