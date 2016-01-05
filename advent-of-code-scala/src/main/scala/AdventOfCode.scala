import scala.util.matching.Regex
import scala.io.Source

object AdventOfCode {
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

	def main(args: Array[String]): Unit = {
		println("Nice: " + numNice(args(0)))
	}
}
