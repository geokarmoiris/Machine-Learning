package ml.id3

import scala.math._

object EntropyCalculation extends App {

  def log2(x: Double) = log(x) / log(2)

  def entropy( v:String ) = { v
    .groupBy (a => a)
    .values
    .map( i => i.length.toDouble / v.length )
    .map( p => - p * log2(p))
    .sum
  }

  val x = - 0.66 * log2(0.66)
  val y = - 0.33 * log2(0.33)

  println("ZZZ", x+y)
  println("XXXX", entropy("1223334444"))
}
