package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.util.NumberUtil

trait DistanceCalculator {

  def calculateDistance(pointA: Point, pointB: Point): Double

}

object Euclidean extends DistanceCalculator{

  def calculateDistance(pointA: Point, pointB: Point): Double = {
    NumberUtil.round((pointA.dimensions, pointB.dimensions).zipped.map((a, b) => scala.math.pow(b - a, 2)).sum)
  }

}

object Cosine extends DistanceCalculator{

  def calculateDistance(pointA: Point, pointB: Point): Double ={
    try{
      val normA = math.sqrt(dotProduct(pointA,pointA))
      val normB = math.sqrt(dotProduct(pointB,pointB))
      val dot = dotProduct(pointA, pointB)
      if(normA == 0 || normB == 0) {
        0
      } else {
        val cosine = dot / (normA * normB)

        NumberUtil.round(1 - cosine)
      }
    } catch {
      case e: NumberFormatException => printf("Error in number")
        throw e
      case t: Throwable => printf("Unexpected exception")
        t.printStackTrace()
        throw t
    }
  }

  def dotProduct(pointA: Point, pointB: Point): Double = {
    scala.math.abs((pointA.dimensions, pointB.dimensions).zipped.map((a, b) => a * b).sum)
  }
}
