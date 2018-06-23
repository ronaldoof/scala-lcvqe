package br.ufms.facom.ma.lcvqe

trait DistanceCalculator {

  def calculateDistance(pointA: Point, pointB: Point): Double

}

object Euclidean extends DistanceCalculator{

  def calculateDistance(pointA: Point, pointB: Point): Double ={
    (pointA.dimensions, pointB.dimensions).zipped.map((a, b) => scala.math.pow(b - a, 2)).sum
  }

}

object Cosine extends DistanceCalculator{

  def calculateDistance(pointA: Point, pointB: Point): Double ={
    val normA = euclideanNorm(pointA)
    val normB = euclideanNorm(pointB)
    val dot = dotProduct(pointA, pointB)
    1 - (dot / (normA * normB))
  }


  def euclideanNorm (point: Point): Double = {
     scala.math.sqrt(point.dimensions.map(scala.math.pow(_,2)).sum)
  }

  def dotProduct(pointA: Point, pointB: Point): Double = {
    scala.math.abs((pointA.dimensions, pointB.dimensions).zipped.map((a, b) => a * b).sum)
  }
}
