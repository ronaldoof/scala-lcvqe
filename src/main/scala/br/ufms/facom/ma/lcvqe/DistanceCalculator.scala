package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.util.NumberUtil
import br.ufms.facom.ma.cache.Cache.distanceCache
import scalacache.{get, put}
import scalacache.modes.sync._


trait DistanceCalculator {

  def calculateDistance(pointA: Point, pointB: Point): Double

}


object Euclidean extends DistanceCalculator{

  def calculateDistance(pointA: Point, pointB: Point): Double = {
    get(pointA.id + pointB.id).getOrElse {
      calculateWithoutCache(pointA, pointB)
    }
  }

  private def calculateWithoutCache(pointA: Point, pointB: Point): Double = {
    var dist: Double = 0.0
    (0 until pointA.dimensions.length).foreach { i =>
      dist = dist + scala.math.pow(pointB.dimensions(i) - pointA.dimensions(i), 2)
    }
    dist
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
