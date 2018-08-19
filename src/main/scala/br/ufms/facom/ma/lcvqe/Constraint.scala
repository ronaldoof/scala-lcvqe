package br.ufms.facom.ma.lcvqe

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


object ConstraintType extends Enumeration {
  val MustLink, CannotLink = Value
}

case class Constraint (val pointA: Point, val pointB: Point,
                       val consType: ConstraintType.Value = ConstraintType.MustLink) {

  def contains(point: Point): Boolean = point.equals(pointA) || point.equals(pointB)

  def other(point: Point): Point = point match {
      case this.pointA => this.pointB
      case this.pointB => this.pointA
      case _ => this.pointA
  }

}

object Constraint {
  private val threshold = 10000 // In kilometers


  def buildCannotLink(geoTags: Set[GeoTag], level: Int): List[Constraint] = {
    printf("Building constraints.\n")
    val time = System.currentTimeMillis()
    val comb =  Await.result(Future.sequence(buildCombination(geoTags)), Duration.Inf).flatten

    val constraints = comb.filter(geo => geo(0).distanceTo(geo(1)) > threshold / level).map {
      geo =>
        Future {
          Constraint(geo(0).point, geo(1).point, ConstraintType.CannotLink)
        }
    }
    val result = Await.result(Future.sequence(constraints), Duration.Inf)
    printf("Constraints built, took [%d] milliseconds\n", System.currentTimeMillis() - time)
    result
  }

  private def buildCombination(geoTags: Set[GeoTag]): List[Future[List[List[GeoTag]]]] ={
    @tailrec
    def tailBuildCombination(geoTags: List[GeoTag], futures: List[Future[List[List[GeoTag]]]]): List[Future[List[List[GeoTag]]]] = {
      if (geoTags.isEmpty) {
        futures
      } else {
        val future = Future {
          val l = geoTags.tail.map(g => List(geoTags.head, g))
          l
        }
        tailBuildCombination(geoTags.tail, future :: futures)
      }
    }
   tailBuildCombination(geoTags.toList, Nil)
  }
}