package br.ufms.facom.ma.lcvqe

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
  private val threshold = 1000 // In kilometers


//  val actDataC = actData.map(c => c.id).toSet
//  constraints.getOrElse(Nil).filter(c => actDataC.contains(c.pointA.id))
  //    printf("Stating to build cannot link for [%d] data points and [%d] geotags\n", data.size, geoTags.size)
  //    val time1 = System.currentTimeMillis()
  //    val actDataS = data.map(_.id).toSet
  //    val time2 = System.currentTimeMillis()
  //    printf("1. Result [%d]\n", time2-time1)
  //    val result1 = geoTags.filter(g => actDataS.contains(g.point.id))
  //    val time3 = System.currentTimeMillis()
  //    printf("2. Result [%d]\n", time3-time2)
  //    val result2 = result1.combinations(2)
  //    val time4 = System.currentTimeMillis()
  //    printf("3. Result [%d]\n", time4-time3)
  //    val result3 = result2.filter(geoTags => geoTags(0).distanceTo(geoTags(1))
  //      >= (threshold/level))
  //    val time5 = System.currentTimeMillis()
  //    printf("4. Result [%d]\n", time5-time4)
  //
  //    val result4 = result3.toList
  //    val time6 = System.currentTimeMillis()
  //    printf("5. Result [%d]\n", time6-time5)
  //    val result5 = result4.map{
  //      geoTags => Constraint(geoTags(0).point, geoTags(1).point, ConstraintType.CannotLink)
  //    }
  //    val time7 = System.currentTimeMillis()
  //    printf("6. Result [%d]\n", time7-time6)
  //    val result6 = result4.toList
  //    val time8 = System.currentTimeMillis()
  //    printf("7. Result [%d]\n", time8-time7)
  //    printf("Done with constraint in [%d] milliseconds\n", System.currentTimeMillis() - time1)
  //    result5
  def buildCannotLink(geoTags: List[GeoTag], level: Int): List[Constraint] = {
    printf("Building constraints.\n")
    val time = System.currentTimeMillis()
    val constraints = geoTags.combinations(2).filter(geo => geoTags(0).distanceTo(geoTags(1)) > threshold / level).map {
       geo => Future{
        Constraint(geo(0).point, geo(1).point, ConstraintType.CannotLink)
       }
    }.toList
    printf("Constraints built, took [%d] milliseconds", System.currentTimeMillis() - time)
    Await.result(Future.sequence(constraints), Duration.Inf)
  }

//  def buildGeoMap(data: List[Point], geoTags: List[GeoTag]): Map[String, Map[String, Double]] = {
//    val actDataS = data.map(_.id).toSet
//    printf("Starting to build geoMap.\n")
//    val time = System.currentTimeMillis()
//    val result = geoTags.filter(g => actDataS.contains(g.point.id)).combinations(2).map{
//      geoTags => (geoTags(0).point.id -> (geoTags(1).point.id, geoTags(0).distanceTo(geoTags(1))))
//    }.toMap
//    printf("GeoMap built, took [%d] milliseconds", System.currentTimeMillis() - time)
//    result
//  }
}