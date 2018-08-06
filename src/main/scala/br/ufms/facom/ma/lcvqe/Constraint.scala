package br.ufms.facom.ma.lcvqe

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

  def buildCannotLink(data: List[Point], geoTags: List[GeoTag], level: Int): List[Constraint] = {
    require(level > 0)
    geoTags.filter(g => data.contains(g.point)).combinations(2).filter(geoTags => geoTags(0).distanceTo(geoTags(1)) >= (threshold/level)).map{
      geoTags => Constraint(geoTags(0).point, geoTags(1).point, ConstraintType.CannotLink)
    }.toList
  }
}