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
