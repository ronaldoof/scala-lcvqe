package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.util.NumberUtil

case class Point (val id: String, val dimensions: Array[Double], var cluster: Option[Cluster] = None) {

  def getDimension(dimension: Int): Double = dimensions(dimension)

  def distanceTo(pointB: Point)(implicit calculator: DistanceCalculator = Cosine): Double = calculator.calculateDistance(this, pointB)

  def clearCluster: Unit = {this.cluster = None}

  def assignClosest(clusters: List[Cluster])(implicit distanceCalculator: DistanceCalculator = Cosine): Unit = {
    this.cluster = Some(clusters.map(c => (c, this.distanceTo(c.centroid))).toMap.toSeq.sortBy(_._2).head._1)
    this.cluster.map(_.addPoint(point = this))
  }

  def assingCluster(cluster: Cluster): Unit = {
    this.cluster = Some(cluster)
    cluster.addPoint(point = this)
  }

  override def canEqual(a: Any): Boolean = a.isInstanceOf[Point]

  override def equals(that: Any): Boolean = that match {
      case that: Point => that.canEqual(this) && that.dimensions.deep.equals(this.dimensions.deep)
      case _ => false
  }

  override def hashCode: Int = {
    this.id.##
  }

  override def toString: String = {
    s"${this.id}"
  }
}


object Point {

  def random(id: String, max: Array[Double], min: Array[Double]): Point = {
    val dim: Int = max.length-1
    val coords: Array[Double] = (0 to dim).map((i) => NumberUtil.round(min(i) + ((max(i) - min(i)) * scala.util.Random.nextDouble))).toArray
    Point(id, coords)
  }
}
