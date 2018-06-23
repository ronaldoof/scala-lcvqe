package br.ufms.facom.ma.lcvqe

case class Point (val id: String, val dimensions: Array[Double], var cluster: Option[Cluster] = None) {

  def getDimension(dimension: Int): Double = dimensions(dimension)

  def distanceTo(pointB: Point, calculator: DistanceCalculator = Cosine): Double = calculator.calculateDistance(this, pointB)

  def clearCluster: Unit = {this.cluster = None}

  def assignClosest(clusters: List[Cluster])(implicit distanceCalculator: DistanceCalculator = Cosine): Unit = {
    this.cluster = Option(clusters.sortWith((a, b) => this.distanceTo(a.centroid) < this.distanceTo(b.centroid)).head)
    this.cluster.map(_.addPoint(this))
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
    s""
  }
}


object Point {

  def random(id: String, max: Array[Double], min: Array[Double]): Point = {
    val dim: Int = max.length-1
    val coords: Array[Double] = (0 to dim).map((i) => min(i) + ((max(i) - min(i)) * scala.util.Random.nextDouble)).toArray
    Point(id, coords)
  }
}
