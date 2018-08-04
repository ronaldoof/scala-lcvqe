package br.ufms.facom.ma.lcvqe

import scala.collection.mutable.ListBuffer

case class Cluster (id: String, var centroid: Point,
                    points: ListBuffer[Point] = new ListBuffer[Point](),
                    descriptors: ListBuffer[String] = new ListBuffer[String](),
                    father: Option[Cluster] = None) {

  val gMLV: ListBuffer[Point] = ListBuffer.empty[Point]

  val gCLV: ListBuffer[Point] = ListBuffer.empty[Point]

  def addPoint(point: Point): Unit = this.points += point

  def addGMLV(point: Point): Unit = this.gMLV += point

  def addGCLV(point: Point): Unit = this.gCLV += point

  def descriptorsIterator: Iterator[String] = this.descriptors.iterator

  def addDescriptor (descriptor: String): Unit = this.descriptors += descriptor

  def violatedConstraints: List[Point] = gCLV.toList ::: gMLV.toList

  /**
    * Reposition the cluster based on the average of each dimension of his points and verify if the reposition caused
    * a change on the cluster position
    */
  def reposition: Boolean = {
    val oldCentroid = this.centroid.copy()
    val newDimensions = new Array[Double](this.centroid.dimensions.length)

    this.centroid.dimensions.indices.foreach {
      i => {
        val nj = this.points.size + (0.5 * this.gMLV.size) + this.gCLV.size
        val pointSum = this.points.map(p => p.dimensions(i)).sum
        val gMLVSum = this.gMLV.map(m => m.dimensions(i)).sum
        val gCLVSum = this.gCLV.map(c => c.dimensions(i)).sum
        val average = (pointSum + (0.5 * gMLVSum) + gCLVSum) / nj
        newDimensions(i) = average
      }
    }
    this.centroid = this.centroid.copy(dimensions = newDimensions)

    ! (oldCentroid.dimensions.deep == newDimensions.deep)
  }

  /**
    * Clear the cluster from all its points
    */
  def clear(): Unit = {
    this.points.foreach(p => p.clearCluster)
    this.points.clear()
    this.gCLV.clear()
    this.gMLV.clear()
  }

  override def toString: String = {
    s"Id: ${this.id}, Centroid: ${this.centroid}, " +
      s"Points: ${this.points.map(_.id).mkString(",")} "
  }


  def quadraticError(implicit distanceCalculator: DistanceCalculator): Double = {
    points.map(p => distanceCalculator.calculateDistance(p, centroid)).sum
  }
}
