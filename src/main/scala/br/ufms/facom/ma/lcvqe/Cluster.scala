package br.ufms.facom.ma.lcvqe

import scala.collection.mutable.ListBuffer

case class Cluster (val id: String, var centroid: Point,
                    val points: ListBuffer[Point] = new ListBuffer[Point](),
                    val descriptors: ListBuffer[String] = new ListBuffer[String]()) {

  def addPoint(point: Point): Unit = this.points += point

  def descriptorsIterator: Iterator[String] = this.descriptors.iterator

  def addDescriptor (descriptor: String): Unit = this.descriptors += descriptor

  /**
    * calculates the quadratic error for a cluster as a Sum of the distances of all points to the cluster
    * @return The quadratic error
    */
  def quadraticError: Double = this.points.map(_.distanceTo(this.centroid)).sum


  /**
    * Reposition the cluster based on the average of each dimension of his points and verify if the reposition caused
    * a change on the cluster position
    */
  def reposition: Boolean = {
    if (this.points.isEmpty) false
    val oldCentroid = this.centroid.copy()
    val newDimensions = new Array[Double](this.centroid.dimensions.length)

    this.centroid.dimensions.indices.foreach {
      i => {
        val average = this.points.map(p => p.dimensions(i)).sum / this.centroid.dimensions.length
        newDimensions(i) = average
      }
    }
    this.centroid = this.centroid.copy(dimensions = newDimensions)

    ! (oldCentroid.dimensions.deep == newDimensions.deep)
  }

  /**
    * Clear the cluster from all its points
    */
  def clear: Unit = {
    this.points.foreach(p => p.clearCluster)
    this.points.clear()
  }

  override def toString: String = {
    s"Id: ${this.id}, Centroid: ${this.centroid}, " +
      s"Points: ${this.points.map(_.id).mkString(",")} "
  }

}
