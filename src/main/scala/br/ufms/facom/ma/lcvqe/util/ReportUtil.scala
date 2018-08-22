package br.ufms.facom.ma.lcvqe.util

import br.ufms.facom.ma.lcvqe.error.LCVQEError
import br.ufms.facom.ma.lcvqe.{Cluster, DistanceCalculator, Euclidean, Point}

import scala.collection.AbstractSeq
object ReportUtil {

  def printPointBalance(clusters: List[Cluster], data: List[Point]): Unit = {
    val totalClusterPoints = clusters.map(c => c.points.toList.size).sum
    printf("Total of Points in Clusters [%d] | Total points in Data [%d]\n", totalClusterPoints, data.distinct.size)
  }

  def printRatioClusterPoint(actClusters: List[Cluster]): Unit = {
    actClusters.foreach(c => printf("Cluster [%s] has [%d] points\n", c.id, c.points.size))
  }


  def timeIt[A](tag: String)(f: => A): A = {
    val time = System.currentTimeMillis()
    val result = f
    val elapsedTime = System.currentTimeMillis() - time
    printf("Time for [%s] was [%d]\n", tag, elapsedTime)
    result
  }

  def countIt[A](tag: String)(c: AbstractSeq[A]): Unit = {
    val count = c.size
    printf("The size of [%s] is [%d]\n", tag, count)
  }

  def reportError(clusters: List[Cluster])(implicit distanceCalculator: DistanceCalculator): Unit = {
    clusters.par.foreach(c => printf("Cluster [%s] has error [%f]", c.id, LCVQEError.calcError(clusters, c)))
  }

}
