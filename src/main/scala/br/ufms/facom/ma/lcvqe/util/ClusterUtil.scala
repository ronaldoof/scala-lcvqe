package br.ufms.facom.ma.lcvqe.util

import br.ufms.facom.ma.lcvqe.{Cluster, Point}

object ClusterUtil {

  def addToCluster(point: Point, cluster: Option[Cluster]): Unit = {
    point.cluster = cluster
    cluster match {
      case Some(cl) => cl.addPoint(point)
      case None =>
    }
  }

  def removeFromCluster(point: Point): Unit = {
    point.cluster match {
      case Some(cluster) =>
        val index = cluster.points.toSet.zipWithIndex.find(p => p._1.id == point.id).map(p => p._2)
        index match {
          case Some(i) => cluster.points.remove(i)
          case _ =>
        }
        point.cluster = None
      case None =>
    }

  }

}
