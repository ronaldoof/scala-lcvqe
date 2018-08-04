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
        cluster.points -= point
        point.cluster = None
      case None =>
    }

  }

}
