package br.ufms.facom.ma.lcvqe.util

import br.ufms.facom.ma.lcvqe.{Cluster, Point}

object ClusterUtil {

  def addToCluster(point: Point, cluster: Option[Cluster]): Unit = {
    point.cluster = cluster
    cluster match {
      case Some(cl) => cl.addPoint(point)
      case None => printf("POINT WITHOUT CLUSTER!! THIS WILL CAUSE A FLOATING POINT PROBLEM")
    }
  }

  def removeFromCluster(point: Point): Unit = {
    point.cluster.get.points -= point
  }

}
