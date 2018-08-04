package br.ufms.facom.ma.lcvqe.rule

import br.ufms.facom.ma.lcvqe.{Cluster, Constraint, DistanceCalculator, Point}

case class CannotLinkDistance(distA: Double, distB: Double)

object CannotLinkDistance {

  def calculateRtoCMMDistances(clusters: List[Cluster], constraint: Constraint, closestClusterToA: Cluster,
                                       closestClusterToB: Cluster)(implicit distanceCalculator: DistanceCalculator): CannotLinkDistance = {
    val (rn: Point, rj: Point, mMj: Cluster, mMn: Cluster) = calculateRAndMM(clusters, constraint, closestClusterToA, closestClusterToB)
    val distA = Math.pow(distanceCalculator.calculateDistance(rj, mMj.centroid),2)
    val distB = Math.pow(distanceCalculator.calculateDistance(rn, mMn.centroid),2)
    CannotLinkDistance(distA, distB)
  }

  def calculateRAndMM(clusters: List[Cluster], constraint: Constraint, closestClusterToA: Cluster,
                      closestClusterToB: Cluster)(implicit distanceCalculator: DistanceCalculator):(Point, Point, Cluster, Cluster) = {
    val rj: Point = calculateR(constraint, closestClusterToA)
    val mMj = mM(clusters, closestClusterToA, rj)
    val rn: Point = calculateR(constraint, closestClusterToB)
    val mMn = mM(clusters, closestClusterToB, rn)
    (rn, rj, mMj, mMn)
  }

  def calculateR(constraint: Constraint, closestCluster: Cluster)(implicit distanceCalculator: DistanceCalculator): Point = {
    val distAtoCCB = math.pow(distanceCalculator.calculateDistance(constraint.pointA, closestCluster.centroid), 2)
    val distBtoCCB = math.pow(distanceCalculator.calculateDistance(constraint.pointB, closestCluster.centroid), 2)
    val r = {
      if (math.max(distAtoCCB, distBtoCCB) == distAtoCCB) {
        constraint.pointA
      } else {
        constraint.pointB
      }
    }
    r
  }

  def r(cluster: Cluster, constraint: Constraint)(implicit distanceCalculator: DistanceCalculator): Point = {
    val distanceA = distanceCalculator.calculateDistance(cluster.centroid, constraint.pointA)
    val distanceB = distanceCalculator.calculateDistance(cluster.centroid, constraint.pointB)
    if (distanceA >= distanceB) {
      constraint.pointA
    } else {
      constraint.pointB
    }
  }

  def mM(clusters: List[Cluster], cluster: Cluster, point: Point)(implicit distanceCalculator: DistanceCalculator): Cluster = {
    clusters.withFilter(_ != cluster).map(x => x)
      .minBy(c => distanceCalculator.calculateDistance(c.centroid, point))
  }

}
