package br.ufms.facom.ma.lcvqe.rule

import br.ufms.facom.ma.lcvqe.{Cluster, Constraint, DistanceCalculator}

case class CommonDistance (distA: Double, distB: Double, distC: Double, distD: Double)


object CommonDistance {
  def apply(constraint: Constraint, closestClusterToA: Cluster,
                                       closestClusterToB: Cluster)(implicit distanceCalculator: DistanceCalculator): CommonDistance = {
    val distA = Math.pow(distanceCalculator.calculateDistance(constraint.pointA, closestClusterToA.centroid),2)
    val distB = Math.pow(distanceCalculator.calculateDistance(constraint.pointB, closestClusterToB.centroid),2)
    val distC = Math.pow(distanceCalculator.calculateDistance(constraint.pointA, closestClusterToB.centroid),2)
    val distD = Math.pow(distanceCalculator.calculateDistance(constraint.pointB, closestClusterToA.centroid),2)

    CommonDistance(distA, distB, distC, distD)
  }
}
