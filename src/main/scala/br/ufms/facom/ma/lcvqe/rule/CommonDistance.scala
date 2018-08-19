package br.ufms.facom.ma.lcvqe.rule

import br.ufms.facom.ma.lcvqe.{Cluster, Constraint, DistanceCalculator}
import scalacache._
import scalacache.modes.sync._
import br.ufms.facom.ma.cache.Cache.distanceCache

case class CommonDistance (distA: Double, distB: Double, distC: Double, distD: Double)


object CommonDistance {

  def apply(constraint: Constraint, closestClusterToA: Cluster,
                                       closestClusterToB: Cluster)(implicit distanceCalculator: DistanceCalculator): CommonDistance = {
    val distA = get(constraint.pointA.id + closestClusterToA.centroid.id, None).getOrElse {
      val dist = Math.pow(distanceCalculator.calculateDistance(constraint.pointA, closestClusterToA.centroid),2)
      put(constraint.pointA.id + closestClusterToA.centroid.id, None)(dist)
      dist
    }

    val distB = get(constraint.pointB.id + closestClusterToB.centroid.id, None).getOrElse {
      val dist = Math.pow(distanceCalculator.calculateDistance(constraint.pointB, closestClusterToB.centroid), 2)
      put(constraint.pointB.id + closestClusterToB.centroid.id, None)(dist)
      dist
    }

    val distC = get(constraint.pointA.id + closestClusterToB.centroid.id, None).getOrElse {
      val dist = Math.pow(distanceCalculator.calculateDistance(constraint.pointA, closestClusterToB.centroid),2)
      put(constraint.pointA.id + closestClusterToB.centroid.id, None)(dist)
      dist
    }

    val distD = get(constraint.pointB.id + closestClusterToA.centroid.id, None).getOrElse {
      val dist = Math.pow(distanceCalculator.calculateDistance(constraint.pointB, closestClusterToA.centroid),2)
      put(constraint.pointB.id + closestClusterToA.centroid.id, None)(dist)
      dist
    }

    CommonDistance(distA, distB, distC, distD)
  }
}
