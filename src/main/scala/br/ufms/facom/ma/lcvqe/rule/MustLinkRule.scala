package br.ufms.facom.ma.lcvqe.rule

import br.ufms.facom.ma.lcvqe.util.ClusterUtil
import br.ufms.facom.ma.lcvqe.{Constraint, DistanceCalculator}

object MustLinkRule {

  def apply(constraint: Constraint)(implicit distanceCalculator: DistanceCalculator): Unit ={

    if(constraint.pointA.cluster != constraint.pointB.cluster) {
      assume(constraint.pointA.cluster != None)
      assume(constraint.pointB.cluster != None)
      val commonDistance = CommonDistance(constraint, constraint.pointA.cluster.get, constraint.pointB.cluster.get)
      val a = calculateMLA(commonDistance)
      val b = calculateMLB(commonDistance)
      val c = calculateMLC(commonDistance)

      val min = math.min(a, math.min(b, c))
      min match {
        case `a` =>
          constraint.pointA.cluster.get.addGMLV(constraint.pointB)
          constraint.pointB.cluster.get.addGMLV(constraint.pointA)
        case `b` =>
          ClusterUtil.removeFromCluster(constraint.pointB)
          ClusterUtil.addToCluster(constraint.pointB, constraint.pointA.cluster)
        case `c` =>
          ClusterUtil.removeFromCluster(constraint.pointA)
          ClusterUtil.addToCluster(constraint.pointA, constraint.pointB.cluster)
      }
    }
  }

  def calculateMLA(commonDistance: CommonDistance)
                  (implicit distanceCalculator: DistanceCalculator): Double = {
    (0.5 * (commonDistance.distA + commonDistance.distB)) + (0.25 * (commonDistance.distC + commonDistance.distD))
  }

  def calculateMLB (commonDistance: CommonDistance)
                   (implicit distanceCalculator: DistanceCalculator): Double = {
    (0.5 * commonDistance.distA) + (0.5 * commonDistance.distD)
  }

  def calculateMLC (commonDistance: CommonDistance)(implicit distanceCalculator: DistanceCalculator): Double = {
    (0.5 * commonDistance.distC) + (0.5 * commonDistance.distB)
  }

}
