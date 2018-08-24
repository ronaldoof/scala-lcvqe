package br.ufms.facom.ma.lcvqe.rule

import br.ufms.facom.ma.lcvqe.util.{ClusterUtil, ReportUtil}
import br.ufms.facom.ma.lcvqe.{Constraint, DistanceCalculator}

import scala.collection.mutable.ListBuffer

object MustLinkRule {

  def apply(constraint: Constraint, brokenConstraints: ListBuffer[Constraint])(implicit distanceCalculator: DistanceCalculator): Unit ={
    if(constraint.pointA.cluster.isDefined && constraint.pointB.cluster.isDefined && constraint.pointA.cluster != constraint.pointB.cluster) {
      val commonDistance = CommonDistance(constraint, constraint.pointA.cluster.get, constraint.pointB.cluster.get)
      val a = calculateMLA(commonDistance)
      val b = calculateMLB(commonDistance)
      val c = calculateMLC(commonDistance)

      val min = math.min(a, math.min(b, c))
      min match {
        case `a` =>
          applyRuleA(constraint, b, c, brokenConstraints)
        case `b` =>
          applyRuleB(constraint)
        case `c` =>
          applyRuleC(constraint)
      }
     }
  }


  private def applyRuleC(constraint: Constraint) = {
    if (constraint.pointA.cluster.size > 1) {
      ClusterUtil.removeFromCluster(constraint.pointA)
      ClusterUtil.addToCluster(constraint.pointA, constraint.pointB.cluster)
    }
  }

  private def applyRuleB(constraint: Constraint) = {
    if (constraint.pointB.cluster.size > 1) {
      ClusterUtil.removeFromCluster(constraint.pointB)
      ClusterUtil.addToCluster(constraint.pointB, constraint.pointA.cluster)
    }
  }

  private def applyRuleA(constraint: Constraint, b: Double, c: Double, brokenContraints: ListBuffer[Constraint]) = {
    if (constraint.pointA.id.split("\\.").head != constraint.pointB.id.split("\\.").head) {
      constraint.pointA.cluster.get.addGMLV(constraint.pointB)
      constraint.pointB.cluster.get.addGMLV(constraint.pointA)
      brokenContraints += constraint
    } else {
      if (b < c) {
        applyRuleB(constraint)
      } else {
        applyRuleC(constraint)
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
