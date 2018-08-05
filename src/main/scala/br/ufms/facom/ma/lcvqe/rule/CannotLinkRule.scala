package br.ufms.facom.ma.lcvqe.rule

import br.ufms.facom.ma.lcvqe.util.ClusterUtil
import br.ufms.facom.ma.lcvqe.{Cluster, Constraint, DistanceCalculator, Point}

object CannotLinkRule {

  /**
    * Apply the Cannot Link logic after the points had been assigned to your closest cluster.
    * @param constraint constraint to be applied to the rule
    * @param clusters list of clusters that will be analyzed by the rule
    * @param distanceCalculator strategy on how to calculate distance
    */
  def apply(constraint: Constraint, clusters: List[Cluster])(implicit distanceCalculator: DistanceCalculator): Unit ={
    if(constraint.pointA.cluster == constraint.pointB.cluster) {
      val commonDistance = CommonDistance(constraint, constraint.pointA.cluster.get, constraint.pointB.cluster.get)
      val rtoCMMDistances = CannotLinkDistance.calculateRtoCMMDistances(clusters, constraint, constraint.pointA.cluster.get, constraint.pointB.cluster.get)
      val a = calculateCLA(commonDistance, rtoCMMDistances)
      val b = calculateCLB(commonDistance, rtoCMMDistances)
      val c = calculateCLC(commonDistance)

      val min = math.min(a, math.min(b, c))
      min match {
        case `a` =>
          val rj: Point = CannotLinkDistance.calculateR(constraint, constraint.pointA.cluster.get)
          val mMj = CannotLinkDistance.mM(clusters, constraint.pointA.cluster.get, rj)

          mMj.addGCLV(constraint.pointA)
          ClusterUtil.removeFromCluster(constraint.pointB)
          ClusterUtil.addToCluster(constraint.pointB, constraint.pointA.cluster)

        case `b` =>
          val rn: Point = CannotLinkDistance.calculateR(constraint, constraint.pointB.cluster.get)
          val mMn = CannotLinkDistance.mM(clusters, constraint.pointB.cluster.get, rn)

          mMn.addGCLV(constraint.pointB)
          ClusterUtil.removeFromCluster(constraint.pointA)
          ClusterUtil.addToCluster(constraint.pointA, constraint.pointB.cluster)

        case `c` =>
          constraint.pointA.cluster.get.addGCLV(constraint.pointA)
          constraint.pointB.cluster.get.addGCLV(constraint.pointB)

        case _ => {
          printf(s"commonDistance = ${commonDistance} \n")
          printf(s"rtoCMMDistances = ${rtoCMMDistances} \n")
          printf(s"a = ${a}, b = ${b}, c = ${c} \n")
        }
      }
    }
  }


  private def calculateCLA(commonDistance: CommonDistance, rtoCMMDistance: CannotLinkDistance) : Double = {
    (0.5 * commonDistance.distA) +  (0.5 * commonDistance.distD) + (0.5 * rtoCMMDistance.distA)
  }

  private def calculateCLB(commonDistance: CommonDistance, rtoCMMDistance: CannotLinkDistance) : Double = {
    (0.5 * commonDistance.distC) +  (0.5 * commonDistance.distB) + (0.5 * rtoCMMDistance.distB)
  }

  private def calculateCLC(commonDistance: CommonDistance) : Double = {
    0.5 * (commonDistance.distA + commonDistance.distB)
  }


}
