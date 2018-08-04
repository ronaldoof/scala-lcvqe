package br.ufms.facom.ma.lcvqe.error

import br.ufms.facom.ma.lcvqe.rule.CannotLinkDistance
import br.ufms.facom.ma.lcvqe.{Cluster, Cosine, DistanceCalculator}

object LCVQEError {

  def calcError(clusters: List[Cluster], cluster: Cluster)(implicit distanceCalculator: DistanceCalculator): Double = {

    val tj1 = math.pow(cluster.points.map(p => distanceCalculator.calculateDistance(p, cluster.centroid)).sum,2)

    val tj23 = 0.5 * math.pow(cluster.violatedConstraints.map(distanceCalculator.calculateDistance(_, cluster.centroid)).sum,2)

    val tj4 = 0.5 * math.pow(cluster.points.map(p => distanceCalculator.calculateDistance(CannotLinkDistance.mM(clusters, cluster, p).centroid,p)).sum,2)

    tj1 + tj23 + tj4
  }

}
