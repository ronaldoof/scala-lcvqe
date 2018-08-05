package br.ufms.facom.ma.lcvqe.rule

import br.ufms.facom.ma.lcvqe.{Cluster, Euclidean, Point}
import org.scalatest.{FlatSpec, Matchers}

class CannotLinkDistanceTest extends FlatSpec with Matchers{

  implicit val distanceCalculator = Euclidean

  "Given a list of clusters and a Point" should "Find the second closest centroid to the point" in {
    val clusters = List(Cluster("0", Point("c1", Array(2.0, 10.0))), Cluster("1", Point("c2", Array(5.0, 7.0))),
      Cluster("2", Point("c3", Array(3.0, 2.0))), Cluster("3", Point("c1", Array(3.0, 6.0))))
    val point = Point("a", Array(1,3))
    point.assignClosest(clusters)

    val result = CannotLinkDistance.mM(clusters, point.cluster.get, point)
    result shouldBe Cluster("3", Point("c1", Array(3.0, 6.0)))
  }

}
