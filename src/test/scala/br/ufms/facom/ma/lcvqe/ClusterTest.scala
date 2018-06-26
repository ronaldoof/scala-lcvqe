package br.ufms.facom.ma.lcvqe

import org.scalatest.{FlatSpec, Matchers}

class ClusterTest extends FlatSpec with Matchers{

  "A cluster " should " increase the number of points it has " in {
    val p1 = Point("a",Array(1.0,2.0))
    val p2 = Point("b",Array(1.0,2.0))
    val p3 = Point("c",Array(1.0,2.0))

    val cluster = Cluster("c1", p1)
    val oldCount = cluster.points.size

    cluster.addPoint(p2)
    cluster.addPoint(p3)
    oldCount + 2 shouldEqual cluster.points.size

  }

}
