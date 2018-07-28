package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.util.KmeansUtil

import scala.collection.mutable.ListBuffer


/**
  * Class that implements the LCVQE Algorithm.
  */
case class LCVQE (data: List[Point], constraints: Option[List[Constraint]], k: Int, iterations: Int)(implicit distanceCalculator: DistanceCalculator = Cosine) {

  private case class CommonDistance (distA: Double, distB: Double, distC: Double, distD: Double);
  private case class RtoCMMDistance(distA: Double, distB: Double)

  def run(): Result = {
    println(s"=======================\nInitializing LCVQE APP \n=======================   \n k = ${k}\n iterations = ${iterations}\n=======================")

    var bestResult = Result()
    (0 to 10).foreach { _ =>
      val clusters = initClusters(k, data);
      var changed = true
      var changedList = List.empty[Boolean]
      var i = 0
      while (changed && i < iterations) {
//      limpa os clusters
        clusters.foreach(c => c.clear)

//      calcula a funcao erro e tenta minimizar
        data.foreach(p => p.assignClosest(clusters)(distanceCalculator))

//      aplica regra de Must Link e Cannot Link do LCVQE
        constraints match {
          case Some(constraints) => {
            constraints.foreach(constraint => {
              applyML(constraint)
              applyCL(constraint, clusters)
            })
          }
          case None =>
        }

//      reposiciona os clusters
        changed = clusters.map(c => c.reposition).reduceLeft(_ || _)

        i = i + 1
      }

      def calcError: (Cluster => Double) = lCVQEError(clusters, _)

      val newResult = Result(Some(data), Some(clusters))
      if (newResult.error(calcError) < bestResult.error(calcError)) bestResult = newResult
      printf("LCVQE = %d",bestResult.error(calcError))
    }
    bestResult

  }

  def lCVQEError(clusters: List[Cluster], cluster: Cluster): Double = {

    val tj1 = math.pow(cluster.points.map(p => distanceCalculator.calculateDistance(p, cluster.centroid)).sum,2)

    val tj23 = 1/2 * math.pow(cluster.violatedConstraints.map(distanceCalculator.calculateDistance(_, cluster.centroid)).sum,2)

    val tj4 = 1/2 * math.pow(cluster.points.map(p => distanceCalculator.calculateDistance(mM(clusters, cluster, p).centroid,p)).sum,2)

    tj1 + tj23 + tj4
  }

  /**
    *
    * @param k
    * @param data
    * @return
    */
  def initClusters(k: Int, data: List[Point]): List[Cluster] = {
    (0 until k).map(i => Cluster(i.toString, Point.random(s"centroid-$i", KmeansUtil.max(data), KmeansUtil.min(data)))).toList
  }


  private def applyML(constraint: Constraint)(implicit distanceCalculator: DistanceCalculator): Unit ={
    val commonDistance = calculateCommonDistances(constraint, constraint.pointA.cluster.get, constraint.pointB.cluster.get)
    val a = calculateMLA(commonDistance)
    val b = calculateMLB(commonDistance)
    val c = calculateMLC(commonDistance)

    val min = math.min(a, math.min(b, c))
    min match {
      case `a` => {
        constraint.pointA.cluster.get.addGMLV(constraint.pointA)
        constraint.pointB.cluster.get.addGMLV(constraint.pointB)
      }
      case `b` => {
        constraint.pointB.cluster = constraint.pointA.cluster
        constraint.pointA.cluster.get.addPoint(constraint.pointB)
      }
      case `c` => {
        constraint.pointA.cluster = constraint.pointB.cluster
        constraint.pointB.cluster.get.addPoint(constraint.pointA)
      }
    }

  }

  /**
    * Apply the Cannot Link logic after the points had been assigned to your closest cluster.
    * @param constraint
    * @param clusters
    * @param distanceCalculator
    */
  private def applyCL(constraint: Constraint, clusters: List[Cluster])(implicit distanceCalculator: DistanceCalculator): Unit ={
    val commonDistance = calculateCommonDistances(constraint, constraint.pointA.cluster.get, constraint.pointB.cluster.get)
    val rtoCMMDistances = calculateRtoCMMDistances(clusters, constraint, constraint.pointA.cluster.get, constraint.pointB.cluster.get)
    val a = calculateCLA(commonDistance, rtoCMMDistances)
    val b = calculateCLB(commonDistance, rtoCMMDistances)
    val c = calculateCLC(commonDistance)

    val min = math.min(a, math.min(b, c))
    min match {
      case `a` => {
        val rj: Point = calculateR(constraint, constraint.pointA.cluster.get)
        val mMj = mM(clusters, constraint.pointA.cluster.get, rj)

        mMj.addGCLV(constraint.pointA)
        constraint.pointB.cluster = constraint.pointA.cluster
        constraint.pointA.cluster.get.addPoint(constraint.pointB)
      }
      case `b` => {
        val rn: Point = calculateR(constraint, constraint.pointB.cluster.get)
        val mMn = mM(clusters, constraint.pointB.cluster.get, rn)

        mMn.addGCLV(constraint.pointB)
        constraint.pointA.cluster = constraint.pointB.cluster
        constraint.pointB.cluster.get.addPoint(constraint.pointA)
      }
      case `c` => {
        constraint.pointA.cluster.get.addGCLV(constraint.pointA)
        constraint.pointB.cluster.get.addGCLV(constraint.pointB)
      }
    }
  }


  private def calculateCommonDistances(constraint: Constraint, closestClusterToA: Cluster,
                                 closestClusterToB: Cluster): CommonDistance = {
    val distA = Math.pow(distanceCalculator.calculateDistance(constraint.pointA, closestClusterToA.centroid),2)
    val distB = Math.pow(distanceCalculator.calculateDistance(constraint.pointB, closestClusterToB.centroid),2)
    val distC = Math.pow(distanceCalculator.calculateDistance(constraint.pointA, closestClusterToB.centroid),2)
    val distD = Math.pow(distanceCalculator.calculateDistance(constraint.pointB, closestClusterToA.centroid),2)

    CommonDistance(distA, distB, distC, distD)
  }

  private def calculateRtoCMMDistances(clusters: List[Cluster], constraint: Constraint, closestClusterToA: Cluster,
                                   closestClusterToB: Cluster)(implicit distanceCalculator: DistanceCalculator): RtoCMMDistance = {
    val (rn: Point, rj: Point, mMj: Cluster, mMn: Cluster) = calculateRAndMM(clusters, constraint, closestClusterToA, closestClusterToB)
    val distA = Math.pow(distanceCalculator.calculateDistance(rj, mMj.centroid),2)
    val distB = Math.pow(distanceCalculator.calculateDistance(rn, mMn.centroid),2)
    RtoCMMDistance(distA, distB)
  }

  private def calculateRAndMM(clusters: List[Cluster], constraint: Constraint, closestClusterToA: Cluster, closestClusterToB: Cluster) = {
    val rj: Point = calculateR(constraint, closestClusterToA)
    val mMj = mM(clusters, closestClusterToA, rj)
    val rn: Point = calculateR(constraint, closestClusterToB)
    val mMn = mM(clusters, closestClusterToB, rn)
    (rn, rj, mMj, mMn)
  }

  private def calculateR(constraint: Constraint, closestCluster: Cluster): Point = {
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


  def calculateMLA(commonDistance: CommonDistance)
                  (implicit distanceCalculator: DistanceCalculator): Double = {
    (1/2 * (commonDistance.distA + commonDistance.distB)) + (1/4 * (commonDistance.distC + commonDistance.distD))
  }

  def calculateMLB (commonDistance: CommonDistance)
                   (implicit distanceCalculator: DistanceCalculator): Double = {
    (1/2 * commonDistance.distA) + (1/2 * commonDistance.distD)
  }

  def calculateMLC (commonDistance: CommonDistance)(implicit distanceCalculator: DistanceCalculator): Double = {
    (1/2 * commonDistance.distC) + (1/2 * commonDistance.distB)
  }

  def calculateCLA(commonDistance: CommonDistance, rtoCMMDistance: RtoCMMDistance) : Double = {
    (1/2 * commonDistance.distA) +  (1/2 * commonDistance.distD) + (1/2 * rtoCMMDistance.distA)
  }

  def calculateCLB(commonDistance: CommonDistance, rtoCMMDistance: RtoCMMDistance) : Double = {
    (1/2 * commonDistance.distC) +  (1/2 * commonDistance.distB) + (1/2 * rtoCMMDistance.distB)
  }

  def calculateCLC(commonDistance: CommonDistance) : Double = {
    1/2 * (commonDistance.distA + commonDistance.distB)
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
