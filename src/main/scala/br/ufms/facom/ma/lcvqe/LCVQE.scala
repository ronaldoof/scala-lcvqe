package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.util.{KmeansUtil, Sequence}

import scala.collection.mutable.ListBuffer


/**
  * Class that implements the LCVQE Algorithm.
  */
case class LCVQE (data: List[Point], constraints: Option[List[Constraint]], k: Int, iterations: Int)(implicit distanceCalculator: DistanceCalculator = Cosine) {

  private case class CommonDistance (distA: Double, distB: Double, distC: Double, distD: Double);
  private case class RtoCMMDistance(distA: Double, distB: Double)

  def run(): Result = {

    val seq = Sequence
    val root = Cluster(seq.next(), Point("root_point", Array(0,0)))
    val clusters = initClusters(data, root)
    val clusterHistory =  ListBuffer(root)
    val stack = ListBuffer.empty[Cluster]

    clusters.foreach(cl => {
      stack += cl
      clusterHistory += cl
    })

    var bestResult = Result()
    buildRootLevel(clusters)

    while(!stack.isEmpty) {

      var changed = true
      var i = 0

      // remove o topo da pilha
      val actCluster = stack.head
      stack.remove(0)

      if(actCluster.points.size > 1 && actCluster.quadraticError > 0) {
        // o novo conjunto de dados e composto pelos pontos do cluster no topo da pilha
        val actData = actCluster.points.toList
        // inicializa os clusteres desse nivel
        val actClusters = initHClusters(actData)
        def calcError: (Cluster => Double) = lCVQEError(actClusters, _)


        // executa o LCVQE para o conjunto de pontos do cluster send repartido ate convergencia
        while (changed && i < iterations) {

          changed = applyLCVQE (actClusters, actData)

          i = i + 1
        }

        val newResult = Result(Some(actData), Some(actClusters))
        if (newResult.error(calcError) < bestResult.error(calcError)) bestResult = newResult

        actClusters.foreach(clu => stack += clu)
      }
    }
    bestResult

  }


  def applyLCVQE (actClusters: List[Cluster], data: List[Point]): Boolean = {
    //      limpa os clusters
    actClusters.foreach(c => c.clear)

    //      calcula a funcao erro e tenta minimizar
    data.foreach(p => p.assignClosest(actClusters)(distanceCalculator))

    val actConstraints = constraints.getOrElse(List.empty[Constraint]).filter(cons => data.contains(cons.pointA) || data.contains(cons.pointB))

    //      aplica regra de Must Link e Cannot Link do LCVQE
    actConstraints.foreach(constraint => {
      applyML(constraint)
      applyCL(constraint, actClusters)
    })

    //      reposiciona os clusters
    actClusters.map(c => c.reposition).reduceLeft(_ || _)

  }

  def lCVQEError(clusters: List[Cluster], cluster: Cluster): Double = {

    val tj1 = math.pow(cluster.points.map(p => distanceCalculator.calculateDistance(p, cluster.centroid)).sum,2)

    val tj23 = 1/2 * math.pow(cluster.violatedConstraints.map(distanceCalculator.calculateDistance(_, cluster.centroid)).sum,2)

    val tj4 = 1/2 * math.pow(cluster.points.map(p => distanceCalculator.calculateDistance(mM(clusters, cluster, p).centroid,p)).sum,2)

    tj1 + tj23 + tj4
  }

  /**
    * Sets a point to a cluster considering constraints. If the point cannot join the cluster
    * creates a new cluster for that point.
    * @param point
    * @param cluster
    * @return
    */
  private def setCluster(point: Point, cluster: Cluster, root: Cluster): Cluster = {
    val seq = Sequence
    if (canAddToCluster(point, cluster)) {
      cluster.addPoint(point)
      point.cluster = Some(cluster)
      cluster
    } else {
      val newCluster = Cluster(seq.next(), point, ListBuffer(point), father = Some(root))
      point.cluster = Some(newCluster)
      newCluster
    }
  }

  /**
    * Inicializa os clusteres encontrando um K minimo para garantir o cumprimento de todas as restricoes.
    * @param k
    * @param data
    * @return
    */
  def initClusters(data: List[Point], root: Cluster): List[Cluster] = {
    val seq = Sequence

    this.constraints.get.filter(_.consType == ConstraintType.MustLink).map {
      c => c.pointA.cluster match {
        case None => {
          c.pointB.cluster match {
            case None => {
              val newCluster = Cluster(seq.next(), c.pointA, ListBuffer(c.pointA, c.pointB), father = Some(root))
              c.pointA.cluster = Some(newCluster)
              c.pointB.cluster = Some(newCluster)
              newCluster
            }
            case Some(clusterB) => {
              setCluster(c.pointA, clusterB, root)
            }
          }
        }
        case Some(clusterA) => {
          c.pointB.cluster match {
            case None => {
              setCluster(c.pointB, clusterA, root)
            }
          }
        }
      }
    }
  }

  /**
    *
    * @param k
    * @param data
    * @return
    */
  def initHClusters(data: List[Point]): List[Cluster] = {
    (0 until 2).map(i => Cluster(i.toString, Point.random(s"centroid-$i", KmeansUtil.max(data), KmeansUtil.min(data)))).toList
  }


  /**
    * Constroi o primeiro nivel do algoritmo hierarquico. Nesse caso nos ja temos os clusters
    * e queremos apenas aperfeicoar o agrupamento usando LCVQE
    * @param clusters
    */
  def buildRootLevel(clusters: List[Cluster]): Unit ={

    var changed = true
    var i = 0
    while (changed && i < this.iterations) {

      //      limpa os clusters
      clusters.foreach(_.clear)

      //      calcula a funcao erro e tenta minimizar
      data.foreach(p => p.assignClosest(clusters)(distanceCalculator))

      //      aplica regra de Must Link e Cannot Link do LCVQE
      constraints match {
        case Some(constraints) => {
          val (mustLinkConstraints, cannotLinkConstraints) = constraints.partition(_.consType == ConstraintType.MustLink)

          mustLinkConstraints.foreach(constraint => {
            applyML(constraint)
          })

          cannotLinkConstraints.foreach(constraint => {
            applyCL(constraint, clusters)
          })

        }
        case None =>
      }

      //      reposiciona os clusters
      changed = clusters.map(c => c.reposition).reduceLeft(_ || _)

      i = i + 1
    }
  }

  def canAddToCluster(point: Point, cluster: Cluster): Boolean = {
    val cannotLinkPoints = this.constraints.get.filter(const => const.consType == ConstraintType.CannotLink && (const.pointA == point || const.pointB == point))

    ! cluster.points.exists(cannotLinkPoints.contains)
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
        removeFromCluster(constraint.pointB)
        addToCluster(constraint.pointB, constraint.pointA.cluster)
      }
      case `c` => {
        removeFromCluster(constraint.pointA)
        addToCluster(constraint.pointA, constraint.pointB.cluster)
      }
    }

  }

  private def addToCluster(point: Point, cluster: Option[Cluster]): Unit = {
    point.cluster = cluster
    cluster match {
      case Some(cl) => cl.addPoint(point)
      case None =>
    }
  }

  private def removeFromCluster(point: Point): Unit = {
    point.cluster match {
     case Some(cluster) => {
        cluster.points -= point
        point.cluster = None
      }
     case None =>
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
        removeFromCluster(constraint.pointB)
        addToCluster(constraint.pointB, constraint.pointA.cluster)
      }
      case `b` => {
        val rn: Point = calculateR(constraint, constraint.pointB.cluster.get)
        val mMn = mM(clusters, constraint.pointB.cluster.get, rn)

        mMn.addGCLV(constraint.pointB)
        removeFromCluster(constraint.pointA)
        addToCluster(constraint.pointA, constraint.pointB.cluster)
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
