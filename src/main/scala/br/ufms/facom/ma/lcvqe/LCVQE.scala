package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.error.LCVQEError
import br.ufms.facom.ma.lcvqe.rule.{CannotLinkRule, MustLinkRule}
import br.ufms.facom.ma.lcvqe.util.{KmeansUtil, Sequence}

import scala.collection.mutable.ListBuffer


/**
  * Class that implements the LCVQE Algorithm.
  */
case class LCVQE (data: List[Point], constraints: Option[List[Constraint]], k: Int, iterations: Int)(implicit distanceCalculator: DistanceCalculator = Cosine) {


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

    while(stack.nonEmpty) {

      var changed = true
      var i = 0

      // remove o topo da pilha
      val actCluster = stack.head
      stack.remove(0)

      if(actCluster.points.size > 1 && actCluster.quadraticError > 0) {
        // the data set is composed by the points inside the cluster in the top of the stack
        val actData = actCluster.points.toList
        // init the clusters for this level
        val actClusters = initHClusters(actData)
        def calcError: Cluster => Double = LCVQEError.calcError(actClusters, _)

        // runs LCVQE on the data set with K=2 until convergence
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
    val (mustLinkConstraints, cannotLinkConstraints) = actConstraints.partition(_.consType == ConstraintType.MustLink)

    mustLinkConstraints.foreach(constraint => {
      MustLinkRule(constraint)
    })
    cannotLinkConstraints.foreach(constraint => {
      CannotLinkRule(constraint, actClusters)
    })

    //      reposiciona os clusters
    actClusters.map(c => c.reposition).reduceLeft(_ || _)

  }

  /**
    * Sets a point to a cluster considering constraints. If the point cannot join the cluster
    * creates a new cluster for that point.
    * @param point point to be added to the cluster
    * @param cluster cluster where the point will be set
    * @return cluster with the added point or a new cluster for that point
    */
  private def setCluster(point: Point, cluster: Cluster, root: Cluster): Cluster = {
    val seq = Sequence
    if (canAddToCluster(point, cluster)) {
      cluster.addPoint(point)
      point.cluster = Some(cluster)
      cluster
    } else {
      val id = seq.next()
      val newCluster = Cluster(id, Point.random(s"centroid-$id", KmeansUtil.max(data), KmeansUtil.min(data)), ListBuffer(point), father = Some(root))
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
              val id = seq.next()
              val newCluster = Cluster(id, Point.random(s"centroid-$id", KmeansUtil.max(data), KmeansUtil.min(data)), ListBuffer(c.pointA, c.pointB), father = Some(root))
              c.pointA.cluster = Some(newCluster)
              c.pointB.cluster = Some(newCluster)
              newCluster
            }
            case Some(clusterB) => setCluster(c.pointA, clusterB, root)
          }
        }
        case Some(clusterA) => {
          c.pointB.cluster match {
            case None => setCluster(c.pointB, clusterA, root)
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

      // limpa os clusters
      clusters.foreach(_.clear)

      // calcula a funcao erro e tenta minimizar
      data.foreach(p => p.assignClosest(clusters)(distanceCalculator))

      // aplica regra de Must Link e Cannot Link do LCVQE
      constraints match {
        case Some(constraints) => {
          val (mustLinkConstraints, cannotLinkConstraints) = constraints.partition(_.consType == ConstraintType.MustLink)

          mustLinkConstraints.foreach(constraint => {
            MustLinkRule(constraint)
          })

          cannotLinkConstraints.foreach(constraint => {
            CannotLinkRule(constraint, clusters)
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
}
