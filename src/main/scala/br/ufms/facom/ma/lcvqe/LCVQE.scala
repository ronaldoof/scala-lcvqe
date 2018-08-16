package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.rule.{CannotLinkRule, MustLinkRule}
import br.ufms.facom.ma.lcvqe.util.{KmeansUtil, NumberUtil, Sequence}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
/**
  * Class that implements the LCVQE Algorithm.
  */
case class LCVQE (data: List[Point], constraints: Option[List[Constraint]], geoTags: Option[List[GeoTag]], k: Int,
                  iterations: Int)(implicit distanceCalculator: DistanceCalculator = Cosine) {


  def run(): Result = {

    printf("Starting HLCVQE Algorithm...\n")
//    val geoMap = Constraint.buildGeoMap(data, geoTags.get)
    val seq = Sequence
    val root = Cluster(seq.next(), Point("root_point", Array(0,0)))
    val clusters = initClusters(data, root)
    val clusterHistory =  ListBuffer(root)
    val stack = ListBuffer.empty[Cluster]

    clusters.foreach(cl => {
      stack += cl
      clusterHistory += cl
    })

    printf("==> Building root level\n")
    buildRootLevel(clusters)
    printf("==> Root level built\n")

    while(stack.nonEmpty) {
      // remove o topo da pilha
      val actCluster = stack.remove(0)
      // the data set is composed by the points inside the cluster in the top of the stack
      val actData = actCluster.points.toList

      if(actData.size > 1) {
        // init the clusters for this level
        val actClusters = initHClusters(actData, actCluster)
        val actConstraints: List[Constraint] = filterConstraints(actData)
        val filteredGeoTags = {
          val actDataS = data.map(_.id).toSet
          geoTags.getOrElse(Nil).filter(g => actDataS.contains(g.point.id))
        }

        val geoCannotLink = Constraint.buildCannotLink(filteredGeoTags, actCluster.level())
        // runs LCVQE on the data set with K=2 until convergence
        (0 until iterations).takeWhile(_ => {
          printf("========> Runing LCVQE on the Level\n")
          applyLCVQE(actClusters, actData, geoCannotLink ,actConstraints)
        })

        // add new cluster to history and stack
        stack ++=actClusters.filter(c => c.points.size > 1)
        clusterHistory ++=actClusters
      }
    }

    Result(Some(data), Some(clusterHistory.toList))

  }


  /**
    * Apply the main core of the LCQVE Algorithm
    * @param actClusters the current cluster to be taken in considerations
    * @param data the current dataSet to be taken in consideration
    * @return true if some cluster changed position and false if no cluster changed position. The latest
    *         means conversion of the algorithm
    */
  def applyLCVQE (actClusters: List[Cluster], points: List[Point], cannotLinkConstraint: List[Constraint], mustLinkConstraints: List[Constraint]): Boolean = {
    //      clean the clusters
    actClusters.foreach(c => c.clear())

    // there is a special case where two points may be so close they won't allow convergence on the algorithm
    // to overcome that we focefuly assing each point to one cluster
    if(points.size == 2) {
      printRationClusterPoint(actClusters)
      points.head.assingCluster(actClusters.head)
      points.tail.head.assingCluster(actClusters.tail.head)
      actClusters.head.reposition
      actClusters.tail.head.reposition
      printRationClusterPoint(actClusters)
      false
    } else {
      // assing each point to the nearest cluster
      assignPointsToClosestCluster(points, actClusters)

      if (constraints.nonEmpty) {
        applyLCVQEConstrants(actClusters, points, cannotLinkConstraint, mustLinkConstraints)
      }

      printRationClusterPoint(actClusters)
      val finalClusters = actClusters.filter(c => c.points.nonEmpty)

      if(finalClusters.nonEmpty) {
        // reposition the cluster using LCVQE rules
        finalClusters.map(c => c.reposition).reduceLeft(_ || _)
      } else {
        false
      }
    }
  }

  private def printRationClusterPoint(actClusters: List[Cluster]) = {
    actClusters.foreach(c => printf("Cluster [%s] has [%d] points\n", c.id, c.points.size))
  }

  def filterConstraints(actData: List[Point]): List[Constraint] = {
    // get the constraints that only refers to the points being analyzed
    val time = System.currentTimeMillis()
    val aConstraints = Future {
      val actDataC = actData.map(c => c.id).toSet
      constraints.getOrElse(Nil).filter(c => actDataC.contains(c.pointA.id))
    }
    val bConstraints = Future {
      val time = System.currentTimeMillis()
      val actDataC = actData.map(c => c.id).toSet
      constraints.getOrElse(Nil).filter(c => actDataC.contains(c.pointB.id))
    }
    val result = Await.result(Future.sequence(List(aConstraints, bConstraints)), Duration.Inf).flatMap(_.distinct)
    result
  }

  private def applyLCVQEConstrants(actClusters: List[Cluster], points: List[Point],
                                   cannotLinkConstraints: List[Constraint], mustLinkConstraints: List[Constraint]) = {
    // apply LCVQE must link and cannot link rules
    val mustLinkFutures = mustLinkConstraints.map(constraint => Future {
      MustLinkRule(constraint)
    })

    val cannotLinkFutures =
      cannotLinkConstraints.map(constraint => Future {
      CannotLinkRule(constraint, actClusters)
    })
    Await.ready(Future.sequence(mustLinkFutures ::: cannotLinkFutures), Duration.Inf)
  }

  /**
    * Sets a point to a cluster considering constraints. If the point cannot join the cluster
    * creates a new cluster for that point.
    *
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
      val newCluster = Cluster(id, point.copy(id = id),
        ListBuffer(point), father = Some(root))
      point.cluster = Some(newCluster)
      newCluster
    }
  }

  /**
    * Inicializa os clusteres encontrando um K minimo para garantir o cumprimento de todas as restricoes.
    * @param data the dataset to be clustered
    * @return the list of clusters created
    */
  def initClusters(points: List[Point], root: Cluster): List[Cluster] = {
    val seq = Sequence
    printf("About to initialize clusters based on [%d] ML Constraints", this.constraints.get.size)
    this.constraints.get.filter(_.consType == ConstraintType.MustLink).map {
      c => c.pointA.cluster match {
        case None =>
          c.pointB.cluster match {
            case None =>
              val id = seq.next()
              val newCluster = Cluster(id, c.pointA.copy(id = id),
                ListBuffer(c.pointA, c.pointB), father = Some(root))
              c.pointA.cluster = Some(newCluster)
              c.pointB.cluster = Some(newCluster)
              newCluster
            case Some(clusterB) => setCluster(c.pointA, clusterB, root)
        }
        case Some(clusterA) =>
          c.pointB.cluster match {
            case None => setCluster(c.pointB, clusterA, root)
            case Some(cluster)=> cluster
          }
      }
    }.distinct
  }

  /**
    *
    * @param data the data set where the first clusters will be built uppon
    * @return a list of initial clusters
    */
  def initHClusters(points: List[Point], father: Cluster): List[Cluster] = {
    (0 until 2).map( _ => {
      val id = Sequence.next()
      val pointIndex = scala.util.Random.nextInt(points.size - 1)
      val centroid = points(pointIndex).copy(id = id)
      Cluster(id, centroid, father = Some(father))
    }).toList
  }


  /**
    * Constroi o primeiro nivel do algoritmo hierarquico. Nesse caso nos ja temos os clusters
    * e queremos apenas aperfeicoar o agrupamento usando LCVQE
    * @param clusters the cluster that the root level will be built uppon
    */
  def buildRootLevel(clusters: List[Cluster]): Unit ={
    val geoCannotLink = Constraint.buildCannotLink(this.geoTags.getOrElse(Nil), clusters.head.level())

    (0 until this.iterations).takeWhile {
      _ =>
        val time1 = System.currentTimeMillis()
        // clean the clusters
        clusters.foreach(_.clear())
        val time2 = System.currentTimeMillis()

        assignPointsToClosestCluster(data, clusters)

        val time3 = System.currentTimeMillis()
        // aplica regra de Must Link e Cannot Link do LCVQE
        constraints match {
          case Some(const) =>
            applyLCVQEConstrants(clusters, data, geoCannotLink, const)

          case None =>
        }

        val time4 = System.currentTimeMillis()
        //      reposiciona os clusters
        val repo = clusters.filter(c => c.points.nonEmpty).map(c => c.reposition).reduceLeft(_ || _)
        val time5 = System.currentTimeMillis()
        printf("Times 1-2 [%d] :: 2-3 [%d] :: 3-4 [%d] :: 4-5 [%d]\n", time2-time1, time3-time2, time4-time3, time5-time4)
        repo
    }
  }

  def assignPointsToClosestCluster(points: List[Point], clusters: List[Cluster]): Unit ={
    // find the closest cluster for every point. We use future here to paralelize the task
    Await.ready(Future.sequence(points.map(p => {
      Future {
        p.assignClosest(clusters)(distanceCalculator)
      }
    })), Duration.Inf)
  }

  def canAddToCluster(point: Point, cluster: Cluster): Boolean = {
    val cannotLinkPoints = this.constraints.get.filter(const => const.consType == ConstraintType.CannotLink && (const.pointA == point || const.pointB == point))

    ! cluster.points.exists(cannotLinkPoints.contains)
  }

}
