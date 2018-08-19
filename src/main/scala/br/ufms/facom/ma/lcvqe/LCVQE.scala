package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.rule.{CannotLinkRule, MustLinkRule}
import br.ufms.facom.ma.lcvqe.util.{KmeansUtil, NumberUtil, ReportUtil, Sequence}
import scala.collection.mutable.ListBuffer
import br.ufms.facom.ma.cache.Cache.mMCache
import scalacache._
import scalacache.modes.sync.mode

/**
  * Class that implements the LCVQE Algorithm.
  */
case class LCVQE (data: List[Point], constraints: Option[List[Constraint]], geoTags: Option[Set[GeoTag]], k: Int,
                  iterations: Int)(implicit distanceCalculator: DistanceCalculator = Cosine) {

  private val errorThreshold = 5.0

  def run(): Result = {

    printf("Starting HLCVQE Algorithm...\n")
    ReportUtil.countIt("Data")(data)


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
        core(clusterHistory, stack, actCluster, actData)
      }
    }
    Result(Some(data), Some(clusterHistory.toList))
  }


  private def core(clusterHistory: ListBuffer[Cluster], stack: ListBuffer[Cluster], actCluster: Cluster, actData: List[Point]) = {
    // init the clusters for this level
    val actClusters = initHClusters(actData, actCluster)
    val filteredConstraints: List[Constraint] = filterConstraints(actData)
    val filteredGeoTags = {
      val actDataS = data.map(_.id).toSet
      geoTags.getOrElse(Nil).filter(g => actDataS.contains(g.point.id))
    }.toSet

    val geoCannotLink = ReportUtil.timeIt("BuildGeoConstraints")(
      Constraint.buildCannotLink(filteredGeoTags, actCluster.level())
    )
    ReportUtil.countIt("MustLinkConstraints")(filteredConstraints)
    ReportUtil.countIt("CannotLinkConstraints")(geoCannotLink)

    // runs LCVQE on the data set with K=2 until convergence
    (0 until iterations).takeWhile(_ => {
      ReportUtil.timeIt("ApplyLCVQE")(
        applyLCVQE(actClusters, actData, geoCannotLink, filteredConstraints)
      )
    })
    ReportUtil.reportError(actClusters)

    // add new cluster to history and stack
    stack ++= actClusters.filter(c => c.points.size > 1)
    clusterHistory ++= actClusters
  }



  /**
    * Apply the main core of the LCQVE Algorithm
 *
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
      printf("===========> Only two points to divide.\n")
      ReportUtil.printRatioClusterPoint(actClusters)
      points.head.assingCluster(actClusters.head)
      points.tail.head.assingCluster(actClusters.tail.head)
      actClusters.head.centroid = points.head
      actClusters.tail.head.centroid = points.tail.head
      ReportUtil.printRatioClusterPoint(actClusters)
      false
    } else {
      // assing each point to the nearest cluster
      assignPointsToClosestCluster(points, actClusters)
      ReportUtil.printPointBalance(actClusters, points)
      if (constraints.nonEmpty) {
        applyLCVQEConstraints(actClusters, cannotLinkConstraint, mustLinkConstraints)
      }

      ReportUtil.printRatioClusterPoint(actClusters)
      ReportUtil.printPointBalance(actClusters, points)
      val finalClusters = actClusters.filter(c => c.points.nonEmpty)

      if(finalClusters.nonEmpty) {
        // reposition the cluster using LCVQE rules
        finalClusters.map(c => c.reposition).reduceLeft(_ || _)
      } else {
        false
      }
    }
  }

  def filterConstraints(actData: List[Point]): List[Constraint] = {
      val actDataC = actData.map(c => c.id).toSet
      constraints.getOrElse(Nil).filter(c => actDataC.contains(c.pointA.id) && actDataC.contains(c.pointB.id))
    }

  /**
    * Apply LCVE ML and CL rules
    * @param actClusters Cluster where the rules will apply
    * @param points Data set
    * @param cannotLinkConstraints List of CL constraints to apply
    * @param mustLinkConstraints List of ML constraints to apply
    */
  private def applyLCVQEConstraints(actClusters: List[Cluster], cannotLinkConstraints: List[Constraint],
                                    mustLinkConstraints: List[Constraint]): Unit = {
    ReportUtil.timeIt("ApplyMustLinkConstraint")(
      mustLinkConstraints.foreach(constraint => MustLinkRule(constraint))
    )

    ReportUtil.timeIt("ApplyCannotLinkConstraint")(
      cannotLinkConstraints.foreach(constraint =>
        CannotLinkRule(constraint, actClusters)
      )
    )
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
    printf("About to initialize clusters based on [%d] ML Constraints\n", this.constraints.get.size)
   val clusters = this.constraints.get.filter(_.consType == ConstraintType.MustLink).map {
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
    ReportUtil.printPointBalance(clusters, data)
    clusters
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
    val geoCannotLink = Constraint.buildCannotLink(this.geoTags.getOrElse(Nil).toSet, clusters.head.level())
    ReportUtil.countIt("GeoCannotLink")(geoCannotLink)

    (0 until this.iterations).takeWhile {
      _ =>
        // clean the clusters
        ReportUtil.timeIt("Clear Cluster")(
          clusters.foreach(_.clear())
        )
        ReportUtil.timeIt("Assign Closest Cluster")(
          assignPointsToClosestCluster(data, clusters)
        )
        // aplica regra de Must Link e Cannot Link do LCVQE
        ReportUtil.timeIt("ApplyLCVQEConstraints")(
            constraints match {
              case Some(const) =>
                applyLCVQEConstraints(clusters, geoCannotLink, const)

              case None => printf("NO CONSTRAINTS FOUND!\n")
            }
          )
        //      reposiciona os clusters
        val repo = ReportUtil.timeIt("RepositionClusters")(
          clusters.filter(c => c.points.nonEmpty).map(c => c.reposition).reduceLeft(_ || _)
        )
        ReportUtil.printPointBalance(clusters, this.data)
        repo
    }
  }

  def printIfNoPoint(cluster: Cluster): Unit ={
    if(cluster.points.isEmpty){
      printf("=========> The Cluster [%s] is empty\n", cluster.id)
    }
  }

  def assignPointsToClosestCluster(points: List[Point], clusters: List[Cluster]): Unit ={
    // find the closest cluster for every point
    points.foreach(p => p.assignClosest(clusters)(distanceCalculator))
  }

  def canAddToCluster(point: Point, cluster: Cluster): Boolean = {
    val cannotLinkPoints = this.constraints.get.filter(const => const.consType ==
      ConstraintType.CannotLink && (const.pointA == point || const.pointB == point))

    ! cluster.points.exists(cannotLinkPoints.contains)
  }

}
