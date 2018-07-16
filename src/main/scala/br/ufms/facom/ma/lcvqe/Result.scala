package br.ufms.facom.ma.lcvqe

case class Result(data: Option[List[Point]] = None, clusters: Option[List[Cluster]] = None) {

  def error (f: Cluster => Double): Double =
    this.clusters match {
      case Some(clu) => clu.map(c => f(c)).sum
      case None => Double.MaxValue
    }

}
