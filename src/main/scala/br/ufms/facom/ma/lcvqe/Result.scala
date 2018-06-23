package br.ufms.facom.ma.lcvqe

case class Result(data: Option[List[Point]] = None, clusters: Option[List[Cluster]] = None) {

  def quadraticError: Double =
    this.clusters match {
      case Some(clu) => clu.map(c => c.quadraticError).sum
      case None => Double.MaxValue
    }


}
