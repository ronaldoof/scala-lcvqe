package br.ufms.facom.ma.lcvqe.output

import br.ufms.facom.ma.lcvqe.Result

object XMLOutput {

  def exportResult(result: Result): String = {
    val xml = <tree>
      {result.clusters.getOrElse(List.empty).map { c =>
        <node>
          <id>{c.id}</id>
          <parent>{c.father match {
            case Some(father) => father.id
            case None => ""
          }
            }</parent>
          <topic/>
          <documents>
          {c.points.map(_.id).mkString("[", ",", "]")}
          </documents>
          <descriptors>{c.centroid.dimensions.mkString(",")}</descriptors>
        </node>
      }}
    </tree>
    xml.toString()
  }

}
