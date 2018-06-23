package br.ufms.facom.ma.lcvqe.output

import br.ufms.facom.ma.lcvqe.Result

object XMLOutput {

  def exportResult(result: Result): String = {
    val xml = <tree>
      {result.clusters.getOrElse(List.empty).map { c =>
        <node>
          <id>{c.id}</id>
          <parent>0</parent>
          <topic/>
          <documents>
          {c.points.map(_.id).mkString("[", ",", "]")}
          </documents>
          <descriptors>{c.centroid.dimensions.deep.toString()}</descriptors>
        </node>
      }}
    </tree>
    xml.toString()
  }

}
