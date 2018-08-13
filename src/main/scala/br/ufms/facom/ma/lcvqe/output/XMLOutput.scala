package br.ufms.facom.ma.lcvqe.output

import br.ufms.facom.ma.lcvqe.Result

object XMLOutput {

  def exportResult(result: Result, metadata: List[String]): String = {
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
          <descriptors>{findDescriptors(c.centroid.dimensions, metadata).mkString(",")}</descriptors>
        </node>
      }}
    </tree>
    xml.toString()
  }

  private def findDescriptors(dimensions: Array[Double], metadata: List[String]): Array[String] ={
    dimensions.zipWithIndex.collect{
      case (value, i) if value != 0.0 => metadata(i)
    }
  }
}
