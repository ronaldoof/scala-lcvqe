package br.ufms.facom.ma.lcvqe.input


import br.ufms.facom.ma.lcvqe.{Constraint, ConstraintType, GeoTag, Point}
import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}

import scala.reflect.io.File

object CSVInput {

  implicit object MyFormat extends DefaultCSVFormat {
    override val delimiter = ';'
  }

  def readData(path: String, filter: Option[String] = None): List[Point] = {
    val reader = CSVReader.open(path)(MyFormat)
    val data = reader.toStream.tail.filter(_.nonEmpty).map { item =>
      Point(item.head, item.tail.map(_.toDouble).toArray)
    }.toList
    reader.close()
    filter match {
      case Some(f) => {
        val filters = CSVReader.open(f).all().toSet
        val filteredData = data.filter(d => filters.map(_(0)).contains(d.id))
        filteredData
      }
      case None => data.distinct
    }
  }

  def readMetadata(path: String): List[String] = {
    val reader = CSVReader.open(path)(MyFormat)
    val data = reader.toStream.head
    reader.close()
    data.distinct
  }

  def readConstraint(path: String, data: List[Point]): List[Constraint] = {
    val reader = CSVReader.open(path)
    val constraints = reader.all().filter(item => data.exists(p => p.id == item(0)) && data.exists(p => p.id == item(1))).map { item =>
        val pointA: Point = data.find(p => p.id.equals(item(0))).get
        val pointB = data.find(p => p.id.equals(item(1))).get

        val constraintType = if (item.size < 3 ||  item(2) == "1") {
          ConstraintType.MustLink
        } else {
          ConstraintType.CannotLink
        }
        Constraint(pointA, pointB, constraintType)
    }
    reader.close()
    constraints
  }

  def readGeotag(path: String, data: List[Point]): Set[GeoTag] = {
    val reader = CSVReader.open(path)
    val geoTags = reader.toStream.tail.collect { case item if (data.map(_.id).contains(item(0))) =>
      val point = data.find(p => p.id.equals(item(0)))
      val lat = item(1).toDouble
      val long = item(2).toDouble
      GeoTag(point.get, lat, long)
    }.toSet
    reader.close()
    geoTags
  }
}
