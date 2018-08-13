package br.ufms.facom.ma.lcvqe.input


import br.ufms.facom.ma.lcvqe.{Constraint, ConstraintType, GeoTag, Point}
import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}

import scala.reflect.io.File

object CSVInput {

  implicit object MyFormat extends DefaultCSVFormat {
    override val delimiter = ';'
  }

  def readData(path: String): List[Point] = {
    val reader = CSVReader.open(path)(MyFormat)
    val data = reader.toStream.tail.map { item =>
      Point(item.head, item.tail.map(_.toDouble).toArray)
    }.toList
    reader.close()
    data.distinct
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
        val constraintType = if (item(2) == "1") {
          ConstraintType.MustLink
        } else {
          ConstraintType.CannotLink
        }
        Constraint(pointA, pointB, constraintType)
    }
    reader.close()
    constraints
  }

  def readGeotag(path: String, data: List[Point]): List[GeoTag] = {
    val reader = CSVReader.open(path)
    val geoTags = reader.toStream.tail.map { item =>
      val point: Point = data.find(p => p.id.equals(item(0))).get
      val lat = item(1).toDouble
      val long = item(2).toDouble
      if(point != None){
        Option(GeoTag(point, lat, long))
      } else {
        None
      }
    }.filterNot(_ == None).map(_.get).toList
    reader.close()
    geoTags
  }
}
