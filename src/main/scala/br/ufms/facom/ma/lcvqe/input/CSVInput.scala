package br.ufms.facom.ma.lcvqe.input


import br.ufms.facom.ma.lcvqe.{Constraint, ConstraintType, Point}
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
    data
  }

  def readConstraint(path: String, data: List[Point]): List[Constraint] = {
    val reader = CSVReader.open(path)
    val constraints = reader.toStream.tail.map { item =>
      val pointA: Point = data.find(p => p.id.equals(item(0))).get
      val pointB = data.find(p => p.id.equals(item(1))).get
      val constraintType = if (item(2).equals("1")) {
        ConstraintType.MustLink
      } else {
        ConstraintType.CannotLink
      }
      Constraint(pointA, pointB, constraintType)
    }.toList
    reader.close()
    constraints
  }
}
