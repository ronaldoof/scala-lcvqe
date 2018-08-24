package br.ufms.facom.ma.lcvqe.output

import java.io.File

import br.ufms.facom.ma.lcvqe.{Constraint, ConstraintType}
import com.github.tototoshi.csv.{CSVWriter, DefaultCSVFormat}

object CSVOutput {

  implicit object MyFormat extends DefaultCSVFormat {
    override val delimiter = ';'
  }

  def exportConstraint(file: File, data: List[Constraint]): Unit = {
      val writer = CSVWriter.open(file)
    writer.writeAll(data.map { c => {
      if (c.consType == ConstraintType.CannotLink) {
        List(c.pointA.id, c.pointB.id, "0").toSeq
      } else {
        List(c.pointA.id, c.pointB.id, "1").toSeq
      }
    }
    })
  }

}
