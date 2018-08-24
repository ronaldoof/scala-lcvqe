package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.input.CSVInput
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ListBuffer

class LCVQETest extends FlatSpec with Matchers{

  "Given a list of constraints" should " filter only constraints applied to the list of points" in {
    val data = CSVInput.readData("/Users/rflorence/git/scala-lcvqe/src/main/resources/exp1/bow.csv")
    val constraints = CSVInput.readConstraint("/Users/rflorence/git/scala-lcvqe/src/main/resources/exp1/constraints.csv", data)
    val a  = new LCVQE(data, Some(constraints), None, 0, 0, ListBuffer.empty[Constraint])
    a.filterConstraints(data)

  }
}
