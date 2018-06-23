package br.ufms.facom.ma.lcvqe.input

import br.ufms.facom.ma.lcvqe.Point
import org.scalatest.{FunSpec, Matchers}

class CSVInputTest extends FunSpec with Matchers {

  val pointA = Point("a", Array(1.5, 2.8, 3.3))
  val pointB = Point("b", Array(2.3, 4.5, 2.3))
  val pointC = Point("c", Array(4.5, 5.6, 2.3))


  describe("Reading a csv file should produce a list of points") {
   it(" should be equal to expected data "){
    val points = CSVInput.readCSV("/Users/rflorence/git/LCVQE/src/main/resources/data.csv")
    points(0) shouldEqual(pointA)
    points(1) shouldEqual(pointB)
    points(2) shouldEqual(pointC)
   }
  }

}
