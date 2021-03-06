package br.ufms.facom.ma.lcvqe

import org.scalatest.{FlatSpec, Matchers}


class DistanceCalculatorTest extends FlatSpec with Matchers{
  val EXPECTED_DIST_1 = 8
  val EXPECTED_DIST_2 = 40
  val EXPECTED_DIST_3 = 0.106

  "The simplified Euclidean Distance from two points (0,0) and (2,2) " should " be 8" in {
    val pointA = new Point("a", Array(0,0), None)
    val pointB = new Point("b", Array(2,2), None)
    val euclidean = Euclidean
    assertResult(EXPECTED_DIST_1)(euclidean.calculateDistance(pointA, pointB))
  }

  "The dot product of two vectors with coords Va(1,1) and Vb(2,2)" should "be equals 40" in{
    val pointA = new Point("a", Array(5,5))
    val pointB = new Point("b", Array(2,6))
    assertResult(EXPECTED_DIST_2){
      Cosine.dotProduct(pointA, pointB)
    }
  }

  "The  Cosine similarity" should " from two points (5,5) and (2,6) should be 0.10557280900008414" in {
    val pointA = new Point("a", Array(5,5))
    val pointB = new Point("b", Array(2,6))
    val distance = Cosine.calculateDistance(pointA, pointB)
    val rounded = BigDecimal((distance)).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    rounded shouldBe EXPECTED_DIST_3
  }
}
