package br.ufms.facom.ma.lcvqe


import org.scalatest.{FlatSpec, Matchers}

class PointTest extends FlatSpec with Matchers{

  "A random point generated with max coordenates of (9,10,15) and minimum coordinates of (-9.8, 0, 3) " should
    " have coordinates within the following ranges ([-9.8 - 9], [0 - 10], [3 - 15])" in {

    val max = Array[Double](9,10,15)
    val min = Array[Double](-9.8, 0, 3)
    val point = Point.random("a", max, min)
    val dim = 2

    (0 to dim).foreach(i => point.dimensions(i) should (be >= min(i) and be <= max(i)))
  }


  "Two points with the same coordinates" should "be equal even with different IDs" in {
    val pointA = Point("a", Array(3,2,1))
    val pointB = Point("a", Array(3,2,1))


    assert(pointA.equals(pointB))

  }

  "Two points with the same coordinates" should "have distance equal 0" in {
    val a = Point("a", Array(1.5,1.5))
    val b = Point("b", Array(1.5,1.5))
    val c = a.distanceTo(b)
    c shouldBe 0
  }

  "Two points with coordinates (0,0) and (15, 15)" should "have Euclidean distance equals to 450" in {

    val a = Point("a", Array(0,0))
    val b = Point("b", Array(15,15))

    a.distanceTo(b)(Euclidean) shouldBe 450

  }

}
