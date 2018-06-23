package br.ufms.facom.ma.lcvqe.util

import br.ufms.facom.ma.lcvqe.Point

class KmeansUtil{

}

object KmeansUtil {


  private def maxVal(data: List[Point], dim: Int): Double = {
      data.map(p => p.dimensions(dim)).max
  }


  private def minVal(data: List[Point], dim: Int): Double = {
    data.map(p => p.dimensions(dim)).min
  }

  def max(data: List[Point]): Array[Double] = {
    val dim = data(0).dimensions.length

    (0 until dim).map(maxVal(data, _)).toArray
  }

  def min(data: List[Point]): Array[Double] = {
    val dim = data(0).dimensions.length

    (0 until dim).map(minVal(data, _)).toArray
  }
}
