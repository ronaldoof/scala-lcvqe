package br.ufms.facom.ma.lcvqe.util

/**
  * Defines a simple sequence object to help create IDs for clusters
  */

object Sequence {

  private var seed = 0

  def next(): String = {
    seed = seed + 1
    seed.toString
  }
}
