package br.ufms.facom.ma.lcvqe.util

object NumberUtil {

 def round(n: Double): Double = {
   try {
    BigDecimal(n).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
   } catch {
     case e: NumberFormatException =>
       printf(s"n = ${n.toString}")
       e.printStackTrace()
       throw e
   }
  }
}
