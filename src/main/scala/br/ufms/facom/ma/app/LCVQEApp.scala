package br.ufms.facom.ma.app

import br.ufms.facom.ma.lcvqe.{Constraint, Cosine, Euclidean, LCVQE}
import br.ufms.facom.ma.lcvqe.input.CSVInput
import br.ufms.facom.ma.lcvqe.output.XMLOutput

object LCVQEApp {

  def main(args: Array[String]): Unit = {
    implicit val formats = net.liftweb.json.DefaultFormats

    val data = CSVInput.readData(args(0))
    val constraints = CSVInput.readConstraint(args(1), data)
    val lcvqe = LCVQE.apply(data, Option(constraints), args(2).toInt, args(3).toInt)(Cosine)
    val result = lcvqe.run()
    val xml = XMLOutput.exportResult(result)
    println(xml)
  }

}
