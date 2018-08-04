package br.ufms.facom.ma.app

import br.ufms.facom.ma.lcvqe.input.CSVInput
import br.ufms.facom.ma.lcvqe.output.XMLOutput
import br.ufms.facom.ma.lcvqe.{Euclidean, LCVQE}

object LCVQEApp {

  def main(args: Array[String]): Unit = {
    implicit val formats = net.liftweb.json.DefaultFormats

    if (args.size == 0 ) {
      printInstructions()
    } else {
      val data = CSVInput.readData(args(0))
      val constraints = CSVInput.readConstraint(args(1), data)
      val lcvqe = LCVQE.apply(data, Option(constraints), args(2).toInt, args(3).toInt)(Euclidean)
      val result = lcvqe.run()
      val xml = XMLOutput.exportResult(result)
      println(xml)
    }
  }


  def printInstructions (): Unit = {
    println(s"=======================\n LCVQE APP " +
      s"\n=======================   " +
      s"\n Please enter the path to a csv file containing your data" +
      s" and another file containing your constraints. You can also provide K = " +
      s"number of clusters and I = number of iterations you want to run.\n" +
      s"\n=======================")

  }
}

