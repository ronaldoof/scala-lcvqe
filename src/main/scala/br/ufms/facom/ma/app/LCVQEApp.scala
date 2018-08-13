package br.ufms.facom.ma.app

import java.io.{File, PrintWriter}

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
      val metadata = CSVInput.readMetadata(args(0))
      val constraints = CSVInput.readConstraint(args(1), data)
      val geoTags = CSVInput.readGeotag(args(2), data)
      val lCVQE = LCVQE(data, Some(constraints), Some(geoTags), args(3).toInt, args(4).toInt)(Euclidean)
      val result = lCVQE.run()
      val xml = XMLOutput.exportResult(result, metadata)
      val pw = new PrintWriter(new File("/Users/rflorence/git/scala-lcvqe/src/main/resources/exp1/tarefa1/output.xml"))
      pw.write(xml)
      pw.close()
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

