package br.ufms.facom.ma.app

import java.io.{File, PrintWriter}

import br.ufms.facom.ma.lcvqe.input.CSVInput
import br.ufms.facom.ma.lcvqe.output.XMLOutput
import br.ufms.facom.ma.lcvqe.output.CSVOutput
import br.ufms.facom.ma.lcvqe.{Constraint, Cosine, Euclidean, LCVQE}

import scala.collection.mutable.ListBuffer

object LCVQEApp {

  def main(args: Array[String]): Unit = {
    implicit val formats = net.liftweb.json.DefaultFormats

    if (args.size == 0 ) {
      printInstructions()
    } else {
      val filter =
        if(args(1).nonEmpty) {
          Some(args(1))
        } else {
          None
        }
      val data = CSVInput.readData(args(0), filter)
      val metadata = CSVInput.readMetadata(args(0))
      val constraints = CSVInput.readConstraint(args(2), data)
      val geoTags = CSVInput.readGeotag(args(3), data)
      val brokenConstraints = ListBuffer.empty[Constraint]
      val lCVQE = LCVQE(data, Some(constraints), Some(geoTags), args(4).toInt, args(5).toInt, brokenConstraints)(Euclidean)
      val result = lCVQE.run()
      val xml = XMLOutput.exportResult(result, metadata)
      val outFile = new File(args(6))
      if(!outFile.exists()) {
        outFile.createNewFile()
      }

      val constraintFile = new File(args(7))
      if(!constraintFile.exists()) {
        constraintFile.createNewFile()
      }

      val pw = new PrintWriter(new File(args(6)))
      pw.write(xml)
      pw.close()

      CSVOutput.exportConstraint(constraintFile, brokenConstraints.toList)
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

