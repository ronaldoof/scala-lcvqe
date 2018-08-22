package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.input.CSVInput
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
class ConstraintTest extends FlatSpec with Matchers{

  behavior of "ConstraintTest"

  it should "buildCannotLink" in {
//    val data = CSVInput.readData("/Users/rflorence/git/scala-lcvqe/src/main/resources/datasets/BUSINESS_TRANSACTIONS_bag.csv",
//      Some("/Users/rflorence/git/scala-lcvqe/src/main/resources/datasets/BUSINESS_TRANSACTIONS_clustering.list"))
//    val geoTags = CSVInput.readGeotag("/Users/rflorence/git/scala-lcvqe/src/main/resources/datasets/BUSINESS_TRANSACTIONS_geographic_lat_lng.csv",
//      data)
////    val geoMap = Constraint.buildGeoMap(data, geoTags)
//     Constraint.buildCannotLink(geoTags, 1)

  }


}
