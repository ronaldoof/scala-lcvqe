package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.util.NumberUtil

case class GeoTag (point: Point, lat: Double, long: Double) {
  val R = 6372.8; // Radios of the Earth In kilometers


  /**
    * Calculate the distance between two geotags using the Haversine distance
    * @param other the other geotag to calculate the distance to
    * @return the distance between two poins in earth in Km
    */
  def distanceTo(other: GeoTag): Double = {
    val dLat = math.toRadians(other.lat - this.lat)
    val dLong = math.toRadians(other.long - this.long)
    val radLat = math.toRadians(this.lat)
    val otherRadLat = math.toRadians(other.lat)
    val a = math.pow(math.sin(dLat / 2), 2) + math.pow(math.sin(dLong / 2), 2) * math.cos(radLat) * math.cos(otherRadLat)
    val c = 2 * math.asin(math.sqrt(a))
    NumberUtil.round(R * c)
  }
}

