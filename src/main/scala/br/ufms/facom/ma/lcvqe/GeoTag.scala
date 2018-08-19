package br.ufms.facom.ma.lcvqe

import br.ufms.facom.ma.lcvqe.util.NumberUtil

case class GeoTag (point: Point, lat: Double, long: Double) {
  val R = 6378d; // Radios of the Earth In kilometers


  /**
    * Calculate the distance between two geotags using the Haversine distance
    *
    * @param other the other geotag to calculate the distance to
    * @return the distance between two poins in earth in Km
    *         val latDistance = Math.toRadians(userLocation.lat - warehouseLocation.lat)
    *         val lngDistance = Math.toRadians(userLocation.lon - warehouseLocation.lon)
    *         val sinLat = Math.sin(latDistance / 2)
    *         val sinLng = Math.sin(lngDistance / 2)
    *         val a = sinLat * sinLat +
    *         (Math.cos(Math.toRadians(userLocation.lat))
    *         * Math.cos(Math.toRadians(warehouseLocation.lat))
    *         * sinLng * sinLng)
    *         val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    *         (AVERAGE_RADIUS_OF_EARTH_KM * c).toInt
    *
    */
  def distanceTo(other: GeoTag): Double = {
    val dLat = math.toRadians(other.lat - this.lat)
    val dLong = math.toRadians(other.long - this.long)
    val sinLat = Math.sin(dLat / 2)
    val sinLng = Math.sin(dLong / 2)
    val a = sinLat * sinLat +
    (Math.cos(Math.toRadians(this.lat)) *
      Math.cos(Math.toRadians(other.lat)) * sinLng * sinLng)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    (R * c).toInt
//
//    val radLat = math.toRadians(this.lat)
//    val otherRadLat = math.toRadians(other.lat)
//    val a = math.pow(math.sin(dLat * 0.5), 2) + math.pow(math.sin(dLong * 0.5), 2) * math.cos(radLat) * math.cos(otherRadLat)
//    val c = 2 * math.asin(math.sqrt(a))
//    NumberUtil.round(R * c)
  }

  override def canEqual(a: Any): Boolean = a.isInstanceOf[GeoTag]

  override def equals(that: Any): Boolean = that match {
    case that: GeoTag => that.canEqual(this) && this.point.id == that.point.id
    case _ => false
  }

  override def hashCode: Int = {
    this.point.id.##
  }

  override def toString: String = {
    s"${this.point.id}"
  }
}

