package br.ufms.facom.ma.cache

import br.ufms.facom.ma.lcvqe.Cluster
import scalacache.guava.GuavaCache
import scalacache.Cache

object Cache {
//  val underlyingGuavaCache = CacheBuilder.newBuilder().maximumSize(100000L).build[String, Entry[T]]
  implicit val distanceCache: Cache[Double] = GuavaCache[Double]
  implicit val mMCache: Cache[Cluster] = GuavaCache[Cluster]

}
