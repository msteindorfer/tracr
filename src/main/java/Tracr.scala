import play.api.libs.json.{JsArray, Json}
import scala.collection.{GenSet, GenSeq, GenMap, immutable}

object ObjectLifetime {
  implicit val fmt = Json.format[ObjectLifetime]
}

case class ObjectLifetime(digest: String, ctorTime: Long, dtorTime: Long, measuredSizeInBytes: Long)

object Tracr extends App {

  val filename = "/Users/Michael/Dropbox/Research/ObjectLifetime/FirstRun/target/universe.json"

  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values.benchmarks/target/universe-SingleElementSetJUnitBenchmark.json"
  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values/target/universe.json"
  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values.benchmarks/target/universe.json"

  val jsonString = scala.io.Source.fromFile(filename).mkString
  val jsonArray = Json.parse(jsonString).asInstanceOf[JsArray]

  val universe: GenSet[ObjectLifetime] = Set.empty ++ jsonArray.value map (_.as[ObjectLifetime])
  //  println(universe)

  val sortedUniverse = universe.toList sortWith (_.ctorTime < _.ctorTime)
  //  sortedUniverse foreach println

  val overlapStatistics = valueOverlapStatistics(sortedUniverse)

  for ((digest, runs) <- overlapStatistics filter {!_._2.isEmpty}) {
    println(s"${runs.length} runs for $digest")

    for (run <- runs) {
      println("A RUN")
      run.foreach(println)
    }
  }

  def valueOverlapStatistics(sortedUniverse: GenSeq[ObjectLifetime]): GenMap[String, GenSeq[GenSeq[ObjectLifetime]]] = {
    val identitiesByValue: GenMap[String, GenSeq[ObjectLifetime]] = sortedUniverse groupBy (_.digest)
    val runsByValue: GenMap[String, GenSeq[GenSeq[ObjectLifetime]]] = identitiesByValue mapValues calculateRuns
    runsByValue
  }

  def calculateRuns(sortedIdentities: GenSeq[ObjectLifetime]): GenSeq[GenSeq[ObjectLifetime]] = {
    val resBuilder = immutable.List.newBuilder[GenSeq[ObjectLifetime]]
    val runBuilder = immutable.List.newBuilder[ObjectLifetime]
    var isEmpty = true
    var (ctorMin, dtorMax) = (0L, 0L)

    def maxDtorTime(newDtorTime: Long) = if (newDtorTime == -1) Long.MaxValue else newDtorTime

    for (identity <- sortedIdentities) {
      if (isEmpty) {
        // case for head of the list
        isEmpty = false

        // init
        runBuilder += identity
        ctorMin = identity.ctorTime
        dtorMax = maxDtorTime(identity.dtorTime)
      } else {
        if (ctorMin < identity.ctorTime && identity.ctorTime < dtorMax) {
          runBuilder += identity
          dtorMax = scala.math.max(dtorMax, maxDtorTime(identity.dtorTime))
        } else {
          val currentRun = runBuilder.result
          if (currentRun.size > 1) resBuilder += currentRun

          // reset
          runBuilder.clear();

          // init
          runBuilder += identity
          ctorMin = identity.ctorTime
          dtorMax = maxDtorTime(identity.dtorTime)
        }
      }
    }

    resBuilder.result
  }

}
