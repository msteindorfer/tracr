import java.io.{FileWriter, BufferedWriter, File}
import play.api.libs.json.{JsArray, Json}
import scala.collection.{GenIterable, GenSeq, GenMap, GenSet}
import scala.collection.immutable.NumericRange

object ObjectLifetime {
  implicit val fmt = Json.format[ObjectLifetime]
}

case class ObjectLifetime(digest: String, ctorTime: Long, dtorTime: Option[Long], measuredSizeInBytes: Long) {
  require(ctorTime >= 0)
  require(!dtorTime.isDefined || dtorTime.get >= 0)
  require(measuredSizeInBytes >= 0)
}

object Tracr extends App {

  import TracrUtil._

  val filename = "/Users/Michael/Dropbox/Research/ObjectLifetime/ActualRun/target/universe.json"

  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values.benchmarks/target/universe-SingleElementSetJUnitBenchmark.json"
  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values/target/universe.json"
  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values.benchmarks/target/universe.json"

  val jsonString = scala.io.Source.fromFile(filename).mkString
  val jsonArray = Json.parse(jsonString).asInstanceOf[JsArray]

  val universe: GenSet[ObjectLifetime] = Set.empty ++ jsonArray.value map (_.as[ObjectLifetime])
  //  println(universe)

  val sortedUniverse = universe.toList sortWith (_.ctorTime < _.ctorTime)
  //  sortedUniverse foreach println

  val overlapStatistics: GenMap[String, GenSeq[GenSeq[ObjectLifetime]]] = valueOverlapStatistics(sortedUniverse)

  //  for ((digest, runs) <- overlapStatistics filter (!_._2.isEmpty)) {
  //    println(s"${runs.length} runs for $digest")
  //
  //    for (run <- runs) {
  //      println("A RUN")
  //      run.foreach(println)
  //    }
  //  }

  /*
   * Replay heap size history.
   */
  val timestampUniverse: GenSet[Long] = universe.map(_.ctorTime) union universe.flatMap(_.dtorTime)

  val timestampRange = timestampUniverse.min to timestampUniverse.max
  val heapSizes = projectHeapSize(universe, timestampRange)

  writeHeapSizeHistory("heapSizes-nom.dat", (timestampRange zip heapSizes))

  /*
   * Suggest optimistic heap history.
   */
  val replacements: GenIterable[ObjectLifetime] = for {
    overlaps <- overlapStatistics.values
    overlap <- overlaps
    digest = overlap.head.digest
    ctorTime = overlap.map(_.ctorTime).min
    dtorTime = overlap.map(_.dtorTime).max
    size = overlap.head.measuredSizeInBytes
  } yield ObjectLifetime(digest, ctorTime, dtorTime, size);

  assert (replacements.size == overlapStatistics.values.flatten.size)

  val operlapsMin = overlapStatistics.values.flatten.flatten
  val universeMin = (universe union replacements.toSet) diff operlapsMin.toSet
  val heapSizesMin = projectHeapSize(universeMin, timestampRange)

  writeHeapSizeHistory("heapSizes-min.dat", (timestampRange zip heapSizesMin))

}

object TracrUtil {

  def writeHeapSizeHistory(filename: String, heapHistory: GenIterable[(Long, BigInt)]) {
    val outputFile = new File(filename)
    val writer = new BufferedWriter(new FileWriter(outputFile))

    heapHistory map {
      case (timestamp, heapSize) => writer.write(s"$timestamp $heapSize"); writer.newLine
    }

    writer.flush
    writer.close
  }

  def valueOverlapStatistics(sortedUniverse: GenSeq[ObjectLifetime]): GenMap[String, GenSeq[GenSeq[ObjectLifetime]]] = {
    val identitiesByValue: GenMap[String, GenSeq[ObjectLifetime]] = sortedUniverse groupBy (_.digest)
    val runsByValue: GenMap[String, GenSeq[GenSeq[ObjectLifetime]]] = identitiesByValue mapValues calculateRuns
    runsByValue
  }

  def calculateRuns(sortedIdentities: GenSeq[ObjectLifetime]): GenSeq[GenSeq[ObjectLifetime]] = {
    val resBuilder = List.newBuilder[GenSeq[ObjectLifetime]]
    val runBuilder = List.newBuilder[ObjectLifetime]
    var isEmpty = true
    var (ctorMin, dtorMax) = (0L, Option.empty[Long])

    def maxOption(candidates: Option[Long]*): Option[Long] = candidates.flatten match {
      case Seq() => None
      case xs => Some(xs.max)
    }

    for (identity <- sortedIdentities) {
      if (isEmpty) {
        // case for head of the list
        isEmpty = false

        // init
        runBuilder += identity
        ctorMin = identity.ctorTime
        dtorMax = identity.dtorTime
      } else {
        if (ctorMin < identity.ctorTime && (!dtorMax.isDefined || identity.ctorTime < dtorMax.get)) {
          runBuilder += identity
          dtorMax = maxOption(dtorMax, identity.dtorTime)
        } else {
          val currentRun = runBuilder.result
          if (currentRun.size > 1) resBuilder += currentRun

          // reset
          runBuilder.clear();

          // init
          runBuilder += identity
          ctorMin = identity.ctorTime
          dtorMax = identity.dtorTime
        }
      }
    }

    resBuilder.result
  }

  def projectHeapSize(universe: GenSet[ObjectLifetime], timestampRange: NumericRange[Long]): GenSeq[BigInt] = {
    val universeList = universe.toList
    val ctorSorted = universeList sortBy (_.ctorTime)
    val dtorSorted = universeList sortBy (_.dtorTime) filter (_.dtorTime.isDefined)

    val ctorAccumulated = accumulatedSize(_.ctorTime    )(ctorSorted);
    val dtorAccumulated = accumulatedSize(_.dtorTime.get)(dtorSorted);

    var ctorSum = BigInt(0);
    var dtorSum = BigInt(0);
    val builder = List.newBuilder[BigInt]

    for (timestamp <- timestampRange) {
      ctorSum = ctorAccumulated.getOrElse(timestamp, ctorSum)
      dtorSum = dtorAccumulated.getOrElse(timestamp, dtorSum)
      builder += ctorSum - dtorSum
    }

    builder.result
  }

  /*
   * Creates a Map from the selctor property to the accumulated size of traces previoulsy processed.
   */
  def accumulatedSize(selector: ObjectLifetime => Long)(traceSeq: GenSeq[ObjectLifetime]): Map[Long, BigInt] = {
    var max = BigInt(0);
    val builder = Map.newBuilder[Long, BigInt]

    for (trace <- traceSeq) {
      max += trace.measuredSizeInBytes
      builder += selector(trace) -> max
    }

    builder.result
  }

}
