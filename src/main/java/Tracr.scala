import java.io._
import org.eclipse.imp.pdb.facts.tracking.TrackingProtocolBuffers
import scala.collection.{GenIterable, GenSeq, GenMap, GenSet}
import scala.collection.immutable.NumericRange
import scala.Some

case class ObjectLifetime(digest: String, ctorTime: Long, dtorTime: Option[Long], measuredSizeInBytes: Long) {
  require(ctorTime >= 0)
  require(!dtorTime.isDefined || dtorTime.get >= 0)
  require(measuredSizeInBytes >= 0)
}

object Tracr extends App {

  import TracrUtil._

  //  val filename = "/Users/Michael/Dropbox/Research/ObjectLifetime/ActualRun/target/universe"

  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values.benchmarks/target/universe-SingleElementSetJUnitBenchmark"
  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values/target/universe"
  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values.benchmarks/target/universe"
  val filename = "/Users/Michael/Development/rascal-devel/rascal-shell/target/universe"

  /*
   * Deserialize universe from Google Protocol Buffers
   */
  val universe: GenSet[ObjectLifetime] = time("Deserialize universe from Google Protocol Buffers") {
    val universeBuilder = Set.newBuilder[ObjectLifetime]

    {
      val protoInputStream = new FileInputStream(filename + ".raw")
      try {
        while (true) {
          val protoObjectLifetime = TrackingProtocolBuffers.ObjectLifetime.parseDelimitedFrom(protoInputStream)

          val digest = protoObjectLifetime.getDigest
          val ctorTime = protoObjectLifetime.getCtorTime
          val dtorTime = if (protoObjectLifetime.hasDtorTime) Some(protoObjectLifetime.getDtorTime) else None
          val measuredSizeInBytes = protoObjectLifetime.getMeasuredSizeInBytes

          universeBuilder += ObjectLifetime(digest, ctorTime, dtorTime, measuredSizeInBytes)
        }
      } catch {
        case _: Exception => {}
      }
    }

    universeBuilder.result
  }

  val sortedUniverse = time("Sort universe") {
    universe.toList sortWith (_.ctorTime < _.ctorTime)
  }
  //  sortedUniverse foreach println

  val overlapStatistics: GenMap[String, GenSeq[GenSeq[ObjectLifetime]]] = time("Calculate overlap statistics") {
    valueOverlapStatistics(sortedUniverse)
  }

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
//  val timestampUniverse: GenSet[Long] = universe.map(_.ctorTime) union universe.flatMap(_.dtorTime)
//  val timestampRange = timestampUniverse.min to timestampUniverse.max

  // initialize min/max with any value (i.e. first one encountered)
  var tsMin: Long = universe.head.ctorTime
  var tsMax: Long = universe.head.ctorTime

  time ("Calculate min / max timestamp") {
    for (olt <- universe) {
      tsMin = math.min(tsMin, olt.ctorTime)

      tsMax = math.max(tsMax, olt.ctorTime)
      if (olt.dtorTime.isDefined)
        tsMax = math.max(tsMax, olt.dtorTime.get)
    }
  }

  val timestampRange = tsMin to tsMax

  {
    val universeList = time("set to list") { universe.toVector }
    val ctorSorted: Vector[ObjectLifetime] = time("ctorSorted") { universeList sortWith (_.ctorTime < _.ctorTime) }
    val dtorSorted: Vector[ObjectLifetime] = time("dtorSorted") { universeList   filter (_.dtorTime.isDefined) sortWith (_.dtorTime.get < _.dtorTime.get) }

    time ("Project Heap Size") {
      projectProperty("heapSizes-nom.dat", ctorSorted, dtorSorted, timestampRange)(_.measuredSizeInBytes)
    }

    time ("Project Object Size") {
      projectProperty("objectCount-nom.dat", ctorSorted, dtorSorted, timestampRange)(_ => 1)
    }
  }

  /*
   * Suggest optimistic heap history.
   */
  val replacements: GenIterable[ObjectLifetime] = time ("Suggest optimistic heap history.") {
    for {
      overlaps <- overlapStatistics.values
      overlap <- overlaps
      digest = overlap.head.digest
      ctorTime = overlap.map(_.ctorTime).min
      dtorTime = overlap.map(_.dtorTime).max
      size = overlap.head.measuredSizeInBytes
    } yield ObjectLifetime(digest, ctorTime, dtorTime, size)
  };

  /*
   * There are hardly any differences in size. It's neglectable.
   */
  // for (overlaps <- overlapStatistics.values; overlap: GenSeq[ObjectLifetime] <- overlaps) {
  //   val sizeSet = overlap.map(_.measuredSizeInBytes).toSet
  //     if (sizeSet.size > 1)
  //       println(sizeSet)
  // }

  assert (replacements.size == overlapStatistics.values.flatten.size)

  val operlapsMin = overlapStatistics.values.flatten.flatten
  val universeMin = (universe union replacements.toSet) diff operlapsMin.toSet

  {
    val universeMinList = time("set to list") { universeMin.toVector }
    val ctorMinSorted: Vector[ObjectLifetime] = time("ctorSorted") { universeMinList sortWith (_.ctorTime < _.ctorTime) }
    val dtorMinSorted: Vector[ObjectLifetime] = time("dtorSorted") { universeMinList   filter (_.dtorTime.isDefined) sortWith (_.dtorTime.get < _.dtorTime.get) }

    time ("Project Heap Size [min]") {
      projectProperty("heapSizes-min.dat", ctorMinSorted, dtorMinSorted, timestampRange)(_.measuredSizeInBytes)
    }

    time ("Project Object Count [min]") {
      projectProperty("objectCount-min.dat", ctorMinSorted, dtorMinSorted, timestampRange)(_ => 1)
    }
  }
}

object TracrUtil {

  def writePropertyHistory(filename: String, heapHistory: GenIterable[(Long, BigInt)]) {
    val outputFile = new File(filename)
    val writer = new BufferedWriter(new FileWriter(outputFile))

    heapHistory map {
      case (timestamp, property) => writer.write(s"$timestamp $property"); writer.newLine
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
          // end run
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

    // end run (if unfinished)
    val currentRun = runBuilder.result
    if (currentRun.size > 1) resBuilder += currentRun

    resBuilder.result
  }

  def projectProperty(filename: String, ctorSorted: Vector[ObjectLifetime], dtorSorted: Vector[ObjectLifetime], timestampRange: NumericRange[Long])
                     (accumulatorProperty: ObjectLifetime => Long) {
    var ctorIdx = 0;
    var dtorIdx = 0;

    var ctorSum = BigInt(0);
    var dtorSum = BigInt(0);

    time("Iterate and project") {
      val outputFile = new File(filename)
      val writer = new BufferedWriter(new FileWriter(outputFile))

      for (timestamp <- timestampRange) {
        while (ctorIdx < ctorSorted.length && ctorSorted(ctorIdx).ctorTime <= timestamp) {
          ctorSum += accumulatorProperty(ctorSorted(ctorIdx))
          ctorIdx += 1
        }
//        println(s"ctorSum: $ctorSum")

        while (dtorIdx < dtorSorted.length && dtorSorted(dtorIdx).dtorTime.get <= timestamp) {
          // previously filtered, thus dtor is always defined
          dtorSum += accumulatorProperty(dtorSorted(dtorIdx))
          dtorIdx += 1
        }
//        println(s"dtorSum: $dtorSum")

//        println(s"deltSum: ${ctorSum - dtorSum}")
        writer.write(s"$timestamp ${ctorSum - dtorSum}"); writer.newLine
      }
      writer.flush
      writer.close
    }
  }

  /*
   * Creates a Map from the selctor property to the accumulated size of traces previoulsy processed.
   */
  def accumulatedProperty(selector: ObjectLifetime => Long, accumulatorProperty: ObjectLifetime => Long)
                         (traceSeq: GenSeq[ObjectLifetime]): Map[Long, BigInt] = {
    var max = BigInt(0);
    val builder = Map.newBuilder[Long, BigInt]

    for (trace <- traceSeq) {
      max += accumulatorProperty(trace)
      builder += selector(trace) -> max
    }

    builder.result
  }

  /*
   * Timing a block of code.
   * Originates from: http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
   */
  def time[R](blockCaption: String)(block: => R): R = {
    println(blockCaption)
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000000 + "s")
    result
  }

}
