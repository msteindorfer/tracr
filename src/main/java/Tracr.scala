import java.io._
import java.nio.{ByteOrder, ByteBuffer}
import org.eclipse.imp.pdb.facts.tracking.TrackingProtocolBuffers
import scala.collection.{GenIterable, GenSeq, GenMap, GenSet}
import scala.collection.immutable.NumericRange
import scala.Some

case class ObjectLifetime(tag: Option[Long], digest: String, ctorTime: Long, dtorTime: Option[Long], measuredSizeInBytes: Long) {
  require(ctorTime >= 0)
  require(!dtorTime.isDefined || dtorTime.get >= 0)
  require(measuredSizeInBytes >= 0)
}

case class EqualsCall(tag1: Long, tag2: Long, result: Boolean, deepCount: Int, deepTime: Long, timestamp: Long)
case class TagInfo(digest: String, classname: String)

object Tracr extends App {

  import TracrUtil._

  //  val filename = "/Users/Michael/Dropbox/Research/ObjectLifetime/ActualRun/target/universe"

  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values.benchmarks/target/universe-SingleElementSetJUnitBenchmark"
  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values/target/universe"
  //  val filename = "/Users/Michael/Development/rascal-devel/pdb.values.benchmarks/target/universe"
  val path = "/Users/Michael/Development/rascal-devel/pdb.values.benchmarks/target/"

  val tagMap: GenMap[Long, TagInfo] = time("Deserialize tag map from Google Protocol Buffers") {
    /*
     * Read-in equals relation.
     */
    val tagMapBuilder = Map.newBuilder[Long, TagInfo]

    {
      val protoInputStream = new FileInputStream(path + "_tag_map.bin")
      try {
        while (true) {
          val proto = TrackingProtocolBuffers.TagMap.parseDelimitedFrom(protoInputStream)

          tagMapBuilder += proto.getTag -> TagInfo(proto.getDigest.intern(), proto.getClassname.intern())
        }
      } catch {
        case _: Exception => {}
      }
    }

    tagMapBuilder.result
  }

  /*
   * Deserialize universe from Google Protocol Buffers
   */
  val sortedUniverse: GenSeq[ObjectLifetime] = time("Deserialize universe from Google Protocol Buffers") {
    /*
     * Deserialize GC timestamps. File contains tuples [Long, Long] in big endian encoding.
     */
    val mapWriter = Map.newBuilder[Long, Long]

    {
      val stream = new FileInputStream(path + "_object_free_relation.bin")
      val channel = stream.getChannel

      val buffer = ByteBuffer.allocate(2 * 8 * 1024);
      buffer.order(ByteOrder.LITTLE_ENDIAN);

      while (channel.read(buffer) >= 0) {
        buffer.flip

        while (buffer.hasRemaining) {
          val tag = buffer.getLong
          val timestamp = buffer.getLong

          mapWriter += (tag -> timestamp)
        }

        buffer.compact
      }
    }

    val objectFreeMap = mapWriter.result

    /*
     * Read-in allocation records.
     */
    val universeBuilder = Vector.newBuilder[ObjectLifetime]

    {
      val protoInputStream = new FileInputStream(path + "_allocation_relation.bin")
      try {
        while (true) {
          val protoObjectLifetime = TrackingProtocolBuffers.ObjectLifetime.parseDelimitedFrom(protoInputStream)

          val tag = protoObjectLifetime.getTag
          val digest = tagMap.get(tag).get.digest
          val ctorTime = protoObjectLifetime.getCtorTime
          val dtorTime = objectFreeMap.get(tag)
          val measuredSizeInBytes = protoObjectLifetime.getMeasuredSizeInBytes

          universeBuilder += ObjectLifetime(Some(tag), digest, ctorTime, dtorTime, measuredSizeInBytes)
        }
      } catch {
        case _: Exception => {}
      }
    }

    universeBuilder.result
  }

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
  var tsMin: Long = sortedUniverse.head.ctorTime
  var tsMax: Long = sortedUniverse.head.ctorTime

  time ("Calculate min / max timestamp") {
    for (olt <- sortedUniverse) {
      tsMin = math.min(tsMin, olt.ctorTime)

      tsMax = math.max(tsMax, olt.ctorTime)
      if (olt.dtorTime.isDefined)
        tsMax = math.max(tsMax, olt.dtorTime.get)
    }
  }

  val timestampRange = tsMin to tsMax
  var stepSize = math.max(1, (timestampRange.size / (300 * 4 * 2.5)).toLong)

  {
    val universeList = time("set to list") { sortedUniverse.toVector }
    val ctorSorted: Vector[ObjectLifetime] = time("ctorSorted") { universeList sortWith (_.ctorTime < _.ctorTime) }
    val dtorSorted: Vector[ObjectLifetime] = time("dtorSorted") { universeList   filter (_.dtorTime.isDefined) sortWith (_.dtorTime.get < _.dtorTime.get) }

    time ("Project Heap Size") {
      projectProperty("heapSizes-nom.dat", ctorSorted, dtorSorted, timestampRange, stepSize)(_.measuredSizeInBytes)
    }

    time ("Project Object Size") {
      projectProperty("objectCount-nom.dat", ctorSorted, dtorSorted, timestampRange, stepSize)(_ => 1)
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
    } yield ObjectLifetime(None, digest, ctorTime, dtorTime, size)
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
  val universeMin = (sortedUniverse.toSet union replacements.toSet) diff operlapsMin.toSet

  {
    val universeMinList = time("set to list") { universeMin.toVector }
    val ctorMinSorted: Vector[ObjectLifetime] = time("ctorSorted") { universeMinList sortWith (_.ctorTime < _.ctorTime) }
    val dtorMinSorted: Vector[ObjectLifetime] = time("dtorSorted") { universeMinList   filter (_.dtorTime.isDefined) sortWith (_.dtorTime.get < _.dtorTime.get) }

    time ("Project Heap Size [min]") {
      projectProperty("heapSizes-min.dat", ctorMinSorted, dtorMinSorted, timestampRange, stepSize)(_.measuredSizeInBytes)
    }

    time ("Project Object Count [min]") {
      projectProperty("objectCount-min.dat", ctorMinSorted, dtorMinSorted, timestampRange, stepSize)(_ => 1)
    }
  }

  val equalsRelation: GenSet[EqualsCall] = time("Deserialize equals relation from Google Protocol Buffers") {
    /*
     * Read-in equals relation.
     */
    val equalsRelationBuilder = Set.newBuilder[EqualsCall]

    {
      val protoInputStream = new FileInputStream(path + "_equals_relation.bin")
      try {
        while (true) {
          val protoEqualsCall = TrackingProtocolBuffers.EqualsRelation.parseDelimitedFrom(protoInputStream)

          equalsRelationBuilder += EqualsCall(
            protoEqualsCall.getTag1,
            protoEqualsCall.getTag2,
            protoEqualsCall.getResult,
            protoEqualsCall.getDeepCount,
            protoEqualsCall.getDeepTime,
            protoEqualsCall.getTimestamp
          )
        }
      } catch {
        case _: Exception => {}
      }
    }

    equalsRelationBuilder.result
  }

//    time("Create equals/isEqual statistics") {
//      val flatCount = equalsRelation.toList.map(_.deepCount).sum
//      val deepCount = equalsRelation.size
//
//  //    println(equalsRelation)
//  //    println(equalsRelation.toList.map(_.deepCount))
//
//      println(s"$flatCount equals/isEqual calls reducible to $deepCount")
//    }

//    time ("Project equals/isEqual calls") {
//
//      val summarized = equalsRelation.groupBy(_.timestamp).mapValues {
//        case callsByTimestamp => {
//          val sumCount = callsByTimestamp.map(_.deepCount).sum
//          val sumTime  = callsByTimestamp.map(_.deepTime ).sum
//          (sumCount, sumTime)
//        }
//      }
//
//      val outputFile = new File("equalCalls.dat")
//      val writer = new BufferedWriter(new FileWriter(outputFile))
//
//      for ((key, value) <- summarized) {
//        writer.write(s"$key ${value._1} ${value._2}"); writer.newLine
//      }
//
//      writer.flush
//      writer.close
//
//    }
//
//    time ("Project equals/isEqual calls [min]") {
////      projectEqualsProperty("equalCall-min.dat", equalsRelation, timestampRange)
//    }

//    def projectEqualsProperty(filename: String, sortedRelation: Vector[EqualsCall], timestampRange: NumericRange[Long])
//                             (accumulatorProperty: EqualsCall => Long) {
//      var idx = 0;
//      var sum = BigInt(0);
//
//      time("Iterate and project") {
//        val outputFile = new File(filename)
//        val writer = new BufferedWriter(new FileWriter(outputFile))
//
//        for (call <- sortedRelation) {
//          sum = 0
//
//          while (idx < sortedRelation.length && sortedRelation(idx).timestamp <= ) {
//            sum += accumulatorProperty(ctorSorted(idx))
//            idx += 1
//          }
//          //        println(s"ctorSum: $ctorSum")
//
//          writer.write(s"$timestamp ${sum - dtorSum}"); writer.newLine
//        }
//        writer.flush
//        writer.close
//      }
//    }

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

  /*
   * Note: Does not print begin and end of data up to step size.
   */
  def projectProperty(filename: String, ctorSorted: Vector[ObjectLifetime], dtorSorted: Vector[ObjectLifetime], timestampRange: NumericRange[Long], stepSize: Long)
                     (accumulatorProperty: ObjectLifetime => Long) {
    var ctorIdx = 0;
    var dtorIdx = 0;

    var ctorSum = BigInt(0);
    var dtorSum = BigInt(0);

    var lastTimestamp = 0L;

    var stepCntr = 0L;

    time("Iterate and project") {
      val outputFile = new File(filename)
      val writer = new BufferedWriter(new FileWriter(outputFile))

      for (timestamp <- timestampRange) {
        lastTimestamp = timestamp
        stepCntr += 1

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

        if (stepCntr % stepSize == 0) {
          writer.write(s"$timestamp ${ctorSum - dtorSum}"); writer.newLine
        }
      }
      writer.flush
      writer.close
    }
  }

  /*
   * Creates a Map from the selctor property to the accumulated size of traces previoulsy processed.
   */
  def accumulatedProperty[T](selector: T => Long, accumulatorProperty: T => Long)
                         (traceSeq: GenSeq[T]): Map[Long, BigInt] = {
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
