import java.io._
import java.nio.file.{StandardOpenOption, Paths, Files}
import java.nio.{ByteOrder, ByteBuffer}
import java.util.zip.GZIPInputStream
import org.eclipse.imp.pdb.facts.tracking.TrackingProtocolBuffers
import scala.collection.{GenIterable, GenSeq, GenMap, GenSet}
import scala.collection.immutable.{SortedMap, NumericRange}
import scala._
import scala.Some

case class ObjectLifetime(tag: Option[Long], digest: Option[String], classname: Option[String], ctorTime: Long, dtorTime: Option[Long], measuredSizeInBytes: Long, recursiveReferenceEqualitiesEstimate: Int, hashTableOverhead: Long, isRedundant: Boolean, oepDigest: Option[String], oepObjectGraph: Option[String], oepIsSuspectForOrderingProblems: Option[Boolean]) {
  require(ctorTime >= 0)
  require(!dtorTime.isDefined || dtorTime.get >= 0)
  require(measuredSizeInBytes >= 0)
}

case class EqualsCall(tag1: Long, tag2: Long, result: Boolean, deepCount: Int, deepTime: Long, deepReferenceEqualityCount: Int, timestamp: Long, isHashLookup: Boolean, isStructuralEquality: Boolean)
case class TagInfo(digest: String, classname: String)

object Tracr extends App {

  val startTime = System.nanoTime();

  import TracrUtil._

  val isSharingEnabled: Boolean = args.contains("sharingEnabled")
  val path = if (args.head.endsWith("/")) args.head else args.head + "/"

  require (Files.exists(Paths.get(path)))

  val BUF_SIZE = 16777216

//  val tagMap: GenMap[Long, TagInfo] = time("Deserialize tag map from Google Protocol Buffers") {
//    if (isSharingEnabled) {
//      Map.empty[Long, TagInfo]
//    } else {
//      /*
//       * Read-in equals relation.
//       */
//      val tagMapBuilder = Map.newBuilder[Long, TagInfo]
//
//      // currently tags are only used by overlap statistics
//      val protoInputStream = new BufferedInputStream(new GZIPInputStream(new FileInputStream(path + "_tag_map.bin.gz")), BUF_SIZE)
//      try {
//        var continue = true
//        while (continue) {
//          val proto = TrackingProtocolBuffers.TagMap.parseDelimitedFrom(protoInputStream)
//
//          if (proto != null && proto.isInitialized) {
//            tagMapBuilder += proto.getTag -> TagInfo(proto.getDigest.intern(), proto.getClassname.intern())
//          } else {
//            continue = false
//          }
//        }
//      } catch {
//        case e1: Exception => { throw new RuntimeException(e1) }
//      }
//
//      tagMapBuilder.result
//    }
//  }

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
      val protoInputStream = new BufferedInputStream(new GZIPInputStream(new FileInputStream(path + "_allocation_relation.bin.gz")), BUF_SIZE)
      try {
        var continue = true
        while (continue) {
          val protoObjectLifetime = TrackingProtocolBuffers.ObjectLifetime.parseDelimitedFrom(protoInputStream)

          if (protoObjectLifetime != null && protoObjectLifetime.isInitialized) {
            val tag = protoObjectLifetime.getTag
            val digest = if (protoObjectLifetime.hasDigest) Some(protoObjectLifetime.getDigest.intern()) else None
//            val digest = tagMap.get(tag) match {
//              case None => None
//              case Some(tagInfo) => Some(tagInfo.digest)
//            }
            val classname = if (protoObjectLifetime.hasClassname) Some(protoObjectLifetime.getClassname.intern()) else None
            val ctorTime = protoObjectLifetime.getCtorTime
            val dtorTime = objectFreeMap.get(tag)
            val measuredSizeInBytes = protoObjectLifetime.getMeasuredSizeInBytes
            val recursiveReferenceEqualitiesEstimate = protoObjectLifetime.getRecursiveReferenceEqualitiesEstimate
            val hashTableOverhead = protoObjectLifetime.getHashTableOverhead
            val isRedundant = protoObjectLifetime.getIsRedundant

            val oepDigest = if (protoObjectLifetime.hasOepDigest) Some(protoObjectLifetime.getOepDigest.intern()) else None
            val oepObjectGraph = if (protoObjectLifetime.hasOepObjectGraph) Some(protoObjectLifetime.getOepObjectGraph) else None
            val oepIsSuspectForOrderingProblems = if (protoObjectLifetime.hasOepIsSuspectForOrderingProblems) Some(protoObjectLifetime.getOepIsSuspectForOrderingProblems) else None

            universeBuilder += ObjectLifetime(Some(tag), digest, classname, ctorTime, dtorTime, measuredSizeInBytes, recursiveReferenceEqualitiesEstimate, hashTableOverhead, isRedundant, oepDigest, oepObjectGraph, oepIsSuspectForOrderingProblems)
          } else {
            continue = false
          }
        }
      } catch {
        case e1: Exception => { throw new RuntimeException(e1) }
      }
    }

    universeBuilder.result
  }

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
  val stepCountCopy = stepCount
  var stepSize = math.max(1, math.ceil(timestampRange.size / stepCountCopy.toFloat).toLong)

  {
    val universeList = time("set to list") { sortedUniverse.toVector }
    val ctorSorted: Vector[ObjectLifetime] = time("ctorSorted") { universeList sortWith (_.ctorTime < _.ctorTime) }
    val dtorSorted: Vector[ObjectLifetime] = time("dtorSorted") { universeList   filter (_.dtorTime.isDefined) sortWith (_.dtorTime.get < _.dtorTime.get) }

    time ("Project Heap Size") {
      if (isSharingEnabled) {

        projectProperty("heapSizes-sha.dat", ctorSorted, dtorSorted, timestampRange, stepSize)(_.measuredSizeInBytes)

        val ctorMinSorted = ctorSorted.filter(!_.isRedundant)
        val dtorMinSorted = dtorSorted.filter(!_.isRedundant)

        projectProperty("heapSizes-sha-min.dat", ctorMinSorted, dtorMinSorted, timestampRange, stepSize)(_.measuredSizeInBytes)
      } else {
        val filename = "heapSizes-nom.dat"

        projectProperty(filename, ctorSorted, dtorSorted, timestampRange, stepSize)(_.measuredSizeInBytes)
      }
    }

    time ("Project Object Size") {
      val filename = if (isSharingEnabled) "objectCount-sha.dat" else "objectCount-nom.dat"
      projectProperty(filename, ctorSorted, dtorSorted, timestampRange, stepSize)(_ => 1)
    }

    /*
     * TODO: current implementation only works, if hash size is calculated after each object allocation.
     * This is very expensive.
     */
    if (isSharingEnabled) {
      time ("Sample HashTable Size") {
        sampleProperty("hashTableSize-sha.dat", ctorSorted, timestampRange, stepSize)(_.hashTableOverhead)
      }
    }

    /*
     * Remove redundant, short-living, objects.
     */
    if (isSharingEnabled) {
      time ("Project Object Size [sha-min]") {
        projectProperty("objectCount-sha-min.dat", ctorSorted.filter(!_.isRedundant), dtorSorted.filter(!_.isRedundant), timestampRange, stepSize)(_ => 1)
      }

    }
  }

  if (!isSharingEnabled) {
    val overlapStatistics: GenMap[String, GenSeq[GenSeq[ObjectLifetime]]] = time("Calculate overlap statistics") {
      valueOverlapStatistics(sortedUniverse)
    }

    //  for ((digest, runs) <- overlapStatistics filter (!_._2.isEmpty)) {
    //    println(s"${runs.length} runs for $digest")
    //
    //    for (run <- runs) {
    //      println("A RUN")
    //      run.foreach(println)
    //
    ////      val sizes = run.map(_.measuredSizeInBytes).toSet
    ////      if (sizes.size > 1) {
    ////        println(s"${run.head.digest}, $sizes")
    ////      }
    //    }
    //  }


    time("Calculate Object Redundancy Profiling (ORP) over Object Equality Profiling (OEP)") {
      // optinally filter first: sortedUniverse filter { _.oepIsSuspectForOrderingProblems getOrElse false }
      val recordsGroupedByDigest: GenMap[String, GenSeq[ObjectLifetime]] = sortedUniverse groupBy (_.digest.get)
      val distinctOepDigestsByDigest: GenMap[String, Int] = recordsGroupedByDigest mapValues { _.map(_.oepDigest).toSet.size }

      val frequencyByDistinctOepDigests: GenMap[Int, Int] = distinctOepDigestsByDigest.groupBy(_._2).mapValues(_.size)

      val numberOfDataPoints = frequencyByDistinctOepDigests.values.sum
      val formatFrequency = (freq: Int, numberOfDataPoints: Int) => { "%.4f" format (100 * freq.toDouble / numberOfDataPoints) }

      val relativeFrequencyByDistinctOepDigests = frequencyByDistinctOepDigests mapValues { freq => formatFrequency(freq, numberOfDataPoints) }

      // print affected classes
      println("Affected classes:")
      (recordsGroupedByDigest mapValues { _.toSet map { (record: ObjectLifetime) => record.classname.getOrElse("Not Available") } }).values mkString "\n"

      // printing histogram
      println(relativeFrequencyByDistinctOepDigests .toList.sortBy(_._1))
      println()

      // serialize histogram
      val outputFile = new File("orpVsOep.dat")
      val writer = new BufferedWriter(new FileWriter(outputFile))

      for ((distinctOepDigests, frequency) <- frequencyByDistinctOepDigests.toList.sortBy(_._1)) {
        val relativeFrequencyFormatted = formatFrequency(frequency, numberOfDataPoints)

        writer write s"$distinctOepDigests,$frequency,$relativeFrequencyFormatted"
        writer.newLine
      }

      writer.flush
      writer.close

      println()
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
        recursiveReferenceEqualitiesEstimate = overlap.head.recursiveReferenceEqualitiesEstimate
      } yield ObjectLifetime(None, digest, None, ctorTime, dtorTime, size, recursiveReferenceEqualitiesEstimate, 0, false, None, None, None)
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

    val overlapsMin = overlapStatistics.values.flatten.flatten
    val universeMin = (sortedUniverse.toSet union replacements.toSet) diff overlapsMin.toSet

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

      // TODO: only execute when NOT running in shared mode
      time ("Project equals/isEqual calls [internal]") {
        // val tmp = overlapStatistics.values.flatten.map(_.tail)
        val callWithCacheHit = overlapStatistics.values.flatten.map(_.tail).flatten
          .toVector sortWith (_.ctorTime < _.ctorTime)

        projectExpectedEqualsCall("equalCalls-est.dat", callWithCacheHit, timestampRange, stepSize)
        println(s"Calls with CacheHit: ${callWithCacheHit.size}")
      }
    }
  }


  val equalsRelation: GenSeq[EqualsCall] = time("Deserialize equals relation from Google Protocol Buffers") {
    /*
     * Read-in equals relation.
     */
    val equalsRelationBuilder = Vector.newBuilder[EqualsCall]

    {
      val protoInputStream = new BufferedInputStream(new GZIPInputStream(new FileInputStream(path + "_equals_relation.bin.gz")), BUF_SIZE)
      try {
        var continue = true
        while (continue) {
          val protoEqualsCall = TrackingProtocolBuffers.EqualsRelation.parseDelimitedFrom(protoInputStream)

          if (protoEqualsCall != null && protoEqualsCall.isInitialized) {
            val eq = EqualsCall(
              protoEqualsCall.getTag1,
              protoEqualsCall.getTag2,
              protoEqualsCall.getResult,
              protoEqualsCall.getDeepCount,
              protoEqualsCall.getDeepTime,
              protoEqualsCall.getDeepReferenceEqualityCount,
              protoEqualsCall.getTimestamp,
              protoEqualsCall.getIsHashLookup,
              protoEqualsCall.getIsStructuralEquality
            )

            // Filter out collissions because of the sheer amount
            if (!eq.isHashLookup || eq.isHashLookup && eq.result) equalsRelationBuilder += eq
          } else {
            continue = false
          }
        }
      } catch {
        case e1: Exception => { throw new RuntimeException(e1) }
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

  // object EqualsRelationFilter extends Enumeration {
  //   type EqualsRelationFilter = Value
  //   val EXT, INT, INT_TRUE = Value // etc.
  // }
  // import EqualsRelationFilter._
  // http://stackoverflow.com/questions/11067396/understanding-scala-enumerations

  if (isSharingEnabled) {
    time ("Project equals/isEqual calls [external]") {
      projectEqualsProperty("equalCalls-sha-ext.dat", equalsRelation filter { eq => (!eq.isHashLookup) }, timestampRange, stepSize)
    }

    // TODO: only execute when running in shared mode
    time ("Project equals/isEqual calls [internal]") {
      projectEqualsProperty("equalCalls-sha-int.dat", equalsRelation filter { eq => (eq.isHashLookup && eq.result) }, timestampRange, stepSize) // && eq.result && eq.result && eq.tag1 != eq.tag2
    }
  } else {
    time ("Project equals/isEqual calls [external]") {
      projectEqualsProperty("equalCalls-nom.dat", equalsRelation, timestampRange, stepSize)
    }
  }

  def projectEqualsProperty(filename: String, sortedRelation: GenSeq[EqualsCall], timestampRange: NumericRange[Long], stepSize: Long) {
    // TODO: Operate on indices instead of summarizing first (created to much memory overhead)
    // TODO: Use EqualsRelationFilter to apply filtering on the go instead of filtering upfront.

    /*
     * The following shapes of equals/== are expected:
     * * recursive(Logical)Equals == 0 && recursiveReferenceEqualities == 1
     * * recursive(Logical)Equals >  0 && recursiveReferenceEqualities >= 0
     */
    val summarized = sortedRelation.groupBy(_.timestamp).mapValues {
      case callsByTimestamp => {
        val (structuralEquals, logicalEquals) = callsByTimestamp.partition(_.isStructuralEquality)

        val sumRootEquals                   = structuralEquals.filter(_.deepCount != 0).size
        val sumRecursiveEquals              = structuralEquals.map(_.deepCount).sum

        val sumRootLogicalEquals            = logicalEquals.filter(_.deepCount != 0).size
        val sumRecursiveLogicalEquals       = logicalEquals.map(_.deepCount).sum

        val sumRootReferenceEqualities      = structuralEquals.filter(_.deepCount == 0).size + logicalEquals.filter(_.deepCount == 0).size
        val sumRecursiveReferenceEqualities = structuralEquals.map(_.deepReferenceEqualityCount).sum + logicalEquals.map(_.deepReferenceEqualityCount).sum

        (sumRootEquals, sumRecursiveEquals, sumRootReferenceEqualities, sumRecursiveReferenceEqualities, sumRootLogicalEquals, sumRecursiveLogicalEquals)
      }
    }

    val outputFile = new File(filename)
    val writer = new BufferedWriter(new FileWriter(outputFile))

    var lastTimestamp = 0L;
    var timestampCntr = 0L
    var stepCntr = 0L;

    var (sumRootEquals, sumRecursiveEquals) = (BigInt(0), BigInt(0));
    var (sumRootReferenceEqualities, sumRecursiveReferenceEqualities) = (BigInt(0), BigInt(0));
    var (sumRootLogicalEquals, sumRecursiveLogicalEquals) = (BigInt(0), BigInt(0));

    for (timestamp <- timestampRange) {
      lastTimestamp = timestamp
      timestampCntr += 1

      summarized get timestamp match {
        case Some((tmpRootEquals, tmpRecursiveEquals, tmpRootReferenceEqualities, tmpRecursiveReferenceEqualities, tmpRootLogicalEquals, tmpRecursiveLogicalEquals)) => {
          sumRootEquals += tmpRootEquals
          sumRecursiveEquals += tmpRecursiveEquals

          sumRootReferenceEqualities += tmpRootReferenceEqualities
          sumRecursiveReferenceEqualities += tmpRecursiveReferenceEqualities

          sumRootLogicalEquals += tmpRootLogicalEquals
          sumRecursiveLogicalEquals += tmpRecursiveLogicalEquals
        }
        case _ => ()
      }

      if (timestampCntr % stepSize == 0) {
        writer.write(s"$timestamp $sumRootEquals $sumRecursiveEquals $sumRootReferenceEqualities $sumRecursiveReferenceEqualities $sumRootLogicalEquals $sumRecursiveLogicalEquals")
        writer.newLine

        stepCntr += 1

        // reset sums
        sumRootEquals = BigInt(0)
        sumRecursiveEquals = BigInt(0)
        sumRootReferenceEqualities = BigInt(0)
        sumRecursiveReferenceEqualities = BigInt(0)
        sumRootLogicalEquals = BigInt(0)
        sumRecursiveLogicalEquals = BigInt(0)
      }
    }

    if (timestampCntr % stepSize != 0) {
      writer.write(s"$lastTimestamp $sumRootEquals $sumRecursiveEquals $sumRootReferenceEqualities $sumRecursiveReferenceEqualities $sumRootLogicalEquals $sumRecursiveLogicalEquals")
      writer.newLine
    }

    writer.flush
    writer.close
  }

  /*
   * Note: Does not print begin of data, first print is first accumulation.
   */
  def projectExpectedEqualsCall(filename: String, ctorSorted: Vector[ObjectLifetime], timestampRange: NumericRange[Long], stepSize: Long) {
    var sumRootEquals = BigInt(0)
    var sumRecursiveReferenceEqualities = BigInt(0)

    var lastTimestamp = 0L
    var timestampCntr = 0L
    var stepCntr = 0L

    var ctorIdx = 0

    time("Iterate and project") {
      val outputFile = new File(filename)
      val writer = new BufferedWriter(new FileWriter(outputFile))

      for (timestamp <- timestampRange) {
        lastTimestamp = timestamp
        timestampCntr += 1

        while (ctorIdx < ctorSorted.length && ctorSorted(ctorIdx).ctorTime <= timestamp) {
          sumRootEquals += 1
          sumRecursiveReferenceEqualities += ctorSorted(ctorIdx).recursiveReferenceEqualitiesEstimate

          ctorIdx += 1
        }

        if (timestampCntr % stepSize == 0) {
          writer.write(s"$timestamp ${sumRootEquals} ${sumRecursiveReferenceEqualities}")
          writer.newLine

          stepCntr += 1

          // reset sums
          sumRootEquals = BigInt(0)
          sumRecursiveReferenceEqualities = BigInt(0)
        }
      }

      if (timestampCntr % stepSize != 0) {
        writer.write(s"$lastTimestamp ${sumRootEquals} ${sumRecursiveReferenceEqualities}")
        writer.newLine
      }

      writer.flush
      writer.close
    }
  }

  val endTime = System.nanoTime();
  val outputString = s"${endTime - startTime}"

  try {
    java.nio.file.Files.write(Paths.get("target/_timeTracr.txt"),
      outputString.getBytes("UTF-8"), StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING);
  } catch {
    case e1: IOException => throw new RuntimeException(e1)
  }
}

object TracrUtil {

  val stepCount = (300 * 4 * 2.5).toLong

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
    val identitiesByValue: GenMap[String, GenSeq[ObjectLifetime]] = sortedUniverse groupBy (_.digest.get)
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
   * Note: Does not print begin of data, first print is first accumulation.
   */
  def projectProperty(filename: String, ctorSorted: Vector[ObjectLifetime], dtorSorted: Vector[ObjectLifetime], timestampRange: NumericRange[Long], stepSize: Long)
                     (accumulatorProperty: ObjectLifetime => Long) {
    var ctorIdx = 0;
    var dtorIdx = 0;

    var ctorSum = BigInt(0);
    var dtorSum = BigInt(0);

    var lastTimestamp = 0L;
    var timestampCntr = 0L
    var stepCntr = 0L;

    time("Iterate and project") {
      val outputFile = new File(filename)
      val writer = new BufferedWriter(new FileWriter(outputFile))

      for (timestamp <- timestampRange) {
        lastTimestamp = timestamp
        timestampCntr += 1

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

        if (timestampCntr % stepSize == 0) {
          writer.write(s"$timestamp ${ctorSum - dtorSum}"); writer.newLine
          stepCntr += 1
        }
      }

      if (timestampCntr % stepSize != 0) {
        writer.write(s"$lastTimestamp ${ctorSum - dtorSum}"); writer.newLine
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
   * TODO: current implementation only works, if hash size is calculated after each object allocation.
   * This is very expensive.
   */
  def sampleProperty(filename: String, universeSorted: Vector[ObjectLifetime], timestampRange: NumericRange[Long], stepSize: Long)
                     (sampleProperty: ObjectLifetime => Long) {
    var idx = 0;

    var lastValue = 0L;
    var lastTimestamp = 0L;
    var timestampCntr = 0L
    var stepCntr = 0L;

    time("Iterate and sample") {
      val outputFile = new File(filename)
      val writer = new BufferedWriter(new FileWriter(outputFile))

      for (timestamp <- timestampRange) {
        lastTimestamp = timestamp
        timestampCntr += 1


        while (idx < universeSorted.length && universeSorted(idx).ctorTime <= timestamp) {
          lastValue = sampleProperty(universeSorted(idx))
          idx += 1
        }

        if (timestampCntr % stepSize == 0) {
          writer.write(s"$timestamp ${lastValue}"); writer.newLine
          stepCntr += 1
        }
      }

      if (timestampCntr % stepSize != 0) {
        writer.write(s"$lastTimestamp ${lastValue}"); writer.newLine
      }

      writer.flush
      writer.close
    }
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
