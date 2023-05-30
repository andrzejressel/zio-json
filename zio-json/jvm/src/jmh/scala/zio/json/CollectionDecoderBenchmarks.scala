package zio.json

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import zio.Chunk
import zio.prelude.Validation

import scala.collection.immutable

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1)
class CollectionDecoderBenchmarks {
  private[this] var encodedArray: String  = null
  private[this] var encodedObject: String = null

  @Setup
  def setup(): Unit = {
    encodedArray = (for (i <- 1 to 1000) yield s"test_$i").toVector.toJson
    encodedObject = (for (i <- 1 to 1000) yield s"k_$i" -> "test").toMap.toJson
  }

  @Benchmark
  def decodeChunk: Validation[String, Chunk[String]] =
    encodedArray.fromJsonValidation[Chunk[String]]

  @Benchmark
  def decodeList: Validation[String, List[String]] =
    encodedArray.fromJsonValidation[immutable.List[String]]

  @Benchmark
  def decodeMap: Validation[String, Map[String, String]] =
    encodedObject.fromJsonValidation[immutable.Map[String, String]]

  @Benchmark
  def decodeSet: Validation[String, Set[String]] =
    encodedArray.fromJsonValidation[immutable.Set[String]]

  @Benchmark
  def decodeSeq: Validation[String, immutable.Seq[String]] =
    encodedArray.fromJsonValidation[immutable.Seq[String]]

  @Benchmark
  def decodeSortedSet: Validation[String, Set[String]] =
    encodedArray.fromJsonValidation[immutable.SortedSet[String]]

  @Benchmark
  def decodeSortedMap: Validation[String, collection.SortedMap[String, String]] =
    encodedObject.fromJsonValidation[collection.SortedMap[String, String]]

  @Benchmark
  def decodeVector: Validation[String, Vector[String]] =
    encodedArray.fromJsonValidation[immutable.Vector[String]]
}
