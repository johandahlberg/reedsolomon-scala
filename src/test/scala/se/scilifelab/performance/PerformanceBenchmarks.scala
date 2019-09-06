package se.scilifelab.reedsolomon

import org.scalameter.api._
import se.scilifelab.TestUtils

object ReedSolomonBenchmark extends Bench.LocalTime {

  val coder = ReedSolomonCoder(255, 223)

  val sizes =
    Gen.range("size")(1, 101, 10)

  val sequences = for { size <- sizes } yield
    (0 until size).map(_ % 256).toArray
  val encodedSequences = for { s <- sequences } yield coder.encode(s)
  val corruptedSequences = for { s <- sequences } yield
    TestUtils.corruptMessage(s, 1)

  performance of "Coder" in {
    measure method "encode" in {
      using(sequences) in { v =>
        coder.encode(v)
      }
    }
    measure method "decode" in {
      using(corruptedSequences) in { v =>
        coder.decode(v)
      }
    }
  }
}
