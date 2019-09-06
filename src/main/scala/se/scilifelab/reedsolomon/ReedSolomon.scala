package se.scilifelab.reedsolomon

import scala.math
import java.io.StringWriter
import scala.annotation.meta.field

/**
  * A ReedSolomonCoder object can be used to encode a message M
  * so that it can be reliably decoded, despite errors.
  *
  * It's correction power fullfills the following inequality:
  * 2*nbr_erasures + nbr_of_errors <= (n-k)
  *
  * This means that the encoder can decode twice as many erasures
  * as errors
  *
  * Where e is the number of errors, and v is the nu
  * @param n length of the code word. Must be < 2^cExp
  * @param k length of the message. k must be < n
  * @param generator Galois Field parameter
  * @param prim Galois Field parameter
  * @param fcr Galois Field parameter
  * @param cExp Range of Galois Field, limits symbols and length
  * of message+correction bits
  */
case class ReedSolomonCoder(
    n: Int,
    k: Int,
    generator: Int = 3,
    prim: Int = 0x11b,
    fcr: Int = 1,
    cExp: Int = 8
) {

  val gf2Charac = (math.pow(2, cExp) - 1).toInt

  require(n < 256, "n must be smaller than 256")
  require(n > 0 || k > 0, "n and k must be positive")
  require(n <= gf2Charac, s"n must be at most $gf2Charac")
  require(!(k > n), "n must be greater than message length k")

  val field = GaloisField(generator = generator, prim = prim, gf2CExp = cExp)

  val constPoly = Polynomial {
    Range(this.gf2Charac, -1, -1)
      .map(x => field.GF2Int(generator).pow(x + fcr))
      .toArray
  }

  val g: Map[Int, Polynomial[field.GF2Int]] = {

    val initialMap = Map(
      0 -> Polynomial(Array(field.GF2Int(1))),
      1 -> Polynomial(Array(field.GF2Int(1)))
    )
    val initialPolynoial = Polynomial(Array(field.GF2Int(1)))
    val mapAndPolynomials = (0 until n)
      .foldLeft((initialMap, initialPolynoial)) {
        case ((map, polynomial), index) =>
          val p =
            Polynomial(
              Array(
                field.GF2Int(1),
                field.GF2Int(this.generator).pow(index + fcr)
              )
            )
          val g = polynomial * p
          (map.updated(n - (index + 1), g), g)
      }
    mapAndPolynomials._1
  }

  // Based on encode_fast
  def encode(
      message: Array[Int],
      k: Option[Int] = None
  ): Array[Int] = {

    if (message.isEmpty) {
      Array()
    } else {
      val kToUse = k.getOrElse(this.k)

      require(
        message.length <= kToUse,
        s"Message length is $kToUse. Message was: ${message.length}"
      )

      val m = Polynomial.fromFiniteField(field) { message }
      val mPrime = Polynomial(
        m.cooefficients ++ Array.fill(n - kToUse)(field.GF2Int(0))
      )

      val b = mPrime.ggFastMod(g(kToUse))

      val nFirstOfMPrime = mPrime.cooefficients.dropRight(b.length)
      val resArray: Array[Int] =
        (nFirstOfMPrime appendedAll b.cooefficients).map(_.i)
      Array.fill(n - resArray.length)(0) appendedAll resArray
    }
  }

  // Based on decode_fast
  def decode(
      r: Array[Int],
      noStrip: Boolean = false,
      k: Option[Int] = None,
      erasurePosOption: Option[Array[Int]] = None,
      onlyErasure: Boolean = false
  ): (Array[Int], Array[Int]) = {

    val kToUse = k.getOrElse(this.k)

    val rp = Polynomial.fromFiniteField(field)(r)

    val erasurePos = erasurePosOption.map { e =>
      e.map(x => r.length - 1 - x)
    }

    val sz = syndromes(rp, k)

    // There were no errors
    if (sz.cooefficients.count(_.isZero) == sz.length) {
      val ret = r.dropRight(n - kToUse)
      val ecc = r.takeRight(n - kToUse)

      if (noStrip) {
        (ret, ecc)
      } else {
        (ret.dropWhile(x => x == 0), ecc)
      }
    } else {

      val (erasureCount, erasureLoc, erasureEval) = if (erasurePos.isDefined) {
        val erasureLoc = findErasureLocator(erasurePos.get)
        (
          erasurePos.get.length,
          Some(erasureLoc),
          Some(findErrorEvaluator(sz, erasureLoc, k = k))
        )
      } else {
        (0, None, None)
      }

      val (sigma, omega) = if (onlyErasure) {
        (erasureLoc.get, erasureEval.get)
      } else {
        val (sigmaTemp, _) =
          berlekampMassey(sz, k, erasureLoc, erasureEval, erasureCount)
        (sigmaTemp, findErrorEvaluator(sz, sigmaTemp, k))
      }
      val (x, j) = chienSearch(sigma)

      // TODO Include possiblity of failure in type signature
      require(j.length <= n - kToUse, "Potentially incorrect decoding")

      val y = forney(omega, x)

      val eList = Range(0, gf2Charac).foldLeft(Array[field.GF2Int]()) {
        case (res, i) => {
          if (j.contains(i)) {
            res appended y(j.indexOf(i))
          } else {
            res appended field.GF2Int(0)
          }
        }
      }
      val e = Polynomial(eList.reverse)
      val c = rp - e

      // TODO Include possiblity of failure in type signature
      //require(
      //  c.length <= r.length,
      //  s"Failed decoding? c.length: ${c.length} < r.length: ${r.length}\n" +
      //    s"Input was: ${r.mkString("[", ",", "]")} and c was: $c"
      //)
      // TODO Don't know of this, or the above is better
      val correctedC = if (c.length > r.length) {
        rp
      } else {
        c
      }

      val ret = correctedC.cooefficients.dropRight(n - kToUse).map(_.i)
      val ecc = correctedC.cooefficients.takeRight(n - kToUse).map(_.i)

      if (noStrip) {
        if (ret.isEmpty) {
          (Array.fill(kToUse)(0), ecc)
        } else {
          (ret, ecc)
        }
      } else {
        (ret.dropWhile(_ == 0), ecc)
      }
    }
  }

  def check(r: Array[Int], k: Option[Int] = None): Boolean = {

    val kToUse = k.getOrElse(this.k)
    val c = Polynomial.fromFiniteField(field)(r)
    val currentG = g(kToUse)

    (c % currentG).isZero
  }

  def checkFast(r: Array[Int], k: Option[Int] = None): Boolean = {

    val kToUse = k.getOrElse(this.k)

    val newR = Polynomial.fromFiniteField(field)(r)
    val sz = syndromes(newR, k)

    sz.cooefficients.count(x => x.i == 0) == sz.length
  }

  protected[reedsolomon] def syndromes(
      r: Polynomial[field.GF2Int],
      k: Option[Int] = None
  ): Polynomial[field.GF2Int] = {

    val kToUse = k.getOrElse(this.k)

    val firstCoeefs =
      (for { l <- Range(n - kToUse - 1, -1, -1) } yield {
        val pow = field.GF2Int(generator).pow(l + fcr)
        val res = r.evaluate(pow)._2
        res
      }).toArray

    Polynomial(firstCoeefs appended field.GF2Int(0), keepZeros = true)
  }

  protected def findErrorEvaluator(
      synd: Polynomial[field.GF2Int],
      sigma: Polynomial[field.GF2Int],
      k: Option[Int]
  ): Polynomial[field.GF2Int] = {
    val kToUse = k.getOrElse(this.k)
    (synd * sigma) % (Polynomial(
      Array(field.GF2Int(1)) :++ Array.fill(n - kToUse + 1)(field.GF2Int(0))
    ))
  }

  protected def findErasureLocator(
      erasurePos: Array[Int]
  ): Polynomial[field.GF2Int] = {
    erasurePos.foldLeft(Polynomial(Array(field.GF2Int(1)))) {
      case (erasureLoc, i) => {
        erasureLoc * (Polynomial(Array(field.GF2Int(i))) - Polynomial(
          Array(field.GF2Int(generator).pow(i), field.GF2Int(0))
        ))
      }
    }
  }

  def forney(
      omega: Polynomial[field.GF2Int],
      x: Array[field.GF2Int]
  ): Array[field.GF2Int] = {
    val xLength = x.length
    x.zipWithIndex.foldLeft(Array[field.GF2Int]()) {
      case (y, (xl, l)) => {
        val xlInv = xl.inverse
        val sigmaPrimeTmp =
          Range(0, xLength)
            .filter(j => j != l)
            .map(j => field.GF2Int(1) - xlInv * x(j))
        val sigmaPrime = sigmaPrimeTmp.foldLeft(field.GF2Int(1)) {
          case (acc, coef) => acc * coef
        }

        val yl =
          (xl.pow(1 - this.fcr) * (omega.evaluate(xlInv)._2 / sigmaPrime))
            .negate()
        y appended yl
      }
    }
  }

  // Based on Chien Search faster
  protected[reedsolomon] def chienSearch(
      sigma: Polynomial[field.GF2Int]
  ): (Array[field.GF2Int], Array[Int]) = {
    val p = field.GF2Int(this.generator)
    val (x, j) = Range(0, n).foldLeft((Array[field.GF2Int](), Array[Int]())) {
      case ((x, j), l) =>
        val (sigmaEvalV, sigmaEvalSum) = sigma.evaluate(p.pow(-l))
        if (sigmaEvalSum.isZero) {
          (x appended p.pow(l), j appended l)
        } else {
          (x, j)
        }
    }

    val errsNb = sigma.length
    require(
      j.length != errsNb,
      "To many (or few) errors found by Chien Search for the errata locator polynomial"
    )

    return (x, j)
  }

  // based on berlekamp_massey_fast
  protected[reedsolomon] def berlekampMassey(
      s: Polynomial[field.GF2Int],
      k: Option[Int] = None,
      erasures_loc: Option[Polynomial[field.GF2Int]] = None,
      erasures_eval: Option[Polynomial[field.GF2Int]] = None,
      erasures_count: Int = 0
  ): (Polynomial[field.GF2Int], Polynomial[field.GF2Int]) = {

    case class ValueContainer(
        sigma: Polynomial[field.GF2Int],
        sigmaPrev: Polynomial[field.GF2Int],
        b: Polynomial[field.GF2Int],
        omega: Polynomial[field.GF2Int],
        omegaPrev: Polynomial[field.GF2Int],
        a: Polynomial[field.GF2Int],
        lUpdateFlag: Int
    )

    val kToUse = k.getOrElse(this.k)

    val initalValues = erasures_loc match {
      case Some(polynomial) => {
        val sigma = Polynomial(polynomial.cooefficients)
        val sigmaPrev = Polynomial(sigma.cooefficients)
        val b = Polynomial(sigma.cooefficients)
        // TODO Prettier error handling here
        val omega = Polynomial(erasures_eval.get.cooefficients)
        val omegaPrev = Polynomial(omega.cooefficients)
        val a = Polynomial(omega.cooefficients)
        val lUpdate = 0
        ValueContainer(
          sigma,
          sigmaPrev,
          b,
          omega,
          omegaPrev,
          a,
          lUpdateFlag = lUpdate
        )
      }
      case None => {
        ValueContainer(
          sigma = Polynomial(Array(field.GF2Int(1))),
          sigmaPrev = Polynomial(Array(field.GF2Int(1))),
          b = Polynomial(Array(field.GF2Int(1))),
          omega = Polynomial(Array(field.GF2Int(1))),
          omegaPrev = Polynomial(Array(field.GF2Int(1))),
          a = Polynomial(Array(field.GF2Int(0))),
          lUpdateFlag = 0
        )
      }
    }

    val syndShift = if (s.length > (n - kToUse)) {
      s.length - (n - kToUse)
    } else 0

    val one = Polynomial(Array(field.GF2Int(1)))
    val z = Polynomial(Array(field.GF2Int(1), field.GF2Int(0)))

    val s2 = one + s

    val comp = (Range(0, n - kToUse - erasures_count)).foldLeft(initalValues) {
      case (container, l) => {

        val k = erasures_count + l + syndShift

        val delta = s2.*(container.sigma, k)
        val sigmaPrev = container.sigma
        val omegaPrev = container.omega

        val sigma = container.sigma - (z * container.b).scale(delta)
        val omega = container.omega - (z * container.a).scale(delta)

        val (a, b, lUpdate) =
          if (delta.isZero || ((2 * container.lUpdateFlag) > k + erasures_count)) {
            (z * container.a, z * container.b, container.lUpdateFlag)
          } else {
            (
              omegaPrev.scale(delta.inverse),
              sigmaPrev.scale(delta.inverse),
              k - container.lUpdateFlag
            )
          }
        ValueContainer(
          sigma = sigma,
          sigmaPrev = sigmaPrev,
          b = b,
          omega = omega,
          omegaPrev = omegaPrev,
          a = a,
          lUpdateFlag = lUpdate
        )
      }
    }

    if (comp.omega.degree > comp.sigma.degree) {
      val omega = Polynomial(
        comp.omega.cooefficients.takeRight(comp.sigma.degree + 1)
      )
      (comp.sigma, omega)
    } else {
      (comp.sigma, comp.omega)
    }
  }

}
