package se.scilifelab.reedsolomon

import org.scalatest._

class FiniteFieldsSpec extends FlatSpec with Matchers {

  "The finite field" should "be able to create look up tables" in {

    val parametersToTry = List((2, 0x11d, 8), (3, 0x11b, 8), (3, 0xfd, 7))

    for { ((generator, prim, cExp), index) <- parametersToTry.zipWithIndex } {
      val expectedExpTable = expectedExpLogResults(index)(0)
      val expectedLogTable = expectedExpLogResults(index)(1)
      val ff = GaloisField(generator = generator, prim = prim, cExp)

      ff.gf2intExpTable shouldEqual expectedExpTable
      ff.gf2intLogTable shouldEqual expectedLogTable

    }

  }

  it should "be able find find prime polynomials" in {

    val result =
      GaloisField.findPrimePolynomials(generator = 2, cExp = 6)

    result shouldEqual Array(67, 91, 97, 103, 109, 115)
  }

  it should "be able to add" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val a = field.GF2Int(3)
    val b = field.GF2Int(9)

    (a + b).i should be(10)
    (b + a).i should be(10)
  }

  it should "be able to subtract" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val a = field.GF2Int(3)
    val b = field.GF2Int(9)

    (a - b).i should be(10)
    (b - a).i should be(10)
  }

  it should "be able to multiply" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val a = field.GF2Int(3)
    val b = field.GF2Int(9)

    (a * b).i should be(27)
    (b * a).i should be(27)
  }

  it should "be able to inverse" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val a = field.GF2Int(3)
    val b = field.GF2Int(9)

    (b * b.inverse).i should be(1)
    b.inverse.i should be(79)
  }

  it should "be able to do division" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val a = field.GF2Int(3)
    val b = field.GF2Int(9)

    (b / b).i should be(1)
    (b / a).i should be(7)
    (field.GF2Int(9) / a).i should be(7)
    (b / field.GF2Int(3)).i should be(7)
  }

  it should "be able to be raised to the power of an int" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    val b = field.GF2Int(9)

    (b.pow(3)).i should be(127)
  }

  it should "pass Fermats theorem" in {
    val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
    for { i <- 0 until 256 } {
      val field = GaloisField(generator = 3, prim = 0x11b, gf2CExp = 8)
      field.GF2Int(i).pow(255).i should be(1)

    }
  }

  it should "multiply without a look up table" in {
    // This has kind of been tested already when building the exp/log lookup tables...
    pending
  }

  // Putting the expected exp/log tables at the end of the file to
  // make the rest of it easier to read.
  val expectedExpLogResults = Array(
    Array(
      Array(1, 2, 4, 8, 16, 32, 64, 128, 29, 58, 116, 232, 205, 135, 19, 38, 76,
        152, 45, 90, 180, 117, 234, 201, 143, 3, 6, 12, 24, 48, 96, 192, 157,
        39, 78, 156, 37, 74, 148, 53, 106, 212, 181, 119, 238, 193, 159, 35, 70,
        140, 5, 10, 20, 40, 80, 160, 93, 186, 105, 210, 185, 111, 222, 161, 95,
        190, 97, 194, 153, 47, 94, 188, 101, 202, 137, 15, 30, 60, 120, 240,
        253, 231, 211, 187, 107, 214, 177, 127, 254, 225, 223, 163, 91, 182,
        113, 226, 217, 175, 67, 134, 17, 34, 68, 136, 13, 26, 52, 104, 208, 189,
        103, 206, 129, 31, 62, 124, 248, 237, 199, 147, 59, 118, 236, 197, 151,
        51, 102, 204, 133, 23, 46, 92, 184, 109, 218, 169, 79, 158, 33, 66, 132,
        21, 42, 84, 168, 77, 154, 41, 82, 164, 85, 170, 73, 146, 57, 114, 228,
        213, 183, 115, 230, 209, 191, 99, 198, 145, 63, 126, 252, 229, 215, 179,
        123, 246, 241, 255, 227, 219, 171, 75, 150, 49, 98, 196, 149, 55, 110,
        220, 165, 87, 174, 65, 130, 25, 50, 100, 200, 141, 7, 14, 28, 56, 112,
        224, 221, 167, 83, 166, 81, 162, 89, 178, 121, 242, 249, 239, 195, 155,
        43, 86, 172, 69, 138, 9, 18, 36, 72, 144, 61, 122, 244, 245, 247, 243,
        251, 235, 203, 139, 11, 22, 44, 88, 176, 125, 250, 233, 207, 131, 27,
        54, 108, 216, 173, 71, 142, 1),
      Array(-1, 0, 1, 25, 2, 50, 26, 198, 3, 223, 51, 238, 27, 104, 199, 75, 4,
        100, 224, 14, 52, 141, 239, 129, 28, 193, 105, 248, 200, 8, 76, 113, 5,
        138, 101, 47, 225, 36, 15, 33, 53, 147, 142, 218, 240, 18, 130, 69, 29,
        181, 194, 125, 106, 39, 249, 185, 201, 154, 9, 120, 77, 228, 114, 166,
        6, 191, 139, 98, 102, 221, 48, 253, 226, 152, 37, 179, 16, 145, 34, 136,
        54, 208, 148, 206, 143, 150, 219, 189, 241, 210, 19, 92, 131, 56, 70,
        64, 30, 66, 182, 163, 195, 72, 126, 110, 107, 58, 40, 84, 250, 133, 186,
        61, 202, 94, 155, 159, 10, 21, 121, 43, 78, 212, 229, 172, 115, 243,
        167, 87, 7, 112, 192, 247, 140, 128, 99, 13, 103, 74, 222, 237, 49, 197,
        254, 24, 227, 165, 153, 119, 38, 184, 180, 124, 17, 68, 146, 217, 35,
        32, 137, 46, 55, 63, 209, 91, 149, 188, 207, 205, 144, 135, 151, 178,
        220, 252, 190, 97, 242, 86, 211, 171, 20, 42, 93, 158, 132, 60, 57, 83,
        71, 109, 65, 162, 31, 45, 67, 216, 183, 123, 164, 118, 196, 23, 73, 236,
        127, 12, 111, 246, 108, 161, 59, 82, 41, 157, 85, 170, 251, 96, 134,
        177, 187, 204, 62, 90, 203, 89, 95, 176, 156, 169, 160, 81, 11, 245, 22,
        235, 122, 117, 44, 215, 79, 174, 213, 233, 230, 231, 173, 232, 116, 214,
        244, 234, 168, 80, 88, 175)
    ),
    Array(
      Array(1, 3, 5, 15, 17, 51, 85, 255, 26, 46, 114, 150, 161, 248, 19, 53,
        95, 225, 56, 72, 216, 115, 149, 164, 247, 2, 6, 10, 30, 34, 102, 170,
        229, 52, 92, 228, 55, 89, 235, 38, 106, 190, 217, 112, 144, 171, 230,
        49, 83, 245, 4, 12, 20, 60, 68, 204, 79, 209, 104, 184, 211, 110, 178,
        205, 76, 212, 103, 169, 224, 59, 77, 215, 98, 166, 241, 8, 24, 40, 120,
        136, 131, 158, 185, 208, 107, 189, 220, 127, 129, 152, 179, 206, 73,
        219, 118, 154, 181, 196, 87, 249, 16, 48, 80, 240, 11, 29, 39, 105, 187,
        214, 97, 163, 254, 25, 43, 125, 135, 146, 173, 236, 47, 113, 147, 174,
        233, 32, 96, 160, 251, 22, 58, 78, 210, 109, 183, 194, 93, 231, 50, 86,
        250, 21, 63, 65, 195, 94, 226, 61, 71, 201, 64, 192, 91, 237, 44, 116,
        156, 191, 218, 117, 159, 186, 213, 100, 172, 239, 42, 126, 130, 157,
        188, 223, 122, 142, 137, 128, 155, 182, 193, 88, 232, 35, 101, 175, 234,
        37, 111, 177, 200, 67, 197, 84, 252, 31, 33, 99, 165, 244, 7, 9, 27, 45,
        119, 153, 176, 203, 70, 202, 69, 207, 74, 222, 121, 139, 134, 145, 168,
        227, 62, 66, 198, 81, 243, 14, 18, 54, 90, 238, 41, 123, 141, 140, 143,
        138, 133, 148, 167, 242, 13, 23, 57, 75, 221, 124, 132, 151, 162, 253,
        28, 36, 108, 180, 199, 82, 246, 1),
      Array(-1, 0, 25, 1, 50, 2, 26, 198, 75, 199, 27, 104, 51, 238, 223, 3,
        100, 4, 224, 14, 52, 141, 129, 239, 76, 113, 8, 200, 248, 105, 28, 193,
        125, 194, 29, 181, 249, 185, 39, 106, 77, 228, 166, 114, 154, 201, 9,
        120, 101, 47, 138, 5, 33, 15, 225, 36, 18, 240, 130, 69, 53, 147, 218,
        142, 150, 143, 219, 189, 54, 208, 206, 148, 19, 92, 210, 241, 64, 70,
        131, 56, 102, 221, 253, 48, 191, 6, 139, 98, 179, 37, 226, 152, 34, 136,
        145, 16, 126, 110, 72, 195, 163, 182, 30, 66, 58, 107, 40, 84, 250, 133,
        61, 186, 43, 121, 10, 21, 155, 159, 94, 202, 78, 212, 172, 229, 243,
        115, 167, 87, 175, 88, 168, 80, 244, 234, 214, 116, 79, 174, 233, 213,
        231, 230, 173, 232, 44, 215, 117, 122, 235, 22, 11, 245, 89, 203, 95,
        176, 156, 169, 81, 160, 127, 12, 246, 111, 23, 196, 73, 236, 216, 67,
        31, 45, 164, 118, 123, 183, 204, 187, 62, 90, 251, 96, 177, 134, 59, 82,
        161, 108, 170, 85, 41, 157, 151, 178, 135, 144, 97, 190, 220, 252, 188,
        149, 207, 205, 55, 63, 91, 209, 83, 57, 132, 60, 65, 162, 109, 71, 20,
        42, 158, 93, 86, 242, 211, 171, 68, 17, 146, 217, 35, 32, 46, 137, 180,
        124, 184, 38, 119, 153, 227, 165, 103, 74, 237, 222, 197, 49, 254, 24,
        13, 99, 140, 128, 192, 247, 112, 7)
    ),
    Array(
      Array(
        1, 3, 5, 15, 17, 51, 85, 2, 6, 10, 30, 34, 102, 87, 4, 12, 20, 60, 68,
        49, 83, 8, 24, 40, 120, 117, 98, 91, 16, 48, 80, 13, 23, 57, 75, 32, 96,
        93, 26, 46, 114, 107, 64, 61, 71, 52, 92, 25, 43, 125, 122, 115, 104,
        69, 50, 86, 7, 9, 27, 45, 119, 100, 81, 14, 18, 54, 90, 19, 53, 95, 28,
        36, 108, 73, 38, 106, 67, 56, 72, 37, 111, 76, 41, 123, 112, 109, 74,
        35, 101, 82, 11, 29, 39, 105, 70, 55, 89, 22, 58, 78, 47, 113, 110, 79,
        44, 116, 97, 94, 31, 33, 99, 88, 21, 63, 65, 62, 66, 59, 77, 42, 126,
        127, 124, 121, 118, 103, 84, 1
      ),
      Array(-1, 0, 7, 1, 14, 2, 8, 56, 21, 57, 9, 90, 15, 31, 63, 3, 28, 4, 64,
        67, 16, 112, 97, 32, 22, 47, 38, 58, 70, 91, 10, 108, 35, 109, 11, 87,
        71, 79, 74, 92, 23, 82, 119, 48, 104, 59, 39, 100, 29, 19, 54, 5, 45,
        68, 65, 95, 77, 33, 98, 117, 17, 43, 115, 113, 42, 114, 116, 76, 18, 53,
        94, 44, 78, 73, 86, 34, 81, 118, 99, 103, 30, 62, 89, 20, 126, 6, 55,
        13, 111, 96, 66, 27, 46, 37, 107, 69, 36, 106, 26, 110, 61, 88, 12, 125,
        52, 93, 75, 41, 72, 85, 102, 80, 84, 101, 40, 51, 105, 25, 124, 60, 24,
        123, 50, 83, 122, 49, 120, 121)
    )
  )
}
