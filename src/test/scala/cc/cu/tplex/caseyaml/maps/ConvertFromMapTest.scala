package cc.cu.tplex.caseyaml.maps

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import cc.cu.tplex.caseyaml.model._
import cc.cu.tplex.caseyaml.test.CustomMatchers
import cc.cu.tplex.caseyaml.CaseYamlException
import java.util
import cc.cu.tplex.caseyaml.CaseYamlException

/**
 * Date: 15.07.13
 * Time: 12:48
 */
class ConvertFromMapTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  "ConvertFromMap" should "deserialize int-compatible types with upcasting/downcasting, if needed" in {
    val int: Int = 10
    val long: Long = 20
    val bigint: java.math.BigInteger = java.math.BigInteger.valueOf(30)

    ConvertFromMap.convert(YIntCompatible.YByte, int)      should (be (anInstanceOf[Byte])   and be === 10)
    ConvertFromMap.convert(YIntCompatible.YShort, int)     should (be (anInstanceOf[Short])  and be === 10)
    ConvertFromMap.convert(YIntCompatible.YInt, int)       should (be (anInstanceOf[Int])    and be === 10)
    ConvertFromMap.convert(YIntCompatible.YLong, int)      should (be (anInstanceOf[Long])   and be === 10)
    ConvertFromMap.convert(YIntCompatible.YBigInt, int)    should (be (anInstanceOf[BigInt]) and be === 10)

    ConvertFromMap.convert(YIntCompatible.YByte, long)     should (be (anInstanceOf[Byte])   and be === 20)
    ConvertFromMap.convert(YIntCompatible.YShort, long)    should (be (anInstanceOf[Short])  and be === 20)
    ConvertFromMap.convert(YIntCompatible.YInt, long)      should (be (anInstanceOf[Int])    and be === 20)
    ConvertFromMap.convert(YIntCompatible.YLong, long)     should (be (anInstanceOf[Long])   and be === 20)
    ConvertFromMap.convert(YIntCompatible.YBigInt, long)   should (be (anInstanceOf[BigInt]) and be === 20)

    ConvertFromMap.convert(YIntCompatible.YByte, bigint)   should (be (anInstanceOf[Byte])   and be === 30)
    ConvertFromMap.convert(YIntCompatible.YShort, bigint)  should (be (anInstanceOf[Short])  and be === 30)
    ConvertFromMap.convert(YIntCompatible.YInt, bigint)    should (be (anInstanceOf[Int])    and be === 30)
    ConvertFromMap.convert(YIntCompatible.YLong, bigint)   should (be (anInstanceOf[Long])   and be === 30)
    ConvertFromMap.convert(YIntCompatible.YBigInt, bigint) should (be (anInstanceOf[BigInt]) and be === 30)
  }

  it should "throw an exception when int-incompatible object is deserialized as int-compatible" in {
    intercept[CaseYamlException] {
      ConvertFromMap.convert(YIntCompatible.YByte, "abc")
    }.message should equal ("Expected int, long or java.math.BigInteger, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YIntCompatible.YShort, 11.2)
    }.message should equal ("Expected int, long or java.math.BigInteger, got java.lang.Double")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YIntCompatible.YInt, 12.3.toFloat)
    }.message should equal ("Expected int, long or java.math.BigInteger, got java.lang.Float")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YIntCompatible.YLong, Vector.empty)
    }.message should equal ("Expected int, long or java.math.BigInteger, got scala.collection.immutable.Vector")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YIntCompatible.YBigInt, new util.ArrayList[Long]())
    }.message should equal ("Expected int, long or java.math.BigInteger, got java.util.ArrayList")
  }

  it should "throw an exception when null is deserialized as an int-compatible type" in {
    intercept[CaseYamlException] {
      ConvertFromMap.convert(YIntCompatible.YByte, null)
    }.message should equal ("Expected int, long or java.math.BigInteger, got null")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YIntCompatible.YShort, null)
    }.message should equal ("Expected int, long or java.math.BigInteger, got null")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YIntCompatible.YInt, null)
    }.message should equal ("Expected int, long or java.math.BigInteger, got null")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YIntCompatible.YLong, null)
    }.message should equal ("Expected int, long or java.math.BigInteger, got null")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YIntCompatible.YBigInt, null)
    }.message should equal ("Expected int, long or java.math.BigInteger, got null")
  }

  it should "deserialize float-compatible types with upcasting/downcasting, if needed" in {
    val double: Double = 11.1

    ConvertFromMap.convert(YFloatCompatible.YFloat, double) should (
      be (anInstanceOf[Float]) and be === 11.1.toFloat
      )
    ConvertFromMap.convert(YFloatCompatible.YDouble, double) should (
      be (anInstanceOf[Double]) and be === 11.1
      )
    ConvertFromMap.convert(YFloatCompatible.YBigDecimal, double) should (
      be (anInstanceOf[BigDecimal]) and be === BigDecimal(11.1)
      )
  }

  it should "throw an exception when float-incompatible object is deserialized as float-compatible" in {
    intercept[CaseYamlException] {
      ConvertFromMap.convert(YFloatCompatible.YFloat, "abc")
    }.message should equal ("Expected double, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YFloatCompatible.YDouble, 10: Int)
    }.message should equal ("Expected double, got java.lang.Integer")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YFloatCompatible.YBigDecimal, Vector.empty)
    }.message should equal ("Expected double, got scala.collection.immutable.Vector")
  }

  it should "throw an exception when null is deserialized as float-compatible type" in {
    intercept[CaseYamlException] {
      ConvertFromMap.convert(YFloatCompatible.YFloat, null)
    }.message should equal ("Expected double, got null")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YFloatCompatible.YDouble, null)
    }.message should equal ("Expected double, got null")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YFloatCompatible.YBigDecimal, null)
    }.message should equal ("Expected double, got null")
  }

  it should "deserialize strings directly" in {
    ConvertFromMap.convert(YString, "abcd") should equal ("abcd")
    ConvertFromMap.convert(YString, "") should equal ("")
  }

  it should "throw an exception when non-string object is deserialized as a string" in {
    intercept[CaseYamlException] {
      ConvertFromMap.convert(YString, 123: Int)
    }.message should equal ("Expected string, got java.lang.Integer")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YString, Array.empty[Char])
    }.message should equal ("Expected string, got [C")
  }

  it should "throw an exception when null is deserialized as a string" in {
    intercept[CaseYamlException] {
      ConvertFromMap.convert(YString, null)
    }.message should equal ("Expected string, got null")
  }

  it should "deserialize booleans directly" in {
    ConvertFromMap.convert(YBoolean, true) should equal (true)
    ConvertFromMap.convert(YBoolean, false) should equal (false)
  }

  it should "throw an exception when non-boolean object is deserialized as a boolean" in {
    intercept[CaseYamlException] {
      ConvertFromMap.convert(YBoolean, 0: Int)
    }.message should equal ("Expected boolean, got java.lang.Integer")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YBoolean, Array[Boolean](true, true, false))
    }.message should equal ("Expected boolean, got [Z")
  }

  it should "throw an exception when null is deserialized as a boolean" in {
    intercept[CaseYamlException] {
      ConvertFromMap.convert(YBoolean, null)
    }.message should equal ("Expected boolean, got null")
  }

  it should "deserialize a map from string to other type with conversion" in {
    val map = new util.HashMap[String, Any]()
    map.put("a", 11: Int)
    map.put("b", 12: Long)
    map.put("c", java.math.BigInteger.valueOf(13))

    val cm = ConvertFromMap.convert(YMap(YIntCompatible.YInt), map)
    cm should have size 3
    cm("a") should equal (11: Int)
    cm("b") should equal (12: Int)
    cm("c") should equal (13: Int)
  }

  it should "throw an exception when not java.util.Map object is deserialized as a map" in {
    intercept[CaseYamlException] {
      ConvertFromMap.convert(YMap(YIntCompatible.YInt), "avc")
    }.message should equal ("Expected java.util.Map from string to int, long or java.math.BigInteger, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertFromMap.convert(YMap(YBoolean), Array(10.toShort))
    }.message should equal ("Expected java.util.Map from string to boolean, got [S")
  }
}
