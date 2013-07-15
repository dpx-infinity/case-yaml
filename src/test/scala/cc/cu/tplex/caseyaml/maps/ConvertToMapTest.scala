package cc.cu.tplex.caseyaml.maps

import org.scalatest.FlatSpec
import cc.cu.tplex.caseyaml.model._
import org.scalatest.matchers.ShouldMatchers
import scala.reflect.ClassTag
import scala.collection.immutable.HashMap
import cc.cu.tplex.caseyaml.CaseYamlException
import cc.cu.tplex.caseyaml.model.YIntCompatible.YByte

/**
 * Date: 14.07.13
 * Time: 14:22
 *
 * @author Vladimir Matveev
 */
class ConvertToMapTest extends FlatSpec with ShouldMatchers {
  "MapConverter.ToMap" should "serialize int-compatible objects directly" in {
    val byte: Byte = 1
    val short: Short = 2
    val int: Int = 3
    val long: Long = 4
    val bigint: BigInt = 5

    import ConvertToMap.convert

    convert(YIntCompatible.YByte, byte) should equal (1: Byte)
    convert(YIntCompatible.YShort, short) should equal (2: Short)
    convert(YIntCompatible.YInt, int) should equal (3: Int)
    convert(YIntCompatible.YLong, long) should equal (4: Long)
    convert(YIntCompatible.YBigInt, bigint) should equal (java.math.BigInteger.valueOf(5))
  }

  it should "throw an exception when serializing int-incompatible type as int-compatible" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YInt, "abc")
    }.message should equal ("Expected int-compatible type, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YLong, 10.2: Double)
    }.message should equal ("Expected int-compatible type, got java.lang.Double")
  }

  it should "throw an exception when serializing null as int-compatible type" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YShort, null)
    }.message should equal ("Expected int-compatible type, got null")
  }

  it should "serialize float-compatible objects directly" in {
    val float: Float = 1.1.toFloat
    val double: Double = 2.2
    val bigdec: BigDecimal = 3.3

    import ConvertToMap.convert

    convert(YFloatCompatible.YFloat, float) should equal (1.1.toFloat: Float)
    convert(YFloatCompatible.YDouble, double) should equal (2.2)
    convert(YFloatCompatible.YBigDecimal, bigdec) should equal (java.math.BigDecimal.valueOf(3.3))
  }

  it should "throw an exception when serializing float-incompatible type as float-compatible" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YFloatCompatible.YFloat, "abc")
    }.message should equal ("Expected float-compatible type, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YFloatCompatible.YDouble, 10: Long)
    }.message should equal ("Expected float-compatible type, got java.lang.Long")
  }

  it should "throw an exception when serializing null as float-compatible type" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YFloatCompatible.YBigDecimal, null)
    }.message should equal ("Expected float-compatible type, got null")
  }

  it should "serialize strings directly" in {
    ConvertToMap.convert(YString, "abcd") should equal ("abcd")
  }

  it should "throw an exception when serializing non-string object as a string" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YString, 10: Int)
    }.message should equal ("Expected string, got java.lang.Integer")
  }

  it should "throw an exception when serializing null as a string" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YString, null)
    }.message should equal ("Expected string, got null")
  }

  it should "serialize booleans directly" in {
    ConvertToMap.convert(YBoolean, true) should equal (true)
    ConvertToMap.convert(YBoolean, false) should equal (false)
  }

  it should "throw an exception when serializing non-boolean object as a boolean" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YBoolean, "abcd")
    }.message should equal ("Expected boolean, got java.lang.String")
  }

  it should "throw an exception when serializing null as a boolean" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YBoolean, null)
    }.message should equal ("Expected boolean, got null")
  }

  it should "serialize an implementation of Map[String, ?] as java.util.Map[String, ?]" in {
    val m = HashMap("a" -> 1, "b" -> 2, "c" -> 3)

    val rm = ConvertToMap.convert(YMap(YIntCompatible.YInt), m).asInstanceOf[java.util.Map[String, Int]]
    rm should have size 3
    rm.get("a") should equal (1)
    rm.get("b") should equal (2)
    rm.get("c") should equal (3)
  }

  it should "serialize an object to a java.util.Map/java.util.List tree using YEntity tree" in {
    val m = ConvertToMap
            .convert(ModelFixture.yentity, ModelFixture.model)
            .asInstanceOf[java.util.Map[String, Any]]

    m should have size 5
    m.get("id") should equal ("test")
    m.get("name") should equal (null)
    m.get("enabled") should equal (true)
    m.get("count") should equal (10)
    m.get("fraction") should equal (12.2)

    val plugins = m.get("plugins").asInstanceOf[java.util.Map[String, java.util.Map[String, Any]]]
    plugins should have size 1

    val p = plugins.get("plugin1")
    p should have size 3
    p.get("id") should equal ("id")
    p.get("pluginName") should equal ("name")

    val d = p.get("dependencies").asInstanceOf[java.util.List[String]]
    d should have size 2
    d.get(0) should equal ("id1")
    d.get(1) should equal ("id2")
  }
}
