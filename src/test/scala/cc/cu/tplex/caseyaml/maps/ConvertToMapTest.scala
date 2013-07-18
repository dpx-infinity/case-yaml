package cc.cu.tplex.caseyaml.maps

import org.scalatest.FlatSpec
import cc.cu.tplex.caseyaml.model._
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.HashMap
import cc.cu.tplex.caseyaml.CaseYamlException

/**
 * Date: 14.07.13
 * Time: 14:22
 *
 * @author Vladimir Matveev
 */
class ConvertToMapTest extends FlatSpec with ShouldMatchers {
  "ConvertToMap.convert" should "serialize int-compatible values directly (upcasting to int if necessary)" in {
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

  implicit class EntityConvertible(obj: YEntity[_, _]) {
    def as[Obj, Yml] = obj.asInstanceOf[YEntity[Obj, Yml]]
  }

  it should "throw an exception when serializing int-incompatible type as int-compatible" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YByte.as[Boolean, Any], true)
    }.message should equal ("Expected int-compatible type, got java.lang.Boolean")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YShort.as[Array[Char], Any], Array('a'))
    }.message should equal ("Expected int-compatible type, got [C")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YInt.as[String, Any], "abc")
    }.message should equal ("Expected int-compatible type, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YLong.as[Double, Any], 10.2: Double)
    }.message should equal ("Expected int-compatible type, got java.lang.Double")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YBigInt.as[Vector[Long], Any], Vector.empty[Long])
    }.message should equal ("Expected int-compatible type, got scala.collection.immutable.Vector")
  }

  it should "throw an exception when serializing null as int-compatible type" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YByte.as[Any, Number], null)
    }.message should equal ("Expected int-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YShort.as[Any, Number], null)
    }.message should equal ("Expected int-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YInt.as[Any, Number], null)
    }.message should equal ("Expected int-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YLong.as[Any, Number], null)
    }.message should equal ("Expected int-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YIntCompatible.YBigInt, null)
    }.message should equal ("Expected int-compatible type, got null")
  }

  it should "serialize float-compatible objects directly" in {
    val float: Float = 1.1.toFloat
    val double: Double = 2.2
    val bigdec: BigDecimal = 3.3

    import ConvertToMap.convert

    convert(YFloatCompatible.YFloat, float) should equal (1.1.toFloat)
    convert(YFloatCompatible.YDouble, double) should equal (2.2: Double)
    convert(YFloatCompatible.YBigDecimal, bigdec) should equal (java.math.BigDecimal.valueOf(3.3))
  }

  it should "throw an exception when serializing float-incompatible type as float-compatible" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YFloatCompatible.YFloat.as[String, Any], "abc")
    }.message should equal ("Expected float-compatible type, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YFloatCompatible.YDouble.as[Long, Any], 10: Long)
    }.message should equal ("Expected float-compatible type, got java.lang.Long")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YFloatCompatible.YBigDecimal.as[Boolean, Any], false)
    }.message should equal ("Expected float-compatible type, got java.lang.Boolean")
  }

  it should "throw an exception when serializing null as float-compatible type" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YFloatCompatible.YFloat.as[Any, Number], null)
    }.message should equal ("Expected float-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YFloatCompatible.YDouble.as[Any, Number], null)
    }.message should equal ("Expected float-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YFloatCompatible.YBigDecimal, null)
    }.message should equal ("Expected float-compatible type, got null")
  }

  it should "serialize strings directly" in {
    ConvertToMap.convert(YString, "abcd") should equal ("abcd")
  }

  it should "throw an exception when serializing non-string object as a string" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YString.as[Int, Any], 10: Int)
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
      ConvertToMap.convert(YBoolean.as[String, Any], "abcd")
    }.message should equal ("Expected boolean, got java.lang.String")
  }

  it should "throw an exception when serializing null as a boolean" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YBoolean.as[Any, Any], null)
    }.message should equal ("Expected boolean, got null")
  }

  it should "serialize an implementation of Map[String, ?] as java.util.Map[String, ?]" in {
    val m: Map[String, Int] = HashMap("a" -> 1, "b" -> 2, "c" -> 3)

    val rm = ConvertToMap.convert(YMap(YIntCompatible.YInt), m).asInstanceOf[java.util.Map[String, Int]]
    rm should have size 3
    rm.get("a") should equal (1)
    rm.get("b") should equal (2)
    rm.get("c") should equal (3)
  }

  it should "throw an exception when serializing non-Map object as java.util.Map" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YMap(YList(YString)).as[String, Any], "abcd")
    }.message should equal ("Expected map from string to list of string, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToMap.convert(YMap(YNullable(YIntCompatible.YBigInt)).as[Array[BigInt], Any], Array.empty[BigInt])
    }.message should equal ("Expected map from string to nullable int-compatible type, got [Lscala.math.BigInt;")
  }

  it should "throw an exception when serializing null as java.util.Map" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YMap(YString), null)
    }.message should equal ("Expected map from string to string, got null")
  }

  it should "serialize an implementation of Seq[?] as java.util.List[?]" in {
    val list = List(1.1, 1.2, 1.3)

    val rm = ConvertToMap.convert(YList(YFloatCompatible.YDouble), list).asInstanceOf[java.util.List[Double]]
    rm should have size 3
    rm.get(0) should equal (1.1)
    rm.get(1) should equal (1.2)
    rm.get(2) should equal (1.3)
  }

  it should "throw an exception when serializing null as java.util.List" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YList(YString), null)
    }.message should equal ("Expected list of string, got null")
  }

  it should "allow null values to be serialized for nullable entity" in {
    ConvertToMap.convert(YNullable(YString), null)                      should be (null)
    ConvertToMap.convert(YNullable(YIntCompatible.YBigInt), null)       should be (null)
    ConvertToMap.convert(YNullable(YFloatCompatible.YBigDecimal), null) should be (null)
    ConvertToMap.convert(YNullable(YMap(YString)), null)                should be (null)
    ConvertToMap.convert(YNullable(YList(ModelFixture.yentity)), null)  should be (null)
    ConvertToMap.convert(YNullable(ModelFixture.yentity), null)         should be (null)
  }

  it should "throw an exception when serializing YOptional outside of YClassMap" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(YOptional(YString), Option("123"))
    }.message should equal ("YOptional is not applicable outside of YClassMap")
  }

  it should "allow null values for YStringConverted" in {
    val ysc = YStringConverted[String](s => if (s == null) "<null>" else s, s => s)
    ConvertToMap.convert(ysc, "abc") should equal ("abc")
    ConvertToMap.convert(ysc, null) should equal ("<null>")
  }

  it should "serialize an object to a java.util.Map/java.util.List tree using YClassMap tree" in {
    val m = ConvertToMap
      .convert(ModelFixture.yentity, ModelFixture.model)
      .asInstanceOf[java.util.Map[String, Any]]

    m should have size 6
    m.get("id") should equal ("test")
    m.get("name") should equal ("name")
    m.get("enabled") should equal (true)
    m.get("count") should equal (10)
    m.get("fraction") should equal (12.2)

    val plugins = m.get("plugins").asInstanceOf[java.util.Map[String, java.util.Map[String, Any]]]
    plugins should have size 2

    {
      val p = plugins.get("plugin1")
      p should have size 3
      p.get("id") should equal ("id")
      p.get("pluginName") should equal ("name")
      p should not contain key ("data")

      val d = p.get("dependencies").asInstanceOf[java.util.List[String]]
      d should have size 2
      d.get(0) should equal ("id1")
      d.get(1) should equal ("id2")
    }

    {
      val p = plugins.get("plugin2")
      p should have size 4
      p.get("id") should equal ("id2")
      p.get("pluginName") should equal ("name2")
      p.get("data") should equal (java.math.BigDecimal.valueOf(123.45))

      p.get("dependencies").asInstanceOf[java.util.List[String]] should be ('empty)
    }
  }

  it should "throw an exception when an object of incorrect class is serialized using YClassMap" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(ModelFixture.yentity.as[String, Any], "abcd")
    }.message should equal ("Expected cc.cu.tplex.caseyaml.model.ProjectModel, got java.lang.String")
  }

  it should "throw an exception when null is serialized using YClassMap" in {
    intercept[CaseYamlException] {
      ConvertToMap.convert(ModelFixture.yentity, null)
    }.message should equal ("Expected cc.cu.tplex.caseyaml.model.ProjectModel, got null")
  }
}
