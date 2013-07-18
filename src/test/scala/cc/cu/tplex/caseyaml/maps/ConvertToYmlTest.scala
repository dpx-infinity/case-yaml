/*******************************************************************************
 * Copyright 2013 Vladimir Matveev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

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
class ConvertToYmlTest extends FlatSpec with ShouldMatchers {
  "ConvertToYml" should "serialize int-compatible values directly (upcasting to int if necessary)" in {
    val byte: Byte = 1
    val short: Short = 2
    val int: Int = 3
    val long: Long = 4
    val bigint: BigInt = 5

    import ConvertToYml.apply

    apply(YIntCompatible.YByte, byte) should equal (1: Byte)
    apply(YIntCompatible.YShort, short) should equal (2: Short)
    apply(YIntCompatible.YInt, int) should equal (3: Int)
    apply(YIntCompatible.YLong, long) should equal (4: Long)
    apply(YIntCompatible.YBigInt, bigint) should equal (java.math.BigInteger.valueOf(5))
  }

  implicit class EntityConvertible(obj: YEntity[_, _]) {
    def as[Obj, Yml] = obj.asInstanceOf[YEntity[Obj, Yml]]
  }

  it should "throw an exception when serializing int-incompatible type as int-compatible" in {
    intercept[CaseYamlException] {
      ConvertToYml(YIntCompatible.YByte.as[Boolean, Any], true)
    }.message should equal ("Expected int-compatible type, got java.lang.Boolean")

    intercept[CaseYamlException] {
      ConvertToYml(YIntCompatible.YShort.as[Array[Char], Any], Array('a'))
    }.message should equal ("Expected int-compatible type, got [C")

    intercept[CaseYamlException] {
      ConvertToYml(YIntCompatible.YInt.as[String, Any], "abc")
    }.message should equal ("Expected int-compatible type, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToYml(YIntCompatible.YLong.as[Double, Any], 10.2: Double)
    }.message should equal ("Expected int-compatible type, got java.lang.Double")

    intercept[CaseYamlException] {
      ConvertToYml(YIntCompatible.YBigInt.as[Vector[Long], Any], Vector.empty[Long])
    }.message should equal ("Expected int-compatible type, got scala.collection.immutable.Vector")
  }

  it should "throw an exception when serializing null as int-compatible type" in {
    intercept[CaseYamlException] {
      ConvertToYml(YIntCompatible.YByte.as[Any, Number], null)
    }.message should equal ("Expected int-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToYml(YIntCompatible.YShort.as[Any, Number], null)
    }.message should equal ("Expected int-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToYml(YIntCompatible.YInt.as[Any, Number], null)
    }.message should equal ("Expected int-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToYml(YIntCompatible.YLong.as[Any, Number], null)
    }.message should equal ("Expected int-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToYml(YIntCompatible.YBigInt, null)
    }.message should equal ("Expected int-compatible type, got null")
  }

  it should "serialize float-compatible objects directly" in {
    val float: Float = 1.1.toFloat
    val double: Double = 2.2
    val bigdec: BigDecimal = 3.3

    import ConvertToYml.apply

    apply(YFloatCompatible.YFloat, float) should equal (1.1.toFloat)
    apply(YFloatCompatible.YDouble, double) should equal (2.2: Double)
    apply(YFloatCompatible.YBigDecimal, bigdec) should equal (java.math.BigDecimal.valueOf(3.3))
  }

  it should "throw an exception when serializing float-incompatible type as float-compatible" in {
    intercept[CaseYamlException] {
      ConvertToYml(YFloatCompatible.YFloat.as[String, Any], "abc")
    }.message should equal ("Expected float-compatible type, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToYml(YFloatCompatible.YDouble.as[Long, Any], 10: Long)
    }.message should equal ("Expected float-compatible type, got java.lang.Long")

    intercept[CaseYamlException] {
      ConvertToYml(YFloatCompatible.YBigDecimal.as[Boolean, Any], false)
    }.message should equal ("Expected float-compatible type, got java.lang.Boolean")
  }

  it should "throw an exception when serializing null as float-compatible type" in {
    intercept[CaseYamlException] {
      ConvertToYml(YFloatCompatible.YFloat.as[Any, Number], null)
    }.message should equal ("Expected float-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToYml(YFloatCompatible.YDouble.as[Any, Number], null)
    }.message should equal ("Expected float-compatible type, got null")

    intercept[CaseYamlException] {
      ConvertToYml(YFloatCompatible.YBigDecimal, null)
    }.message should equal ("Expected float-compatible type, got null")
  }

  it should "serialize strings directly" in {
    ConvertToYml(YString, "abcd") should equal ("abcd")
  }

  it should "throw an exception when serializing non-string object as a string" in {
    intercept[CaseYamlException] {
      ConvertToYml(YString.as[Int, Any], 10: Int)
    }.message should equal ("Expected string, got java.lang.Integer")
  }

  it should "throw an exception when serializing null as a string" in {
    intercept[CaseYamlException] {
      ConvertToYml(YString, null)
    }.message should equal ("Expected string, got null")
  }

  it should "serialize booleans directly" in {
    ConvertToYml(YBoolean, true) should equal (true)
    ConvertToYml(YBoolean, false) should equal (false)
  }

  it should "throw an exception when serializing non-boolean object as a boolean" in {
    intercept[CaseYamlException] {
      ConvertToYml(YBoolean.as[String, Any], "abcd")
    }.message should equal ("Expected boolean, got java.lang.String")
  }

  it should "throw an exception when serializing null as a boolean" in {
    intercept[CaseYamlException] {
      ConvertToYml(YBoolean.as[Any, Any], null)
    }.message should equal ("Expected boolean, got null")
  }

  it should "serialize an implementation of Map[String, ?] as java.util.Map[String, ?]" in {
    val m: Map[String, Int] = HashMap("a" -> 1, "b" -> 2, "c" -> 3)

    val rm = ConvertToYml(YMap(YIntCompatible.YInt), m).asInstanceOf[java.util.Map[String, Int]]
    rm should have size 3
    rm.get("a") should equal (1)
    rm.get("b") should equal (2)
    rm.get("c") should equal (3)
  }

  it should "throw an exception when serializing non-Map object as java.util.Map" in {
    intercept[CaseYamlException] {
      ConvertToYml(YMap(YList(YString)).as[String, Any], "abcd")
    }.message should equal ("Expected map from string to list of string, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToYml(YMap(YNullable(YIntCompatible.YBigInt)).as[Array[BigInt], Any], Array.empty[BigInt])
    }.message should equal ("Expected map from string to nullable int-compatible type, got [Lscala.math.BigInt;")
  }

  it should "throw an exception when serializing null as java.util.Map" in {
    intercept[CaseYamlException] {
      ConvertToYml(YMap(YString), null)
    }.message should equal ("Expected map from string to string, got null")
  }

  it should "serialize an implementation of Seq[?] as java.util.List[?]" in {
    val list = List(1.1, 1.2, 1.3)

    val rm = ConvertToYml(YList(YFloatCompatible.YDouble), list).asInstanceOf[java.util.List[Double]]
    rm should have size 3
    rm.get(0) should equal (1.1)
    rm.get(1) should equal (1.2)
    rm.get(2) should equal (1.3)
  }

  it should "throw an exception when serializing null as java.util.List" in {
    intercept[CaseYamlException] {
      ConvertToYml(YList(YString), null)
    }.message should equal ("Expected list of string, got null")
  }

  it should "allow null values to be serialized for nullable entity" in {
    ConvertToYml(YNullable(YString), null)                      should be (null)
    ConvertToYml(YNullable(YIntCompatible.YBigInt), null)       should be (null)
    ConvertToYml(YNullable(YFloatCompatible.YBigDecimal), null) should be (null)
    ConvertToYml(YNullable(YMap(YString)), null)                should be (null)
    ConvertToYml(YNullable(YList(ModelFixture.yentity)), null)  should be (null)
    ConvertToYml(YNullable(ModelFixture.yentity), null)         should be (null)
  }

  it should "throw an exception when serializing YOptional outside of YClassMap" in {
    intercept[CaseYamlException] {
      ConvertToYml(YOptional(YString), Option("123"))
    }.message should equal ("YOptional is not applicable outside of YClassMap")
  }

  it should "allow null values for YStringConverted" in {
    val ysc = YStringConverted[String](s => if (s == null) "<null>" else s, s => s)
    ConvertToYml(ysc, "abc") should equal ("abc")
    ConvertToYml(ysc, null) should equal ("<null>")
  }

  it should "serialize an object to a java.util.Map/java.util.List tree using YClassMap tree" in {
    val m = ConvertToYml(ModelFixture.yentity, ModelFixture.model)
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
      ConvertToYml(ModelFixture.yentity.as[String, Any], "abcd")
    }.message should equal ("Expected cc.cu.tplex.caseyaml.model.ProjectModel, got java.lang.String")
  }

  it should "throw an exception when null is serialized using YClassMap" in {
    intercept[CaseYamlException] {
      ConvertToYml(ModelFixture.yentity, null)
    }.message should equal ("Expected cc.cu.tplex.caseyaml.model.ProjectModel, got null")
  }
}
