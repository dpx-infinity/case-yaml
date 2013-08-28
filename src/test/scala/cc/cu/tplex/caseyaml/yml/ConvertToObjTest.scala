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

package cc.cu.tplex.caseyaml.yml

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import cc.cu.tplex.caseyaml.model._
import cc.cu.tplex.caseyaml.test.CustomMatchers
import java.util
import cc.cu.tplex.caseyaml.CaseYamlException

/**
 * Date: 15.07.13
 * Time: 12:48
 */
class ConvertToObjTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  "ConvertToObj" should "deserialize int-compatible types with upcasting/downcasting, if needed" in {
    val int: java.lang.Integer = 10
    val long: java.lang.Long = 20
    val bigint: java.math.BigInteger = java.math.BigInteger.valueOf(30)

    ConvertToObj(YIntCompatible.YByte, int)      should (be (anInstanceOf[Byte])   and be === 10)
    ConvertToObj(YIntCompatible.YShort, int)     should (be (anInstanceOf[Short])  and be === 10)
    ConvertToObj(YIntCompatible.YInt, int)       should (be (anInstanceOf[Int])    and be === 10)
    ConvertToObj(YIntCompatible.YLong, int)      should (be (anInstanceOf[Long])   and be === 10)
    ConvertToObj(YIntCompatible.YBigInt, int)    should (be (anInstanceOf[BigInt]) and be === 10)

    ConvertToObj(YIntCompatible.YByte, long)     should (be (anInstanceOf[Byte])   and be === 20)
    ConvertToObj(YIntCompatible.YShort, long)    should (be (anInstanceOf[Short])  and be === 20)
    ConvertToObj(YIntCompatible.YInt, long)      should (be (anInstanceOf[Int])    and be === 20)
    ConvertToObj(YIntCompatible.YLong, long)     should (be (anInstanceOf[Long])   and be === 20)
    ConvertToObj(YIntCompatible.YBigInt, long)   should (be (anInstanceOf[BigInt]) and be === 20)

    ConvertToObj(YIntCompatible.YByte, bigint)   should (be (anInstanceOf[Byte])   and be === 30)
    ConvertToObj(YIntCompatible.YShort, bigint)  should (be (anInstanceOf[Short])  and be === 30)
    ConvertToObj(YIntCompatible.YInt, bigint)    should (be (anInstanceOf[Int])    and be === 30)
    ConvertToObj(YIntCompatible.YLong, bigint)   should (be (anInstanceOf[Long])   and be === 30)
    ConvertToObj(YIntCompatible.YBigInt, bigint) should (be (anInstanceOf[BigInt]) and be === 30)
  }

  implicit class EntityConvertible(obj: YEntity[_, _]) {
    def as[Obj, Yml] = obj.asInstanceOf[YEntity[Obj, Yml]]
  }

  it should "throw an exception when int-incompatible object is deserialized as int-compatible" in {
    intercept[CaseYamlException] {
      ConvertToObj(YIntCompatible.YByte.as[Any, String], "abc")
    }.message should equal ("Expected int, long or java.math.BigInteger, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToObj(YIntCompatible.YShort.as[Any, Double], 11.2)
    }.message should equal ("Expected int, long or java.math.BigInteger, got java.lang.Double")

    intercept[CaseYamlException] {
      ConvertToObj(YIntCompatible.YInt.as[Any, Float], 12.3.toFloat)
    }.message should equal ("Expected int, long or java.math.BigInteger, got java.lang.Float")

    intercept[CaseYamlException] {
      ConvertToObj(YIntCompatible.YLong.as[Any, Vector[Any]], Vector.empty)
    }.message should equal ("Expected int, long or java.math.BigInteger, got scala.collection.immutable.Vector")

    intercept[CaseYamlException] {
      ConvertToObj(YIntCompatible.YBigInt.as[Any, util.List[Long]], new util.ArrayList[Long]())
    }.message should equal ("Expected int, long or java.math.BigInteger, got java.util.ArrayList")
  }

  it should "throw an exception when null is deserialized as an int-compatible type" in {
    intercept[CaseYamlException] {
      ConvertToObj(YIntCompatible.YByte, null)
    }.message should equal ("Expected int, long or java.math.BigInteger, got null")

    intercept[CaseYamlException] {
      ConvertToObj(YIntCompatible.YShort, null)
    }.message should equal ("Expected int, long or java.math.BigInteger, got null")

    intercept[CaseYamlException] {
      ConvertToObj(YIntCompatible.YInt, null)
    }.message should equal ("Expected int, long or java.math.BigInteger, got null")

    intercept[CaseYamlException] {
      ConvertToObj(YIntCompatible.YLong, null)
    }.message should equal ("Expected int, long or java.math.BigInteger, got null")

    intercept[CaseYamlException] {
      ConvertToObj(YIntCompatible.YBigInt, null)
    }.message should equal ("Expected int, long or java.math.BigInteger, got null")
  }

  it should "deserialize float-compatible types with upcasting/downcasting, if needed" in {
    val double: java.lang.Double = 11.1

    ConvertToObj(YFloatCompatible.YFloat, double) should (
      be (anInstanceOf[Float]) and be === 11.1.toFloat
      )
    ConvertToObj(YFloatCompatible.YDouble, double) should (
      be (anInstanceOf[Double]) and be === 11.1
      )
    ConvertToObj(YFloatCompatible.YBigDecimal, double) should (
      be (anInstanceOf[BigDecimal]) and be === BigDecimal(11.1)
      )
  }

  it should "throw an exception when float-incompatible object is deserialized as float-compatible" in {
    intercept[CaseYamlException] {
      ConvertToObj(YFloatCompatible.YFloat.as[Any, String], "abc")
    }.message should equal ("Expected double, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToObj(YFloatCompatible.YDouble.as[Any, Int], 10: Int)
    }.message should equal ("Expected double, got java.lang.Integer")

    intercept[CaseYamlException] {
      ConvertToObj(YFloatCompatible.YBigDecimal.as[Any, Vector[Any]], Vector.empty)
    }.message should equal ("Expected double, got scala.collection.immutable.Vector")
  }

  it should "throw an exception when null is deserialized as float-compatible type" in {
    intercept[CaseYamlException] {
      ConvertToObj(YFloatCompatible.YFloat, null)
    }.message should equal ("Expected double, got null")

    intercept[CaseYamlException] {
      ConvertToObj(YFloatCompatible.YDouble, null)
    }.message should equal ("Expected double, got null")

    intercept[CaseYamlException] {
      ConvertToObj(YFloatCompatible.YBigDecimal, null)
    }.message should equal ("Expected double, got null")
  }

  it should "deserialize strings directly" in {
    ConvertToObj(YString, "abcd") should equal ("abcd")
    ConvertToObj(YString, "") should equal ("")
  }

  it should "throw an exception when non-string object is deserialized as a string" in {
    intercept[CaseYamlException] {
      ConvertToObj(YString.as[Any, Int], 123: Int)
    }.message should equal ("Expected string, got java.lang.Integer")

    intercept[CaseYamlException] {
      ConvertToObj(YString.as[Any, Array[Char]], Array.empty[Char])
    }.message should equal ("Expected string, got [C")
  }

  it should "throw an exception when null is deserialized as a string" in {
    intercept[CaseYamlException] {
      ConvertToObj(YString, null)
    }.message should equal ("Expected string, got null")
  }

  it should "deserialize booleans directly" in {
    ConvertToObj(YBoolean, java.lang.Boolean.TRUE) should equal (true)
    ConvertToObj(YBoolean, java.lang.Boolean.FALSE) should equal (false)
  }

  it should "throw an exception when non-boolean object is deserialized as a boolean" in {
    intercept[CaseYamlException] {
      ConvertToObj(YBoolean.as[Any, Int], 0: Int)
    }.message should equal ("Expected boolean, got java.lang.Integer")

    intercept[CaseYamlException] {
      ConvertToObj(YBoolean.as[Any, Array[Boolean]], Array(true, true, false))
    }.message should equal ("Expected boolean, got [Z")
  }

  it should "throw an exception when null is deserialized as a boolean" in {
    intercept[CaseYamlException] {
      ConvertToObj(YBoolean, null)
    }.message should equal ("Expected boolean, got null")
  }

  it should "deserialize a map from string to other type with conversion" in {
    {
      val map = new util.HashMap[String, Number]()
      map.put("a", 11: Int)
      map.put("b", 12: Long)
      map.put("c", java.math.BigInteger.valueOf(13))

      val cmap = ConvertToObj(YMap(YIntCompatible.YInt), map)
      cmap should have size 3
      cmap("a") should equal (11: Int)
      cmap("b") should equal (12: Int)
      cmap("c") should equal (13: Int)
    }

    {
      val map = new util.HashMap[String, java.lang.Boolean]()
      map.put("1", true)
      map.put("2", false)
      map.put("4", false)

      val cmap = ConvertToObj(YMap(YBoolean), map)
      cmap should have size 3
      cmap("1") should be (true)
      cmap("2") should be (false)
      cmap("4") should be (false)
    }

    {
      val map = new util.HashMap[String, String]()
      map.put("a", "b")
      map.put("c", "d")
      map.put("z", null)

      val cmap = ConvertToObj(YMap(YNullable(YString)), map)
      cmap should have size 3
      cmap("a") should equal ("b")
      cmap("c") should equal ("d")
      cmap("z") should be (null)
    }
  }

  it should "throw an exception when not java.util.Map object is deserialized as a map" in {
    intercept[CaseYamlException] {
      ConvertToObj(YMap(YIntCompatible.YInt).as[Any, String], "avc")
    }.message should equal ("Expected java.util.Map from string to int, long or java.math.BigInteger, got java.lang.String")

    intercept[CaseYamlException] {
      ConvertToObj(YMap(YBoolean).as[Any, Array[Short]], Array(10.toShort))
    }.message should equal ("Expected java.util.Map from string to boolean, got [S")
  }

  it should "throw an exception when null is deserialized as a map" in {
    intercept[CaseYamlException] {
      ConvertToObj(YMap(YFloatCompatible.YBigDecimal), null)
    }.message should equal ("Expected java.util.Map from string to double, got null")
  }

  it should "deserialize a list with conversion" in {
    {
      val list = new util.ArrayList[Number]()
      list.add(1: Int)
      list.add(2: Long)
      list.add(java.math.BigInteger.valueOf(3))

      val clist = ConvertToObj(YList(YIntCompatible.YLong), list)
      clist should have size 3
      clist(0) should equal (1: Long)
      clist(1) should equal (2: Long)
      clist(2) should equal (3: Long)
    }

    {
      val list = new util.ArrayList[String]()
      list.add("a")
      list.add(null)
      list.add("b")

      val clist = ConvertToObj(YList(YNullable(YString)), list)
      clist should have size 3
      clist(0) should equal ("a")
      clist(1) should be (null)
      clist(2) should equal ("b")
    }
  }

  it should "throw an exception when not java.util.List object is deserialized as a list" in {
    intercept[CaseYamlException] {
      ConvertToObj(YList(YBoolean).as[Any, Array[Boolean]], Array(true, false))
    }.message should equal ("Expected java.util.List of boolean, got [Z")

    intercept[CaseYamlException] {
      ConvertToObj(YList(YIntCompatible.YByte).as[Any, String], "abcd")
    }.message should equal ("Expected java.util.List of int, long or java.math.BigInteger, got java.lang.String")
  }

  it should "throw an exception when null is deserialized as a list" in {
    intercept[CaseYamlException] {
      ConvertToObj(YList(YFloatCompatible.YBigDecimal), null)
    }.message should equal ("Expected java.util.List of double, got null")
  }

  it should "deserialize a set with conversion" in {
    {
      val set = new util.HashSet[Number]()
      set.add(1: Int)
      set.add(2: Long)
      set.add(java.math.BigInteger.valueOf(3))

      val cset = ConvertToObj(YSet(YIntCompatible.YLong), set)
      cset should have size 3
      cset should contain (1: Long)
      cset should contain (2: Long)
      cset should contain (3: Long)
    }

    {
      val set = new util.HashSet[String]()
      set.add("a")
      set.add("b")

      val clist = ConvertToObj(YSet(YString), set)
      clist should have size 2
      clist should contain ("a")
      clist should contain ("b")
    }
  }

  it should "throw an exception when not java.util.Set object is deserialized as a set" in {
    intercept[CaseYamlException] {
      ConvertToObj(YSet(YBoolean).as[Any, Array[Boolean]], Array(true, false))
    }.message should equal ("Expected java.util.Set of boolean, got [Z")

    intercept[CaseYamlException] {
      ConvertToObj(YSet(YIntCompatible.YByte).as[Any, String], "abcd")
    }.message should equal ("Expected java.util.Set of int, long or java.math.BigInteger, got java.lang.String")
  }

  it should "throw an exception when null is deserialized as a set" in {
    intercept[CaseYamlException] {
      ConvertToObj(YSet(YFloatCompatible.YBigDecimal), null)
    }.message should equal ("Expected java.util.Set of double, got null")
  }

  it should "throw an exception when YNullable is used inside YSet" in {
    intercept[CaseYamlException] {
      ConvertToObj(YSet(YNullable(YIntCompatible.YBigInt)), new util.HashSet[java.lang.Number]())
    }.message should equal ("YNullable cannot be used inside YSet")
  }

  it should "allow null values to be deserialized for nullable entity" in {
    ConvertToObj(YNullable(YString), null)                      should be (null)
    ConvertToObj(YNullable(YIntCompatible.YBigInt), null)       should be (null)
    ConvertToObj(YNullable(YFloatCompatible.YBigDecimal), null) should be (null)
    ConvertToObj(YNullable(YMap(YString)), null)                should be (null)
    ConvertToObj(YNullable(YList(ModelFixture.manualEntity)), null)  should be (null)
    ConvertToObj(YNullable(ModelFixture.manualEntity), null)         should be (null)
  }

  it should "throw an exception when deserializing YOptional outside of YClassMap" in {
    intercept[CaseYamlException] {
      ConvertToObj(YOptional(YBoolean), java.lang.Boolean.FALSE)
    }.message should equal ("YOptional is not applicable outside of YClassMap")
  }

  it should "throw an exception when deserializing YDefault outside of YClassMap" in {
    intercept[CaseYamlException] {
      ConvertToObj(YDefault(YBoolean, true), java.lang.Boolean.FALSE)
    }.message should equal ("YDefault is not applicable outside of YClassMap")
  }

  implicit class Kestrelable[T](obj: T) {
    def tap(f: T => Any): T = { f(obj); obj }
  }

  val ymlObjMap = new util.HashMap[String, Any]()
    .tap(_.put("id", "projectId"))
    .tap(_.put("name", "name"))
    .tap(_.put("enabled", true))
    .tap(_.put("count", 123: java.lang.Long))
    .tap(_.put("fraction", 11.1: java.lang.Double))
    .tap(_.put("shape", new util.HashMap[String, Any]()
      .tap(_.put("$type$", "Circle"))
      .tap(_.put("x", 0.0))
      .tap(_.put("y", 0.0))
      .tap(_.put("r", 0.0))
    ))
    .tap(_.put("plugins", new util.HashMap[String, Any]()
      .tap(_.put("plugin1", new util.HashMap[String, Any]()
        .tap(_.put("id", "pluginId1"))
        .tap(_.put("pluginName", "Plugin 1"))
        .tap(_.put("data", 0.999: java.lang.Double))
        .tap(_.put("status", "inactive"))
        .tap(_.put("dependencies", new util.ArrayList[String]().tap(_.add("id1")).tap(_.add("id2"))))
       ))
       .tap(_.put("plugin2", new util.HashMap[String, Any]()
         .tap(_.put("id", "pluginId2"))
         .tap(_.put("pluginName", "Plugin 2"))
         .tap(_.put("dependencies", util.Collections.emptyList[String]))
       ))
    ))

  // The only difference is "pluginName" -> "name" key change,
  // because reflective model generator cannot handle renamed keys
  val ymlObjMap2 = new util.HashMap[String, Any]()
    .tap(_.put("id", "projectId"))
    .tap(_.put("name", "name"))
    .tap(_.put("enabled", true))
    .tap(_.put("count", 123: java.lang.Long))
    .tap(_.put("fraction", 11.1: java.lang.Double))
    .tap(_.put("shape", new util.HashMap[String, Any]()
      .tap(_.put("$type$", "Circle"))
      .tap(_.put("x", 0.0))
      .tap(_.put("y", 0.0))
      .tap(_.put("r", 0.0))
    ))
    .tap(_.put("plugins", new util.HashMap[String, Any]()
      .tap(_.put("plugin1", new util.HashMap[String, Any]()
        .tap(_.put("id", "pluginId1"))
        .tap(_.put("name", "Plugin 1"))
        .tap(_.put("data", 0.999: java.lang.Double))
        .tap(_.put("status", "inactive"))
        .tap(_.put("dependencies", new util.ArrayList[String]().tap(_.add("id1")).tap(_.add("id2"))))
      ))
      .tap(_.put("plugin2", new util.HashMap[String, Any]()
        .tap(_.put("id", "pluginId2"))
        .tap(_.put("name", "Plugin 2"))
        .tap(_.put("dependencies", util.Collections.emptyList[String]))
       ))
    ))

  def checkObj(obj: ProjectModel) {
    obj.id should equal (ModelId("projectId"))
    obj.name should equal ("name")
    obj.enabled should be (true)
    obj.count should equal (123)
    obj.fraction should equal (11.1)
    obj.plugins should have size 2

    {
      val plugin = obj.plugins("plugin1")
      plugin.id should equal (ModelId("pluginId1"))
      plugin.name should equal ("Plugin 1")
      plugin.data should equal (Some(BigDecimal(0.999)))
      plugin.status should equal ("inactive")
      plugin.dependencies should have size 2
      plugin.dependencies(0) should equal (ModelId("id1"))
      plugin.dependencies(1) should equal (ModelId("id2"))
    }

    {
      val plugin = obj.plugins("plugin2")
      plugin.id should equal (ModelId("pluginId2"))
      plugin.name should equal ("Plugin 2")
      plugin.data should be (None)
      plugin.status should equal ("active")
      plugin.dependencies should be ('empty)
    }
  }

  it should "deserialize an object from java.util.Map[String, Any] using YClassMap constructed manually" in {
    val obj = ConvertToObj(ModelFixture.manualEntity, ymlObjMap)
    checkObj(obj)
  }

  it should "deserialize an object from java.util.Map[String, Any] using YClassMap constructed reflectively" in {
    val obj = ConvertToObj(ModelFixture.reflectiveEntity, ymlObjMap2)
    checkObj(obj)
  }

  it should "throw an exception when an object of incorrect class is deserialized using YClassMap" in {
    intercept[CaseYamlException] {
      ConvertToObj(ModelFixture.manualEntity.as[Any, String], "abcd")
    }.message should equal (
      "Expected java.util.Map from string to any for cc.cu.tplex.caseyaml.model.ProjectModel, got java.lang.String"
    )

    intercept[CaseYamlException] {
      ConvertToObj(ModelFixture.reflectiveEntity.as[Any, String], "abcd")
    }.message should equal (
      "Expected java.util.Map from string to any for cc.cu.tplex.caseyaml.model.ProjectModel, got java.lang.String"
    )
  }

  it should "throw an exception when null is deserialized using YClassMap" in {
    intercept[CaseYamlException] {
      ConvertToObj(ModelFixture.manualEntity, null)
    }.message should equal (
      "Expected java.util.Map from string to any for cc.cu.tplex.caseyaml.model.ProjectModel, got null"
    )

    intercept[CaseYamlException] {
      ConvertToObj(ModelFixture.reflectiveEntity, null)
    }.message should equal (
      "Expected java.util.Map from string to any for cc.cu.tplex.caseyaml.model.ProjectModel, got null"
    )
  }
}
