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

import cc.cu.tplex.caseyaml.model._
import scala.Some
import cc.cu.tplex.caseyaml.CaseYamlException
import scala.collection.JavaConverters
import java.util

/**
 * Date: 15.07.13
 * Time: 11:06
 */
object ConvertToYml {
  import JavaConverters._

  def apply[Obj, Yml](entity: YEntity[Obj, Yml], obj: Obj): Yml = entity match {
    case yn: YNullable[Obj, Yml] => convertNullable[Obj, Yml](yn.entity, obj)
    case ys: YString.type => convertString(checkNull(ys, obj))
    case yb: YBoolean.type => convertBoolean(checkNull(yb, obj))
    case ic: YIntCompatible[Obj] => convertInt(ic, checkNull(ic, obj))
    case fc: YFloatCompatible[Obj] => convertFloat(fc, checkNull(fc, obj))
    case m: YMap[o, y] => convertMap(m, checkNull(m, obj))
    case l: YList[o, y] => convertList(l, checkNull(l, obj))
    case s: YSet[o, y] => convertSet(s, checkNull(s, obj))
    case YStringConverted(to: Function1[Obj, String], _) => to(obj)
    case st: YSealedTrait[Obj] =>
      if (obj == null || st.clazz.isInstance(obj)) convertSealedTrait(st, checkNull(st, obj))
      else throw CaseYamlException(s"Expected ${st.clazz.getName}, got ${obj.getClass.getName}")
    case cm: YClassMap[Obj] =>
      if (obj == null || cm.clazz.isInstance(obj)) convertClassMap(cm, checkNull(cm, obj))
      else throw CaseYamlException(s"Expected ${cm.clazz.getName}, got ${obj.getClass.getName}")
    case _: YOptional[_, _] =>
      throw CaseYamlException("YOptional is not applicable outside of YClassMap")
    case _: YDefault[_, _] =>
      throw CaseYamlException("YDefault is not applicable outside of YClassMap")
  }

  def checkNull[Obj, Yml](entity: YEntity[Obj, Yml], obj: Obj): Obj = obj match {
    case null => throw CaseYamlException(s"Expected ${entity.objReprName}, got null")
    case _ => obj
  }

  def convertNullable[Obj, Yml](entity: YEntity[Obj, Yml], obj: Obj): Yml = obj match {
    case null => null.asInstanceOf[Yml]
    case _ => apply(entity, obj)
  }

  def convertString[T](obj: T): String = obj match {
    case s: String => s
    case _ => throw CaseYamlException("Expected string, got " + obj.getClass.getName)
  }

  def convertBoolean[T](obj: T): Boolean = obj match {
    case b: Boolean => b
    case _ => throw CaseYamlException("Expected boolean, got " + obj.getClass.getName)
  }

  def convertInt[Obj](ic: YIntCompatible[Obj], obj: Obj): Number = obj match {
    case _: Byte | _: Short | _: Int | _: Long | _: BigInt => ic.toYml(obj)
    case _ => throw CaseYamlException(s"Expected ${ic.objReprName}, got ${obj.getClass.getName}")
  }

  def convertFloat[Obj](fc: YFloatCompatible[Obj], obj: Obj): Number = obj match {
    case _: Float | _: Double | _: BigDecimal => fc.toYml(obj)
    case _ => throw CaseYamlException(s"Expected ${fc.objReprName}, got ${obj.getClass.getName}")
  }

  def convertMap[Obj, Yml, T](entity: YMap[Obj, Yml], obj: T): java.util.Map[String, Yml] = obj match {
    case m: Map[String, Obj] => m.mapValues(v => apply(entity.valueEntity, v)).asJava
    case _ => throw CaseYamlException(s"Expected ${entity.objReprName}, got ${obj.getClass.getName}")
  }

  def convertList[Obj, Yml, T](entity: YList[Obj, Yml], obj: T): java.util.List[Yml] = obj match {
    case s: Seq[Obj] => s.map(v => apply(entity.valueEntity, v)).asJava
    case _ => throw CaseYamlException(s"Expected ${entity.objReprName}, got ${obj.getClass.getName}")
  }

  def convertSet[Obj, Yml, T](entity: YSet[Obj, Yml], obj: T): java.util.Set[Yml] = obj match {
    case s: Set[Obj] => entity.valueEntity match {
      case YNullable(_) => throw CaseYamlException(s"YNullable cannot be used inside YSet")
      case _ => s.map(v => apply(entity.valueEntity, v)).asJava
    }

    case _ => throw CaseYamlException(s"Expected ${entity.objReprName}, got ${obj.getClass.getName}")
  }

  def convertClassMap[T](cm: YClassMap[T], obj: T): java.util.Map[String, Any] =
    cm.entries.map {
      case entry: YEntry[T, o, y] =>
        val (entity, possibleValue) = entry.entity match {
          case yd: YDefault[`o`, `y`] =>
            // Just pass through the defaultable
            yd.entity -> Some(entry.field(obj).get.asInstanceOf[o])
          case yo: YOptional[`o`, `y`] => yo.entity -> (entry.field(obj).get match {
            case Some(value: `o`) => Some(value)
            case None => None
            case null => throw CaseYamlException(s"Expected ${yo.objReprName}, got null")
            case other => throw CaseYamlException(s"Expected ${yo.objReprName}, got " + other.getClass.getName)
          })
          case _ => entry.entity -> Some(entry.field(obj).get.asInstanceOf[o])
        }
        possibleValue map { value => entry.name -> apply[o, y](entity, value) }
    }.flatten.toMap.asJava

  def convertSealedTrait[T](st: YSealedTrait[T], obj: T): java.util.Map[String, Any] = {
    st.subclasses.find(_.clazz.isInstance(obj)) match {
      case Some(entity: YClassMap[T]) =>
        val result = new util.HashMap[String, Any]()
        result.putAll(apply(entity, obj))
        result.put("$type$", entity.clazz.getSimpleName)
        result
      case None =>
        throw CaseYamlException(
          s"Class ${obj.getClass.getName} is not registred as a subclass of sealed trait ${st.clazz.getName}"
        )
    }
  }
}

