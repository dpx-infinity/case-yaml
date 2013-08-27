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
import cc.cu.tplex.caseyaml.CaseYamlException
import scala.collection.JavaConverters

/**
 * Date: 15.07.13
 * Time: 11:08
 */
object ConvertToObj {
  import JavaConverters._

  def apply[Obj, Yml](entity: YEntity[Obj, Yml], obj: Any, key: String): Obj =
    obj match {
      case m: java.util.Map[String, Any] => apply(entity, m.get(key).asInstanceOf[Yml])
      case _ => throw CaseYamlException(s"Expected java.util.Map[String, Any], got ${obj.getClass.getName}")
    }


  def apply[Obj, Yml](entity: YEntity[Obj, Yml], yml: Yml): Obj = entity match {
    case yn: YNullable[Obj, Yml] => convertNullable[Obj, Yml](yn.entity, yml)
    case ys: YString.type => convertString(checkNull(ys, yml))
    case yb: YBoolean.type => convertBoolean(checkNull(yb, yml))
    case ic: YIntCompatible[Obj] => convertInt(ic, checkNull(ic, yml))
    case fc: YFloatCompatible[Obj] => convertFloat(fc, checkNull(fc, yml))
    case m: YMap[o, y] => convertMap(m, checkNull(m, yml))
    case l: YList[o, y] => convertList(l, checkNull(l, yml))
    case s: YSet[o, y] => convertSet(s, checkNull(s, yml))
    case YStringConverted(_, from: Function1[String, Obj]) => from(convertString(yml))
    case cm: YClassMap[Obj] => convertClassMap(cm, checkNull(cm, yml))
    case _: YOptional[_, _] =>
      throw CaseYamlException("YOptional is not applicable outside of YClassMap")
    case _: YDefault[_, _] =>
      throw CaseYamlException("YDefault is not applicable outside of YClassMap")
  }

  def checkNull[Obj, Yml](entity: YEntity[Obj, Yml], obj: Yml): Yml = obj match {
    case null => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got null")
    case _ => obj
  }

  def convertNullable[Obj, Yml](entity: YEntity[Obj, Yml], src: Yml): Obj = src match {
    case null => null.asInstanceOf[Obj]
    case _ => apply(entity, src)
  }

  def convertString[T](src: T): String = src match {
    case s: String => s
    case _ => throw CaseYamlException("Expected string, got " + src.getClass.getName)
  }

  def convertBoolean[T](src: T): Boolean = src match {
    case b: Boolean => b
    case _ => throw CaseYamlException("Expected boolean, got " + src.getClass.getName)
  }

  def convertInt[Obj, T](entity: YIntCompatible[Obj], src: T): Obj = src match {
    case n: Number => n match {
      case _: java.lang.Integer | _: java.lang.Long | _: java.math.BigInteger => entity.toObj(n)
      case _ => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got ${src.getClass.getName}")
    }
    case _ => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got ${src.getClass.getName}")
  }

  def convertFloat[Obj, T](entity: YFloatCompatible[Obj], src: T): Obj = src match {
    case n: java.lang.Double => entity.toObj(n)
    case _ => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got ${src.getClass.getName}")
  }

  def convertMap[Obj, Yml, T](entity: YMap[Obj, Yml], src: T): Map[String, Obj] = src match {
    case map: java.util.Map[String, Yml] => map.asScala.mapValues(v => apply(entity.valueEntity, v)).toMap
    case _ => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got ${src.getClass.getName}")
  }

  def convertList[Obj, Yml, T](entity: YList[Obj, Yml], src: T): Seq[Obj] = src match {
    case list: java.util.List[Yml] => list.asScala.map(v => apply(entity.valueEntity, v)).toVector
    case _ => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got ${src.getClass.getName}")
  }

  def convertSet[Obj, Yml, T](entity: YSet[Obj, Yml], src: T): Set[Obj] = src match {
    case set: java.util.Set[Yml] => entity.valueEntity match {
      case YNullable(_) => throw CaseYamlException(s"YNullable cannot be used inside YSet")
      case _ => set.asScala.map(v => apply(entity.valueEntity, v)).toSet
    }
    case _ => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got ${src.getClass.getName}")
  }

  def convertClassMap[Obj, T](cm: YClassMap[Obj], src: T): Obj = src match {
    case map: java.util.Map[String, Any] =>
      val ctorArgs = cm.entries.map {
        case YFieldEntry(name, _, entity: YEntity[o, y]) =>
          entity match {
            case o: YOptional[io, `y`] =>
              map.asScala.get(name).map { case value: y => apply(o.entity, value) }
            case d: YDefault[`o`, `y`] =>
              map.asScala.get(name).map { case value: y => apply(d.entity, value) }.getOrElse(d.default)
            case _ =>
              if (map.containsKey(name))
                apply(entity, map.get(name).asInstanceOf[y])
              else
                throw CaseYamlException(s"Key '$name' is not present in ${cm.clazz.getName} class map")
          }
      }
      cm.ctormirror(ctorArgs: _*).asInstanceOf[Obj]

    case _ => throw CaseYamlException(s"Expected ${cm.ymlReprName}, got " + src.getClass.getName)
  }
}
