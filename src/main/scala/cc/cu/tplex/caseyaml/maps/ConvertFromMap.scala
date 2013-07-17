package cc.cu.tplex.caseyaml.maps

import cc.cu.tplex.caseyaml.model._
import cc.cu.tplex.caseyaml.CaseYamlException
import scala.collection.JavaConverters

/**
 * Date: 15.07.13
 * Time: 11:08
 */
object ConvertFromMap {
  import JavaConverters._

  def convert[T](entity: YEntity[T], src: java.util.Map[String, Any], key: String): T =
    convert(entity, src.get(key).asInstanceOf[java.util.Map[String, Any]])

  def convert[T](entity: YEntity[T], src: Any): T = entity match {
    case YNullable(valueEntity) => convertNullable(valueEntity, src) as entity
    case YString => convertString(checkNull(src, entity)) as entity
    case YBoolean => convertBoolean(checkNull(src, entity)) as entity
    case ic: YIntCompatible[T] => convertInt(ic, checkNull(src, entity))
    case fc: YFloatCompatible[T] => convertFloat(fc, checkNull(src, entity))
    case m: YMap[Any] => convertMap(m, checkNull(src, entity)) as entity
    case l: YList[Any] => convertList(l, checkNull(src, entity)) as entity
    case YStringConverted(_, from: Function1[String, T]) => from(convertString(src))
    case cm: YClassMap[T] => convertClassMap(cm, checkNull(src, entity))
    case _: YOptional[_] =>
      throw CaseYamlException("YOptional is not applicable outside of YClassMap")
  }

  def checkNull[T](obj: T, entity: YEntity[T]): T = obj match {
    case null => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got null")
    case _ => obj
  }

  def convertNullable[T >: Null](entity: YEntity[T], src: Any): T = src match {
    case null => null
    case _ => convert(entity, src)
  }

  def convertString[T](src: T): String = src match {
    case s: String => s
    case _ => throw CaseYamlException("Expected string, got " + src.getClass.getName)
  }

  def convertBoolean[T](src: T): Boolean = src match {
    case b: Boolean => b
    case _ => throw CaseYamlException("Expected boolean, got " + src.getClass.getName)
  }

  def convertInt[T](entity: YIntCompatible[T], src: Any): T = src match {
    case int: Int => entity.fromInt(int)
    case long: Long => entity.fromLong(long)
    case bigint: java.math.BigInteger => entity.fromBigInt(bigint)
    case _ => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got ${src.getClass.getName}")
  }

  def convertFloat[T](entity: YFloatCompatible[T], src: Any): T = src match {
    case double: Double => entity.fromDouble(double)
    case _ => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got ${src.getClass.getName}")
  }

  def convertMap[T](entity: YMap[T], src: Any): Map[String, T] = src match {
    case map: java.util.Map[String, _] => map.asScala.mapValues(v => convert(entity.valueEntity, v)).toMap
    case _ => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got ${src.getClass.getName}")
  }

  def convertList[T](entity: YList[T], src: Any): Seq[T] = src match {
    case list: java.util.List[T] => list.asScala.map(v => convert(entity.valueEntity, v)).toVector
    case _ => throw CaseYamlException(s"Expected ${entity.ymlReprName}, got ${src.getClass.getName}")
  }

  def convertClassMap[T](cm: YClassMap[T], src: Any): T = src match {
    case map: java.util.Map[String, Any] =>
      val ctorArgs = cm.entries.map {
        case SkipField(_) => None
        case YFieldEntry(name, _, entity) =>
          val (realEntity, possibleValue) = entity match {
            case o: YOptional[_] => o.entity -> map.asScala.get(name)
            case _ =>
              entity -> (
                if (map.containsKey(name))
                  Some(map.get(name))
                else
                  throw CaseYamlException(s"Key '$name' is not present in ${cm.clazz.getName} class map")
              )
          }
          possibleValue map (value => convert(realEntity, value))
      }.flatten
      cm.ctormirror(ctorArgs: _*).asInstanceOf[T]

    case _ => throw CaseYamlException("Expected java.util.Map, got " + src.getClass.getName)
  }
}
