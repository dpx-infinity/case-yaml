package cc.cu.tplex.caseyaml.maps

import cc.cu.tplex.caseyaml.model._
import scala.Some
import cc.cu.tplex.caseyaml.CaseYamlException
import scala.collection.JavaConverters

/**
 * Date: 15.07.13
 * Time: 11:06
 */
object ConvertToMap {
  import JavaConverters._

  def convert[T](entity: YEntity[T], obj: T): Any = entity match {
    case YNullable(valueEntity: YEntity[Any]) => convertNullable(valueEntity, obj)
    case YString => convertString(checkNull(obj, entity))
    case YBoolean => convertBoolean(checkNull(obj, entity))
    case ic: YIntCompatible[T] => convertInt(ic, checkNull(obj, ic))
    case fc: YFloatCompatible[T] => convertFloat(fc, checkNull(obj, fc))
    case m: YMap[Any] => convertMap(m, checkNull(obj, m))
    case l: YList[Any] => convertList(l, checkNull(obj, l))
    case YStringConverted(to: Function1[T, String], _) => to(obj)
    case cm: YClassMap[T] =>
      if (cm.clazz.isInstance(obj)) convertClassMap(cm, checkNull(obj, cm))
      else throw CaseYamlException(s"Expected ${cm.clazz.getName}, got ${obj.getClass.getName}")
    case _: YOptional[_] =>
      throw CaseYamlException("YOptional is not applicable outside of YClassMap")
  }

  def checkNull[T](obj: T, entity: YEntity[T]): T = obj match {
    case null => throw CaseYamlException(s"Expected ${entity.objReprName}, got null")
    case _ => obj
  }

  def convertNullable[T](entity: YEntity[T], obj: T) = obj match {
    case null => null
    case _ => convert(entity, obj)
  }

  def convertString[T](obj: T): String = obj match {
    case s: String => s
    case _ => throw CaseYamlException("Expected string, got " + obj.getClass.getName)
  }

  def convertBoolean[T](obj: T): Boolean = obj match {
    case b: Boolean => b
    case _ => throw CaseYamlException("Expected boolean, got " + obj.getClass.getName)
  }

  def convertMap[T, E](entity: YMap[E], obj: T): java.util.Map[String, Any] = obj match {
    case m: Map[String, E] => m.mapValues(v => convert(entity.valueEntity, v)).asJava
    case _ => throw CaseYamlException(s"Expected ${entity.objReprName}, got ${obj.getClass.getName}")
  }

  def convertList[T, E](entity: YList[E], obj: T): java.util.List[Any] = obj match {
    case s: Seq[E] => s.map(v => convert(entity.valueEntity, v)).asJava
    case _ => throw CaseYamlException(s"Expected ${entity.objReprName}, got ${obj.getClass.getName}")
  }

  def convertClassMap[T](cm: YClassMap[T], obj: T): java.util.Map[String, Any] =
    cm.entries.map {
      case SkipField(_) => None
      case entry =>
        val (entity, possibleValue) = entry.entity match {
          case yo: YOptional[_] => yo.entity -> (entry.field(obj).get match {
            case Some(value) => Some(value)
            case None => None
            case null => throw CaseYamlException(s"Expected ${yo.objReprName}, got null")
            case other => throw CaseYamlException(s"Expected ${yo.objReprName}, got " + other.getClass.getName)
          })
          case _ => entry.entity -> Some(entry.field(obj).get)
        }
        possibleValue map { value => entry.name -> convert(entity, value) }
    }.flatten.toMap.asJava

  def convertInt[T](ic: YIntCompatible[T], obj: T): Any = obj match {
    case byte: Byte => byte.toInt
    case short: Short => short.toInt
    case int: Int => int
    case long: Long => long
    case bigint: BigInt => bigint.bigInteger
    case _ => throw CaseYamlException(s"Expected ${ic.objReprName}, got ${obj.getClass.getName}")
  }

  def convertFloat[T](fc: YFloatCompatible[T], obj: Any): Any = obj match {
    case float: Float => float
    case double: Double => double
    case bigdec: BigDecimal => bigdec.bigDecimal
    case _ => throw CaseYamlException(s"Expected ${fc.objReprName}, got ${obj.getClass.getName}")
  }
}

