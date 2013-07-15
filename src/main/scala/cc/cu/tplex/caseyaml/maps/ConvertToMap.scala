package cc.cu.tplex.caseyaml.maps

import cc.cu.tplex.caseyaml.model._
import cc.cu.tplex.caseyaml.model.YNullable
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
    case YString => convertString(checkNull(obj, "string"))
    case YBoolean => convertBoolean(checkNull(obj, "boolean"))
    case _: YIntCompatible[_] => convertInt(checkNull(obj, "int-compatible type"))
    case _: YFloatCompatible[_] => convertFloat(checkNull(obj, "float-compatible type"))
    case YMap(valueEntity: YEntity[_]) => convertMap(valueEntity, checkNull(obj, "map-compatible type"))
    case YList(valueEntity: YEntity[_]) => convertList(valueEntity, checkNull(obj, "list-compatible type"))
    case YStringConverted(to, _) => to.asInstanceOf[(T) => String](obj)
    case cm @ YClassMap(entries: Seq[YEntry[T, S] forSome {type S}]) =>
      if (cm.clazz.isInstance(obj)) convertClassMap(entries, checkNull(obj, cm.clazz.getName))
      else throw CaseYamlException(s"Expected ${cm.clazz.getName}, got ${obj.getClass.getName}")
  }

  def checkNull[T](obj: T, tpe: String) = obj match {
    case null => throw CaseYamlException(s"Expected $tpe, got null")
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

  def convertMap[T, E](valueEntity: YEntity[E], obj: T): java.util.Map[String, Any] = obj match {
    case m: Map[String, Any] => m.mapValues(v => convert(valueEntity, v)).asJava
    case _ => throw CaseYamlException("Expected map-compatible type, got " + obj.getClass)
  }

  def convertList[T, E](valueEntity: YEntity[E], obj: T): java.util.List[Any] = obj match {
    case s: Seq[Any] => s.map(v => convert(valueEntity, v)).asJava
    case _ => throw CaseYamlException("Expected seq-compatible type, got " + obj.getClass.getName)
  }

  def convertClassMap[T](entries: Iterable[YEntry[T, S] forSome {type S}], obj: T): java.util.Map[String, Any] =
    entries.map {
      case SkipField(_) => None
      case entry => Some(entry.name -> convert(entry.entity, entry.field(obj).get))
    }.flatten.toMap.asJava

  def convertInt(obj: Any): Any = obj match {
    case byte: Byte => byte.toInt
    case short: Short => short.toInt
    case int: Int => int
    case long: Long => long
    case bigint: BigInt => bigint.bigInteger
    case _ => throw CaseYamlException("Expected int-compatible type, got " + obj.getClass.getName)
  }

  def convertFloat(obj: Any): Any = obj match {
    case float: Float => float
    case double: Double => double
    case bigdec: BigDecimal => bigdec.bigDecimal
    case _ => throw CaseYamlException("Expected float-compatible type, got " + obj.getClass.getName)
  }
}

