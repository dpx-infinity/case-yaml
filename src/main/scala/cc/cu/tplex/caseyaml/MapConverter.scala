package cc.cu.tplex.caseyaml

import cc.cu.tplex.caseyaml.model._
import scala.collection.JavaConverters

/**
 * Date: 13.07.13
 * Time: 22:11
 *
 * @author Vladimir Matveev
 */
private[caseyaml] object MapConverter {
  import JavaConverters._

  def checkNull[T](obj: T, tpe: String) = obj match {
    case null => throw CaseYamlException(s"Requested $tpe, got null")
    case _ => obj
  }

  object ToMap {
    def convert[T](entity: YEntity[T], obj: T): Any = entity match {
      case YNullable(valueEntity: YEntity[T]) => convertNullable(valueEntity, obj)
      case YString => convertString(checkNull(obj, "string"))
      case YBoolean => convertBoolean(checkNull(obj, "boolean"))
      case YIntCompatible() => convertInt(checkNull(obj, "int-compatible type"))
      case YFloatCompatible() => convertFloat(checkNull(obj, "float-compatible type"))
      case YMap(valueEntity: YEntity[_]) => convertMap(valueEntity, checkNull(obj, "map-compatible type"))
      case YList(valueEntity: YEntity[_]) => convertList(valueEntity, checkNull(obj, "list-compatible type"))
      case YStringConverted(to, _) => to.asInstanceOf[(T) => String](obj)
      case cm @ YClassMap(entries @ _*) =>
        if (cm.clazz.isInstance(obj)) convertClassMap(entries, checkNull(obj, cm.clazz.getName))
        else throw CaseYamlException(s"Requested ${cm.clazz.getName}, got ${obj.getClass.getName}")
    }

    def convertNullable[T](entity: YEntity[T], obj: T) = obj match {
      case null => null
      case _ => convert(entity, obj)
    }

    def convertString[T](obj: T): String = obj match {
      case s: String => s
      case _ => throw CaseYamlException("Requested string, got " + obj.getClass.getName)
    }

    def convertBoolean[T](obj: T): Boolean = obj match {
      case b: Boolean => b
      case _ => throw CaseYamlException("Requested boolean, got " + obj.getClass.getName)
    }

    def convertMap[T, E](valueEntity: YEntity[E], obj: T): java.util.Map[String, Any] = obj match {
      case m: Map[String, Any] => m.mapValues(convert(valueEntity, _)).asJava
      case _ => throw CaseYamlException("Requested map-compatible type, got " + obj.getClass)
    }

    def convertList[T, E](valueEntity: YEntity[E], obj: T): java.util.List[Any] = obj match {
      case s: Seq[Any] => s.map(convert(valueEntity, _)).asJava
      case _ => throw CaseYamlException("Requested seq-compatible type, got " + obj.getClass.getName)
    }

    def convertClassMap[T](entries: Iterable[YEntry[T, _]], obj: T): java.util.Map[String, Any] =
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
      case _ => throw CaseYamlException("Requested int-compatible type, got " + obj.getClass.getName)
    }

    def convertFloat(obj: Any): Any = obj match {
      case float: Float => float
      case double: Double => double
      case bigdec: BigDecimal => bigdec.bigDecimal
      case _ => throw CaseYamlException("Requested float-compatible type, got " + obj.getClass.getName)
    }
  }

  object FromMap {
    def convert[T](entity: YEntity[T], src: java.util.Map[String, Any], key: String) =
      convert(entity, src.get(key).asInstanceOf[java.util.Map[String, Any]])


    def convert[T](entity: YEntity[T], src: Any): T = {
      case YNullable(valueEntity: YEntity[_ <: AnyRef]) => convertNullable(valueEntity, src)
      case YString => convertString(checkNull(src, "string"))
      case YBoolean => convertBoolean(checkNull(src, "boolean"))
      case YIntCompatible() => convertInt(checkNull(src, "int-compatible type"))
      case YFloatCompatible() => convertFloat(checkNull(src, "float-compatible type"))
      case YMap(valueEntity) => convertMap(valueEntity, checkNull(src, "map-compatible type"))
      case YList(valueEntity) => convertList(valueEntity, checkNull(src, "list-compatible type"))
      case YStringConverted(_, from) => from.asInstanceOf[(String) => T](convertString(src))
      case cm @ YClassMap(entries @ _*) =>
        if (cm.clazz.isInstance(obj)) convertClassMap(entries, checkNull(obj, cm.clazz.getName))
        else throw CaseYamlException(s"Requested ${cm.clazz.getName}, got ${obj.getClass.getName}")
    }

    def convertNullable[T <: AnyRef](entity: YEntity[T], src: Any): T = src match {
      case null => null
      case _ => convert(entity, src)
    }

    def convertString(src: Any): String = src match {
      case s: String => s
      case _ => throw CaseYamlException("Requested string, got " + src.getClass.getName)
    }

    def convertBoolean(src: Any): Boolean = src match {
      case b: Boolean => b
      case _ => throw CaseYamlException("Requested boolean, got " + src.getClass.getName)
    }

    def convertInt[T](entity: YIntCompatible[T], src: Any): T = {

    }
  }
}
