package cc.cu.tplex.caseyaml.maps

import cc.cu.tplex.caseyaml.model._
import cc.cu.tplex.caseyaml.model.YNullable
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
    case YNullable(valueEntity: YEntity[AnyRef]) => convertNullable(valueEntity, src).asInstanceOf[T]
    case YString => convertString(checkNull(src, "string")).asInstanceOf[T]
    case YBoolean => convertBoolean(checkNull(src, "boolean")).asInstanceOf[T]
    case ic: YIntCompatible[_] => convertInt(ic, checkNull(src, "int, long or java.math.BigInteger")).asInstanceOf[T]
    case fc: YFloatCompatible[_] => convertFloat(fc, checkNull(src, "double")).asInstanceOf[T]
    case YMap(valueEntity: YEntity[_]) => convertMap(valueEntity, checkNull(src, "java.util.Map")).asInstanceOf[T]
    case YList(valueEntity: YEntity[_]) => convertList(valueEntity, checkNull(src, "java.util.List")).asInstanceOf[T]
    case YStringConverted(_, from) => from.asInstanceOf[(String) => T](convertString(src))
    case cm @ YClassMap(_) => convertClassMap(cm, checkNull(src, "java.util.Map"))
  }

  def checkNull[T](obj: T, tpe: String) = obj match {
    case null => throw CaseYamlException(s"Expected $tpe, got null")
    case _ => obj
  }

  def convertNullable[T >: Null](entity: YEntity[T], src: Any): T = src match {
    case null => null
    case _ => convert(entity, src)
  }

  def convertString(src: Any): String = src match {
    case s: String => s
    case _ => throw CaseYamlException("Expected string, got " + src.getClass.getName)
  }

  def convertBoolean(src: Any): Boolean = src match {
    case b: Boolean => b
    case _ => throw CaseYamlException("Expected boolean, got " + src.getClass.getName)
  }

  def convertInt[T](entity: YIntCompatible[T], src: Any): T = src match {
    case int: Int => entity.fromInt(int)
    case long: Long => entity.fromLong(long)
    case bigint: java.math.BigInteger => entity.fromBigInt(bigint)
    case _ => throw CaseYamlException("Expected int, long or java.math.BigInteger, got " + src.getClass.getName)
  }

  def convertFloat[T](entity: YFloatCompatible[T], src: Any): T = src match {
    case double: Double => entity.fromDouble(double)
    case _ => throw CaseYamlException("Expected double, got " + src.getClass.getName)
  }

  def convertMap[T](valueEntity: YEntity[T], src: Any): Map[String, T] = src match {
    case map: java.util.Map[String, _] => map.asScala.mapValues(v => convert(valueEntity, v)).toMap
    case _ => throw CaseYamlException("Expected java.util.Map, got " + src.getClass.getName)
  }

  def convertList[T](valueEntity: YEntity[T], src: Any): Seq[T] = src match {
    case list: java.util.List[T] => list.asScala.map(v => convert(valueEntity, v))
    case _ => throw CaseYamlException("Expected java.util.List, got " + src.getClass.getName)
  }

  def convertClassMap[T](cm: YClassMap[T], src: Any): T = src match {
    case map: java.util.Map[String, Any] =>
      val ctorArgs = cm.entries.map {
        case SkipField(_) => None
        case YFieldEntry(name, _, entity) => Some(convert(entity, map.get("name")))
      }.flatten
      cm.ctormirror(ctorArgs: _*).asInstanceOf[T]

    case _ => throw CaseYamlException("Expected java.util.Map, got " + src.getClass.getName)
  }

//  def convertClassMap[T](cm: YClassMap[T], src: Any): T = src match {
//    case map: java.util.Map[String, _] =>
//      val ctorArgs = cm.entries.map {
//        case SkipField(_) => None
//        case YFieldEntry(name, field, entity) => Some(convert(entity, map.get("name")))
//      }.flatten
//      cm.ctormirror(ctorArgs: _*).asInstanceOf[T]
//    case _ => throw CaseYamlException("Expected java.util.Map, got " + src.getClass.getName)
//  }
}
