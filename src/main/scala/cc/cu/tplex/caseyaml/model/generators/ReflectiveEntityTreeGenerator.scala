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

package cc.cu.tplex.caseyaml.model.generators

import scala.reflect.runtime.universe._
import cc.cu.tplex.caseyaml.model._
import cc.cu.tplex.caseyaml.model.YClassMap
import cc.cu.tplex.caseyaml.CaseYamlException
import scala.Some
import scala.collection.mutable.ArrayBuffer
import scala.reflect.api.{Universe, TypeCreator}
import scala.reflect.api

/**
 * Date: 20.07.13
 * Time: 15:01
 *
 * @author Vladimir Matveev
 */
class ReflectiveEntityTreeGenerator[Obj: TypeTag] {
  import ReflectiveEntityTreeGenerator._

  private final val mirror = typeTag[Obj].mirror

  private final val stringConvertedTypes = ArrayBuffer[StringConvertedField[_]]()

  def withStringConvertedField[T: TypeTag](field: StringConvertedField[T]): ReflectiveEntityTreeGenerator[Obj] = {
    stringConvertedTypes += field
    this
  }

  def withStringConvertedField[T: TypeTag](toStr: T => String,
                                           fromStr: String => T): ReflectiveEntityTreeGenerator[Obj] =
    withStringConvertedField(StringConvertedField(toStr, fromStr))

  def generate[Yml]: YEntity[Obj, Yml] = {
    val tpe = typeOf[Obj]
    generate(tpe).asInstanceOf[YEntity[Obj, Yml]]
  }

  def generateClass = generate.asInstanceOf[YClassMap[Obj]]

  private def generate(tpe: Type) = generator(tpe) match {
    case Some(entity) => entity
    case None => throw CaseYamlException("Reached the end of type generators chain for type " + tpe)
  }

  private final val generator: Type => Option[YEntity[_, _]] =
    generateIntCompatible _ +>
    generateFloatCompatible +>
    generateString +>
    generateBoolean +>
    generateOptional +>
    generateList +>
    generateSet +>
    generateMap +>
    generateStringConverted +>
    generateClassMap

  private final def firstTypeParam(tpe: Type) =
    tpe.asInstanceOf[TypeRefApi].args.head

  private final def secondTypeParam(tpe: Type) =
    tpe.asInstanceOf[TypeRefApi].args.tail.head

  private def generateStringConverted(tpe: Type): Option[YEntity[_, _]] =
    stringConvertedTypes.find(_.tpe =:= tpe) map {
      case StringConvertedField(toStr, fromStr) => YStringConverted(toStr, fromStr)
    }

  private def generateMap(tpe: Type): Option[YEntity[_, _]] =
    if (tpe <:< typeOf[Map[_, _]])
      Some(YMap(generate(secondTypeParam(tpe.baseType(typeOf[Map[_, _]].typeSymbol)))))
    else
      None

  private def generateSet(tpe: Type): Option[YEntity[_, _]] =
    if (tpe <:< typeOf[Set[_]])
      Some(YSet(generate(firstTypeParam(tpe.baseType(typeOf[Set[_]].typeSymbol)))))
    else
      None

  private def generateList(tpe: Type): Option[YEntity[_, _]] =
    if (tpe <:< typeOf[Seq[_]])
      Some(YList(generate(firstTypeParam(tpe.baseType(typeOf[Seq[_]].typeSymbol)))))
    else
      None

  private def generateOptional(tpe: Type): Option[YEntity[_, _]] =
    if (tpe <:< typeOf[Option[_]])
      Some(YOptional(generate(firstTypeParam(tpe))))
    else
      None

  private def generateBoolean(tpe: Type): Option[YEntity[_, _]] =
    if (tpe =:= typeOf[Boolean]) Some(YBoolean) else None

  private def generateString(tpe: Type): Option[YEntity[_, _]] =
    if (tpe =:= typeOf[String]) Some(YString) else None

  private def generateForEntry(idx: Int, param: Symbol, classTpe: Type): YEntity[_, _] =
    if (param.asTerm.isParamWithDefault) {
      val moduleMirror = mirror.reflectModule(classTpe.typeSymbol.asClass.companionSymbol.asModule)

      val defaultName = {
        import scala.reflect.internal.{Definitions, SymbolTable, StdNames}
        val cu = scala.reflect.runtime.universe.asInstanceOf[Definitions with SymbolTable with StdNames]
        cu.nme.defaultGetterName(cu.nme.CONSTRUCTOR, idx+1).asInstanceOf[Name]
//        newTermName("$lessinit$greater$default$" + (idx + 1))
      }
      val defaultSymbol = moduleMirror.symbol.typeSignature.declaration(defaultName)

      val defaultMethod = mirror.reflect(moduleMirror.instance).reflectMethod(defaultSymbol.asMethod)
      val defaultValue = defaultMethod.apply()

      YDefault(generate(param.typeSignature).asInstanceOf[YEntity[Any, Any]], defaultValue)
    } else generate(param.typeSignature)

  // The following is just a black magic I don't really understand, but it seems to work
  private def generateClassMap(tpe: Type): Option[YEntity[_, _]] = if (tpe.typeSymbol.asClass.isCaseClass) Some {
    val List(params) = tpe.declaration(nme.CONSTRUCTOR).asMethod.paramss
    val entries: Seq[YEntry[_, _, _]] = params.zipWithIndex map { case (param, idx) =>
      import cc.cu.tplex.caseyaml.model._
      NamedField(param.name.toString).createEntry(generateForEntry(idx, param, tpe), mirror, tpe)
    }
    YClassMap(entries: _*)(TypeTag(mirror, new TypeCreator {
      def apply[U <: Universe with Singleton](m: api.Mirror[U]) =
        if (m eq mirror) tpe.asInstanceOf[U # Type]
        else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
    }))
  } else None

  private final val intCompatibles = Seq[(Type, YEntity[_, _])](
    typeOf[Byte] -> YIntCompatible.YByte,
    typeOf[Short] -> YIntCompatible.YShort,
    typeOf[Int] -> YIntCompatible.YInt,
    typeOf[Long] -> YIntCompatible.YLong,
    typeOf[BigInt] -> YIntCompatible.YBigInt
  )
  private def generateIntCompatible(tpe: Type): Option[YEntity[_, _]] = intCompatibles get tpe

  private final val floatCompatibles = Seq[(Type, YEntity[_, _])](
    typeOf[Float] -> YFloatCompatible.YFloat,
    typeOf[Double] -> YFloatCompatible.YDouble,
    typeOf[BigDecimal] -> YFloatCompatible.YBigDecimal
  )
  private def generateFloatCompatible(tpe: Type): Option[YEntity[_, _]] = floatCompatibles get tpe
}

case class StringConvertedField[T: TypeTag](toStr: T => String, fromStr: String => T) {
  val tpe = typeOf[T]
}

private object ReflectiveEntityTreeGenerator {
  implicit class TypeLookable[T](val seq: Iterable[(Type, T)]) extends AnyVal {
    def get(k: Type): Option[T] = seq.find(_._1 =:= k).map(_._2)
  }
}
