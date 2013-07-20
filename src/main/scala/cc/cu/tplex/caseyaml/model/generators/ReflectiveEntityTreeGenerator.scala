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
class ReflectiveEntityTreeGenerator[Obj: TypeTag] { outer =>
  private final val mirror = typeTag[Obj].mirror

  private final val stringConvertedTypes = ArrayBuffer[
    (Type, Function1[T, String], Function1[String, T]) forSome { type T }
  ]()

  def stringConverted[T: TypeTag](toStr: T => String, fromStr: String => T) = {
    stringConvertedTypes += ((typeOf[T], toStr, fromStr))
    this
  }

  def generate[Yml]: YEntity[Obj, Yml] = {
    val tpe = typeOf[Obj]
    generate(tpe).asInstanceOf[YEntity[Obj, Yml]]
  }

  def generateClassMap = generate.asInstanceOf[YClassMap[Obj]]

  private def generate(tpe: Type) = generatorChain.generate(tpe) match {
    case Some(entity) => entity
    case None => throw CaseYamlException("Reached the end of type generators chain for type " + tpe)
  }

  private final val generatorChain =
    IntCompatibleChainLink +>
    FloatCompatibleChainLink +>
    StringChainLink +>
    BooleanChainLink +>
    OptionalChainLink +>
    ListChainLink +>
    SetChainLink +>
    MapChainLink +>
    StringConvertedLink +>
    ClassMapChainLink

  private final def firstTypeParam(tpe: Type) =
    tpe.asInstanceOf[TypeRefApi].args.head

  private final def secondTypeParam(tpe: Type) =
    tpe.asInstanceOf[TypeRefApi].args.tail.head

  private object StringConvertedLink extends GeneratorChain {
    def generate(tpe: Type) = stringConvertedTypes.find(_._1 =:= tpe) map {
      case (_, toStr, fromStr) => YStringConverted(toStr, fromStr)
    }
  }

  private object MapChainLink extends GeneratorChain {
    def generate(tpe: Type) =
      if (tpe <:< typeOf[Map[_, _]])
        Some(YMap(outer.generate(secondTypeParam(tpe.baseType(typeOf[Map[_, _]].typeSymbol)))))
      else
        None
  }

  private object SetChainLink extends GeneratorChain {
    def generate(tpe: Type) =
      if (tpe <:< typeOf[Set[_]])
        Some(YList(outer.generate(firstTypeParam(tpe.baseType(typeOf[Set[_]].typeSymbol)))))
      else
        None
  }

  private object ListChainLink extends GeneratorChain {
    def generate(tpe: Type) =
      if (tpe <:< typeOf[Seq[_]])
        Some(YList(outer.generate(firstTypeParam(tpe.baseType(typeOf[Seq[_]].typeSymbol)))))
      else
        None
  }

  private object OptionalChainLink extends GeneratorChain {
    def generate(tpe: Type) =
      if (tpe <:< typeOf[Option[_]])
        Some(YOptional(outer.generate(firstTypeParam(tpe))))
      else
        None
  }

  private object BooleanChainLink extends GeneratorChain {
    def generate(tpe: Type) = if (tpe =:= typeOf[Boolean]) Some(YBoolean) else None
  }

  private object StringChainLink extends GeneratorChain {
    def generate(tpe: Type) = if (tpe =:= typeOf[String]) Some(YString) else None
  }

  private object ClassMapChainLink extends GeneratorChain {
    // The following is just a black magic I don't really understand, but it seems to work
    def generate(tpe: Type) = if (tpe.typeSymbol.asClass.isCaseClass) Some {
      val List(params) = tpe.declaration(nme.CONSTRUCTOR).asMethod.paramss
      val entries: Seq[YEntry[_, _, _]] = params map { param =>
        import cc.cu.tplex.caseyaml.model._
        NamedField(param.name.toString).createEntry(outer.generate(param.typeSignature), mirror, tpe)
      }
      YClassMap(entries: _*)(TypeTag(mirror, new TypeCreator {
        def apply[U <: Universe with Singleton](m: api.Mirror[U]) =
          if (m eq mirror) tpe.asInstanceOf[U # Type]
          else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
      }))
    } else None
  }

  private object IntCompatibleChainLink extends GeneratorChain {
    private final val m = Seq[(Type, YEntity[_, _])](
      typeOf[Byte] -> YIntCompatible.YByte,
      typeOf[Short] -> YIntCompatible.YShort,
      typeOf[Int] -> YIntCompatible.YInt,
      typeOf[Long] -> YIntCompatible.YLong,
      typeOf[BigInt] -> YIntCompatible.YBigInt
    )
    def generate(tpe: Type) = m get tpe
  }

  private object FloatCompatibleChainLink extends GeneratorChain {
    private final val m = Seq[(Type, YEntity[_, _])](
      typeOf[Float] -> YFloatCompatible.YFloat,
      typeOf[Double] -> YFloatCompatible.YDouble,
      typeOf[BigDecimal] -> YFloatCompatible.YBigDecimal
    )
    def generate(tpe: Type) = m get tpe
  }

  private implicit class TypeLookable[T](val seq: Iterable[(Type, T)]) {
    def get(k: Type): Option[T] = seq.find(_._1 =:= k).map(_._2)
  }

  private trait GeneratorChain { outer =>
    def generate(tpe: Type): Option[YEntity[_, _]]
    def +>(chain: GeneratorChain) = new GeneratorChain {
      def generate(tpe: Type) = outer.generate(tpe) orElse chain.generate(tpe)
    }
  }
}
