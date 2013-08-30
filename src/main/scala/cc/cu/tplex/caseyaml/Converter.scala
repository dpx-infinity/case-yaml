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

package cc.cu.tplex.caseyaml

import cc.cu.tplex.caseyaml.model.{YClassMap, YEntity}
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.SafeConstructor
import java.io._
import cc.cu.tplex.caseyaml.yml.{ConvertToObj, ConvertToYml}
import java.nio.charset.Charset
import cc.cu.tplex.caseyaml.Converter.{Deserializer, Serializer}
import org.yaml.snakeyaml.error.YAMLException
import scala.reflect.ClassTag

/**
 * Date: 13.07.13
 * Time: 21:36
 *
 * @author Vladimir Matveev
 */
trait Converter[Obj, Yml] {
  def withYaml(yaml: Yaml): Converter[Obj, Yml]

  def serialize(obj: Obj): Serializer[Yml]
  def deserialize: Deserializer[Obj]
  def deserialize(subkey: String): Deserializer[Obj]
}

object Converter {
  private final val UTF_8 = Charset.forName("UTF-8")

  trait Deserializer[Obj] {
    def from(reader: Reader): Obj
    def from(string: String): Obj

    def fromStream(stream: InputStream, charset: Charset = UTF_8): Obj =
      from(new InputStreamReader(stream, charset))

    def fromFile(file: File, charset: Charset = UTF_8): Obj =
      fromStream(new FileInputStream(file), charset)

    def fromFilePath(path: String, charset: Charset = UTF_8): Obj =
      fromFile(new File(path), charset)

    def fromBytes(array: Array[Byte], charset: Charset = UTF_8): Obj =
      fromStream(new ByteArrayInputStream(array), charset)
  }

  trait Serializer[Yml] {
    def toStr: String
    def to(writer: Writer)
    def toYml: Yml

    def toStream(stream: OutputStream, charset: Charset = UTF_8) {
      to(new OutputStreamWriter(stream ,charset))
    }

    def toFile(file: File, charset: Charset = UTF_8) {
      toStream(new FileOutputStream(file), charset)
    }

    def toFilePath(path: String, charset: Charset = UTF_8) {
      toFile(new File(path), charset)
    }

    def toBytes(charset: Charset = UTF_8): Array[Byte] = {
      val stream = new ByteArrayOutputStream()
      toStream(stream, charset)
      stream.toByteArray
    }
  }
}

private[caseyaml] class DefaultConverter[Obj, Yml] (entity: YEntity[Obj, Yml],
                                                    yaml: Yaml = new Yaml(new SafeConstructor))
  extends Converter[Obj, Yml] {

  def withYaml(yaml: Yaml) = new DefaultConverter(entity, yaml)

  private def wrap[T](e: => T) = try {
    e
  } catch {
    case e: YAMLException => throw CaseYamlException("YAML [de]serialization error", e)
  }

  def serialize(obj: Obj): Serializer[Yml] = new Serializer[Yml] {
    def toStr = wrap(yaml.dump(ConvertToYml(entity, obj)))

    def to(writer: Writer) {
      wrap(yaml.dump(ConvertToYml(entity, obj), writer))
    }

    def toYml: Yml = ConvertToYml(entity, obj)
  }

  private abstract class BaseDeserializer extends Deserializer[Obj] {
    def from(reader: Reader) = extractMap(wrap(yaml.load(reader)))

    def from(string: String) = extractMap(wrap(yaml.load(string)))

    protected def extractMap(obj: Any): Obj
  }

  def deserialize: Deserializer[Obj] = new BaseDeserializer {
    protected def extractMap(obj: Any) = ConvertToObj(entity, obj.asInstanceOf[Yml])
  }

  def deserialize(subkey: String): Deserializer[Obj] = new BaseDeserializer {
    protected def extractMap(obj: Any) = ConvertToObj(entity, obj, subkey)
  }
}

trait AccumulatingClassConverter {
  def classMaps: Map[ClassTag[_], YClassMap[_]]

  def withYaml(yaml: Yaml): AccumulatingClassConverter

  def <>[Obj: ClassTag](entity: YClassMap[Obj]): AccumulatingClassConverter
  def <+>(classMaps: Map[ClassTag[_], YClassMap[_]]): AccumulatingClassConverter

  def serialize(obj: Any): Serializer[Any]
  def deserialize[T: ClassTag]: Deserializer[T]
  def deserialize[T: ClassTag](subkey: String): Deserializer[T]
}

private[caseyaml] class MultiConverter(yaml: Yaml = new Yaml(new SafeConstructor),
                                       val classMaps: Map[ClassTag[_], YClassMap[_]] = Map.empty)
  extends AccumulatingClassConverter {
  private def classTag[T: ClassTag] = implicitly[ClassTag[T]]

  private def converterFor[Obj](entity: YEntity[_, _]) =
    new DefaultConverter(entity, yaml).asInstanceOf[Converter[Obj, Any]]

  def withYaml(yaml: Yaml) = new MultiConverter(yaml, classMaps)

  def serialize(obj: Any) = {
    classMaps.get(ClassTag(obj.getClass)) match {
      case Some(entity) => converterFor(entity).serialize(obj)
      case None => throw CaseYamlException(s"No entity is configured for class ${obj.getClass}")
    }
  }

  def deserialize[T: ClassTag] = classMaps.get(classTag[T]) match {
    case Some(entity) => converterFor[T](entity).deserialize
    case None => throw CaseYamlException(s"No entity is configured for class ${classTag[T].runtimeClass}")
  }

  def deserialize[T: ClassTag](subkey: String) = classMaps.get(classTag[T]) match {
    case Some(entity) => converterFor[T](entity).deserialize(subkey)
    case None => throw CaseYamlException(s"No entity is configured for class ${classTag[T].runtimeClass}")
  }

  def <>[Obj: ClassTag](entity: YClassMap[Obj]) =
    new MultiConverter(yaml, this.classMaps + (classTag[Obj] -> entity))

  def <+>(classMaps: Map[ClassTag[_], YClassMap[_]]) =
    new MultiConverter(yaml, this.classMaps ++ classMaps)
}