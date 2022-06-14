package dx.cwl

import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.immutable.SeqMap
import org.scalatest.matchers.should.Matchers

class CwlTypeTest extends AnyFlatSpec with Matchers {
  val p1 = "bar/baz1/"
  val p2 = "bar/baz2/"
  val id1 = Identifier(Some("foo1"), p1 + java.util.UUID.randomUUID().toString())
  val id2 = Identifier(Some("foo2"), p2 + java.util.UUID.randomUUID().toString())
  val id3 = Identifier(Some("foo"), p1 + "id3")
  val id3_2 = Identifier(Some("foo"), p2 + "id3")
  val id4 = Identifier(Some("foo"), p1 + "id4")

  it should "simplify arrays with random ids as identical" in {
    val arr1 = CwlArray(CwlString, Some(id1), Some("array 1"), Some("array 1 doc"))
    val arr2 = CwlArray(CwlString, Some(id2), Some("array 2"), Some("array 2 doc"))
    val simple_arr1 = arr1.copySimplifyIds(dropNamespace = true,
                                           replacePrefix = (Left(true), None),
                                           simplifyAutoNames = true,
                                           dropCwlExtension = true)
    val simple_arr2 = arr2.copySimplifyIds(dropNamespace = true,
                                           replacePrefix = (Left(true), None),
                                           simplifyAutoNames = true,
                                           dropCwlExtension = true)
    simple_arr1.hashCode() shouldEqual (simple_arr2.hashCode())
    simple_arr1.equals(simple_arr2)

    val arr3 = CwlArray(CwlString, Some(id3), Some("array 3"), Some("array 3 doc"))
    val arr4 = CwlArray(CwlString, Some(id4), Some("array 4"), Some("array 4 doc"))
    val simple_arr3 = arr3.copySimplifyIds(dropNamespace = true,
                                           replacePrefix = (Left(true), None),
                                           simplifyAutoNames = true,
                                           dropCwlExtension = true)
    val simple_arr4 = arr4.copySimplifyIds(dropNamespace = true,
                                           replacePrefix = (Left(true), None),
                                           simplifyAutoNames = true,
                                           dropCwlExtension = true)
    simple_arr3.hashCode() should not equal simple_arr4.hashCode()
    simple_arr3 should not equal simple_arr4

    val arr3_2 = CwlArray(CwlString, Some(id3_2), Some("array 3 copy"), Some("array 3 copy doc"))
    val simple_arr3_2 = arr3_2.copySimplifyIds(dropNamespace = true,
                                               replacePrefix = (Left(true), None),
                                               simplifyAutoNames = true,
                                               dropCwlExtension = true)
    simple_arr3.id.get shouldBe (simple_arr3_2.id.get)
    simple_arr3.hashCode() shouldBe (simple_arr3_2.hashCode())
    simple_arr3.equals(simple_arr3_2)
  }

  it should "simplify enums with random ids as identical" in {
    val symbols = Vector[String]("a", "b", "c")
    val symbols1 = symbols.map(p1 + _)
    val symbols2 = symbols.map(p2 + _)

    val enum1 = CwlEnum(symbols1, Some(id1), Some("enum 1"), Some("enum 1 doc"))
    val enum2 = CwlEnum(symbols2, Some(id2), Some("enum 2"), Some("enum 2 doc"))
    val simple_enum1 = enum1.copySimplifyIds(dropNamespace = true,
                                             replacePrefix = (Left(true), None),
                                             simplifyAutoNames = true,
                                             dropCwlExtension = true)
    val simple_enum2 = enum2.copySimplifyIds(dropNamespace = true,
                                             replacePrefix = (Left(true), None),
                                             simplifyAutoNames = true,
                                             dropCwlExtension = true)
    simple_enum1.hashCode() shouldEqual (simple_enum2.hashCode())
    simple_enum1.equals(simple_enum2)
  }

  it should "simplify multi/optional whose inner types have random ids as identical" in {
    val arr1 = CwlArray(CwlString, Some(id1), Some("array 1"), Some("array 1 doc"))
    val arr2 = CwlArray(CwlString, Some(id2), Some("array 2"), Some("array 2 doc"))
    val symbols = Vector[String]("a", "b", "c")
    val symbols1 = symbols.map(p1 + _)
    val symbols2 = symbols.map(p2 + _)
    val enum1 = CwlEnum(symbols1, Some(id1), Some("enum 1"), Some("enum 1 doc"))
    val enum2 = CwlEnum(symbols2, Some(id2), Some("enum 2"), Some("enum 2 doc"))

    val t1 = CwlMulti(Vector[CwlType](CwlOptional(enum1), arr2))
    val t2 = CwlMulti(Vector[CwlType](arr1, CwlOptional(enum2)))

    val simple_t1 = CwlType.copySimplifyIds(t1,
                                            dropNamespace = true,
                                            replacePrefix = (Left(true), None),
                                            simplifyAutoNames = true,
                                            dropCwlExtension = true)
    val simple_t2 = CwlType.copySimplifyIds(t2,
                                            dropNamespace = true,
                                            replacePrefix = (Left(true), None),
                                            simplifyAutoNames = true,
                                            dropCwlExtension = true)
    simple_t1.hashCode() shouldEqual (simple_t2.hashCode())
    simple_t1.equals(simple_t2)
  }

  it should "simplify records with random ids as identical" in {
    val arr1 = CwlArray(CwlString, Some(id1), Some("array 1"), Some("array 1 doc"))
    val arr2 = CwlArray(CwlString, Some(id2), Some("array 2"), Some("array 2 doc"))
    val f1 = CwlInputRecordField("input_field", arr1, Some("field label1"), Some("field 1 doc"))
    val f2 = CwlInputRecordField("input_field", arr2, Some("field label2"), Some("field 2 doc"))
    val r1 = CwlInputRecord(SeqMap("f" -> f1), Some(id1))
    val r2 = CwlInputRecord(SeqMap("f" -> f2), Some(id2))

    val simple_r1 = r1.copySimplifyIds(dropNamespace = true,
                                       replacePrefix = (Left(true), None),
                                       simplifyAutoNames = true,
                                       dropCwlExtension = true)
    val simple_r2 = r2.copySimplifyIds(dropNamespace = true,
                                       replacePrefix = (Left(true), None),
                                       simplifyAutoNames = true,
                                       dropCwlExtension = true)
    simple_r1.hashCode() shouldEqual (simple_r2.hashCode())
    simple_r1.equals(simple_r2)
  }
}
