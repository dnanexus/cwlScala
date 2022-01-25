package dx.cwl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommonTest extends AnyFlatSpec with Matchers {
  it should "simplify Identifier" in {
    val id = Identifier(Some("foo"), "bar/baz/wf@step_step1@blorf.cwl")
    id.simplify(true, (Left(true), None), true, true) shouldBe Identifier(None, "blorf")
    id.simplify(true, (Left(true), Some("bing/")), true, true) shouldBe Identifier(None,
                                                                                   "bing/blorf")
    id.simplify(false, (Right("bar/"), None), true, false) shouldBe Identifier(Some("foo"),
                                                                               "baz/blorf.cwl")
    id.simplify(false, (Right("bar/"), Some("bing/")), true, false) shouldBe Identifier(
        Some("foo"),
        "bing/baz/blorf.cwl"
    )

    val id2 = Identifier(Some("foo"), "bar/baz/wf@step_step1@run")
    id2.simplify(true, (Left(true), None), true, true) shouldBe Identifier(None, "step1_run")
    id2.simplify(false, (Right("bar/"), None), true, false) shouldBe Identifier(Some("foo"),
                                                                                "baz/step1_run")

    val id3 = Identifier(Some("foo"), "bar/baz/wf@step_step1@run@step_step2@run")
    id3.simplify(true, (Left(true), None), true, true) shouldBe Identifier(None,
                                                                           "step1_run_step2_run")
  }
}
