package dx.cwl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RequirementTest extends AnyFlatSpec with Matchers {
  it should "merge requirements" in {
    val req1 = ResourceRequirement(coresMin = Some(LongValue(10)), ramMax = Some(LongValue(1024)))
    req1.merge shouldBe ResourceRequirement(
        coresMin = Some(LongValue(10)),
        ramMin = Some(LongValue(ResourceRequirement.DefaultRamMin)),
        ramMax = Some(LongValue(1024)),
        tmpdirMin = Some(LongValue(ResourceRequirement.DefaultTmpdirMin)),
        outdirMin = Some(LongValue(ResourceRequirement.DefaultOutdirMin))
    )
    val req2 = ResourceRequirement(coresMax = Some(LongValue(8)))
    req1.merge(req2) shouldBe ResourceRequirement(coresMin = Some(LongValue(10)),
                                                  coresMax = Some(LongValue(10)),
                                                  ramMax = Some(LongValue(1024)))
  }
}
