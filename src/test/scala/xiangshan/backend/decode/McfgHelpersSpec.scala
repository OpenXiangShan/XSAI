package xiangshan.backend.decode

import cute.MatrixIsaParams
import org.scalatest.flatspec.AnyFlatSpec

class McfgHelpersSpec extends AnyFlatSpec {
  private val packedExtension = MatrixIsaParams(
    enableInt8Int32 = true,
    enableInt8Fp2Pack4I32 = true
  )
  private val int8OnlyExtension = MatrixIsaParams(enableInt8Int32 = true)

  behavior of "McfgHelpers fp2pack4 support"

  it should "decode proposal-14 table-0 type 13 as packed signed i2 with an i8 AMU payload" in {
    val decoded = McfgHelpers.decodeRaw(13)
    assert(decoded.legal)
    assert(decoded.semanticType.contains(McfgHelpers.Fp2Pack4))
    assert(decoded.elementWidth.contains(2))
    assert(decoded.isInteger)
    assert(decoded.amuPayload.contains(4))
    assert(!McfgHelpers.decodeRaw(0x1d).legal)
  }

  it should "accept only signed-int8 x fp2pack4 to int32" in {
    assert(McfgHelpers.isMmaTripleSupportedRaw(2, 13, 4, packedExtension))
    assert(!McfgHelpers.isMmaTripleSupportedRaw(3, 13, 4, packedExtension))
    assert(!McfgHelpers.isMmaTripleSupportedRaw(13, 2, 4, packedExtension))
    assert(!McfgHelpers.isMmaTripleSupportedRaw(2, 13, 12, packedExtension))
    assert(!McfgHelpers.isMmaTripleSupportedRaw(2, 13, 4, int8OnlyExtension))
  }

}
