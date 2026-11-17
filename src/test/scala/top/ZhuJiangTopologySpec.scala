package top

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import xijiang.NodeType
import zhujiang.ZJParameters

class ZhuJiangTopologySpec extends AnyFlatSpec with Matchers {
  behavior of "XSTop ZhuJiang topology"

  for ((cores, ids) <- Seq(
    1 -> Seq(0x08),
    2 -> Seq(0x08, 0x18),
    4 -> Seq(0x08, 0x18, 0x58, 0x60)
  )) {
    it should s"allocate $cores synchronous CC sockets with native node IDs" in {
      val topology = ZhuJiangNoCTopology(cores)
      val ccNodes = topology.island.filter(_.nodeType == NodeType.CC)
      ccNodes.map(_.nodeId) shouldBe ids
      ccNodes.map(_.socket) shouldBe Seq.fill(cores)("sync")
      topology.island.map(_.nodeId).distinct.size shouldBe topology.island.size
    }
  }

  it should "preserve all dual-core endpoints when adding two CC sockets" in {
    val dual = ZhuJiangNoCTopology(2)
    val quad = ZhuJiangNoCTopology(4)
    quad.nodeParams.take(dual.nodeParams.size) shouldBe dual.nodeParams
    quad.nodeParams.drop(dual.nodeParams.size).map(_.nodeType) shouldBe Seq.fill(2)(NodeType.CC)
    quad.island.take(dual.island.size).map(_.nodeId) shouldBe dual.island.map(_.nodeId)
  }

  it should "leave the non-CC endpoint parameters unchanged" in {
    val dual = ZhuJiangNoCTopology(2)
    val quad = ZhuJiangNoCTopology(4)
    quad.nodeParams.filterNot(_.nodeType == NodeType.CC) shouldBe
      dual.nodeParams.filterNot(_.nodeType == NodeType.CC)
  }

  it should "retain caller parameters and the explicit configuration bus width" in {
    val base = ZJParameters().copy(
      cacheSizeInB = 8 * 1024 * 1024,
      ciSuffix = "_quad_topology_test",
      cfgAxiDataBits = 64
    )
    ZhuJiangNoCTopology(4, base).cfgAxiDataBits shouldBe base.cfgAxiDataBits
    for (width <- Seq(64, 256)) {
      val topology = ZhuJiangNoCTopology(4, base, width)
      topology.cacheSizeInB shouldBe base.cacheSizeInB
      topology.ciSuffix shouldBe base.ciSuffix
      topology.cfgAxiDataBits shouldBe width
      topology.nodeNidBits shouldBe 8
      topology.nodeAidBits shouldBe 3
      topology.tfbParams shouldBe None
    }
  }

  it should "reject core counts outside the supported single, dual and quad configurations" in {
    for (cores <- Seq(-1, 0, 3, 5, 6, 8)) {
      an[IllegalArgumentException] should be thrownBy {
        ZhuJiangNoCTopology(cores)
      }
    }
  }
}
