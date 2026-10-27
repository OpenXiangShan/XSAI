package top

import org.scalatest.flatspec.AnyFlatSpec
import system.SoCParamsKey

class ZhuJiangConfigTest extends AnyFlatSpec {
  behavior of "XSAI ZhuJiang snoop-filter sizing"

  for (numCores <- Seq(1, 2)) {
    it should s"preserve the per-cluster capacity with $numCores cores" in {
      val config = new DefaultMatrixConfig(numCores).alter(LLCConfig("ZhuJiang"))
      val soc = config(SoCParamsKey)
      assert(soc.ZhuJiangParams.clusterCacheSizeInB == 512 * 1024)
      val params = ZhuJiangNoCTopology(numCores, soc.ZhuJiangParams)
      assert(params.djParams.sfSizeInB == numCores * 512 * 1024)
    }
  }
}
