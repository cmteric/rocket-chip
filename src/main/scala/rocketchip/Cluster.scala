// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import diplomacy._
import rocketchip._
import util.ParameterizedBundle


class ExampleRocketTop()(implicit p: Parameters) extends Module {
  val sys0_module = Module(LazyModule(new OldExampleRocketTop).module)
  val sys1_module = Module(LazyModule(new OldExampleRocketTop()(p.alterPartial({case coreplex.CacheBlockBytes => 32}))).module)

  val io = new Bundle {
    val sys0 = new Bundle
        with HasPeripheryExtInterruptsBundle
        with HasPeripheryMasterAXI4MemPortBundle
        with HasPeripheryMasterAXI4MMIOPortBundle
        with HasPeripherySlaveAXI4PortBundle
        with HasPeripheryDebugBundle {
      implicit val p = sys0_module.p
      val interrupts = sys0_module.interrupts.cloneType.flip
      val mem_axi4 = sys0_module.mem_axi4.cloneType
      val mmio_axi4 = sys0_module.mmio_axi4.cloneType
      val l2_frontend_bus_axi4 = sys0_module.l2_frontend_bus_axi4.cloneType.flip
      val debug = sys0_module.debug.cloneType
    }
    val sys1 = new Bundle
        with HasPeripheryMasterAXI4MMIOPortBundle
        with HasPeripheryDebugBundle {
      implicit val p = sys1_module.p
      val mmio_axi4 = sys1_module.mmio_axi4.cloneType
      val debug = sys1_module.debug.cloneType
    }
  }

  //io.sys0.interrupts <> sys0_module.interrupts
  sys0_module.interrupts <> io.sys0.interrupts
  io.sys0.mem_axi4 <> sys0_module.mem_axi4
  io.sys0.mmio_axi4 <> sys0_module.mmio_axi4
  sys0_module.l2_frontend_bus_axi4 <> io.sys0.l2_frontend_bus_axi4
  //io.sys0.l2_frontend_bus_axi4 <> sys0_module.l2_frontend_bus_axi4
  io.sys0.debug <> sys0_module.debug

  sys1_module.tieOffInterrupts()
  sys1_module.connectSimAXIMem()
  io.sys1.mmio_axi4 <> sys1_module.mmio_axi4
  sys1_module.tieOffAXI4SlavePort()
  io.sys1.debug <> sys1_module.debug
}

class TestHarness()(implicit p: Parameters) extends Module {
  val io = new Bundle { val success = Bool(OUTPUT) }
  val dut = Module(new ExampleRocketTop)
  dut.io.sys0.tieOffInterrupts()
  dut.io.sys0.connectSimAXIMem()
  dut.io.sys0.connectSimAXIMMIO()
  dut.io.sys0.tieOffAXI4SlavePort()
  dut.io.sys0.connectDebug(clock, reset, io.success)
  dut.io.sys1.connectSimAXIMMIO()
  dut.io.sys1.connectDebug(clock, reset, io.success)
}
