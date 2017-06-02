// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import coreplex._
import diplomacy._
import jtag.JTAGIO
import uncore.tilelink2._
import uncore.devices._
import util._

/** All the traits defined in this file assume that they are being mixed in
  * to a system that has a standard RISCV-based coreplex platform.
  */
trait HasCoreplexRISCVPlatform {
  val coreplex: CoreplexRISCVPlatform
}

/** Adds JTAG DTM to system, and exports JTAG interface. */
trait HasPeripheryJTAGDTM extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val module: HasPeripheryJTAGDTMModuleImp
}

class SystemJTAGIO extends Bundle {
  val jtag = new JTAGIO(hasTRSTn = false).flip
  val reset = Bool(INPUT)
  val mfr_id = UInt(INPUT, 11)
}

trait HasPeripheryJTAGDTMModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryJTAGDTM

  val io_jtag = IO(new SystemJTAGIO)

  val dtm = Module (new DebugTransportModuleJTAG(p(DMKey).nDMIAddrSize, p(JtagDTMKey)))
  dtm.io.jtag <> io_jtag.jtag
  
  dtm.clock             := io_jtag.jtag.TCK
  dtm.io.jtag_reset     := io_jtag.reset
  dtm.io.jtag_mfr_id    := io_jtag.mfr_id
  dtm.reset             := dtm.io.fsmReset

  outer.coreplex.module.io.debug.dmi <> dtm.io.dmi
  outer.coreplex.module.io.debug.dmiClock := io_jtag.jtag.TCK
  outer.coreplex.module.io.debug.dmiReset := ResetCatchAndSync(io_jtag.jtag.TCK, io_jtag.reset, "dmiResetCatch")
}

/** Adds Debug Module Interface (DMI) to systeme. Any sort of DTM
  * can be connected outside. DMI Clock and Reset must be provided.
  */
trait HasPeripheryDMI extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val module: HasPeripheryDMIModuleImp
}

trait HasPeripheryDMIModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryDMI
  val io_debug = IO(new ClockedDMIIO().flip)

  outer.coreplex.module.io.debug <> io_debug
}

/** Add DMI or JTAG interface to system based on a global parameter */
trait HasPeripheryDebug extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val module: HasPeripheryDebugModuleImp
}

trait HasPeripheryDebugModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryDebug

  val io_debug = (!p(IncludeJtagDTM)).option(IO(new ClockedDMIIO().flip))
  val io_jtag  = (p(IncludeJtagDTM)).option(IO(new SystemJTAGIO))
  val io_ndreset = IO(Bool(OUTPUT))
  val io_dmactive = IO(Bool(OUTPUT))

  io_debug.foreach { dbg => outer.coreplex.module.io.debug <> dbg }

  val dtm = io_jtag.map { jtag => 
    val dtm = Module(new DebugTransportModuleJTAG(p(DMKey).nDMIAddrSize, p(JtagDTMKey)))
    dtm.io.jtag <> jtag.jtag

    dtm.clock          := jtag.jtag.TCK
    dtm.io.jtag_reset  := jtag.reset
    dtm.io.jtag_mfr_id := jtag.mfr_id
    dtm.reset          := dtm.io.fsmReset

    outer.coreplex.module.io.debug.dmi <> dtm.io.dmi
    outer.coreplex.module.io.debug.dmiClock := jtag.jtag.TCK
    outer.coreplex.module.io.debug.dmiReset := ResetCatchAndSync(jtag.jtag.TCK, jtag.reset, "dmiResetCatch")
    dtm
  }

  io_ndreset  := outer.coreplex.module.io.ndreset
  io_dmactive := outer.coreplex.module.io.dmactive
}

/** Real-time clock is based on RTCPeriod relative to system clock.
  * Note: nothing about this is diplomatic, all the work is done in the ModuleImp
  */
trait HasPeripheryRTCCounter extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val module: HasPeripheryRTCCounterModuleImp
}

trait HasPeripheryRTCCounterModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryRTCCounter
  val period = p(rocketchip.RTCPeriod)
  val rtcCounter = RegInit(UInt(0, width = log2Up(period)))
  val rtcWrap = rtcCounter === UInt(period-1)

  rtcCounter := Mux(rtcWrap, UInt(0), rtcCounter + UInt(1))
  outer.coreplex.module.io.rtcToggle := rtcCounter(log2Up(period)-1)
}

/** Adds a boot ROM that contains the DTB describing the system's coreplex. */
trait HasPeripheryBootROM extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val bootrom_address = 0x10000
  val bootrom_size    = 0x10000
  val bootrom_hang    = 0x10040
  private lazy val bootrom_contents = GenerateBootROM(coreplex.dtb)
  val bootrom = LazyModule(new TLROM(bootrom_address, bootrom_size, bootrom_contents, true, peripheryBusConfig.beatBytes))

  bootrom.node := TLFragmenter(peripheryBusConfig.beatBytes, cacheBlockBytes)(peripheryBus.node)
}

/** Coreplex will power-on running at 0x10040 (BootROM) */
trait HasPeripheryBootROMModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryBootROM
  outer.coreplex.module.io.resetVector := UInt(outer.bootrom_hang)
}
