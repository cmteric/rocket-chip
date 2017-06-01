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

trait HasPeripheryJTAGDTMModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryJTAGDTM

  val jtag = IO(new JTAGIO(hasTRSTn = false).flip)
  val jtag_reset = IO(Bool(INPUT))
  val jtag_mfr_id = IO(UInt(INPUT, 11))

  val dtm = Module (new DebugTransportModuleJTAG(p(DMKey).nDMIAddrSize, p(JtagDTMKey)))
  dtm.io.jtag <> jtag
  
  dtm.clock             := jtag.TCK
  dtm.io.jtag_reset     := jtag_reset
  dtm.io.jtag_mfr_id    := jtag_mfr_id
  dtm.reset             := dtm.io.fsmReset

  outer.coreplex.module.io.debug.dmi <> dtm.io.dmi
  outer.coreplex.module.io.debug.dmiClock := jtag.TCK
  outer.coreplex.module.io.debug.dmiReset := ResetCatchAndSync(jtag.TCK, jtag_reset, "dmiResetCatch")
}

/** Adds Debug Module Interface (DMI) to systeme. Any sort of DTM
  * can be connected outside. DMI Clock and Reset must be provided.
  */
trait HasPeripheryDMI extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val module: HasPeripheryDMIModuleImp
}

trait HasPeripheryDMIModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryDMI
  val debug = IO(new ClockedDMIIO().flip)

  outer.coreplex.module.io.debug <> debug
}

/** Add DMI or JTAG interface to system based on a global parameter */
trait HasPeripheryDebug extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val module: HasPeripheryDebugModuleImp
}

trait HasPeripheryDebugModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryDebug

  val debug = (!p(IncludeJtagDTM)).option(IO(new ClockedDMIIO().flip))

  val jtag        = (p(IncludeJtagDTM)).option(IO(new JTAGIO(hasTRSTn = false).flip))
  val jtag_reset  = (p(IncludeJtagDTM)).option(IO(Bool(INPUT)))
  val jtag_mfr_id = (p(IncludeJtagDTM)).option(IO(UInt(INPUT, 11)))

  val ndreset = IO(Bool(OUTPUT))
  val dmactive = IO(Bool(OUTPUT))

  debug.foreach { dbg => outer.coreplex.module.io.debug <> dbg }

  val dtm = jtag.map { _ => Module(new DebugTransportModuleJTAG(p(DMKey).nDMIAddrSize, p(JtagDTMKey))) }
  dtm.foreach { dtm =>
    dtm.io.jtag <> jtag.get

    dtm.clock          := jtag.get.TCK
    dtm.io.jtag_reset  := jtag_reset.get
    dtm.io.jtag_mfr_id := jtag_mfr_id.get
    dtm.reset          := dtm.io.fsmReset

    outer.coreplex.module.io.debug.dmi <> dtm.io.dmi
    outer.coreplex.module.io.debug.dmiClock := jtag.get.TCK
    outer.coreplex.module.io.debug.dmiReset := ResetCatchAndSync(jtag.get.TCK, jtag_reset.get, "dmiResetCatch")
  }

  ndreset  := outer.coreplex.module.io.ndreset
  dmactive := outer.coreplex.module.io.dmactive
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
