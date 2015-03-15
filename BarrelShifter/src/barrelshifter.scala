/*******************************************************************************
  * Copyright (c) 2015 Geoffrey Ndu. Distributed under the MIT License.
  * See accompanying file LICENSE or copy at http://opensource.org/licenses/MIT.
  ******************************************************************************/

import Chisel._

class BarrelShifter extends Module {
  val io = new Bundle {
    val load = Bool(INPUT)
    val rshift = Bool(INPUT)
    val lshift = Bool(INPUT)
    val shiftnum = UInt(INPUT, 3)
    val inbit = UInt(INPUT, 1)
    //Bit to shift into empty places
    val in = UInt(INPUT, 8)
    val out = UInt(OUTPUT, 8)
  }

  val reg_out = Reg(init = UInt(0, width = 8))


  when(io.load) {
    reg_out := io.in

  }.elsewhen(io.rshift) {
    switch(io.shiftnum) {
      is(UInt(0)) {
        reg_out := reg_out
      }
      is(UInt(1)) {
        reg_out := Cat(io.inbit, reg_out(7, 1))
      }
      is(UInt(2)) {
        reg_out := Cat(Fill(2, io.inbit), reg_out(7, 2))
      }
      is(UInt(3)) {
        reg_out := Cat(Fill(3, io.inbit), reg_out(7, 3))
      }
      is(UInt(4)) {
        reg_out := Cat(Fill(4, io.inbit), reg_out(7, 4))
      }
      is(UInt(5)) {
        reg_out := Cat(Fill(5, io.inbit), reg_out(7, 5))
      }
      is(UInt(6)) {
        reg_out := Cat(Fill(6, io.inbit), reg_out(7, 6))
      }
      is(UInt(7)) {
        reg_out := Cat(Fill(7, io.inbit), reg_out(7))
      }
    }


  }.elsewhen(io.lshift) {
    switch(io.shiftnum) {
      is(UInt(0)) {
        reg_out := reg_out
      }
      is(UInt(1)) {
        reg_out := Cat(reg_out(7, 1), io.inbit)
      }
      is(UInt(2)) {
        reg_out := Cat(reg_out(7, 2), Fill(2, io.inbit))
      }
      is(UInt(3)) {
        reg_out := Cat(reg_out(7, 3), Fill(3, io.inbit))
      }
      is(UInt(4)) {
        reg_out := Cat(reg_out(7, 4), Fill(4, io.inbit))
      }
      is(UInt(5)) {
        reg_out := Cat(reg_out(7, 5), Fill(5, io.inbit))
      }
      is(UInt(6)) {
        reg_out := Cat(reg_out(7, 6), Fill(6, io.inbit))
      }
      is(UInt(7)) {
        reg_out := Cat(reg_out(7), Fill(7, io.inbit))
      }
    }
  }


  io.out := reg_out


}


class BarrelShifterTests(c: BarrelShifter) extends Tester(c) {
  poke(c.io.load, 1)
  poke(c.io.in, 128)
  poke(c.io.inbit, 0)

  step(1)
  expect(c.io.out, 128)
  poke(c.io.shiftnum, 7)
  poke(c.io.load, 0)
  poke(c.io.rshift, 1)
  step(1)
  expect(c.io.out, 1)

}


object BarrelShifterTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
      () => Module(new BarrelShifter())) { c => new BarrelShifterTests(c)}
  }
}
