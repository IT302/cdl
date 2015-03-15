/*******************************************************************************
  * Copyright (c) 2015 Geoffrey Ndu. Distributed under the MIT License.
  * See accompanying file LICENSE or copy at http://opensource.org/licenses/MIT.
  ******************************************************************************/

import Chisel._

class StateMachine extends Module {
  val io = new Bundle {
    // INPUTS
    // Active low, synchronous reset
    val reset_n = Bool(INPUT)
    // write command from processor
    val wr = Bool(INPUT)
    // Read command from processor
    val rd = Bool(INPUT)
    // Ready signal from memory device
    val ready = Bool(INPUT)
    // OUTPUTS
    // Output enable to memory
    val out_en = Bool(OUTPUT)
    // write enable to memory
    val write_en = Bool(OUTPUT)
    // Acknowledge signal to processor
    val ack = Bool(OUTPUT)
  }


  val idle :: write :: read1 :: read2 :: Nil = Enum(UInt(), 4)
  val s_even :: s_odd :: Nil = Enum(UInt(), 2)

  val mem_state = Reg(init = UInt(0, width = 2))



  def isRising(x : Bool) = x && !Reg(x)
  def isFalling(x : Bool) = !x && Reg(x)


  when(isFalling(~io.reset_n)) {
    mem_state := idle

  }


  when(mem_state === idle) {

    when(io.wr) {
      mem_state := write
    }
    when(io.rd) {
      mem_state := read1
    }
  }
  when(mem_state === write) {


    mem_state := idle

  }
  when(mem_state === read1) {
    when(io.ready) {
      mem_state := read2
    }
  }
  when(mem_state === read2) {
    mem_state := idle
  }



  io.out_en := (mem_state === read1) || (mem_state === read2)
  io.write_en := (mem_state === write)
  io.ack := (mem_state === write) || (mem_state === read2)


}


class StateMachineTest(c: StateMachine) extends Tester(c) {
  poke(c.io.reset_n, 0)
  step(1)
  expect(c.io.out_en, 0)
  expect(c.io.write_en, 0)
  expect(c.io.ack, 0)
  poke(c.io.reset_n, 1)
  step(2)
  poke(c.io.wr, 1)
  step(1)
  expect(c.io.out_en, 0)
  expect(c.io.write_en, 1)
  expect(c.io.ack, 1)
  step(1)
  expect(c.io.out_en, 0)
  expect(c.io.write_en, 0)
  expect(c.io.ack, 0)
  poke(c.io.wr, 0)
  poke(c.io.rd, 1)
  step(1)
  expect(c.io.out_en, 1)
  expect(c.io.write_en, 0)
  expect(c.io.ack, 0)
  poke(c.io.ready, 1)
  step(1)
  expect(c.io.out_en, 1)
  expect(c.io.write_en, 0)
  expect(c.io.ack, 1)
  step(1)
  expect(c.io.out_en, 0)
  expect(c.io.write_en, 0)
  expect(c.io.ack, 0)
  step(1)
  expect(c.io.out_en, 1)
  expect(c.io.write_en, 0)
  expect(c.io.ack, 0)


}

object  StateMachineTop {
  def main(args: Array[String]): Unit = {
    chiselMain(args, () => Module(new StateMachine()))
  }
}

//object StateMachineTester {
//  def main(args: Array[String]): Unit = {
//    chiselMainTest(Array[String]("--backend", "c", "v","--compile", "--test", "--genHarness"),
//      () => Module(new StateMachine())) { c => new StateMachineTest(c)}
//  }
//}