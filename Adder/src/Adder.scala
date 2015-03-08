/**
 * Created by geoffrey on 16/02/15.
 */

import Chisel._

//DESCRIPTION: This module defines an adder with
// synchronous add enable and reset inputs. When the adder
//is synchronously reset, the outputs go to zero and the
// valid signal is asserted on the next clock cycle. When
// the add enable input is asserted and the valid output is
// asserted during the same clock cycle, the adder begins
// adding. When the valid output signal is again asserted
// on a subsequent clock cycle, the new output is correct.
// Note that the inputs must be held steady from the cycle
// during which the add enable input is asserted until the
// cycle during which the valid output signal is asserted.
//*******************************************************//

class FullAdder extends Module {
  val io = new Bundle {
    val a = UInt(INPUT, 1)
    val b = UInt(INPUT, 1)
    val cin = UInt(INPUT, 1)
    val sum = UInt(OUTPUT, 1)
    val cout = UInt(OUTPUT, 1)
  }
  // Generate the sum
  val a_xor_b = io.a ^ io.b
  io.sum := a_xor_b ^ io.cin
  // Generate the carry
  val a_and_b = io.a & io.b
  val b_and_cin = io.b & io.cin
  val a_and_cin = io.a & io.cin
  io.cout := a_and_b | b_and_cin | a_and_cin
}

//A n-bit adder with carry in and carry out
class NBitAdder(val n:Int) extends Module {
  val io = new Bundle {
    val A = UInt(INPUT, n)
    val B = UInt(INPUT, n)
    val Cin = UInt(INPUT, 1)
    val Sum = UInt(OUTPUT, n)
    val Cout = UInt(OUTPUT, 1)
  }
  //create a vector of FullAdders
  val FAs = Vec.fill(n){ Module(new FullAdder()).io }
  val carry = Vec.fill(n+1){ UInt(width = 1) }
  val sum = Vec.fill(n){ Bool() }
  //first carry is the top level carry in
  carry(0) := io.Cin
  //wire up the ports of the full adders
  for (i <- 0 until n) {
    FAs(i).a := io.A(i)
    FAs(i).b := io.B(i)
    FAs(i).cin := carry(i)
    carry(i+1) := FAs(i).cout
    sum(i) := FAs(i).sum.toBool()
  }
  io.Sum := sum.toBits().toUInt()
  io.Cout := carry(n)
}

class Adder_4bit extends Module {
  val io = new Bundle {
    // 4-bit A input
    val a = UInt(INPUT, 4)
    // 4-bit B input
    val b = UInt(INPUT, 4)
    // Carry in
    val cin = UInt(INPUT, 1)
    // Active low, synchronous reset
    val reset_n = Bool(INPUT)
    // Synchronous add enable control
    val add_en = Bool(INPUT)
    // 4-bit output
    val out = UInt(OUTPUT, 4)
    // Carry output
    val cout = UInt(OUTPUT, 1)

  }

  val reg_out = Reg(init = UInt(0, width = 5))

  val Adder = Module(new NBitAdder(4))
  Adder.io.A := io.a
  Adder.io.B := io.b
  Adder.io.Cin := io.cin
  val sum = Adder.io.Sum
  val cout = Adder.io.Cout


  when(!io.reset_n) {
    reg_out := UInt(6)
  }.elsewhen(io.add_en) {

    reg_out := Cat(cout,sum)

    printf("reg_out = %d \n", reg_out)
  }

  //create 8 4bits adders chained together to form an 32bit adder


  io.out := reg_out(3, 0)
  io.cout := reg_out(4)
}


class Adder extends Module {

  val io = new Bundle {
    // 32-bit A input
    val a = UInt(INPUT, 32)
    // 32-bit B input
    val b = UInt(INPUT, 32)
    // Active low, synchronous reset
    val reset_n = Bool(INPUT)
    // Synchronous add enable control
    val add_en = Bool(INPUT)

    // 32-bit output
    val out = UInt(OUTPUT, 32)
    // Carry output
    val cout = Bool(OUTPUT)
    //Is the output valid yet?
    val valid = Bool(OUTPUT)



  }


  val Adders = Vec.fill(8){ Module(new Adder_4bit()).io }
  val Sum    = Vec.fill(8){UInt(width = 4)}
  val Carry   = Vec.fill(8+1){UInt(width = 1)}


  //set the top level carryin
   Carry(0) := UInt(0)

  //wire up the ports of the adders
  for (i <- 0 until 8) {

    Adders(i).a       := io.a( ((i+1)*4)-1, i*4 )
    Adders(i).b       := io.b( ((i+1)*4)-1, i*4 )
    Adders(i).reset_n := io.reset_n
    Adders(i).add_en  := io.add_en
    Adders(i).cin     := Carry(i)
    Sum(i)            := Adders(i).out
    Carry(i+1)        :=Adders(i).cout

  }

  io.out := Sum.toBits().toUInt()
  io.cout := Carry(8)

}


class Adder_4bitTests(c: Adder_4bit) extends Tester(c) {
  poke(c.io.reset_n, 1)
  step(1)
  expect(c.io.cout, 0)
  expect(c.io.out, 0)
  step(1)
  poke(c.io.add_en, 1)
  //clear reset
  poke(c.io.reset_n, 1)
  step(1)
  peek(c.io.cout)
  poke(c.io.a, 15)
  poke(c.io.b, 1)
  poke(c.io.cin, 0)
  step(1)
  expect(c.io.cout, 1)
  expect(c.io.out, 0)

}


class AdderTests(c: Adder) extends Tester(c) {
  /*poke(c.io.load, 1)
  poke(c.io.in, 128)
  poke(c.io.inbit, 0)

  step(1)
  expect(c.io.out, 128)
  poke(c.io.shiftnum, 7)
  poke(c.io.load, 0)
  poke(c.io.rshift, 1)
  step(1)
  expect(c.io.out, 1)*/

}


object AdderTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
      () => Module(new Adder())) { c => new AdderTests(c)}
  }
}


//object Adder_4bitTester {
//  def main(args: Array[String]): Unit = {
//    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
//      () => Module(new Adder_4bit())) { c => new Adder_4bitTests(c)}
//  }
//}