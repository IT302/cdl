/**
 * Created by geoffrey on 23/01/15.
 */
import Chisel._

class DFF extends Module
{

  val io = new Bundle
  {
    val d  = UInt(INPUT,  1)
    val en = Bool(INPUT)
    val q  = UInt(OUTPUT, 1)
    val notq  = UInt(OUTPUT, 1)
  }

  val x  = Reg(init = UInt(0, width = 1))
  val y  = Reg(init = UInt(0, width = 1))

  def falling_edge(x: Bool) = !x && Reg(x)

  when (io.en) { x := io.d; io.q := y; y := x }


  io.q := x
  io.notq := !(io.q)
}

class DFFTester(c: DFF) extends Tester(c)
{
  for(sel <- 0 until 2){
    if (sel == 0) {poke(c.io.en,1); step (1)}
    poke(c.io.d,0)
    step(1)
    expect(c.io.q,0); expect(c.io.notq,1)
    step(1)
    poke(c.io.d,1)
    step(1)
    expect(c.io.q,1); expect(c.io.notq,0)


  }

  // clear x
  //step (1)
 // poke(c.io.nclr,0); step (1)
  //expect(c.io.q,0); expect(c.io.notq,1)

}

/*object Example {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
      () => Module(new DFF())) {c => new DFFTester(c)}
  }
}*/


class Counter extends Module
{
  val io = new Bundle
  {
    val in = UInt(INPUT, 8) //Input
    val reset_n = Bool(INPUT) //Active low asynch reset
    val preset_n = Bool(INPUT) // Active low preset
    val  load = Bool(INPUT) //Synchronous load input
    val  up_down = Bool(INPUT) //Synchronous up/down control
    val count_en = Bool(INPUT) //Synchronous count enable control

    val out = UInt(OUTPUT, 8) //Output
    val carry_out = UInt(OUTPUT, 1)

  }
  val count = Reg(init = UInt(0, width = 9))


  when(io.load) {count := io.in}
  count := count + UInt(1)

  io.out := count(7, 0)
  io.carry_out := count(8)



}


class CounterTester(c:Counter) extends Tester(c)
{
  poke(c.io.load,1)
  poke(c.io.in,0)
  step(1)
  for(sel <- 0 until 4)
  {

  }

}

/*object Example {
  def main(args: Array[String]): Unit = {
    chiselMain(args, () => Module(new DFF()))
  }
}*/

//object  CounterTop {
//  def main(args: Array[String]): Unit = {
//    chiselMain(args, () => Module(new Counter()))
//  }
//}

object CounterTopTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
      () => Module(new Counter())) {c => new CounterTester(c)}
  }
}