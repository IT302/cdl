// See LICENSE for license details.

package aes

import Chisel._


/** An implementation  of the 128-bits AES algorithm.
  * See http://csrc.nist.gov/publications/fips/fips197/fips-197.pdf for
  * specification. The Sbox is based David Canright's tiny AES S-boxes
  * on https://github.com/coruus/canright-aes-sboxes. The NIST test inputs arrays
  * are in column major format while this  implementation use  row major. The
  * function is used to Row_to_column is used to convert from column major to
  * to row major.
  */


object Utilities {
  def Row_to_column(input: Array[BigInt]): Array[BigInt] = {


    assert(input.length == 16)

    val a = Array.ofDim[BigInt](4)
    val b = Array.ofDim[BigInt](4)
    val c = Array.ofDim[BigInt](4)
    val d = Array.ofDim[BigInt](4)

    for (i <- 0 until 4) {
      a(i) = input(i * 4)
      b(i) = input((i * 4) + 1)
      c(i) = input((i * 4) + 2)
      d(i) = input((i * 4) + 3)
    }
    val output = a ++ b ++ c ++ d
    println(output)
    output
  }
}


/* convert to new basis in GF(2^8) */
/* i.e., bit matrix multiply */

class G256NBStage extends Module {
  val io = new Bundle {
    val in_x = UInt(INPUT, 8)
    val in_b = UInt(INPUT, 8)
    val in_y = UInt(INPUT, 8)
    val out_x = UInt(OUTPUT, 8)
    val out_y = UInt(OUTPUT, 8)
  }

  //printf("G256NBStage.io.in_x %x  \n", io.in_x)
  //printf("G256NBStage.io.in_b %x \n", io.in_b)

  when((io.in_x(0) & UInt(1)).toBool()) {
    io.out_y := io.in_y ^ io.in_b

  }.otherwise {
    io.out_y := io.in_y
  }


  io.out_x := io.in_x >> 1
  //printf("G256NBStage.io.out_x %x \n", io.out_x)


}


class G256NB extends Module {
  val io = new Bundle {
    val in_x = UInt(INPUT, 8)
    val in_b = Vec(8, UInt(INPUT, 8))
    val out_y = UInt(OUTPUT, 8)
  }


  // Create vector of G256 new basis stages

  val G256NBs = Vec.fill(8) {
    Module(new G256NBStage()).io
  }


  // connect the top wires
  G256NBs(0).in_x := io.in_x
  G256NBs(0).in_y := UInt(0)
  G256NBs(0).in_b := io.in_b(7)


  //wire up the rest of the ports

  G256NBs(1).in_x := G256NBs(0).out_x
  G256NBs(1).in_y := G256NBs(0).out_y
  G256NBs(1).in_b := io.in_b(6)

  G256NBs(2).in_x := G256NBs(1).out_x
  G256NBs(2).in_y := G256NBs(1).out_y
  G256NBs(2).in_b := io.in_b(5)

  G256NBs(3).in_x := G256NBs(2).out_x
  G256NBs(3).in_y := G256NBs(2).out_y
  G256NBs(3).in_b := io.in_b(4)


  G256NBs(4).in_x := G256NBs(3).out_x
  G256NBs(4).in_y := G256NBs(3).out_y
  G256NBs(4).in_b := io.in_b(3)

  G256NBs(5).in_x := G256NBs(4).out_x
  G256NBs(5).in_y := G256NBs(4).out_y
  G256NBs(5).in_b := io.in_b(2)


  G256NBs(6).in_x := G256NBs(5).out_x
  G256NBs(6).in_y := G256NBs(5).out_y
  G256NBs(6).in_b := io.in_b(1)

  G256NBs(7).in_x := G256NBs(6).out_x
  G256NBs(7).in_y := G256NBs(6).out_y
  G256NBs(7).in_b := io.in_b(0)



  // connect the bottom wires

  io.out_y := G256NBs(7).out_y

  //printf("G256NB.io.in_x %x \n", io.in_x)

}


object G4_sq {
  def apply(x: UInt): UInt = {

    val a = (x & UInt("h02")) >> 1
    val b = (x & UInt("h1"))
    (b << 1) | a


  }
}


object G4_mul {
  def apply(x: UInt, y: UInt) = {
    val a = (x & UInt("h2")) >> 1
    val b = x & UInt("h1")
    val c = (y & UInt("h2")) >> 1
    val d = y & UInt("h1")
    val e = (a ^ b) & (c ^ d)
    val p = (a & c) ^ e
    val q = (b & d) ^ e
    (p << 1) | q
  }
}


object G4_scl_N {
  def apply(x: UInt): UInt = {
    val a = (x & UInt("h2")) >> 1
    val b = x & UInt("h1")
    val p = b
    val q = a ^ b
    (p << 1) | q


  }
}


object G4_scl_N2 {
  def apply(x: UInt): UInt = {
    val a = (x & UInt("h2")) >> 1
    val b = x & UInt("h1")
    val p = a ^ b
    val q = a
    (p << 1) | q

  }
}


object G16_sq_scl {
  def apply(x: UInt): UInt = {
    val a = (x & UInt("hC")) >> 2
    val b = x & Bits("h3")
    val p = G4_sq(a ^ b)
    val q = G4_scl_N2(G4_sq(b))
    p << 2 | q
  }
}


object G16_mul {
  def apply(x: UInt, y: UInt) = {
    val a = (x & UInt("hc")) >> 2
    val b = x & UInt("h3")
    val c = (y & UInt("hc")) >> 2
    val d = y & UInt("h3")
    val e = G4_scl_N(G4_mul(a ^ b, c ^ d))
    val p = G4_mul(a, c) ^ e
    val q = G4_mul(b, d) ^ e
    ((p << 2) | q)


  }
}


object G16_inv {
  def apply(x: UInt) = {
    val a = (x & UInt("hC")) >> 2
    val b = x & UInt("h3")
    val c = G4_scl_N(G4_sq(a ^ b))
    val d = G4_mul(a, b)
    val e = G4_sq(c ^ d) // really inverse, but same as square
    val p = G4_mul(e, b)
    val q = G4_mul(e, a)
    ((p << 2) | q)

  }

}


/* inverse in GF(2^8), using normal basis (d^16,d) */
class G256_inv extends Module {
  val io = new Bundle {
    val in = UInt(INPUT, 8)
    val out = UInt(OUTPUT, 8)
  }


  val a = (io.in & UInt("hF0")) >> 4
  val b = io.in & UInt("h0F")
  //printf("b %x \n", b)
  val c = G16_sq_scl(a ^ b)
  //printf("c %x \n", c)
  val d = G16_mul(a, b)
  val e = G16_inv(c ^ d)
  val p = G16_mul(e, b)
  val q = G16_mul(e, a)
  io.out := (p << 4) | q

}


class SBox extends Module {
  val io = new Bundle {
    val in = UInt(INPUT, 8)
    val out = UInt(OUTPUT, 8)
  }

  val A2X = Vec(UInt("h98"), UInt("hF3"), UInt("hF2"), UInt("h48"), UInt("h09"),
    UInt("h81"), UInt("hA9"), UInt("hFF"))
  val X2A = Vec(UInt("h64"), UInt("h78"), UInt("h6E"), UInt("h8C"), UInt("h68"),
    UInt("h29"), UInt("hDE"), UInt("h60"))
  val X2S = Vec(UInt("h58"), UInt("h2D"), UInt("h9E"), UInt("h0B"), UInt("hDC"),
    UInt("h04"), UInt("h03"), UInt("h24"))
  val S2X = Vec(UInt("h8C"), UInt("h79"), UInt("h05"), UInt("hEB"), UInt("h12"),
    UInt("h04"), UInt("h51"), UInt("h53"))


  val G256_nbs_1 = Module(new G256NB())
  val G256_nbs_2 = Module(new G256NB())
  val G256_inv = Module(new G256_inv())


  G256_nbs_1.io.in_x := io.in
  G256_nbs_1.io.in_b := A2X

  G256_inv.io.in := G256_nbs_1.io.out_y


  G256_nbs_2.io.in_x := G256_inv.io.out
  G256_nbs_2.io.in_b := X2S

  io.out := G256_nbs_2.io.out_y ^ UInt("h63")

}

class SBoxTests(c: SBox) extends Tester(c) {

  val S = Array[BigInt](
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16)


  for {i <- 0 until 256} {

    poke(c.io.in, i)
    step(1)
    expect(c.io.out, S(i))

  }


}


/* find inverse Sbox of n in GF(2^8) mod POLY */
class ISBox extends Module {
  val io = new Bundle {
    val in = UInt(INPUT, 8)
    val out = UInt(OUTPUT, 8)
  }

  val A2X = Vec(UInt("h98"), UInt("hF3"), UInt("hF2"), UInt("h48"), UInt("h09"),
    UInt("h81"), UInt("hA9"), UInt("hFF"))
  val X2A = Vec(UInt("h64"), UInt("h78"), UInt("h6E"), UInt("h8C"), UInt("h68"),
    UInt("h29"), UInt("hDE"), UInt("h60"))
  val X2S = Vec(UInt("h58"), UInt("h2D"), UInt("h9E"), UInt("h0B"), UInt("hDC"),
    UInt("h04"), UInt("h03"), UInt("h24"))
  val S2X = Vec(UInt("h8C"), UInt("h79"), UInt("h05"), UInt("hEB"), UInt("h12"),
    UInt("h04"), UInt("h51"), UInt("h53"))


  val G256_nbs_1 = Module(new G256NB())
  val G256_nbs_2 = Module(new G256NB())
  val G256_inv = Module(new G256_inv())


  G256_nbs_1.io.in_x := io.in ^ UInt("h63")
  G256_nbs_1.io.in_b := S2X

  G256_inv.io.in := G256_nbs_1.io.out_y


  G256_nbs_2.io.in_x := G256_inv.io.out
  G256_nbs_2.io.in_b := X2A

  io.out := G256_nbs_2.io.out_y

}

class ISBoxTests(c: ISBox) extends Tester(c) {

  val iS = Array[BigInt](
    0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e,
    0x81, 0xf3, 0xd7, 0xfb,
    0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44,
    0xc4, 0xde, 0xe9, 0xcb,
    0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b,
    0x42, 0xfa, 0xc3, 0x4e,
    0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49,
    0x6d, 0x8b, 0xd1, 0x25,
    0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc,
    0x5d, 0x65, 0xb6, 0x92,
    0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57,
    0xa7, 0x8d, 0x9d, 0x84,
    0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05,
    0xb8, 0xb3, 0x45, 0x06,
    0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03,
    0x01, 0x13, 0x8a, 0x6b,
    0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce,
    0xf0, 0xb4, 0xe6, 0x73,
    0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8,
    0x1c, 0x75, 0xdf, 0x6e,
    0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e,
    0xaa, 0x18, 0xbe, 0x1b,
    0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe,
    0x78, 0xcd, 0x5a, 0xf4,
    0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59,
    0x27, 0x80, 0xec, 0x5f,
    0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f,
    0x93, 0xc9, 0x9c, 0xef,
    0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c,
    0x83, 0x53, 0x99, 0x61,
    0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63,
    0x55, 0x21, 0x0c, 0x7d)


  for {i <- 0 until 256} {

    poke(c.io.in, i)
    step(1)
    expect(c.io.out, iS(i))

  }


}


/* Non-linear byte substitution using SBox */
class SubByte extends Module {

  val io = new Bundle {
    val in = UInt(INPUT, 8)
    val out = UInt(OUTPUT, 8)
  }
  val x = (UInt("h10") * io.in(7, 4)) + io.in(3, 0)
  val sbox = Module(new SBox())

  sbox.io.in := x
  io.out := sbox.io.out

}


case object Nk extends Field[Int]

/* Sub bytes using  SubByte */

class SubBytes extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4

  val io = new Bundle {
    val state_in = Vec(num_bytes, UInt(INPUT, 8))
    val state_out = Vec(num_bytes, UInt(OUTPUT, 8))
  }

  val sub_bytes = Vec.fill(num_bytes) {
    Module(new SubByte()).io
  }

  for (i <- 0 until num_bytes) {
    sub_bytes(i).in := io.state_in(i)
    io.state_out(i) := sub_bytes(i).out

  }


}


/* Non-linear byte substitution using iSBox */
class InvSubByte extends Module {

  val io = new Bundle {
    val in = UInt(INPUT, 8)
    val out = UInt(OUTPUT, 8)
  }
  val x = (UInt("h10") * io.in(7, 4)) + io.in(3, 0)
  val isbox = Module(new ISBox())

  isbox.io.in := x
  io.out := isbox.io.out

}


class InvSubBytes extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4

  val io = new Bundle {
    val state_in = Vec(num_bytes, UInt(INPUT, 8))
    val state_out = Vec(num_bytes, UInt(OUTPUT, 8))
  }

  val sub_bytes = Vec.fill(num_bytes) {
    Module(new InvSubByte()).io
  }

  for (i <- 0 until num_bytes) {
    sub_bytes(i).in := io.state_in(i)
    io.state_out(i) := sub_bytes(i).out

  }


}


class InvSubBytesTests(c: InvSubBytes) extends Tester(c) {

  val output = Array[BigInt](0x19, 0xa0, 0x9a, 0xe9, 0x3d, 0xf4, 0xc6, 0xf8,
    0xe3, 0xe2, 0x8d, 0x48, 0xbe, 0x2b, 0x2a,
    0x08)

  val input = Array[BigInt](0xd4, 0xe0, 0xb8, 0x1e, 0x27, 0xbf, 0xb4, 0x41,
    0x11, 0x98, 0x5d, 0x52, 0xae, 0xf1, 0xe5,
    0x30)

  poke(c.io.state_in, input)
  step(1)
  expect(c.io.state_out, output)

}


class InvSubByteTests(c: InvSubByte) extends Tester(c) {
  poke(c.io.in, 0xd4)
  step(1)
  expect(c.io.out, 0x19)

  poke(c.io.in, 0xe0)
  step(1)
  expect(c.io.out, 0xa0)

  poke(c.io.in, 0xb8)
  step(1)
  expect(c.io.out, 0x9a)

  poke(c.io.in, 0x1e)
  step(1)
  expect(c.io.out, 0xe9)

  poke(c.io.in, 0x27)
  step(1)
  expect(c.io.out, 0x3d)

  poke(c.io.in, 0xbf)
  step(1)
  expect(c.io.out, 0xf4)

  poke(c.io.in, 0xb4)
  step(1)
  expect(c.io.out, 0xc6)
}


class InvShiftRows extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4
  val io = new Bundle {
    val state_in = Vec(num_bytes, UInt(INPUT, 8))
    val state_out = Vec(num_bytes, UInt(OUTPUT, 8))
  }

  // Select the last three rows for shifting
  val r1 = io.state_in.slice(num_bytes - 12, num_bytes - 8)
  val r2 = io.state_in.slice(num_bytes - 8, num_bytes - 4)
  val r3 = io.state_in.slice(num_bytes - 4, num_bytes)


  //unmodified rows
  for (i <- 0 until (num_bytes - 12)) {
    io.state_out(i) := io.state_in(i)
  }


  //row need cyclical shift from MSB to LSB by one
  var counter = 3
  for (i <- (num_bytes - 12) until (num_bytes - 8)) {
    println("counter: %d , i: %d ", counter, i)
    io.state_out(i) := r1(counter)

    if (counter > 2) {
      counter = 0
    } else {
      counter = counter + 1
    }

  }



  //row need cyclical shift from MSB to LSB by two
  counter = 2
  for (i <- (num_bytes - 8) until (num_bytes - 4)) {
    io.state_out(i) := r2(counter)

    if (counter > 2) {
      counter = 0
    } else {
      counter = counter + 1
    }


  }

  //row need cyclical shift from MSB to LSB by three
  counter = 1
  for (i <- (num_bytes - 4) until num_bytes) {
    io.state_out(i) := r3(counter)

    if (counter > 2) {
      counter = 0
    } else {
      counter = counter + 1
    }


  }


}

class ShiftRows extends Module {

  val key_size = params(Nk)
  val num_bytes = key_size * 4
  val io = new Bundle {
    val state_in = Vec(num_bytes, UInt(INPUT, 8))
    val state_out = Vec(num_bytes, UInt(OUTPUT, 8))
  }
  // Select the last three rows for shifting
  val r1 = io.state_in.slice(num_bytes - 12, num_bytes - 8)
  val r2 = io.state_in.slice(num_bytes - 8, num_bytes - 4)
  val r3 = io.state_in.slice(num_bytes - 4, num_bytes)


  //unmodified rows
  for (i <- 0 until (num_bytes - 12)) {
    io.state_out(i) := io.state_in(i)
  }


  //row need shifting by one
  var counter = 1
  for (i <- (num_bytes - 12) until (num_bytes - 8)) {
    println("counter: %d , i: %d ", counter, i)
    io.state_out(i) := r1(counter)

    if (counter < 3) {
      counter = counter + 1
    } else {
      counter = 0
    }


  }


  //row need shifting by two
  counter = 2
  for (i <- (num_bytes - 8) until (num_bytes - 4)) {
    io.state_out(i) := r2(counter)

    if (counter < 3) {
      counter = counter + 1
    } else {
      counter = 0
    }


  }


  //row need shifting by three
  counter = 3
  for (i <- (num_bytes - 4) until num_bytes) {
    io.state_out(i) := r3(counter)

    if (counter < 3) {
      counter = counter + 1
    } else {
      counter = 0
    }


  }


}


/* MixColumns() transformation*/
//TODO: implement to handle all possible key sizes
class MixColumns extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4
  val io = new Bundle {
    val state_in = Vec(num_bytes, UInt(INPUT, 8))
    val state_out = Vec(num_bytes, UInt(OUTPUT, 8))
  }

  def GFMul2(a: UInt): UInt = {
    val h = a.toSInt() >> 7
    val temp = a << 1
    val xor_val = UInt("h1B") & h
    temp ^ xor_val

  }

  if (key_size == 4) {
    for (i <- 0 until 4) {
      /* 2 * a0 + a3 + a2 + 3 * a1 */
      io.state_out(0 + i) := GFMul2(io.state_in(0 + i)) ^ io.state_in(12 + i) ^ io.state_in(8 + i) ^
        GFMul2(io.state_in(4 + i)) ^ io.state_in(4 + i)
      /* 2 * a1 + a0 + a3 + 3 * a2 */
      io.state_out(4 + i) := GFMul2(io.state_in(4 + i)) ^ io.state_in(0 + i) ^ io.state_in(12 + i) ^
        GFMul2(io.state_in(8 + i)) ^ io.state_in(8 + i)
      /* 2 * a2 + a1 + a0 + 3 * a3 */
      io.state_out(8 + i) := GFMul2(io.state_in(8 + i)) ^ io.state_in(4 + i) ^ io.state_in(0 + i) ^
        GFMul2(io.state_in(12 + i)) ^ io.state_in(12 + i)
      /* 2 * a3 + a2 + a1 + 3 * a0 */
      io.state_out(12 + i) := GFMul2(io.state_in(12 + i)) ^ io.state_in(8 + i) ^ io.state_in(4 + i) ^
        GFMul2(io.state_in(0 + i)) ^ io.state_in(0 + i)

    }

  }

}


/* MixColumns() transformation*/
//TODO: implement to handle all possible key sizes
class InvMixColumns extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4
  val io = new Bundle {
    val state_in = Vec(num_bytes, UInt(INPUT, 8))
    val state_out = Vec(num_bytes, UInt(OUTPUT, 8))
  }

  def GFMul2(a: UInt): UInt = {
    val h = a.toSInt() >> 7
    val temp = a << 1
    val xor_val = UInt("h1B") & h
    temp ^ xor_val

  }

  def Multiply(x: UInt, y: UInt): UInt = {
    (((y & UInt(1)) * x) ^
      ((y >> UInt(1) & UInt(1)) * xtime(x)) ^
      ((y >> UInt(2) & UInt(1)) * xtime(xtime(x))) ^
      ((y >> UInt(3) & UInt(1)) * xtime(xtime(xtime(x)))) ^
      ((y >> UInt(4) & UInt(1)) * xtime(xtime(xtime(xtime(x))))))
  }

  def xtime(x: UInt): UInt = {
    ((x << UInt(1)) ^ (((x >> UInt(7)) & UInt(1)) * UInt("h1b")))

  }


  if (key_size == 4) for (i <- 0 until 4) {
    //Multiplyiply(a, 0x0e) ^ Multiplyiply(b, 0x0b) ^ Multiplyiply(c, 0x0d) ^ Multiplyiply(d, 0x09);
    io.state_out(0 + i) := Multiply(io.state_in(0 + i), UInt("h0e")) ^ Multiply(io.state_in(4 + i), UInt("h0b")) ^
      Multiply(io.state_in(8 + i), UInt("h0d")) ^ Multiply(io.state_in(12 + i), UInt("h09"))

    //Multiplyiply(a, 0x09) ^ Multiplyiply(b, 0x0e) ^ Multiplyiply(c, 0x0b) ^ Multiplyiply(d, 0x0d);
    io.state_out(4 + i) := Multiply(io.state_in(0 + i), UInt("h09")) ^ Multiply(io.state_in(4 + i), UInt("h0e")) ^
      Multiply(io.state_in(8 + i), UInt("h0b")) ^ Multiply(io.state_in(12 + i), UInt("h0d"))
    //Multiplyiply(a, 0x0d) ^ Multiplyiply(b, 0x09) ^ Multiplyiply(c, 0x0e) ^ Multiplyiply(d, 0x0b);
    io.state_out(8 + i) := Multiply(io.state_in(0 + i), UInt("h0d")) ^ Multiply(io.state_in(4 + i), UInt("h09")) ^
      Multiply(io.state_in(8 + i), UInt("h0e")) ^ Multiply(io.state_in(12 + i), UInt("h0b"))
    //Multiplyiply(a, 0x0b) ^ Multiplyiply(b, 0x0d) ^ Multiplyiply(c, 0x09) ^ Multiplyiply(d, 0x0e);
    io.state_out(12 + i) := Multiply(io.state_in(0 + i), UInt("h0b")) ^ Multiply(io.state_in(4 + i), UInt("h0d")) ^
      Multiply(io.state_in(8 + i), UInt("h09")) ^ Multiply(io.state_in(12 + i), UInt("h0e"))
  }

}


class AddRoundKey extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4

  val io = new Bundle {
    val state_in = Vec(num_bytes, UInt(INPUT, 8))
    val round_key_in = Vec(num_bytes, UInt(INPUT, 8))
    val state_out = Vec(num_bytes, UInt(OUTPUT, 8))
  }

  for (i <- 0 until num_bytes) {
    io.state_out(i) := (io.state_in(i) ^ io.round_key_in(i))
  }

}

class SubWord(num_bytes: Int) extends Module {


  val io = new Bundle {
    val word_in = Vec(num_bytes, UInt(INPUT, 8))
    val word_out = Vec(num_bytes, UInt(OUTPUT, 8))
  }

  val sub_bytes = Vec.fill(num_bytes) {
    Module(new SubByte()).io
  }

  for (i <- 0 until num_bytes) {
    sub_bytes(i).in := io.word_in(i)
    io.word_out(i) := sub_bytes(i).out

  }


}


class KeyCore extends Module {
  val io = new Bundle {
    val key_in = Vec(4, UInt(INPUT, 8))
    val stage = UInt(INPUT, 8)
    val key_out = Vec(4, UInt(OUTPUT, 8))

  }

  //Recon[]
  val Rcon = Vec(UInt("h8d"), UInt("h01"), UInt("h02"), UInt("h04"),
    UInt("h08"), UInt("h10"), UInt("h20"), UInt("h40"), UInt("h80"),
    UInt("h1b"), UInt("h36"))


  //RotWord()

  val rot_word = Vec(4, UInt(width = 8))
  rot_word(0) := io.key_in(1)
  rot_word(1) := io.key_in(2)
  rot_word(2) := io.key_in(3)
  rot_word(3) := io.key_in(0)


  // SubWord()
  val sub_word = Module(new SubWord(4))
  sub_word.io.word_in := rot_word


  io.key_out(0) := sub_word.io.word_out(0) ^ Rcon(io.stage)
  io.key_out(1) := sub_word.io.word_out(1)
  io.key_out(2) := sub_word.io.word_out(2)
  io.key_out(3) := sub_word.io.word_out(3)


}

/* See http://goo.gl/206rn6 from schematics */
/*works only for 128-bits for now*/

class KeyExpansion extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4

  val io = new Bundle {
    val key_in = Vec(num_bytes, UInt(INPUT, 8))
    val stage = UInt(INPUT, 8)
    val key_out = Vec(num_bytes, UInt(OUTPUT, 8))
  }


  val c0 = Vec(key_size, UInt(width = 8))
  val c1 = Vec(key_size, UInt(width = 8))
  val c2 = Vec(key_size, UInt(width = 8))
  val c3 = Vec(key_size, UInt(width = 8))


  val k0 = Vec(key_size, UInt(width = 8))
  val k1 = Vec(key_size, UInt(width = 8))
  val k2 = Vec(key_size, UInt(width = 8))
  val k3 = Vec(key_size, UInt(width = 8))

  for (i <- 0 until 4) {
    c0(i) := io.key_in(i * 4)
    c1(i) := io.key_in((i * 4) + 1)
    c2(i) := io.key_in((i * 4) + 2)
    c3(i) := io.key_in((i * 4) + 3)
  }

  // non linear transformation for last column only

  val key_core = Module(new KeyCore()).io
  key_core.key_in := c3
  key_core.stage := io.stage


  for (i <- 0 until 4) {
    k0(i) := key_core.key_out(i) ^ c0(i)
  }


  for (i <- 0 until 4) {
    k1(i) := c1(i) ^ k0(i)
  }

  for (i <- 0 until 4) {
    k2(i) := c2(i) ^ k1(i)
  }

  for (i <- 0 until 4) {
    k3(i) := c3(i) ^ k2(i)
  }



  for (i <- 0 until 16 by 4) {
    io.key_out(i) := k0(i / 4)
    io.key_out(i + 1) := k1(i / 4)
    io.key_out(i + 2) := k2(i / 4)
    io.key_out(i + 3) := k3(i / 4)
  }


}


class InvRound extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4

  val io = new Bundle {
    val state_in = Vec(num_bytes, UInt(INPUT, 8))
    val round_key = Vec(num_bytes, UInt(INPUT, 8))
    //val ready = Bool(INPUT)
    val state_out = Vec(num_bytes, UInt(OUTPUT, 8))
    //val valid   = Bool(OUTPUT)
  }


  /* ShiftRows */
  val shift_rows = Module(new InvShiftRows).io
  shift_rows.state_in := io.state_in

  /*SubBytes*/
  val sub_bytes = Module(new InvSubBytes).io
  sub_bytes.state_in := shift_rows.state_out


  /* AddRoundKey */
  val add_round_key = Module(new AddRoundKey).io
  add_round_key.state_in := sub_bytes.state_out
  add_round_key.round_key_in := io.round_key

  /*MixColumns */
  val mix_columns = Module(new InvMixColumns).io
  mix_columns.state_in := add_round_key.state_out

  io.state_out := mix_columns.state_out

}

class EqInvRound extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4

  val io = new Bundle {
    val state_in = Vec(num_bytes, UInt(INPUT, 8))
    val round_key = Vec(num_bytes, UInt(INPUT, 8))
    //val ready = Bool(INPUT)
    val state_out = Vec(num_bytes, UInt(OUTPUT, 8))
    //val valid   = Bool(OUTPUT)
  }


  /*SubBytes*/
  val sub_bytes = Module(new InvSubBytes).io
  sub_bytes.state_in := io.state_in

  /* ShiftRows */
  val shift_rows = Module(new InvShiftRows).io
  shift_rows.state_in := sub_bytes.state_out

  /*MixColumns */
  val mix_columns = Module(new InvMixColumns).io
  mix_columns.state_in := shift_rows.state_out

  /* AddRoundKey */

  val add_round_key = Module(new AddRoundKey).io
  add_round_key.state_in := mix_columns.state_out
  add_round_key.round_key_in := io.round_key

  io.state_out := add_round_key.state_out

}


class EqInvRoundTests(c: EqInvRound) extends Tester(c) {

  var input = Utilities.Row_to_column(Array[BigInt](0x7a, 0xd5, 0xfd, 0xa7,
    0x89, 0xef, 0x4e, 0x27, 0x2b, 0xca, 0x10, 0x0b, 0x3d, 0x9f, 0xf5, 0x9f))

  var output = Utilities.Row_to_column(Array[BigInt](0x54, 0xd9, 0x90, 0xa1,
    0x6b, 0xa0, 0x9a, 0xb5, 0x96, 0xbb, 0xf4, 0x0e, 0xa1, 0x11, 0x70, 0x2f))

  var round_key = Utilities.Row_to_column(Array[BigInt](0x13, 0xaa, 0x29, 0xbe,
    0x9c, 0x8f, 0xaf, 0xf6, 0xf7, 0x70, 0xf5, 0x80, 0x00, 0xf7, 0xbf, 0x03))

  poke(c.io.state_in, input)
  poke(c.io.round_key, round_key)
  step(1)
  expect(c.io.state_out, output)
}

class Round extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4

  val io = new Bundle {
    val state_in = Vec(num_bytes, UInt(INPUT, 8))
    val round_key = Vec(num_bytes, UInt(INPUT, 8))
    //val ready = Bool(INPUT)
    val state_out = Vec(num_bytes, UInt(OUTPUT, 8))
    //val valid   = Bool(OUTPUT)
  }


  /*SubBytes*/
  val sub_bytes = Module(new SubBytes).io
  sub_bytes.state_in := io.state_in

  /* ShiftRows */
  val shift_rows = Module(new ShiftRows).io
  shift_rows.state_in := sub_bytes.state_out

  /*MixColumns */
  val mix_columns = Module(new MixColumns).io
  mix_columns.state_in := shift_rows.state_out

  /* AddRoundKey */

  val add_round_key = Module(new AddRoundKey).io
  add_round_key.state_in := mix_columns.state_out
  add_round_key.round_key_in := io.round_key

  io.state_out := add_round_key.state_out

}


class KeyExpander extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4

  val io = new Bundle {
    val input = Vec(num_bytes, UInt(INPUT, 8))
    val output = Vec(key_size + 6, Vec(num_bytes, UInt(OUTPUT, 8)))
  }

  val num_rounds = key_size + 6

  val key_schedule = Vec(num_rounds, Module(new KeyExpansion()).io)

  key_schedule(0).key_in := io.input
  key_schedule(0).stage := UInt(1)



  for (i <- 1 until num_rounds) {

    key_schedule(i).key_in := key_schedule(i - 1).key_out
    key_schedule(i).stage := UInt(i + 1)

  }



  for (i <- 0 until num_rounds) {
    io.output(i) := key_schedule(i).key_out

  }


}


class KeyExpanderTests(c: KeyExpander) extends Tester(c) {

  val output8 = Array[BigInt](0xac, 0x19, 0x28, 0x57, 0x77, 0xfa, 0xd1, 0x5c,
    0x66, 0xdc, 0x29, 0x00, 0xf3, 0x21, 0x41, 0x6e)
  var input = Array[BigInt](0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
    0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f)
  var output0 = Array[BigInt](0xa0, 0x88, 0x23, 0x2a, 0xfa, 0x54, 0xa3, 0x6c,
    0xfe, 0x2c, 0x39, 0x76, 0x17, 0xb1, 0x39, 0x05)
  var output9 = Array[BigInt](0x13, 0x11, 0x1d, 0x7f, 0xe3, 0x94, 0x4a, 0x17, 0xf3, 0x07,
    0xa7, 0x8b, 0x4d, 0x2b, 0x30, 0xc5)

  poke(c.io.input, input)
  step(1)
  //expect(c.io.output(0), output0)
  expect(c.io.output(9), output9)
  //expect(c.io.output(8), output8)
  //peek(c.io.output)

}


class AESDecrypt extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4
  val num_bits = num_bytes * 8
  val num_rounds = key_size + 6

  val io = new Bundle {
    val input = Vec(num_bytes, UInt(INPUT, 8))
    val key = Vec(num_bytes, UInt(INPUT, 8))
    val ready = Bool(INPUT)
    val output = Vec(num_bytes, UInt(OUTPUT, 8))
    val valid = Bool(OUTPUT)
    val busy = Bool(OUTPUT)
  }


  /* Initialize wires touched by when() */
  io.valid := Bool(false)
  io.busy := Bool(false)


  val s_key :: s_rounds :: s_idle :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)


  val key_scheduler = Module(new KeyExpander()).io
  key_scheduler.input := io.key
  val round_keys = key_scheduler.output


  /* InitialRound  */
  val initial_round_key = Module(new AddRoundKey()).io
  initial_round_key.round_key_in := round_keys(num_rounds - 1)
  //initial_round_key.state_in := Vec.fill(num_bytes){UInt(0,8)}
  initial_round_key.state_in := io.input


  val rounds = Vec(num_rounds - 1, Module(new InvRound()).io)

  /* wire up the rounds */
  /* input for the first round comes from add-round_key*/
  rounds(0).round_key := round_keys(num_rounds - 2)
  rounds(0).state_in := initial_round_key.state_out


  for (i <- 1 until (num_rounds - 1)) {
    rounds(i).round_key := round_keys(8 - i)
    println("round_keys %d", 8 - i)
    rounds(i).state_in := rounds(i - 1).state_out

  }

  /* Modules used for Final round */

  val shift_rows = Module(new InvShiftRows()).io
  val sub_bytes = Module(new InvSubBytes()).io
  val final_add_round_key = Module(new AddRoundKey()).io

  /* wire up last round*/
  shift_rows.state_in := rounds(num_rounds - 2).state_out
  sub_bytes.state_in := shift_rows.state_out
  final_add_round_key.state_in := sub_bytes.state_out
  final_add_round_key.round_key_in := io.key

  /* idle/reset state */
  when(state === s_idle) {
    printf("s_idle\n")
    io.valid := Bool(false)
    io.busy := Bool(false)


    when(io.ready) {
      state := s_key
    }

  }


  /* register key and compute round keys */

  when(state === s_key) {
    printf("s_key\n")
    io.busy := Bool(true)
    key_scheduler.input := io.key
    state := s_rounds

    //printf("key #%x\n", io.key(0) )
    //printf("input #%x\n", input)
  }

  /* rounds section */

  when(state === s_rounds) {
    printf("s_rounds\n")
    initial_round_key.round_key_in := round_keys(num_rounds - 1)
    initial_round_key.state_in := io.input
    io.busy := Bool(false)
    io.valid := Bool(true)
    state := s_idle
  }

  io.output := final_add_round_key.state_out


}

class EqInvAESDecrypt extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4
  val num_bits = num_bytes * 8
  val num_rounds = key_size + 6

  val io = new Bundle {
    val input = Vec(num_bytes, UInt(INPUT, 8))
    val key = Vec(num_bytes, UInt(INPUT, 8))
    val ready = Bool(INPUT)
    val output = Vec(num_bytes, UInt(OUTPUT, 8))
    val valid = Bool(OUTPUT)
    val busy = Bool(OUTPUT)
  }


  /* Initialize wires touched by when() */
  io.valid := Bool(false)
  io.busy := Bool(false)


  val s_key :: s_rounds :: s_idle :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)
  //val key    = Vec.fill(num_bytes){ Reg(init = UInt(0, width = 8))}
  //val input  = Vec.fill(num_bytes){ Reg(init = UInt(0, width = 8))}
  //val output = Vec.fill(num_bytes){ Reg(init = UInt(0, width = 8))}

  val key_scheduler = Module(new KeyExpander()).io
  key_scheduler.input := io.key
  //val round_keys = key_scheduler.output

  /* InitialRound  */
  val initial_round_key = Module(new AddRoundKey()).io
  initial_round_key.round_key_in := key_scheduler.output(num_rounds - 1)
  initial_round_key.state_in := io.input


  /* Rounds() */


  val rounds = Vec(num_rounds - 1, Module(new EqInvRound()).io)
  val key_inv_mix_cols = Vec(num_rounds - 1, Module(new InvMixColumns()).io)

  /* wire up the rounds */
  /* input for the first round comes from add-round_key*/
  key_inv_mix_cols(0).state_in := key_scheduler.output(num_rounds - 2)
  rounds(0).round_key := key_inv_mix_cols(0).state_out
  rounds(0).state_in := initial_round_key.state_out



  for (i <- 1 until (num_rounds - 1)) {
    key_inv_mix_cols(i).state_in := key_scheduler.output(num_rounds - 2 - i)
    rounds(i).round_key := key_inv_mix_cols(i).state_out
    rounds(i).state_in := rounds(i - 1).state_out

  }

  /* Modules used for Final round */

  val sub_bytes = Module(new InvSubBytes()).io
  val shift_rows = Module(new InvShiftRows()).io
  val final_add_round_key = Module(new AddRoundKey()).io

  /* wire up last round*/
  sub_bytes.state_in := rounds(num_rounds - 2).state_out
  shift_rows.state_in := sub_bytes.state_out
  final_add_round_key.state_in := shift_rows.state_out
  final_add_round_key.round_key_in := io.key

  /* idle/reset state */
  when(state === s_idle) {
    printf("s_idle\n")
    io.valid := Bool(false)
    io.busy := Bool(false)


    when(io.ready) {
      state := s_key
    }

  }


  /* register key and compute round keys */

  when(state === s_key) {
    printf("s_key\n")
    io.busy := Bool(true)
    key_scheduler.input := io.key
    state := s_rounds

    //printf("key #%x\n", io.key(0) )
    //printf("input #%x\n", input)
  }

  /* rounds section */

  when(state === s_rounds) {
    printf("s_rounds\n")
    //initial_round_key.round_key_in := key_scheduler.output(num_rounds-1)
    initial_round_key.state_in := io.input
    io.busy := Bool(false)
    io.valid := Bool(true)
    state := s_idle
  }

  //io.output := rounds(2).state_out
  io.output := final_add_round_key.state_out
  //io.output :=  initial_round_key.state_out

}


class EqInvAESDecryptTests(c: EqInvAESDecrypt) extends Tester(c) {
  var input = Row_to_column(Array[BigInt](0x69, 0xc4, 0xe0, 0xd8, 0x6a, 0x7b, 0x04,
    0x30, 0xd8, 0xcd, 0xb7, 0x80, 0x70, 0xb4, 0xc5, 0x5a))
  var output = Row_to_column(Array[BigInt](0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66,
    0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff))
  var round0 = Row_to_column(Array[BigInt](0x7a, 0xd5, 0xfd, 0xa7, 0x89, 0xef, 0x4e,
    0x27, 0x2b, 0xca, 0x10, 0x0b, 0x3d, 0x9f, 0xf5, 0x9f))
  var round9 = Row_to_column(Array[BigInt](0x63, 0x53, 0xe0, 0x8c, 0x09, 0x60, 0xe1,
    0x04, 0xcd, 0x70, 0xb7, 0x51, 0xba, 0xca, 0xd0, 0xe7))
  var round1 = Row_to_column(Array[BigInt](0x54, 0xd9, 0x90, 0xa1, 0x6b, 0xa0, 0x9a,
    0xb5, 0x96, 0xbb, 0xf4, 0x0e, 0xa1, 0x11, 0x70, 0x2f))
  var round2 = Row_to_column(Array[BigInt](0x3e, 0x1c, 0x22, 0xc0, 0xb6, 0xfc, 0xbf,
    0x76, 0x8d, 0xa8, 0x50, 0x67, 0xf6, 0x17, 0x04, 0x95))
  var round3 = Row_to_column(Array[BigInt](0xb4, 0x58, 0x12, 0x4c, 0x68, 0xb6, 0x8a,
    0x01, 0x4b, 0x99, 0xf8, 0x2e, 0x5f, 0x15, 0x55, 0x4c))
  var round4 = Row_to_column(Array[BigInt](0xe8, 0xda, 0xb6, 0x90, 0x14, 0x77, 0xd4,
    0x65, 0x3f, 0xf7, 0xf5, 0xe2, 0xe7, 0x47, 0xdd, 0x4f))
  var key = Row_to_column(Array[BigInt](0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
    0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f))

  def Row_to_column(input: Array[BigInt]): Array[BigInt] = {


    assert(input.length == 16)

    val a = Array.ofDim[BigInt](4)
    val b = Array.ofDim[BigInt](4)
    val c = Array.ofDim[BigInt](4)
    val d = Array.ofDim[BigInt](4)

    for (i <- 0 until 4) {
      a(i) = input(i * 4)
      b(i) = input((i * 4) + 1)
      c(i) = input((i * 4) + 2)
      d(i) = input((i * 4) + 3)
    }
    val output = a ++ b ++ c ++ d
    println(output)
    output
  }


  /* Initialize  state machine */
  poke(c.io.input, input)
  poke(c.io.key, key)
  //poke(c.io.ready, 1)
  step(1)
  expect(c.io.busy, 0)
  peek(c.io.output)


  /* start computation */
  poke(c.io.ready, 1)
  step(1)
  expect(c.io.busy, 1)
  expect(c.io.valid, 0)
  peek(c.initial_round_key.state_out)
  //peek(c.io.output)

  step(1)
  poke(c.io.ready, 0)
  peek(c.state)
  expect(c.io.busy, 0)
  expect(c.io.valid, 1)
  peek(c.initial_round_key.state_out)
  //peek(c.rounds(0).state_in)
  //peek(c.rounds(0).state_out)
  expect(c.io.output, output)


  step(1)
  peek(c.state)
  expect(c.io.busy, 0)
  expect(c.io.valid, 0)


}


class AESEncrypt extends Module {
  val key_size = params(Nk)
  val num_bytes = key_size * 4
  val num_bits = num_bytes * 8
  val num_rounds = key_size + 6

  val io = new Bundle {
    val input = Vec(num_bytes, UInt(INPUT, 8))
    val key = Vec(num_bytes, UInt(INPUT, 8))
    val ready = Bool(INPUT)
    val output = Vec(num_bytes, UInt(OUTPUT, 8))
    val valid = Bool(OUTPUT)
    val busy = Bool(OUTPUT)
  }


  /* Initialize wires touched by when() */
  io.valid := Bool(false)
  io.busy := Bool(false)


  val s_key :: s_rounds :: s_idle :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)
  //val key    = Vec.fill(num_bytes){ Reg(init = UInt(0, width = 8))}
  //val input  = Vec.fill(num_bytes){ Reg(init = UInt(0, width = 8))}
  //val output = Vec.fill(num_bytes){ Reg(init = UInt(0, width = 8))}

  val key_scheduler = Module(new KeyExpander()).io
  key_scheduler.input := io.key
  val round_keys = key_scheduler.output

  /* InitialRound  */
  val initial_round_key = Module(new AddRoundKey()).io
  initial_round_key.round_key_in := io.key
  //initial_round_key.state_in := Vec.fill(num_bytes){UInt(0,8)}
  initial_round_key.state_in := io.input


  /* Rounds() */


  val rounds = Vec(num_rounds - 1, Module(new Round()).io)

  /* wire up the rounds */
  /* input for the first round comes from add-round_key*/
  rounds(0).round_key := round_keys(0)
  rounds(0).state_in := initial_round_key.state_out



  for (i <- 1 until (num_rounds - 1)) {
    rounds(i).round_key := round_keys(i)
    rounds(i).state_in := rounds(i - 1).state_out

  }

  /* Modules used for Final round */

  val sub_bytes = Module(new SubBytes()).io
  val shift_rows = Module(new ShiftRows()).io
  val final_add_round_key = Module(new AddRoundKey()).io

  /* wire up last round*/
  sub_bytes.state_in := rounds(num_rounds - 2).state_out
  shift_rows.state_in := sub_bytes.state_out
  final_add_round_key.state_in := shift_rows.state_out
  final_add_round_key.round_key_in := round_keys(num_rounds - 1)

  /* idle/reset state */
  when(state === s_idle) {
    printf("s_idle\n")
    io.valid := Bool(false)
    io.busy := Bool(false)


    when(io.ready) {
      state := s_key
    }

  }


  /* register key and compute round keys */

  when(state === s_key) {
    printf("s_key\n")
    io.busy := Bool(true)
    key_scheduler.input := io.key
    state := s_rounds

    //printf("key #%x\n", io.key(0) )
    //printf("input #%x\n", input)
  }

  /* rounds section */

  when(state === s_rounds) {
    printf("s_rounds\n")
    initial_round_key.round_key_in := io.key
    initial_round_key.state_in := io.input
    io.busy := Bool(false)
    io.valid := Bool(true)
    state := s_idle
  }

  //io.output := final_add_round_key.state_out
  io.output := final_add_round_key.state_out

}

class AESEncryptIter extends Module {

  val key_size = params(Nk)
  val num_bytes = key_size * 4
  val num_bits = num_bytes * 8
  val num_rounds = key_size + 6

  val io = new Bundle {
    val input = Vec(num_bytes, UInt(INPUT, 8))
    val key = Vec(num_bytes, UInt(INPUT, 8))
    val ready = Bool(INPUT)
    val output = Vec(num_bytes, UInt(OUTPUT, 8))
    val valid = Bool(OUTPUT)
    val busy = Bool(OUTPUT)
  }
  val s_idle :: s_init_round :: s_rounds :: s_final_round :: Nil = Enum(UInt(), 4)
  val state = Reg(init = s_idle)
  val busy = Reg(init = Bool(false))
  val valid = Reg(init = Bool(false))
  /* Counters for keeping track of rounds */
  val counter = Reg(init = UInt(1, log2Up(num_rounds + 3)))
  val round = Reg(init = UInt(0, log2Up(num_rounds + 3)))
  val aes_key = Vec.fill(num_bytes) {
    Reg(init = UInt(0, width = 8))
  }
  //val aes_key_prime = Vec.fill(num_bytes){ Reg(init = UInt(0, width = 8))}
  val aes_state = Vec.fill(num_bytes) {
    Reg(init = UInt(0, width = 8))
  }
  /* InitialRound  */
  val initial_round_key = Module(new AddRoundKey()).io
  //val aes_state_prime =  Vec.fill(num_bytes){ Reg(init = UInt(0, width = 8))}


  /* Initialize rest of the items touched by when() */
  io.valid := valid
  io.busy := busy
  //io.output := Vec.fill(num_bytes){UInt(0,8)}
  /* Key  expander, computes stage key sequentially  */
  val key_expand = Module(new KeyExpansion()).io
  initial_round_key.round_key_in := aes_key
  initial_round_key.state_in := aes_state
  val aes_round = Module(new Round()).io
  key_expand.key_in := aes_key
  key_expand.stage := round
  val sub_bytes = Module(new SubBytes()).io
  aes_round.round_key := aes_key
  aes_round.state_in := aes_state


  /* Modules used for Final round */
  val shift_rows = Module(new ShiftRows()).io
  val final_add_round_key = Module(new AddRoundKey()).io

  def risingedge(x: Bool) = x && !Reg(next = x)

  /* wire up last round*/
  sub_bytes.state_in := aes_state
  shift_rows.state_in := sub_bytes.state_out
  final_add_round_key.state_in := shift_rows.state_out
  final_add_round_key.round_key_in := aes_key



  /* idle/reset state */
  when(state === s_idle) {
    printf("s_idle\n")
    valid := Bool(false)
    busy := Bool(false)


    when(io.ready) {
      state := s_init_round;
      round := UInt(1)
    }

  }



  when(state === s_init_round) {
    printf("s_init_round\n")
    busy := Bool(true)
    counter := UInt(1)

    initial_round_key.round_key_in := io.key
    initial_round_key.state_in := io.input
    aes_state := initial_round_key.state_out

    /* Pre-calculate next round's key */
    key_expand.key_in := io.key
    key_expand.stage := round
    aes_key := key_expand.key_out
    //io.output := initial_round_key.state_out


    //increment round for next cycle

    round := round + UInt(1)

    state := s_rounds

  }

  when(state === s_rounds) {
    printf("s_rounds\n")
    printf("counter %d\n", counter)

    when(counter === UInt(9)) {
      /* Pre-calculate next round's key */

      key_expand.key_in := aes_key
      key_expand.stage := round
      aes_key := key_expand.key_out
      //io.output := key_expand.key_out

      aes_round.round_key := aes_key
      aes_round.state_in := aes_state
      aes_state := aes_round.state_out

      state := s_final_round

    }
      .otherwise {
        /* Pre-calculate next round's key */

        key_expand.key_in := aes_key
        key_expand.stage := round
        aes_key := key_expand.key_out
        //io.output := key_expand.key_out

        aes_round.round_key := aes_key
        aes_round.state_in := aes_state
        aes_state := aes_round.state_out

        counter := counter + UInt(1)
        round := round + UInt(1)
        state := s_rounds
      }


  }




  when(state === s_final_round) {
    printf("s_final_round\n")


    sub_bytes.state_in := aes_state
    final_add_round_key.round_key_in := aes_key
    //io.output := final_add_round_key.state_out

    valid := Bool(true)
    state := s_idle
  }

  //io.output := Vec.fill(num_bytes){ UInt(0, width = 8)}
  io.output := final_add_round_key.state_out
  //io.valid  :=  (state === s_ok)

}


class EqInvAESDecryptIter extends Module {

  val key_size = params(Nk)
  val num_bytes = key_size * 4
  val num_bits = num_bytes * 8
  val num_rounds = key_size + 6

  val io = new Bundle {
    val input = Vec(num_bytes, UInt(INPUT, 8))
    val key = Vec(num_bytes, UInt(INPUT, 8))
    val ready = Bool(INPUT)
    val output = Vec(num_bytes, UInt(OUTPUT, 8))
    val valid = Bool(OUTPUT)
    val busy = Bool(OUTPUT)
  }
  val s_idle :: s_init_round :: s_rounds :: s_final_round :: Nil = Enum(UInt(), 4)
  val state = Reg(init = s_idle)
  val busy = Reg(init = Bool(false))
  val valid = Reg(init = Bool(false))
  /* Counters for keeping track of rounds */
  val counter = Reg(init = UInt(1, log2Up(num_rounds + 3)))
  val round = Reg(init = UInt(0, log2Up(num_rounds + 3)))
  val aes_state = Vec.fill(num_bytes) {
    Reg(init = UInt(0, width = 8))
  }
  /* Key  expander, computes key schdeule at once instead of sequentially as
  in the encrypt version */
  val key_expand = Module(new KeyExpander()).io
  //val aes_state_prime =  Vec.fill(num_bytes){ Reg(init = UInt(0, width = 8))}


  /* Initialize rest of the items touched by when() */
  io.valid := valid
  io.busy := busy
  //io.output := Vec.fill(num_bytes){UInt(0,8)}
  val round_keys = key_expand.output
  key_expand.input := io.key
  /* InitialRound  */
  val initial_round_key = Module(new AddRoundKey()).io
  val key_inv_mix_cols = Module(new InvMixColumns()).io
  initial_round_key.round_key_in := round_keys(num_rounds - 1)
  initial_round_key.state_in := aes_state
  val aes_round = Module(new EqInvRound()).io
  key_inv_mix_cols.state_in := Vec.fill(num_bytes) {
    UInt(0, width = 8)
  }
  val sub_bytes = Module(new InvSubBytes()).io
  aes_round.round_key := Vec.fill(num_bytes) {
    UInt(0, 8)
  }
  aes_round.state_in := aes_state


  /* Modules used for Final round */
  val shift_rows = Module(new InvShiftRows()).io
  val final_add_round_key = Module(new AddRoundKey()).io

  def risingedge(x: Bool) = x && !Reg(next = x)

  /* wire up last round*/
  sub_bytes.state_in := aes_state
  shift_rows.state_in := sub_bytes.state_out
  final_add_round_key.state_in := shift_rows.state_out
  //final_add_round_key.round_key_in := aes_key
  final_add_round_key.round_key_in := io.key



  /* idle/reset state */
  when(state === s_idle) {
    printf("s_idle\n")
    valid := Bool(false)
    busy := Bool(false)


    when(io.ready) {
      state := s_init_round
      key_expand.input := io.key
      round := UInt(9)
    }

  }



  when(state === s_init_round) {
    printf("s_init_round\n")
    busy := Bool(true)
    counter := UInt(1)

    initial_round_key.round_key_in := round_keys(round)
    initial_round_key.state_in := io.input
    aes_state := initial_round_key.state_out


    //decrement round for next cycle

    round := round - UInt(1)

    state := s_rounds

  }

  when(state === s_rounds) {
    printf("s_rounds\n")
    printf("counter %d\n", counter)

    when(counter === UInt(9)) {

      key_inv_mix_cols.state_in := round_keys(round)
      aes_round.round_key := key_inv_mix_cols.state_out
      aes_round.state_in := aes_state
      aes_state := aes_round.state_out

      state := s_final_round

    }
      .otherwise {

        key_inv_mix_cols.state_in := round_keys(round)
        aes_round.round_key := key_inv_mix_cols.state_out
        aes_round.state_in := aes_state
        aes_state := aes_round.state_out

        counter := counter + UInt(1)
        round := round - UInt(1)
        state := s_rounds
      }


  }


  when(state === s_final_round) {
    printf("s_final_round\n")


    sub_bytes.state_in := aes_state
    //io.output := final_add_round_key.state_out

    valid := Bool(true)
    state := s_idle
  }

  //io.output := Vec.fill(num_bytes){ UInt(0, width = 8)}
  io.output := final_add_round_key.state_out
  //io.valid  :=  (state === s_ok)

}


class EqInvAESDecryptIterTests(c: EqInvAESDecryptIter) extends Tester(c) {
  var input = Utilities.Row_to_column(Array[BigInt](0x69, 0xc4, 0xe0, 0xd8, 0x6a,
    0x7b, 0x04, 0x30, 0xd8, 0xcd, 0xb7, 0x80, 0x70, 0xb4, 0xc5, 0x5a))
  var output = Utilities.Row_to_column(Array[BigInt](0x00, 0x11, 0x22, 0x33, 0x44,
    0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff))
  var round0 = Utilities.Row_to_column(Array[BigInt](0x7a, 0xd5, 0xfd, 0xa7, 0x89,
    0xef, 0x4e, 0x27, 0x2b, 0xca, 0x10, 0x0b, 0x3d, 0x9f, 0xf5, 0x9f))
  var round9 = Utilities.Row_to_column(Array[BigInt](0x63, 0x53, 0xe0, 0x8c, 0x09,
    0x60, 0xe1, 0x04, 0xcd, 0x70, 0xb7, 0x51, 0xba, 0xca, 0xd0, 0xe7))
  var round1 = Utilities.Row_to_column(Array[BigInt](0x54, 0xd9, 0x90, 0xa1, 0x6b,
    0xa0, 0x9a, 0xb5, 0x96, 0xbb, 0xf4, 0x0e, 0xa1, 0x11, 0x70, 0x2f))
  var round2 = Utilities.Row_to_column(Array[BigInt](0x3e, 0x1c, 0x22, 0xc0, 0xb6,
    0xfc, 0xbf, 0x76, 0x8d, 0xa8, 0x50, 0x67, 0xf6, 0x17, 0x04, 0x95))
  var round3 = Utilities.Row_to_column(Array[BigInt](0xb4, 0x58, 0x12, 0x4c, 0x68,
    0xb6, 0x8a, 0x01, 0x4b, 0x99, 0xf8, 0x2e, 0x5f, 0x15, 0x55, 0x4c))
  var round4 = Utilities.Row_to_column(Array[BigInt](0xe8, 0xda, 0xb6, 0x90, 0x14,
    0x77, 0xd4, 0x65, 0x3f, 0xf7, 0xf5, 0xe2, 0xe7, 0x47, 0xdd, 0x4f))
  var key = Utilities.Row_to_column(Array[BigInt](0x00, 0x01, 0x02, 0x03, 0x04,
    0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f))


  /* Initialize  state machine */
  poke(c.io.input, input)
  poke(c.io.key, key)
  peek(c.state)
  step(1)
  expect(c.io.busy, 0)
  //peek(c.io.output)

  /* start computation */
  poke(c.io.ready, 1)
  step(1)
  //peek(c.aes_key)
  peek(c.state)
  //peek(c.round_key_prime)
  //peek(c.aes_state)

  for (i <- 0 until 10) {
    step(1)
    //poke(c.io.ready, 0)
    //peek(c.aes_key)
    peek(c.counter)
    peek(c.round)
    //peek(c.key_expand.key_in)
    //peek(c.key_expand.key_out)
    peek(c.aes_round.state_in)
    peek(c.aes_round.round_key)
    peek(c.aes_round.state_out)
    //peek(c.initial_round_key.state_out)
    //peek(c.initial_round_key.state_out)
    peek(c.state)
    //peek(c.initial_round_key.state_out)
    expect(c.io.busy, 1)
    //peek(c.round_key_prime)
    //peek(c.aes_state)

  }

  step(1)
  peek(c.state)
  peek(c.io.output)
  peek(c.sub_bytes.state_in)
  peek(c.final_add_round_key.round_key_in)
  expect(c.io.busy, 1)
  expect(c.io.valid, 1)
  expect(c.io.output, output)


}


class AESEncryptIterTests(c: AESEncryptIter) extends Tester(c) {
  var plaintext = Array[BigInt](0x32, 0x88, 0x31, 0xe0, 0x43, 0x5a, 0x31, 0x37, 0xf6, 0x30, 0x98, 0x07, 0xa8, 0x8d, 0xa2, 0x34)
  var ciphertext = Array[BigInt](0x39, 0x02, 0xdc, 0x19, 0x25, 0xdc, 0x11, 0x6a, 0x84, 0x09, 0x85, 0x0b, 0x1d, 0xfb, 0x97, 0x32)
  var round0 = Array[BigInt](0x19, 0xa0, 0x9a, 0xe9, 0x3d, 0xf4, 0xc6, 0xf8, 0xe3, 0xe2, 0x8d, 0x48, 0xbe, 0x2b, 0x2a, 0x08)
  var round9 = Array[BigInt](0xeb, 0x59, 0x8b, 0x1b, 0x40, 0x2e, 0xa1, 0xc3, 0xf2, 0x38, 0x13, 0x42, 0x1e, 0x84, 0xe7, 0xd2)
  var round1 = Array[BigInt](0xa4, 0x68, 0x6b, 0x02, 0x9c, 0x9f, 0x5b, 0x6a, 0x7f, 0x35, 0xea, 0x50, 0xf2, 0x2b, 0x43, 0x49)
  var key = Array[BigInt](0x2b, 0x28, 0xab, 0x09, 0x7e, 0xae, 0xf7, 0xcf, 0x15, 0xd2, 0x15, 0x4f, 0x16, 0xa6, 0x88, 0x3c)


  /* Initialize  state machine */
  poke(c.io.input, plaintext)
  poke(c.io.key, key)
  peek(c.state)
  step(1)
  expect(c.io.busy, 0)
  //peek(c.io.output)

  /* start computation */
  poke(c.io.ready, 1)
  step(1)
  //peek(c.aes_key)
  peek(c.state)
  //peek(c.round_key_prime)
  //peek(c.aes_state)

  for (i <- 0 until 10) {
    step(1)
    //poke(c.io.ready, 0)
    //peek(c.aes_key)
    peek(c.counter)
    peek(c.round)
    peek(c.key_expand.key_in)
    peek(c.key_expand.key_out)
    peek(c.aes_round.state_in)
    peek(c.aes_round.round_key)
    peek(c.aes_round.state_out)
    //peek(c.initial_round_key.state_out)
    //peek(c.initial_round_key.state_out)
    peek(c.state)
    //peek(c.initial_round_key.state_out)
    expect(c.io.busy, 1)
    //peek(c.round_key_prime)
    //peek(c.aes_state)

  }

  step(1)
  peek(c.state)
  peek(c.io.output)
  peek(c.sub_bytes.state_in)
  peek(c.final_add_round_key.round_key_in)
  expect(c.io.busy, 1)
  expect(c.io.valid, 1)
  expect(c.io.output, ciphertext)



  key = Utilities.Row_to_column(Array[BigInt](0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2
    , 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c))
  plaintext = Utilities.Row_to_column(Array[BigInt](0x6b, 0xc1, 0xbe, 0xe2, 0x2e,
    0x40, 0x9f, 0x96, 0xe9, 0x3d, 0x7e, 0x11, 0x73, 0x93, 0x17, 0x2a))
  ciphertext = Utilities.Row_to_column(Array[BigInt](0x3a, 0xd7, 0x7b, 0xb4, 0x0d,
    0x7a, 0x36, 0x60, 0xa8, 0x9e, 0xca, 0xf3, 0x24, 0x66, 0xef, 0x97))

  /* start computation and calculate keys */
  poke(c.io.ready, 1)
  poke(c.io.input, plaintext)
  poke(c.io.key, key)
  step(1)
  expect(c.io.busy, 0)
  expect(c.io.valid, 0)


  for (i <- 0 until 10) {
    step(1)

    expect(c.io.busy, 1)

  }

  step(1)
  expect(c.io.busy, 1)
  expect(c.io.valid, 1)
  expect(c.io.output, ciphertext)

}

class AESEncryptTests(c: AESEncrypt) extends Tester(c) {
  var plaintext = Array[BigInt](0x32, 0x88, 0x31, 0xe0, 0x43, 0x5a, 0x31, 0x37, 0xf6,
    0x30, 0x98, 0x07, 0xa8, 0x8d, 0xa2, 0x34)
  var ciphertext = Array[BigInt](0x39, 0x02, 0xdc, 0x19, 0x25, 0xdc, 0x11,
    0x6a, 0x84, 0x09, 0x85, 0x0b, 0x1d, 0xfb, 0x97, 0x32)
  var round0 = Array[BigInt](0x19, 0xa0, 0x9a, 0xe9, 0x3d, 0xf4, 0xc6, 0xf8,
    0xe3, 0xe2, 0x8d, 0x48, 0xbe, 0x2b, 0x2a, 0x08)
  var round9 = Array[BigInt](0xeb, 0x59, 0x8b, 0x1b, 0x40, 0x2e, 0xa1, 0xc3,
    0xf2, 0x38, 0x13, 0x42, 0x1e, 0x84, 0xe7, 0xd2)
  var round1 = Array[BigInt](0xa4, 0x68, 0x6b, 0x02, 0x9c, 0x9f, 0x5b, 0x6a, 0x7f, 0x35,
    0xea, 0x50, 0xf2, 0x2b, 0x43, 0x49)
  var key = Array[BigInt](0x2b, 0x28, 0xab, 0x09, 0x7e, 0xae, 0xf7, 0xcf, 0x15, 0xd2,
    0x15, 0x4f, 0x16, 0xa6, 0x88, 0x3c)

  def Row_to_column(input: Array[BigInt]): Array[BigInt] = {


    assert(input.length == 16)

    val a = Array.ofDim[BigInt](4)
    val b = Array.ofDim[BigInt](4)
    val c = Array.ofDim[BigInt](4)
    val d = Array.ofDim[BigInt](4)

    for (i <- 0 until 4) {
      a(i) = input(i * 4)
      b(i) = input((i * 4) + 1)
      c(i) = input((i * 4) + 2)
      d(i) = input((i * 4) + 3)
    }
    val output = a ++ b ++ c ++ d
    println(output)
    output
  }


  /* Initialize  state machine */
  poke(c.io.input, plaintext)
  poke(c.io.key, key)
  //poke(c.io.ready, 1)
  step(1)
  expect(c.io.busy, 0)
  //peek(c.io.output)


  /* start computation */
  poke(c.io.ready, 1)
  step(1)
  expect(c.io.busy, 1)
  expect(c.io.valid, 0)
  //peek(c.initial_round_key.state_out)
  //peek(c.io.output)

  step(1)
  poke(c.io.ready, 0)
  peek(c.state)
  expect(c.io.busy, 0)
  expect(c.io.valid, 1)
  peek(c.initial_round_key.state_out)
  //peek(c.rounds(0).state_in)
  //peek(c.rounds(0).state_out)
  expect(c.io.output, ciphertext)


  step(1)
  peek(c.state)
  expect(c.io.busy, 0)
  expect(c.io.valid, 0)


  key = Row_to_column(Array[BigInt](0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6,
    0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c))
  plaintext = Row_to_column(Array[BigInt](0x6b, 0xc1, 0xbe, 0xe2, 0x2e, 0x40, 0x9f,
    0x96, 0xe9, 0x3d, 0x7e, 0x11, 0x73, 0x93, 0x17, 0x2a))
  ciphertext = Row_to_column(Array[BigInt](0x3a, 0xd7, 0x7b, 0xb4, 0x0d, 0x7a, 0x36,
    0x60, 0xa8, 0x9e, 0xca, 0xf3, 0x24, 0x66, 0xef, 0x97))

  /* start computation and calculate keys */
  poke(c.io.ready, 1)
  poke(c.io.input, plaintext)
  poke(c.io.key, key)
  step(1)
  expect(c.io.busy, 1)
  expect(c.io.valid, 0)
  //peek(c.initial_round_key.state_out)
  //peek(c.io.output)

  step(1)
  peek(c.state)
  expect(c.io.busy, 0)
  expect(c.io.valid, 1)
  peek(c.initial_round_key.state_out)
  //peek(c.rounds(0).state_in)
  //peek(c.rounds(0).state_out)
  expect(c.io.output, ciphertext)

  step(1)
  peek(c.state)
  expect(c.io.busy, 0)

  expect(c.io.valid, 0)


  plaintext = Row_to_column(Array[BigInt](0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66,
    0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff))
  key = Row_to_column(Array[BigInt](0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f))
  ciphertext = Row_to_column(Array[BigInt](0x69, 0xc4, 0xe0, 0xd8, 0x6a, 0x7b, 0x04,
    0x30, 0xd8, 0xcd, 0xb7, 0x80, 0x70, 0xb4, 0xc5, 0x5a))

  /* start computation and calculate keys */
  poke(c.io.ready, 1)
  poke(c.io.input, plaintext)
  poke(c.io.key, key)
  step(1)
  expect(c.io.busy, 1)
  expect(c.io.valid, 0)
  //peek(c.initial_round_key.state_out)
  //peek(c.io.output)

  step(1)
  peek(c.state)
  expect(c.io.busy, 0)
  expect(c.io.valid, 1)
  peek(c.initial_round_key.state_out)
  //peek(c.rounds(0).state_in)
  //peek(c.rounds(0).state_out)
  expect(c.io.output, ciphertext)

}


class AESDecryptTests(c: AESDecrypt) extends Tester(c) {
  var input = Row_to_column(Array[BigInt](0x69, 0xc4, 0xe0, 0xd8, 0x6a, 0x7b, 0x04,
    0x30, 0xd8, 0xcd, 0xb7, 0x80, 0x70, 0xb4, 0xc5, 0x5a))
  var output = Row_to_column(Array[BigInt](0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66,
    0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff))
  var round0 = Row_to_column(Array[BigInt](0x7a, 0xd5, 0xfd, 0xa7, 0x89, 0xef, 0x4e,
    0x27, 0x2b, 0xca, 0x10, 0x0b, 0x3d, 0x9f, 0xf5, 0x9f))
  var round9 = Row_to_column(Array[BigInt](0x63, 0x53, 0xe0, 0x8c, 0x09, 0x60, 0xe1,
    0x04, 0xcd, 0x70, 0xb7, 0x51, 0xba, 0xca, 0xd0, 0xe7))
  var round1 = Row_to_column(Array[BigInt](0x54, 0xd9, 0x90, 0xa1, 0x6b, 0xa0, 0x9a,
    0xb5, 0x96, 0xbb, 0xf4, 0x0e, 0xa1, 0x11, 0x70, 0x2f))
  var round2 = Row_to_column(Array[BigInt](0x3e, 0x1c, 0x22, 0xc0, 0xb6, 0xfc, 0xbf,
    0x76, 0x8d, 0xa8, 0x50, 0x67, 0xf6, 0x17, 0x04, 0x95))
  var round3 = Row_to_column(Array[BigInt](0xb4, 0x58, 0x12, 0x4c, 0x68, 0xb6, 0x8a,
    0x01, 0x4b, 0x99, 0xf8, 0x2e, 0x5f, 0x15, 0x55, 0x4c))
  var round4 = Row_to_column(Array[BigInt](0xe8, 0xda, 0xb6, 0x90, 0x14, 0x77, 0xd4,
    0x65, 0x3f, 0xf7, 0xf5, 0xe2, 0xe7, 0x47, 0xdd, 0x4f))
  var key = Row_to_column(Array[BigInt](0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
    0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f))

  def Row_to_column(input: Array[BigInt]): Array[BigInt] = {


    assert(input.length == 16)

    val a = Array.ofDim[BigInt](4)
    val b = Array.ofDim[BigInt](4)
    val c = Array.ofDim[BigInt](4)
    val d = Array.ofDim[BigInt](4)

    for (i <- 0 until 4) {
      a(i) = input(i * 4)
      b(i) = input((i * 4) + 1)
      c(i) = input((i * 4) + 2)
      d(i) = input((i * 4) + 3)
    }
    val output = a ++ b ++ c ++ d
    println(output)
    output
  }


  /* Initialize  state machine */
  poke(c.io.input, input)
  poke(c.io.key, key)
  //poke(c.io.ready, 1)
  step(1)
  expect(c.io.busy, 0)
  peek(c.io.output)


  /* start computation */
  poke(c.io.ready, 1)
  step(1)
  expect(c.io.busy, 1)
  expect(c.io.valid, 0)
  peek(c.initial_round_key.state_out)
  //peek(c.io.output)

  step(1)
  poke(c.io.ready, 0)
  peek(c.state)
  expect(c.io.busy, 0)
  expect(c.io.valid, 1)
  peek(c.initial_round_key.state_out)
  //peek(c.rounds(0).state_in)
  //peek(c.rounds(0).state_out)
  expect(c.io.output, output)


  step(1)
  peek(c.state)
  expect(c.io.busy, 0)
  expect(c.io.valid, 0)

}

class RoundTests(c: Round) extends Tester(c) {

  val sub_byte = Array[BigInt](0x63, 0xca, 0xb7, 0x04, 0x09, 0x53, 0xd0, 0x51, 0xcd,
    0x60, 0xe0, 0xe7, 0xba, 0x70, 0xe1, 0x8c)
  val shift_rows = Array[BigInt](0x63, 0x53, 0xe0, 0x8c, 0x09, 0x60, 0xe1, 0x04, 0xcd,
    0x70, 0xb7, 0x51, 0xba, 0xca, 0xd0, 0xe7)
  var input = Array[BigInt](0x19, 0xa0, 0x9a, 0xe9, 0x3d, 0xf4, 0xc6, 0xf8,
    0xe3, 0xe2, 0x8d, 0x48, 0xbe, 0x2b, 0x2a, 0x08)


  poke(c.io.state_in, input)
  poke(c.io.round_key, round_key)
  step(1)
  expect(c.io.state_out, output)


  input = output
  output = Array[BigInt](0xaa, 0x61, 0x82, 0x68, 0x8f, 0xdd, 0xd2, 0x32, 0x5f, 0xe3, 0x4a,
    0x46, 0x03, 0xef, 0xd2, 0x9a)
  round_key = Array[BigInt](0xf2, 0x7a, 0x59, 0x73, 0xc2, 0x96, 0x35, 0x59,
    0x95, 0xb9, 0x80, 0xf6, 0xf2, 0x43, 0x7a, 0x7f)


  poke(c.io.state_in, input)
  poke(c.io.round_key, round_key)
  step(1)
  expect(c.io.state_out, output)

  input = Array[BigInt](0x00, 0x10, 0x20, 0x30, 0x40, 0x50, 0x60, 0x70, 0x80, 0x90, 0xa0,
    0xb0, 0xc0, 0xd0, 0xe0, 0xf0)
  output = Array[BigInt](0x89, 0xd8, 0x10, 0xe8, 0x85, 0x5a, 0xce, 0x68, 0x2d, 0x18, 0x43,
    0xd8, 0xcb, 0x12, 0x8f, 0xe4)
  round_key = Array[BigInt](0xd6, 0xaa, 0x74, 0xfd, 0xd2, 0xaf, 0x72, 0xfa, 0xda, 0xa6,
    0x78, 0xf1, 0xd6, 0xab, 0x76, 0xfe)
  var output = Array[BigInt](0xa4, 0x68, 0x6b, 0x02, 0x9c, 0x9f, 0x5b, 0x6a, 0x7f, 0x35,
    0xea, 0x50, 0xf2, 0x2b, 0x43, 0x49)
  var round_key = Array[BigInt](0xa0, 0x88, 0x23, 0x2a, 0xfa, 0x54, 0xa3, 0x6c,
    0xfe, 0x2c, 0x39, 0x76, 0x17, 0xb1, 0x39, 0x05)

  poke(c.io.state_in, input)
  poke(c.io.round_key, round_key)
  step(1)
  expect(c.sub_bytes.state_out, sub_byte)
  expect(c.shift_rows.state_out, shift_rows)
  expect(c.io.state_out, output)
}


class InvRoundTests(c: InvRound) extends Tester(c) {

  var input = Utilities.Row_to_column(Array[BigInt](0x7a, 0xd5, 0xfd, 0xa7, 0x89,
    0xef, 0x4e, 0x27, 0x2b, 0xca, 0x10, 0x0b, 0x3d, 0x9f, 0xf5, 0x9f))

  var output = Utilities.Row_to_column(Array[BigInt](0x54, 0xd9, 0x90, 0xa1, 0x6b,
    0xa0, 0x9a, 0xb5, 0x96, 0xbb, 0xf4, 0x0e, 0xa1, 0x11, 0x70, 0x2f))

  var round_key = Utilities.Row_to_column(Array[BigInt](0x54, 0x99, 0x32, 0xd1,
    0xf0, 0x85, 0x57, 0x68, 0x10, 0x93, 0xed, 0x9c, 0xbe, 0x2c, 0x97, 0x4e))

  poke(c.io.state_in, input)
  poke(c.io.round_key, round_key)
  step(1)
  expect(c.io.state_out, output)

}


class KeyExpansionTests(c: KeyExpansion) extends Tester(c) {
  var input = Array[BigInt](0x2b, 0x28, 0xab, 0x09, 0x7e, 0xae, 0xf7, 0xcf,
    0x15, 0xd2, 0x15, 0x4f, 0x16, 0xa6, 0x88, 0x3c)


  var output = Array[BigInt](0xa0, 0x88, 0x23, 0x2a, 0xfa, 0x54, 0xa3, 0x6c,
    0xfe, 0x2c, 0x39, 0x76, 0x17, 0xb1, 0x39, 0x05)


  poke(c.io.key_in, input)
  poke(c.io.stage, 1)
  step(1)
  expect(c.io.key_out, output)


  input = output

  output = Array[BigInt](0xf2, 0x7a, 0x59, 0x73, 0xc2, 0x96, 0x35, 0x59, 0x95,
    0xb9, 0x80, 0xf6, 0xf2, 0x43, 0x7a, 0x7f)

  poke(c.io.key_in, input)
  poke(c.io.stage, 2)
  step(1)
  expect(c.io.key_out, output)



  /* TODO Change test inputs and outputs. They need to be in column major format instead of row major */

  /*

  input = output

  output = Array[BigInt](0x3d, 0x80, 0x47, 0x7d, 0x47, 0x16, 0xfe, 0x3e, 0x1e, 0x23, 0x7e, 0x44, 0x6d, 0x7a, 0x88, 0x3b)

  poke(c.io.key_in, input)
  poke(c.io.stage, 3)
  step(1)
  expect(c.io.key_out, output)

  input = output
  output = Array[BigInt](0xef, 0x44, 0xa5, 0x41, 0xa8, 0x52, 0x5b, 0x7f, 0xb6, 0x71, 0x25, 0x3b, 0xdb, 0x0b, 0xad, 0x00)

  poke(c.io.key_in, input)
  poke(c.io.stage, 4)
  step(1)
  expect(c.io.key_out, output)

  input = output

  output = Array[BigInt](0xd4, 0xd1, 0xc6, 0xf8, 0x7c, 0x83, 0x9d, 0x87, 0xca, 0xf2, 0xb8, 0xbc, 0x11, 0xf9, 0x15, 0xbc)
  poke(c.io.key_in, input)
  poke(c.io.stage, 5)
  step(1)
  expect(c.io.key_out, output)


  input = output
  output =  Array[BigInt](0x6d, 0x88, 0xa3, 0x7a, 0x11, 0x0b, 0x3e, 0xfd, 0xdb, 0xf9, 0x86, 0x41, 0xca, 0x00, 0x93, 0xfd)
  poke(c.io.key_in, input)
  poke(c.io.stage, 6)
  step(1)
  expect(c.io.key_out, output)


  input = output
  output =  Array[BigInt](0x4e, 0x54, 0xf7, 0x0e, 0x5f, 0x5f, 0xc9, 0xf3, 0x84, 0xa6, 0x4f, 0xb2, 0x4e, 0xa6, 0xdc, 0x4f)
  poke(c.io.key_in, input)
  poke(c.io.stage, 7)
  step(1)
  expect(c.io.key_out, output)


  input = output
  output = Array[BigInt](0xea, 0xd2, 0x73, 0x21, 0xb5, 0x8d, 0xba, 0xd2, 0x31, 0x2b, 0xf5, 0x60, 0x7f, 0x8d, 0x29, 0x2f)
  poke(c.io.key_in, input)
  poke(c.io.stage, 8)
  step(1)
  expect(c.io.key_out, output)


  input = output
  output = Array[BigInt](0xac, 0x77, 0x66, 0xf3, 0x19, 0xfa, 0xdc, 0x21, 0x28, 0xd1, 0x29, 0x41, 0x57, 0x5c, 0x00, 0x6e)

  poke(c.io.key_in, input)
  poke(c.io.stage, 9)
  step(1)
  expect(c.io.key_out, output)



  input = output
  output = Array[BigInt](0xd0, 0x14, 0xf9, 0xa8, 0xc9, 0xee, 0x25, 0x89, 0xe1, 0x3f, 0x0c, 0xc8, 0xb6, 0x63, 0x0c, 0xa6)

  poke(c.io.key_in, input)
  poke(c.io.stage, 10)
  step(1)
  expect(c.io.key_out, output)

*/

}

class KeyCoreTests(c: KeyCore) extends Tester(c) {
  val input = Array[BigInt](0x09, 0xcf, 0x4f, 0x3c)
  val output = Array[BigInt](0x8b, 0x84, 0xeb, 0x01)

  val input2 = Array[BigInt](0x2a, 0x6c, 0x76, 0x05)
  val output2 = Array[BigInt](0x52, 0x38, 0x6b, 0xe5)

  poke(c.io.key_in, input)
  poke(c.io.stage, 1)
  step(1)
  expect(c.io.key_out, output)


  poke(c.io.key_in, input2)
  poke(c.io.stage, 2)
  step(1)
  expect(c.io.key_out, output2)


}

class AddRoundKeyTests(c: AddRoundKey) extends Tester(c) {

  val input = Array[BigInt](0x04, 0xe0, 0x48, 0x28, 0x66, 0xcb, 0xf8, 0x06, 0x81, 0x19, 0xd3, 0x26, 0xe5, 0x9a,
    0x7a, 0x4c)

  val key = Array[BigInt](0xa0, 0x88, 0x23, 0x2a, 0xfa, 0x54, 0xa3, 0x6c, 0xfe, 0x2c, 0x39, 0x76, 0x17, 0xb1, 0x39,
    0x05)


  val output = Array[BigInt](0xa4, 0x68, 0x6b, 0x02, 0x9c, 0x9f, 0x5b, 0x6a, 0x7f, 0x35, 0xea, 0x50, 0xf2, 0x2b,
    0x43, 0x49)


  poke(c.io.state_in, input)
  poke(c.io.round_key_in, key)
  step(1)
  expect(c.io.state_out, output)

}


class MixColumnsTests(c: MixColumns) extends Tester(c) {
  val input = Array[BigInt](0xd4, 0xe0, 0xb8, 0x1e, 0xbf, 0xb4, 0x41, 0x27, 0x5d, 0x52, 0x11, 0x98, 0x30, 0xae, 0xf1,
    0xe5)

  val output = Array[BigInt](0x04, 0xe0, 0x48, 0x28, 0x66, 0xcb, 0xf8, 0x06, 0x81, 0x19, 0xd3, 0x26, 0xe5, 0x9a,
    0x7a, 0x4c)


  poke(c.io.state_in, input)
  step(1)
  expect(c.io.state_out, output)

}


class InvMixColumnsTests(c: InvMixColumns) extends Tester(c) {
  var output = Array[BigInt](0xd4, 0xe0, 0xb8, 0x1e, 0xbf, 0xb4, 0x41, 0x27, 0x5d, 0x52, 0x11, 0x98, 0x30, 0xae, 0xf1,
    0xe5)

  var input = Array[BigInt](0x04, 0xe0, 0x48, 0x28, 0x66, 0xcb, 0xf8, 0x06, 0x81, 0x19, 0xd3, 0x26, 0xe5, 0x9a,
    0x7a, 0x4c)


  poke(c.io.state_in, input)
  step(1)
  expect(c.io.state_out, output)

  input = Utilities.Row_to_column(Array[BigInt](0xe9, 0xf7, 0x4e, 0xec, 0x02, 0x30, 0x20, 0xf6, 0x1b, 0xf2, 0xcc, 0xf2, 0x35,
    0x3c, 0x21, 0xc7))
  output = Utilities.Row_to_column(Array[BigInt](0x54, 0xd9, 0x90, 0xa1, 0x6b, 0xa0, 0x9a, 0xb5, 0x96, 0xbb, 0xf4, 0x0e, 0xa1,
    0x11, 0x70, 0x2f))

  poke(c.io.state_in, input)
  step(1)
  expect(c.io.state_out, output)

}


class ShiftRowsTests(c: ShiftRows) extends Tester(c) {
  var input = Array[BigInt](0xd4, 0xe0, 0xb8, 0x1e, 0x27, 0xbf, 0xb4, 0x41, 0x11, 0x98, 0x5d, 0x52, 0xae, 0xf1, 0xe5,
    0x30)

  var output = Array[BigInt](0xd4, 0xe0, 0xb8, 0x1e, 0xbf, 0xb4, 0x41, 0x27, 0x5d, 0x52, 0x11, 0x98, 0x30, 0xae, 0xf1,
    0xe5)

  poke(c.io.state_in, input)
  step(1)
  expect(c.io.state_out, output)

  input = Array[BigInt](0xac, 0xef, 0x13, 0x45, 0x73, 0xc1, 0xb5, 0x23, 0xcf, 0x11, 0xd6, 0x5a, 0x7b, 0xdf, 0xb5, 0xb8)
  output = Array[BigInt](0xac, 0xef, 0x13, 0x45, 0xc1, 0xb5, 0x23, 0x73, 0xd6, 0x5a, 0xcf, 0x11, 0xb8, 0x7b, 0xdf, 0xb5)

  poke(c.io.state_in, input)
  step(1)
  expect(c.io.state_out, output)

  //input  = Array[BigInt](0x63,0xca,0xb7,0x04,0x09,0x53,0xd0,0x51,0xcd,0x60,0xe0,0xe7,0xba,0x70,0xe1,0x8c)
  //output = Array[BigInt](0x63,0x53,0xe0,0x8c,0x09,0x60,0xe1,0x04,0xcd,0x70,0xb7,0x51,0xba,0xca,0xd0,0xe7)

  //poke (c.io.state_in, input)
  //step(1)
  //expect (c.io.state_out, output)

}


class InvShiftRowsTests(c: InvShiftRows) extends Tester(c) {
  var output = Array[BigInt](0xd4, 0xe0, 0xb8, 0x1e, 0x27, 0xbf, 0xb4, 0x41, 0x11, 0x98, 0x5d, 0x52, 0xae, 0xf1, 0xe5,
    0x30)

  var input = Array[BigInt](0xd4, 0xe0, 0xb8, 0x1e, 0xbf, 0xb4, 0x41, 0x27, 0x5d, 0x52, 0x11, 0x98, 0x30, 0xae, 0xf1,
    0xe5)

  poke(c.io.state_in, input)
  step(1)
  expect(c.io.state_out, output)

  output = Array[BigInt](0xac, 0xef, 0x13, 0x45, 0x73, 0xc1, 0xb5, 0x23, 0xcf, 0x11, 0xd6, 0x5a, 0x7b, 0xdf, 0xb5, 0xb8)
  input = Array[BigInt](0xac, 0xef, 0x13, 0x45, 0xc1, 0xb5, 0x23, 0x73, 0xd6, 0x5a, 0xcf, 0x11, 0xb8, 0x7b, 0xdf, 0xb5)

  poke(c.io.state_in, input)
  step(1)
  expect(c.io.state_out, output)


}

class SubBytesTests(c: SubBytes) extends Tester(c) {

  val input = Array[BigInt](0x19, 0xa0, 0x9a, 0xe9, 0x3d, 0xf4, 0xc6, 0xf8, 0xe3, 0xe2, 0x8d, 0x48, 0xbe, 0x2b, 0x2a,
    0x08)

  val output = Array[BigInt](0xd4, 0xe0, 0xb8, 0x1e, 0x27, 0xbf, 0xb4, 0x41, 0x11, 0x98, 0x5d, 0x52, 0xae, 0xf1, 0xe5,
    0x30)

  poke(c.io.state_in, input)
  step(1)
  expect(c.io.state_out, output)

}


class SubByteTests(c: SubByte) extends Tester(c) {
  poke(c.io.in, 0x19)
  step(1)
  expect(c.io.out, 0xd4)

  poke(c.io.in, 0xa0)
  step(1)
  expect(c.io.out, 0xe0)

  poke(c.io.in, 0x9a)
  step(1)
  expect(c.io.out, 0xb8)

  poke(c.io.in, 0xe9)
  step(1)
  expect(c.io.out, 0x1e)

  poke(c.io.in, 0x3d)
  step(1)
  expect(c.io.out, 0x27)

  poke(c.io.in, 0xf4)
  step(1)
  expect(c.io.out, 0xbf)

  poke(c.io.in, 0xc6)
  step(1)
  expect(c.io.out, 0xb4)
}


class G256NBTests(c: G256NB) extends Tester(c) {
  val A2X = Array[BigInt](0x98, 0xF3, 0xF2, 0x48, 0x09, 0x81, 0xA9, 0xFF)
  val X2S = Array[BigInt](0x58, 0x2D, 0x9E, 0x0B, 0xDC, 0x04, 0x03, 0x24)
  val X2A = Array[BigInt](0x64, 0x78, 0x6E, 0x8C, 0x68, 0x29, 0xDE, 0x60)
  val S2X = Array[BigInt](0x8C, 0x79, 0x05, 0xEB, 0x12, 0x04, 0x51, 0x53)

  poke(c.io.in_x, 0)
  poke(c.io.in_b, A2X)
  step(1)
  expect(c.io.out_y, 0)


  poke(c.io.in_x, 0)
  poke(c.io.in_b, X2S)
  step(1)
  expect(c.io.out_y, 0)


  poke(c.io.in_x, 0x63)
  poke(c.io.in_b, S2X)
  step(1)
  expect(c.io.out_y, 0x7E)


  poke(c.io.in_x, 0x12)
  poke(c.io.in_b, X2A)
  step(1)
  expect(c.io.out_y, 0x52)


}

class G256NBStageTests(c: G256NBStage) extends Tester(c) {

  poke(c.io.in_x, 0)
  poke(c.io.in_b, 0xFF)
  poke(c.io.in_y, 0)
  step(1)
  expect(c.io.out_x, 0)
  expect(c.io.out_y, 0)

  poke(c.io.in_x, 0x3e)
  poke(c.io.in_b, 0x60)
  poke(c.io.in_y, 0)
  step(1)
  expect(c.io.out_x, 0x1F)
  expect(c.io.out_y, 0)

  poke(c.io.in_x, 0x1F)
  poke(c.io.in_b, 0xDE)
  poke(c.io.in_y, 0)
  step(1)
  expect(c.io.out_x, 0x0F)
  expect(c.io.out_y, 0xDE)



  poke(c.io.in_x, 0x0F)
  poke(c.io.in_b, 0x29)
  poke(c.io.in_y, 0xDE)
  step(1)
  expect(c.io.out_x, 0x07)
  expect(c.io.out_y, 0xF7)


  poke(c.io.in_x, 0x07)
  poke(c.io.in_b, 0x68)
  poke(c.io.in_y, 0xF7)
  step(1)
  expect(c.io.out_x, 0x03)
  expect(c.io.out_y, 0x9f)

  poke(c.io.in_x, 0x03)
  poke(c.io.in_b, 0x8C)
  poke(c.io.in_y, 0x9F)
  step(1)
  expect(c.io.out_x, 0x01)
  expect(c.io.out_y, 0x13)

  poke(c.io.in_x, 0x01)
  poke(c.io.in_b, 0x6E)
  poke(c.io.in_y, 0x13)
  step(1)
  expect(c.io.out_x, 0x00)
  expect(c.io.out_y, 0x7D)

  poke(c.io.in_x, 0x01)
  poke(c.io.in_b, 0x6E)
  poke(c.io.in_y, 0x13)
  step(1)
  expect(c.io.out_x, 0x00)
  expect(c.io.out_y, 0x7D)


  poke(c.io.in_x, 0x00)
  poke(c.io.in_b, 0x78)
  poke(c.io.in_y, 0x7D)
  step(1)
  expect(c.io.out_x, 0x00)
  expect(c.io.out_y, 0x7D)


  poke(c.io.in_x, 0x00)
  poke(c.io.in_b, 0x64)
  poke(c.io.in_y, 0x7D)
  step(1)
  expect(c.io.out_x, 0x00)
  expect(c.io.out_y, 0x7D)


}


class G256InvTests(c: G256_inv) extends Tester(c) {

  poke(c.io.in, 0x7E)
  step(1)
  expect(c.io.out, 0x12)

  poke(c.io.in, 0xff)
  step(1)
  expect(c.io.out, 0xff)

  poke(c.io.in, 0xa9)
  step(1)
  expect(c.io.out, 0xef)

  poke(c.io.in, 0x68)
  step(1)
  expect(c.io.out, 0x67)

  poke(c.io.in, 0x71)
  step(1)
  expect(c.io.out, 0x3e)

  poke(c.io.in, 0x06)
  step(1)
  expect(c.io.out, 0x10)

  poke(c.io.in, 0x55)
  step(1)
  expect(c.io.out, 0xaa)

}


object G256NBTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new G256NB())) { c => new G256NBTests(c) }
  }
}


object G256NBStageTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new G256NBStage())) { c => new G256NBStageTests(c) }
  }
}


object G256InvTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new G256_inv())) { c => new G256InvTests(c) }
  }
}

object SBoxTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new SBox())) { c => new SBoxTests(c) }
  }
}


object ISBoxTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new ISBox())) { c => new ISBoxTests(c) }
  }
}


object SubByteTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new SubByte())) { c => new SubByteTests(c) }
  }

}

object InvSubByteTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new InvSubByte())) { c => new InvSubByteTests(c) }
  }

}


object SubBytesTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new SubBytes())(subBytes_parameters)) { c => new SubBytesTests(c) }
  }

}


object InvSubBytesTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new InvSubBytes())(subBytes_parameters)) { c => new InvSubBytesTests(c) }
  }

}

object ShiftRowsTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new ShiftRows())(subBytes_parameters)) { c => new ShiftRowsTests(c) }
  }

}


object InvShiftRowsTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new InvShiftRows())(subBytes_parameters)) { c => new InvShiftRowsTests(c) }
  }

}

object MixColumnsTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new MixColumns())(subBytes_parameters)) { c => new MixColumnsTests(c) }
  }

}


object InvMixColumnsTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new InvMixColumns())(subBytes_parameters)) { c => new InvMixColumnsTests(c) }
  }

}


object AddRoundKeyTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new AddRoundKey())(subBytes_parameters)) { c => new AddRoundKeyTests(c) }
  }

}


object KeyCoreTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new KeyCore())) { c => new KeyCoreTests(c) }
  }

}

object KeyExpansionTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new KeyExpansion())(subBytes_parameters)) { c => new KeyExpansionTests(c) }
  }

}


object RoundTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new Round())(subBytes_parameters)) { c => new RoundTests(c) }
  }

}


object InvRoundTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new InvRound())(subBytes_parameters)) { c => new InvRoundTests(c) }
  }

}


object EqInvRoundTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new EqInvRound())(subBytes_parameters)) { c => new EqInvRoundTests(c) }
  }

}

object KeyExpanderTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new KeyExpander())(subBytes_parameters)) { c => new KeyExpanderTests(c) }
  }

}


object AESEncryptTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      //chiselMainTest(Array[String]("--backend", "v", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new AESEncrypt())(subBytes_parameters)) { c => new AESEncryptTests(c) }
  }

}


object AESDecryptTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir",
      "generated-src", "--noCombLoop"),
      //chiselMainTest(Array[String]("--backend", "v", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new AESDecrypt())(subBytes_parameters)) { c => new AESDecryptTests(c) }
  }

}

object EqInvAESDecryptTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir",
      "generated-src", "--noCombLoop"),
      //chiselMainTest(Array[String]("--backend", "v", "--compile", "--test", "--genHarness", "--targetDir",
      //"generated-src", "--noCombLoop"),
      () => Module(new EqInvAESDecrypt())(subBytes_parameters)) { c => new EqInvAESDecryptTests(c) }
  }

}


object EqInvAESDecryptIterTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      //chiselMainTest(Array[String]("--backend", "v", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new EqInvAESDecryptIter())(subBytes_parameters)) { c => new EqInvAESDecryptIterTests(c) }
  }

}


object AESEncryptIterTester {
  val parameters = Parameters.empty
  val subBytes_parameters = parameters.alter(
    mask = (key, site, here, up) => key match {
      case Nk => 4
    })

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      //chiselMainTest(Array[String]("--backend", "v", "--compile", "--test", "--genHarness", "--targetDir", "generated-src"),
      () => Module(new AESEncryptIter())(subBytes_parameters)) { c => new AESEncryptIterTests(c) }
  }

}

