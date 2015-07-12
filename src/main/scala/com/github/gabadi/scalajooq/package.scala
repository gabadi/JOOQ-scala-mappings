package com.github.gabadi

package object scalajooq {

  implicit class ListUnzips[+A](val list: List[A]) extends AnyVal {
    def unzip4[A1, A2, A3, A4](implicit asTuple4: A => (A1, A2, A3, A4)): (List[A1], List[A2], List[A3], List[A4]) = {
      val b1 = List.newBuilder[A1]
      val b2 = List.newBuilder[A2]
      val b3 = List.newBuilder[A3]
      val b4 = List.newBuilder[A4]

      for (abcd <- list) {
        val (a, b, c, d) = asTuple4(abcd)
        b1 += a
        b2 += b
        b3 += c
        b4 += d
      }
      (b1.result(), b2.result(), b3.result(), b4.result())
    }

    def unzip5[A1, A2, A3, A4, A5](implicit asTuple5: A => (A1, A2, A3, A4, A5)): (List[A1], List[A2], List[A3], List[A4], List[A5]) = {
      val b1 = List.newBuilder[A1]
      val b2 = List.newBuilder[A2]
      val b3 = List.newBuilder[A3]
      val b4 = List.newBuilder[A4]
      val b5 = List.newBuilder[A5]

      for (abcd <- list) {
        val (a, b, c, d, e) = asTuple5(abcd)
        b1 += a
        b2 += b
        b3 += c
        b4 += d
        b5 += e
      }
      (b1.result(), b2.result(), b3.result(), b4.result(), b5.result())
    }

    def unzip6[A1, A2, A3, A4, A5, A6](implicit asTuple6: A => (A1, A2, A3, A4, A5, A6)): (List[A1], List[A2], List[A3], List[A4], List[A5], List[A6]) = {
      val b1 = List.newBuilder[A1]
      val b2 = List.newBuilder[A2]
      val b3 = List.newBuilder[A3]
      val b4 = List.newBuilder[A4]
      val b5 = List.newBuilder[A5]
      val b6 = List.newBuilder[A6]

      for (abcd <- list) {
        val (a, b, c, d, e, f) = asTuple6(abcd)
        b1 += a
        b2 += b
        b3 += c
        b4 += d
        b5 += e
        b6 += f
      }
      (b1.result(), b2.result(), b3.result(), b4.result(), b5.result(), b6.result())
    }
  }
}
