package com.ojha.interpreter

/*
Number converter for whitespace

Space = 0
Tab = 1
First character: if space then +ve else -ve

eg [space, tab] = +1
eg [space, space, tab, space, tab, tab] = +11

 */
trait NumberConverter {

  def convert(number: Seq[Char]): Int = {
    number.head match {
      case ' '  => intValue(number.tail)
      case '\t' => intValue(number.tail) * -1
    }

  }

  private def intValue(n: Seq[Char]): Int = {
    n.reverse.zipWithIndex.map {
      case (' ', _) => 0
      case ('\t', i) => math.pow(2,i).toInt
    }.sum
   }

}
