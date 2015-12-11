package com.ojha.interpreter

import scala.collection._

object BatchInterpreter {
  def apply() = new BatchInterpreter(mutable.Stack[Int]())
}

class BatchInterpreter(stack: mutable.Stack[Int]) extends NumberConverter {

  def interpret(code: List[Char]): Unit = {
    code match {
      case ' ' :: t => t match {
        case ' '  :: tl           => pushToTopOfStack(tl)
        case '\n' :: ' '  :: tl   => duplicateTopOfStack()
        case '\n' :: '\t' :: tl   =>
        case '\n' :: '\n' :: tl   =>

      }
    }
  }

  def interpret(code: String): Unit = {
   interpret(code.toList)
  }

  private def pushToTopOfStack(code: List[Char]): Unit = {
    val number = code.takeWhile(_ != '\n')
    val n = convert(number)
    stack.push(n)
  }

  private def duplicateTopOfStack(): Unit = {
    if (stack.nonEmpty) {
      val n = stack.pop()
      stack.push(n)
      stack.push(n)
    }
  }


}
