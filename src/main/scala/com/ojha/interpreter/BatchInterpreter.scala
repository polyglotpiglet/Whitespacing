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
        case '\n' :: '\t' :: tl   => swapTopOfStack()
        case '\n' :: '\n' :: tl   => discardTopOfStack()
      }
      case '\t' :: t => t match {
        case ' '  :: ' '  :: tl => addTopOfStack()
        case ' '  :: '\t' :: tl => subtractTopOfStack()
        case ' '  :: '\n' :: tl => multiplyTopOfStack()
        case '\t' :: ' '  :: tl => divideTopOfStack()
        case '\t' :: '\t' :: tl => moduloTopOfStack()
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

  private def discardTopOfStack(): Unit = {
    if (stack.nonEmpty) stack.pop()
  }

  private def swapTopOfStack(): Unit = {
    if (stack.size > 1) {
      val one = stack.pop()
      val two = stack.pop()
      stack.push(one)
      stack.push(two)
    }

  }

  def addTopOfStack(): Unit = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      stack.push(a+b)
    }
    else throw new RuntimeException(s"Cannot add top two numbers in stack when stack size = ${stack.size}")
  }

  def subtractTopOfStack(): Unit = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      stack.push(b-a)
    }
    else throw new RuntimeException(s"Cannot subtract top two numbers in stack when stack size = ${stack.size}")

  }

  def multiplyTopOfStack(): Unit = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      stack.push(a*b)
    }
    else throw new RuntimeException(s"Cannot multiply top two numbers in stack when stack size = ${stack.size}")

  }

  def divideTopOfStack(): Unit = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      stack.push(b/a)
    }
    else throw new RuntimeException(s"Cannot divide top two numbers in stack when stack size = ${stack.size}")
  }

  def moduloTopOfStack(): Unit = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      stack.push(b%a)
    }
    else throw new RuntimeException(s"Cannot find b mod a of top two numbers in stack when stack size = ${stack.size}")

  }



}
