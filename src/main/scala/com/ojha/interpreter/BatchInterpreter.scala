package com.ojha.interpreter

import scala.annotation.tailrec
import scala.collection._

object BatchInterpreter {
  def apply() = new BatchInterpreter(mutable.Stack[Int]()) with Output with Input
}

class BatchInterpreter(stack: mutable.Stack[Int], labels: mutable.Map[String, List[Char]] = mutable.Map[String, List[Char]]()) extends NumberConverter {

  this: Input with Output =>

  def interpret(code: String): Unit = {
    interpret(code.toList)
  }

  @tailrec
  final def interpret(code: Seq[Char]): Unit = {
    code match {
      case ' ' :: t => t match {
        case ' '  :: tl           => pushToTopOfStack(tl)   ; interpret(tl.dropWhile(_ != '\n').tail)
        case '\n' :: ' '  :: tl   => duplicateTopOfStack()  ; interpret(tl)
        case '\n' :: '\t' :: tl   => swapTopOfStack()       ; interpret(tl)
        case '\n' :: '\n' :: tl   => discardTopOfStack()    ; interpret(tl)
        case _ => throw new RuntimeException(s"Invalid stack command [${prettyPrint(t)}]")

      }
      case '\t' :: ' ' :: t => t match {
        case ' '  :: ' '  :: tl => addTopOfStack()          ; interpret(tl)
        case ' '  :: '\t' :: tl => subtractTopOfStack()     ; interpret(tl)
        case ' '  :: '\n' :: tl => multiplyTopOfStack()     ; interpret(tl)
        case '\t' :: ' '  :: tl => divideTopOfStack()       ; interpret(tl)
        case '\t' :: '\t' :: tl => moduloTopOfStack()       ; interpret(tl)
        case _ => throw new RuntimeException(s"Invalid arithmetic command: [${prettyPrint(t)}]")
      }
      case '\t' :: '\n' :: t => t match {
        case ' '  :: ' '  :: tl => outputCharacterAtTopOfStack()    ; interpret(tl)
        case ' '  :: '\t' :: tl => outputIntAtTopOfStack()          ; interpret(tl)
        case '\t' :: ' '  :: tl => inputCharAndPutOnStack()         ; interpret(tl)
        case '\t' :: '\t' :: tl => inputIntAndPutOnStack()          ; interpret(tl)
        case _ => throw new RuntimeException(s"Invalid IO command: [${prettyPrint(t)}]")
      }
      case '\n' :: t => t match {
        case ' '  :: ' '   :: tl => markALocation(tl)                   ; interpret(tl.dropWhile(_!='\n').tail)
        case ' '  :: '\t'  :: tl => callASubroutine(tl)
        case ' '  :: '\n'  :: tl => interpret(jumpUnconditionallyToALabel(tl))
        case '\t' :: ' '   :: tl => interpret(jumpToLabelIfTopOfStackIsZero(tl))
        case '\t' :: '\t'  :: tl => interpret(jumpToLabelIfTopOfStackIsNegative(tl))
        case '\n' :: '\n'  :: tl => // end program
        case _ => throw new RuntimeException(s"Invalid flow control command: [${prettyPrint(t)}]")
      }
      case h :: t => throw new RuntimeException(s"Unrecognised command: $h")
      case Nil =>
    }
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

  private def addTopOfStack(): Unit = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      stack.push(a+b)
    }
    else throw new RuntimeException(s"Cannot add top two numbers in stack when stack size = ${stack.size}")
  }

  private def subtractTopOfStack(): Unit = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      stack.push(b-a)
    }
    else throw new RuntimeException(s"Cannot subtract top two numbers in stack when stack size = ${stack.size}")

  }

  private def multiplyTopOfStack(): Unit = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      stack.push(a*b)
    }
    else throw new RuntimeException(s"Cannot multiply top two numbers in stack when stack size = ${stack.size}")

  }

  private def divideTopOfStack(): Unit = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      stack.push(b/a)
    }
    else throw new RuntimeException(s"Cannot divide top two numbers in stack when stack size = ${stack.size}")
  }

  private def moduloTopOfStack(): Unit = {
    if (stack.size > 1) {
      val a = stack.pop()
      val b = stack.pop()
      stack.push(b%a)
    }
    else throw new RuntimeException(s"Cannot find b mod a of top two numbers in stack when stack size = ${stack.size}")

  }

  private def outputCharacterAtTopOfStack() = {
    if (stack.nonEmpty) {
      print(stack.pop().toChar)
    }
    else throw new RuntimeException("Cannot print top of stack because stack is empty")
  }

  private def outputIntAtTopOfStack() = {
    if (stack.nonEmpty) {
      print(stack.pop())
    }
    else throw new RuntimeException("Cannot print top of stack because stack is empty")
  }

  private def inputCharAndPutOnStack() = stack.push(readChar())

  private def inputIntAndPutOnStack() = stack.push(readInt())

  def markALocation(t: List[Char]): Unit = {
    val label = t.takeWhile(_ != '\n').mkString
    val toSave = t.dropWhile(_ != '\n').tail
    labels(label) = toSave
  }

  def callASubroutine(t: List[Char]): Unit = ???

  def jumpUnconditionallyToALabel(s: List[Char]): List[Char] = {
    val label = s.takeWhile(_ != '\n').mkString
    labels.getOrElse(label, throw new RuntimeException(s"Unable to goto label: [${prettyPrint(label)}] because label was not found"))
  }

  def jumpToLabelIfTopOfStackIsZero(s: List[Char]): List[Char] = {
    val label = s.takeWhile(_ != '\n').mkString
    if (stack.nonEmpty) {
      val top = stack.pop()
      stack.push(top)
      if (top == 0) return labels.getOrElse(label, throw new RuntimeException(s"Unable to goto label: [${prettyPrint(label)}] because label was not found"))
    }
    s.dropWhile(_!='\n').tail
  }

  def jumpToLabelIfTopOfStackIsNegative(s: List[Char]): List[Char] = {
    val label = s.takeWhile(_ != '\n').mkString
    if (stack.nonEmpty) {
      val top = stack.pop()
      stack.push(top)
      if (top < 0) return labels.getOrElse(label, throw new RuntimeException(s"Unable to goto label: [${prettyPrint(label)}] because label was not found"))
    }
    s.dropWhile(_!='\n').tail
  }


  private def prettyPrint(s: Seq[Char]): String =  {
    s.map {
      case '\t' => "tab"
      case '\n' => "newline"
      case ' '  => "space"
      case x: Char => throw new RuntimeException(s"String contains dodgy char: $x")
    }.mkString(",")
  }

}
