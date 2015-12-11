package com.ojha.interpreter

import org.scalatest.{Matchers, FlatSpec}
import scala.collection._

/**
 * Created by alexandra on 11/12/15.
 */
class BatchInterperterSpec extends FlatSpec with Matchers {

  it should "stick a number on the stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    val cmd = "   \t\n" // [space, space, space, tab, newline]

    // when
    interpreter.interpret(cmd)

    // then
    stack.size should equal(1)
    stack.pop() should equal(1)
  }

  it should "stick a negative number on the stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    val cmd = "  \t\t\n" // [space, space, tab, tab, newline]

    // when
    interpreter.interpret(cmd)

    // then
    stack.size should equal(1)
    stack.pop() should equal(-1)
  }

  it should "stick zero on the stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    val cmd = "   \n" // [space, space, space, newline]

    // when
    interpreter.interpret(cmd)

    // then
    stack.size should equal(1)
    stack.pop() should equal(0)
  }

  it should "stick another number on the stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    val cmd = "   \t \t\t\n" // [Space][Space][Space][Tab][Space][Tab][Tab][line]

    // when
    interpreter.interpret(cmd)

    // then
    stack.size should equal(1)
    stack.pop() should equal(11)
  }

  // -----------------------------------------------------------------------------
  // Duplicate top of stack
  // -----------------------------------------------------------------------------

  it should "duplicate top of stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    val cmd = "   \t \t\t\n" // [Space][Space][Space][Tab][Space][Tab][Tab][line]
    interpreter.interpret(cmd)

    // when
    interpreter.interpret(" \n ") // [Space, linebreak, space]

    // then
    stack.size should equal(2)
    stack.pop() should equal(11)
    stack.pop() should equal(11)
  }

  it should "ignore duplication if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)

    // when
    interpreter.interpret(" \n ") // [Space, linebreak, space]

    // then
    stack.size should equal(0)
  }

}
