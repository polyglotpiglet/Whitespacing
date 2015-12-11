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

  // -----------------------------------------------------------------------------
  // Swap top of stack
  // -----------------------------------------------------------------------------

  it should "ignore swap if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)

    // when
    interpreter.interpret(" \n\t") // [Space, linebreak, tab]

    // then
    stack.size should equal(0)
  }

  it should "ignore swap if stack is of size 1" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    interpreter.interpret("   \t\t\n")
    // when
    interpreter.interpret(" \n\t") // [Space, linebreak, tab]

    // then
    stack.size should equal(1)
    stack.pop should equal(3)
  }

  it should "swap top of stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    interpreter.interpret("   \t\t\n") // 3
    interpreter.interpret("   \t \n")  // 2
    // when
    interpreter.interpret(" \n\t") // [Space, linebreak, tab]

    // then
    stack.size should equal(2)
    stack.pop should equal(3)
    stack.pop should equal(2)
  }

  // -----------------------------------------------------------------------------
  // Discard top of stack
  // -----------------------------------------------------------------------------

  it should "discard where stack size is bigger than 1" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    interpreter.interpret("  \t\t\t\t \n") // push a number onto stack
    interpreter.interpret("  \t\t\t  \n") // push a number onto stack
    interpreter.interpret("    \t\t  \n") // push a number onto stack

    // when
    interpreter.interpret(" \n\n")  // [Space, linebreak, linebreak]

    // then
    stack.size should equal(2)
    stack.pop should equal(-12)
    stack.pop should equal(-14)
  }

  it should "discard where stack size is 1" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    interpreter.interpret("  \t\t\t\t \n") // push a number onto stack

    // when
    interpreter.interpret(" \n\n")  // [Space, linebreak, linebreak]

    // then
    stack.size should equal(0)
  }

  it should "ignore discard if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)

    // when
    interpreter.interpret(" \n\n") // [Space, linebreak, linebreak]

    // then
    stack.size should equal(0)
  }

  // -----------------------------------------------------------------------------
  // Add top of stack
  // -----------------------------------------------------------------------------

  it should "throw exception on addition if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)

    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t  ") // [tab, space, space]
    }
    thrown.getMessage should equal("Cannot add top two numbers in stack when stack size = 0")
  }

  it should "throw exception on addition if stack size is one" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    stack.push(5)

    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t  ") // [tab, space, space]
    }
    thrown.getMessage should equal("Cannot add top two numbers in stack when stack size = 1")
  }

  it should "add top two stack elements" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    stack.push(5)
    stack.push(3)
    stack.push(3)

    // when
    interpreter.interpret("\t  ") // [tab, space, space]

    // then
    stack.size should equal(2)
    stack.pop should equal(6)
  }

  // -----------------------------------------------------------------------------
  // Subtract top of stack
  // -----------------------------------------------------------------------------

  it should "throw exception on subtraction if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t \t")
    }

    // then
    thrown.getMessage should equal("Cannot subtract top two numbers in stack when stack size = 0")
  }

  it should "throw exception on subtraction if stack size is one" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    stack.push(5)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t \t")
    }

    // then
    thrown.getMessage should equal("Cannot subtract top two numbers in stack when stack size = 1")
  }

  it should "subtract top two stack elements" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    stack.push(5)
    stack.push(4)
    stack.push(3)

    // when
    interpreter.interpret("\t \t")

    // then
    stack.size should equal(2)
    stack.pop should equal(1)
  }

  // -----------------------------------------------------------------------------
  // Multiple top of stack
  // -----------------------------------------------------------------------------

  it should "throw exception on multiply if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t \n")
    }

    // then
    thrown.getMessage should equal("Cannot multiply top two numbers in stack when stack size = 0")
  }

  it should "throw exception on multiply if stack size is one" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    stack.push(5)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t \n")
    }

    // then
    thrown.getMessage should equal("Cannot multiply top two numbers in stack when stack size = 1")
  }

  it should "multiply top two stack elements" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    stack.push(5)
    stack.push(4)
    stack.push(3)

    // when
    interpreter.interpret("\t \n")

    // then
    stack.size should equal(2)
    stack.pop should equal(12)
  }

  // -----------------------------------------------------------------------------
  // Divide top of stack
  // -----------------------------------------------------------------------------

  it should "throw exception on divide if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t\t ")
    }

    // then
    thrown.getMessage should equal("Cannot divide top two numbers in stack when stack size = 0")
  }

  it should "throw exception on divide if stack size is one" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    stack.push(5)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t\t ")
    }

    // then
    thrown.getMessage should equal("Cannot divide top two numbers in stack when stack size = 1")
  }

  it should "divide top two stack elements" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    stack.push(5)
    stack.push(4)
    stack.push(3)

    // when
    interpreter.interpret("\t\t ")

    // then
    stack.size should equal(2)
    stack.pop should equal(1)
  }

  // -----------------------------------------------------------------------------
  // Modulo top of stack
  // -----------------------------------------------------------------------------

  it should "throw exception on modulo if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t\t\t")
    }

    // then
    thrown.getMessage should equal("Cannot find b mod a of top two numbers in stack when stack size = 0")
  }

  it should "throw exception on modulo if stack size is one" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    stack.push(5)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t\t\t")
    }

    // then
    thrown.getMessage should equal("Cannot find b mod a of top two numbers in stack when stack size = 1")
  }

  it should "modulo top two stack elements" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack)
    stack.push(5)
    stack.push(7)
    stack.push(3)

    // when
    interpreter.interpret("\t\t\t")

    // then
    stack.size should equal(2)
    stack.pop should equal(1)
  }

}
