package com.ojha.interpreter

import org.scalatest.{Matchers, FlatSpec}
import scala.collection._

/**
 * Created by alexandra on 11/12/15.
 */
class BatchInterperterSpec extends FlatSpec with Matchers {

  // -----------------------------------------------------------------------------
  // Invalid command
  // -----------------------------------------------------------------------------

  it should "throw exception if cmd starts with something funny" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("zzz") // dodgy command
    }

    // then
    thrown.getMessage should equal("Unrecognised command: z")
  }



  // -----------------------------------------------------------------------------
  // Invalid stack manlipulation
  // -----------------------------------------------------------------------------

  it should "throw exception if stack milipulation request is invalid (doesnt match)" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret(" \t\t") // dodgy command
    }

    // then
    thrown.getMessage should equal("""Invalid stack command [tab,tab]""")
  }

  it should "throw exception if stack manipulation request is invalid (contains something really weird)" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret(" efg") // dodgy command
    }

    // then
    thrown.getMessage should equal("String contains dodgy char: e")
  }

  // -----------------------------------------------------------------------------
  // Stack manlipulation
  // -----------------------------------------------------------------------------

  it should "stick a number on the stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
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
    val interpreter = new BatchInterpreter(stack) with Output with Input
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
    val interpreter = new BatchInterpreter(stack) with Output with Input
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
    val interpreter = new BatchInterpreter(stack) with Output with Input
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
    val interpreter = new BatchInterpreter(stack) with Output with Input
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
    val interpreter = new BatchInterpreter(stack) with Output with Input

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
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    interpreter.interpret(" \n\t") // [Space, linebreak, tab]

    // then
    stack.size should equal(0)
  }

  it should "ignore swap if stack is of size 1" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
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
    val interpreter = new BatchInterpreter(stack) with Output with Input
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
    val interpreter = new BatchInterpreter(stack) with Output with Input
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
    val interpreter = new BatchInterpreter(stack) with Output with Input
    interpreter.interpret("  \t\t\t\t \n") // push a number onto stack

    // when
    interpreter.interpret(" \n\n")  // [Space, linebreak, linebreak]

    // then
    stack.size should equal(0)
  }

  it should "ignore discard if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    interpreter.interpret(" \n\n") // [Space, linebreak, linebreak]

    // then
    stack.size should equal(0)
  }

  // -----------------------------------------------------------------------------
  // Invalid arithmetic command
  // -----------------------------------------------------------------------------

  it should "throw exception if arithmetic request is invalid (doesnt match)" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t \n\t") // dodgy command
    }

    // then
    thrown.getMessage should equal("""Invalid arithmetic command [newline,tab]""")
  }

  it should "throw exception if arithmetic request is invalid (contains something really weird)" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t \n\nzzz") // dodgy command
    }

    // then
    thrown.getMessage should equal("String contains dodgy char: z")
  }

  // -----------------------------------------------------------------------------
  // Add top of stack
  // -----------------------------------------------------------------------------

  it should "throw exception on addition if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input

    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t   ") // [tab, space, space]
    }
    thrown.getMessage should equal("Cannot add top two numbers in stack when stack size = 0")
  }

  it should "throw exception on addition if stack size is one" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
    stack.push(5)

    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t   ") // [tab, space, space]
    }
    thrown.getMessage should equal("Cannot add top two numbers in stack when stack size = 1")
  }

  it should "add top two stack elements" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
    stack.push(5)
    stack.push(3)
    stack.push(3)

    // when
    interpreter.interpret("\t   ") // [tab, space, space]

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
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t  \t")
    }

    // then
    thrown.getMessage should equal("Cannot subtract top two numbers in stack when stack size = 0")
  }

  it should "throw exception on subtraction if stack size is one" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
    stack.push(5)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t  \t")
    }

    // then
    thrown.getMessage should equal("Cannot subtract top two numbers in stack when stack size = 1")
  }

  it should "subtract top two stack elements" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
    stack.push(5)
    stack.push(4)
    stack.push(3)

    // when
    interpreter.interpret("\t  \t")

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
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t  \n")
    }

    // then
    thrown.getMessage should equal("Cannot multiply top two numbers in stack when stack size = 0")
  }

  it should "throw exception on multiply if stack size is one" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
    stack.push(5)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t  \n")
    }

    // then
    thrown.getMessage should equal("Cannot multiply top two numbers in stack when stack size = 1")
  }

  it should "multiply top two stack elements" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
    stack.push(5)
    stack.push(4)
    stack.push(3)

    // when
    interpreter.interpret("\t  \n")

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
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t \t ")
    }

    // then
    thrown.getMessage should equal("Cannot divide top two numbers in stack when stack size = 0")
  }

  it should "throw exception on divide if stack size is one" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
    stack.push(5)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t \t ")
    }

    // then
    thrown.getMessage should equal("Cannot divide top two numbers in stack when stack size = 1")
  }

  it should "divide top two stack elements" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
    stack.push(5)
    stack.push(4)
    stack.push(3)

    // when
    interpreter.interpret("\t \t ")

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
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t \t\t")
    }

    // then
    thrown.getMessage should equal("Cannot find b mod a of top two numbers in stack when stack size = 0")
  }

  it should "throw exception on modulo if stack size is one" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
    stack.push(5)

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t \t\t")
    }

    // then
    thrown.getMessage should equal("Cannot find b mod a of top two numbers in stack when stack size = 1")
  }

  it should "modulo top two stack elements" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input
    stack.push(5)
    stack.push(7)
    stack.push(3)

    // when
    interpreter.interpret("\t \t\t")

    // then
    stack.size should equal(2)
    stack.pop should equal(1)
  }

  // -----------------------------------------------------------------------------
  // IO Output char at top of stack
  // -----------------------------------------------------------------------------

  it should "throw exception on output char at top of stack if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input 

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t\n  ")
    }

    // then
    thrown.getMessage should equal("Cannot print top of stack because stack is empty")
  }

  it should "println char at top of stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with MockedOutput with Input
    stack.push(97)

    // when
    interpreter.interpret("\t\n  ")

    // then
    interpreter.messages.size should equal(1)
    interpreter.messages.head should equal('a')
  }

  it should "println a few chars at top of stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with MockedOutput with Input
    stack.push(97)
    stack.push(98)
    stack.push(99)
    stack.push(100)

    // when
    interpreter.interpret("\t\n  \t\n  \t\n  \t\n  ")

    // then
    interpreter.messages.size should equal(4)
    interpreter.messages should equal(Seq('d', 'c', 'b', 'a'))

  }

  // -----------------------------------------------------------------------------
  // Invalid IO
  // -----------------------------------------------------------------------------

  it should "throw exception if io is invalid (doesnt match)" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t\n\n\t") // dodgy command
    }

    // then
    thrown.getMessage should equal("""Invalid IO command [newline,tab]""")
  }

  it should "throw exception if io is invalid (contains something really weird)" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t\n\nc") // dodgy command
    }

    // then
    thrown.getMessage should equal("String contains dodgy char: c")
  }


  // -----------------------------------------------------------------------------
  // IO Output int at top of stack
  // -----------------------------------------------------------------------------

  it should "throw exception on output int at top of stack if stack is empty" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with Output with Input

    // when
    val thrown = intercept[RuntimeException] {
      interpreter.interpret("\t\n \t")
    }

    // then
    thrown.getMessage should equal("Cannot print top of stack because stack is empty")
  }

  it should "println top of stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with MockedOutput with Input
    stack.push(5)

    // when
    interpreter.interpret("\t\n \t")

    // then
    interpreter.messages.size should equal(1)
    interpreter.messages.head should equal(5)
  }


  it should "println top few in stack" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with MockedOutput with Input
    stack.push(5)
    stack.push(1)
    stack.push(6)
    stack.push(8)
    stack.push(9)

    // when
    interpreter.interpret("\t\n \t\t\n \t\t\n \t\t\n \t\t\n \t")

    // then
    interpreter.messages.size should equal(5)
    interpreter.messages should equal(Seq(5,1,6,8,9).reverse)
    assert(stack.isEmpty)
  }

  // -----------------------------------------------------------------------------
  // IO Input stick stuff on stack
  // -----------------------------------------------------------------------------

  it should "add to top of stack (char)" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with MockedOutput with MockedInput {
      override val inputs: scala.Seq[Int] = List[Int](8, 97)
    }

    // when
    interpreter.interpret("\t\n\t ")

    // then
    stack.size should equal(1)
    stack.pop should equal(8)
  }

  it should "add to top of stack (int)" in {
    // given
    val stack = mutable.Stack[Int]()
    val interpreter = new BatchInterpreter(stack) with MockedOutput with MockedInput {
      override val inputs: scala.Seq[Int] = List[Int](8, 97)
    }

    // when
    interpreter.interpret("\t\n\t\t\t\n\t\t")

    // then
    stack.size should equal(2)
    stack.pop should equal(97)
    stack.pop should equal(8)
  }
}
