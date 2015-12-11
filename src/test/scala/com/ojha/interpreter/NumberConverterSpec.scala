package com.ojha.interpreter

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by alexandra on 11/12/15.
 */
class NumberConverterSpec extends FlatSpec with Matchers with NumberConverter {

  it should "convert [space, tab] to +1" in {
    val number = " \t"
    assert(convert(number.toCharArray) == 1)
  }

  it should "convert [tab, tab] to -1" in {
    val number = "\t\t"
    assert(convert(number.toCharArray) == -1)
  }

  it should "convert [space, tab, space, tab, tab] to +11" in {
    val number = " \t \t\t"
    assert(convert(number.toCharArray) == 11)
  }

}
