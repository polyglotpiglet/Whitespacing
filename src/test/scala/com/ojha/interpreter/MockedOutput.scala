package com.ojha.interpreter

/**
 * Created by alexandra on 11/12/15.
 */
trait MockedOutput extends Output {
  var messages: Seq[Any] = Seq()

  override def print(s: String) = messages = messages :+ s

  override def print(s: Int) = messages = messages :+ s

  override def print(c: Char) = messages = messages :+ c
}
