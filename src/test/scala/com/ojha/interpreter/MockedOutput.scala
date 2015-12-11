package com.ojha.interpreter

/**
 * Created by alexandra on 11/12/15.
 */
trait MockedOutput extends Output {
  var messages: Seq[String] = Seq()

  override def print(s: String) = messages = messages :+ s
}
