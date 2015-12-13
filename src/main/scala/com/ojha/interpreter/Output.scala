package com.ojha.interpreter

trait Output {
  def print(s: String) = Console.print(s)
  def print(c: Char) = Console.print(c)
  def print(s: Int) = Console.print(s)
}
