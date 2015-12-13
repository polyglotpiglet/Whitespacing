package com.ojha.interpreter

trait Output {
  def print(s: String) = Console.println(s)
  def print(c: Char) = Console.println(c)
  def print(s: Int) = Console.println(s)
}
