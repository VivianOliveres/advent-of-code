package com.kensai.aoc.aoc2021

import com.kensai.aoc.aoc2021.Day18._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day18Spec extends AnyFlatSpec with GivenWhenThen {

//  private lazy val puzzleInput = readInputLines(
//    "src/test/resources/2021/Day18.input"
//  )
//
//  private lazy val puzzleSpecInput = readInputLines(
//    "src/test/resources/2021/Day18Spec.input"
//  )


  "parse([1,2])" should "work" in {
    Given("[1,2]")
    val input = "[1,2]"

    When("parse([1,2])")
    val result = parse(input)

    Then("Result is Parent(Leaf(1), Leaf(2))")
    result shouldBe Parent(Leaf(1), Leaf(2))
  }

  "parse([[1,2],3])" should "work" in {
    Given("[[1,2],3]")
    val input = "[[1,2],3]"

    When("parse([[1,2],3])")
    val result = parse(input)

    Then("Result is Parent(Parent(Leaf(1), Leaf(2)), Leaf(3))")
    result shouldBe Parent(Parent(Leaf(1), Leaf(2)), Leaf(3))
  }

  "parse([9,[8,7]])" should "work" in {
    Given("[9,[8,7]]")
    val input = "[9,[8,7]]"

    When("parse([9,[8,7]])")
    val result = parse(input)

    Then("Result is Parent(Leaf(9), Parent(Leaf(8), Leaf(7)))")
    result shouldBe Parent(Leaf(9), Parent(Leaf(8), Leaf(7)))
  }

  "parse([[1,9],[8,5]])" should "work" in {
    Given("[[1,9],[8,5]]")
    val input = "[[1,9],[8,5]]"

    When("parse([[1,9],[8,5]])")
    val result = parse(input)

    Then("Result is Parent(Parent(Leaf(1), Leaf(9)), Parent(Leaf(8), Leaf(5)))")
    result shouldBe Parent(Parent(Leaf(1), Leaf(9)), Parent(Leaf(8), Leaf(5)))
  }

  "parse([[[[1,2],[3,4]],[[5,6],[7,8]]],9])" should "work" in {
    Given("[[[[1,2],[3,4]],[[5,6],[7,8]]],9]")
    val input = "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"

    When("parse([[[[1,2],[3,4]],[[5,6],[7,8]]],9])")
    val result = parse(input)

    Then("Result is Parent(Parent(Parent(Parent(1, 2), Parent(3, 4)), Parent(Parent(5, 6), Parent(7, 8))), Leaf(9))")
    result shouldBe Parent(Parent(Parent(Parent(1, 2), Parent(3, 4)), Parent(Parent(5, 6), Parent(7, 8))), Leaf(9))
  }

  "add [1,2] and [[3,4],5]" should "produce [[1,2],[[3,4],5]]" in {
    Given("[1,2] and [[3,4],5]")
    val input1 = parse("[1,2]")
    val input2 = parse("[[3,4],5]")

    When("add [1,2] and [[3,4],5]")
    val result = add(input1, input2)

    Then("Result is Parent(Parent(1, 2), Parent(Parent(3, 4), Leaf(5)))")
    result shouldBe Parent(Parent(1, 2), Parent(Parent(3, 4), Leaf(5)))
  }

  "explode [[[[[9,8],1],2],3],4]" should "produce [[[[0,9],2],3],4]" in {
    Given("[[[[[9,8],1],2],3],4]")
    val input = parse("[[[[[9,8],1],2],3],4]")

    When("explode")
    val result = explode(input)

    Then("Result is Parent(Parent(Parent(Parent(0, 9), Leaf(2)), Leaf(3)), Leaf(4))")
    result shouldBe Parent(Parent(Parent(Parent(0, 9), Leaf(2)), Leaf(3)), Leaf(4))
  }

//  "computeHighestVelocityY" should "find result from spec input" in {
//    Given("Puzzle spec input")
//    val input = puzzleSpecInput.head
//
//    When("computeHighestVelocityY(puzzleSpecInput)")
//    val result = computeHighestVelocityY(input)
//
//    Then("Result is ((6,9), 45)")
//    result shouldBe ((6,9), 45)
//  }
//
//  "computeHighestVelocityY" should "find result from input" in {
//    Given("Puzzle input")
//    val input = puzzleInput.head
//
//    When("computeHighestVelocityY(puzzleInput)")
//    val result = computeHighestVelocityY(input)
//
//    Then("Result is expected")
//    result shouldBe-1
//  }

//  "countVelocityY" should "find result from spec input" in {
//    Given("Puzzle spec input")
//    val input = puzzleSpecInput.head
//
//    When("countVelocityY(puzzleSpecInput)")
//    val result = countVelocityY(input)
//
//    Then("Result is 112")
//    result shouldBe 112
//  }
//
//  "countVelocityY" should "find result from input" in {
//    Given("Puzzle input")
//    val input = puzzleInput.head
//
//    When("countVelocityY(puzzleInput)")
//    val result = countVelocityY(input)
//
//    Then("Result is expected")
//    // 234 is too low
//    result shouldBe 2371
//  }

}
