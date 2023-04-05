package com.kensai.aoc.aoc2022

import com.kensai.aoc.aoc2022.Day07._
import com.kensai.aoc.lib.Lib.readInputLines
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day07Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readInputLines(
    "src/test/resources/2022/Day07.input"
  )

  private lazy val puzzleSpecInput = readInputLines(
    "src/test/resources/2022/Day07Spec.input"
  )

  "parse" should "return node for spec input" in {
    // GIVEN: input
    Given("Spec input")
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is expected")
    result shouldBe DirectoryNode(
      "/",
      Seq(
        DirectoryNode(
          "a",
          Seq(
            DirectoryNode("e", Seq(FileNode("i", 584))),
            FileNode("f", 29116),
            FileNode("g", 2557),
            FileNode("h.lst", 62596)
          )
        ),
        FileNode("b.txt", 14848514),
        FileNode("c.dat", 8504156),
        DirectoryNode("d", Seq(FileNode("j", 4060174), FileNode("d.log", 8033020), FileNode("d.ext", 5626152), FileNode("k", 7214296)))
      )
    )
  }

  "computeDirectorySizes" should "return tree with aggregated sizes" in {
    // GIVEN: input
    Given("Spec input")
    val input = parse(puzzleSpecInput)

    When("parse(input)")
    val result = computeDirectorySizes(input)

    Then("Result is expected")
    result shouldBe SizedDirectoryNode(
      "/",
      48381165,
      Seq(
        SizedDirectoryNode(
          "a",
          94853,
          Seq(SizedDirectoryNode("e", 584, Seq()))
        ),
        SizedDirectoryNode("d", 24933642)
      )
    )
  }

  "sumSizesBellow(100000)" should "return result for Spec Input" in {
    // GIVEN: input
    Given("Spec input and aggregated graph")
    val input           = parse(puzzleSpecInput)
    val aggregatedGraph = computeDirectorySizes(input)

    When("sumSizesBellow(input, 100000)")
    val result = sumSizesBellow(aggregatedGraph, 100000)

    Then("Result is 95437")
    result shouldBe 95437
  }

  "sumSizesBellow(100000)" should "return result for Puzzle Input" in {
    // GIVEN: input
    Given("Puzzle input and aggregated graph")
    val input           = parse(puzzleInput)
    val aggregatedGraph = computeDirectorySizes(input)

    When("sumSizesBellow(input, 100000)")
    val result = sumSizesBellow(aggregatedGraph, 100000)

    Then("Result is expected")
    result shouldBe 919137
  }

  "smallestDirToDelete(70000000, 30000000)" should "return result for Spec Input" in {
    // GIVEN: input
    Given("Spec input and aggregated graph")
    val input           = parse(puzzleSpecInput)
    val aggregatedGraph = computeDirectorySizes(input)

    When("sumSizesBellow(70000000, 30000000, input)")
    val result = smallestDirToDelete(70000000, 30000000, aggregatedGraph)

    Then("Result is 24933642")
    result shouldBe 24933642
  }

  "smallestDirToDelete(70000000, 30000000)" should "return result for Puzzle Input" in {
    // GIVEN: input
    Given("Puzzle input and aggregated graph")
    val input           = parse(puzzleInput)
    val aggregatedGraph = computeDirectorySizes(input)

    When("sumSizesBellow(70000000, 30000000, input)")
    val result = smallestDirToDelete(70000000, 30000000, aggregatedGraph)

    Then("Result is expected")
    result shouldBe 2877389
  }

}
