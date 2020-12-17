package com.kensai.aoc

import com.kensai.aoc.Day17._
import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day17Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInputs = readInputFile("src/test/resources/Day17.input")
  private lazy val specInputs = readInputFile("src/test/resources/Day17Spec.input")

  "computeNeighbors3" should "return 26 points" in {
    val input = Coord3(2, 2, 2)
    Given(s"Input is $input")

    When(s"computeNeighbors3($input)")
    val result = computeNeighbors3(input)

    Then(s"Result contains all 26 points")
    result should contain (Coord3(1, 1, 1))
    result should contain (Coord3(1, 1, 2))
    result should contain (Coord3(1, 1, 3))
    result should contain (Coord3(1, 2, 1))
    result should contain (Coord3(1, 2, 2))
    result should contain (Coord3(1, 2, 3))
    result should contain (Coord3(1, 3, 1))
    result should contain (Coord3(1, 3, 2))
    result should contain (Coord3(1, 3, 3))

    result should contain (Coord3(2, 1, 1))
    result should contain (Coord3(2, 1, 2))
    result should contain (Coord3(2, 1, 3))
    result should contain (Coord3(2, 2, 1))
//    result should contain (Coord(2, 2, 2))
    result should contain (Coord3(2, 2, 3))
    result should contain (Coord3(2, 3, 1))
    result should contain (Coord3(2, 3, 2))
    result should contain (Coord3(2, 3, 3))

    result should contain (Coord3(3, 1, 1))
    result should contain (Coord3(3, 1, 2))
    result should contain (Coord3(3, 1, 3))
    result should contain (Coord3(3, 2, 1))
    result should contain (Coord3(3, 2, 2))
    result should contain (Coord3(3, 2, 3))
    result should contain (Coord3(3, 3, 1))
    result should contain (Coord3(3, 3, 2))
    result should contain (Coord3(3, 3, 3))

    And(s"Result has 26 points")
    result should have size 26
  }

  "parse3 from spec" should "return 4 points" in {
    Given(s"Input is ")

    When("parse")
    val result = parse3(specInputs)

    Then(s"Result is 4 points")
    result shouldBe Set(Coord3(1, 0, 0), Coord3(2, 1, 0), Coord3(0, 2, 0), Coord3(1, 2, 0), Coord3(2, 2, 0))
  }

  "doComputeNextState for spec" should "return 11" in {
    Given(s"Input is spec")
    val input = parse3(specInputs)

    When("doComputeNextState")
    val result = doComputeNextState(input, computeNeighbors3)

    Then(s"Result is 11")
    result should have size 11
  }

  "activeCubeAfter3 for spec" should "return 112" in {
    Given(s"Input is spec")

    When("activeCubeAfter3")
    val result = activeCubeAfter3(specInputs, 6)

    Then(s"Result is ")
    result shouldBe 112L
  }

  "activeCubeAfter3 for puzzle" should "return solution" in {
    Given(s"Input is puzzle")

    When("activeCubeAfter3")
    val result = activeCubeAfter3(puzzleInputs, 6)

    Then(s"Result is 202")
    result shouldBe 202L
  }

  "computeNeighbors4" should "return 80 points" in {
    Given(s"Input is spec")

    When("computeNeighbors4")
    val result = computeNeighbors4(Coord4(0, 0, 0, 0))

    Then(s"Result is ")
    result should have size 80
  }

  "activeCubeAfter4 for spec numCycles=1" should "return 29" in {
    Given(s"Input is spec")

    When("activeCubeAfter4")
    val result = activeCubeAfter4(specInputs, 1)

    Then(s"Result is 29")
    result shouldBe 29L
  }

  "activeCubeAfter4 for spec numCycles=2" should "return 60" in {
    Given(s"Input is spec")

    When("activeCubeAfter4")
    val result = activeCubeAfter4(specInputs, 2)

    Then(s"Result is 60")
    result shouldBe 60L
  }

  "activeCubeAfter4 for spec" should "return 848" in {
    Given(s"Input is spec")

    When("activeCubeAfter4")
    val result = activeCubeAfter4(specInputs, 6)

    Then(s"Result is 848")
    result shouldBe 848L
  }

  "activeCubeAfter4 for puzzle" should "return solution" in {
    Given(s"Input is puzzle")

    When("activeCubeAfter4")
    val result = activeCubeAfter4(puzzleInputs, 6)

    Then(s"Result is 2028")
    result shouldBe 2028L
  }

}
