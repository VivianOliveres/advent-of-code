package com.kensai.aoc.aoc2024

import com.kensai.aoc.aoc2024.Day15._
import com.kensai.aoc.lib.Geo.Point2D
import com.kensai.aoc.lib.Lib.readRawInputFile
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day15Spec extends AnyFlatSpec with GivenWhenThen {

  private lazy val puzzleInput = readRawInputFile(
    "src/test/resources/2024/Day15.input"
  )

  private lazy val puzzleSpecInput = readRawInputFile(
    "src/test/resources/2024/Day15Spec.input"
  )

  private lazy val puzzleSpecInput2 = readRawInputFile(
    "src/test/resources/2024/Day15Spec2.input"
  )

  private lazy val puzzleSpecInput3 = readRawInputFile(
    "src/test/resources/2024/Day15Spec3.input"
  )

  "parse(SpecInput)" should "work for Spec input" in {
    val input = puzzleSpecInput

    When("parse(input)")
    val result = parse(input)

    Then("Result is Spec Input")
    result.robot shouldBe Point2D(4, 4)
    result.walls should have size (10 + 10 + 8 + 8 + 1)
    result.walls should contain (Point2D(2, 5))
    result.boxes should have size 21
    result.remainingMovements should have size (70 * 10)
    result.remainingMovements.head shouldBe Left
    result.remainingMovements(1) shouldBe Down
    result.remainingMovements(2) shouldBe Down
  }

  "parse(SpecInput)" should "work for Spec input 2" in {
    val input = puzzleSpecInput2

    When("parse(input)")
    val result = parse(input)

    Then("Result is Spec Input")
    result.robot shouldBe Point2D(2, 2)
    result.walls should contain (Point2D(1, 2))
    result.remainingMovements.head shouldBe Left
  }

  "doMoveOnce(initial)" should "find result from puzzle spec2 input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput2)

    When("doMoveOnce(<)")
    val (robot, boxes) = doMoveOnce(input, input.remainingMovements.head, input.robot, input.boxes)

    Then("Result is unchanged")
    robot shouldBe input.robot
    boxes shouldBe input.boxes
  }

  "doMoveOnce(step1)" should "find result from puzzle spec2 input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput2)

    When("doMoveOnce(<^)")
    val result = doMove(input, input.remainingMovements.take(2), input.robot, input.boxes)

    Then("Robot has moved")
    result.robot shouldBe Point2D(2, 1)
    result.boxes shouldBe input.boxes
  }

  "doMoveOnce(step2)" should "find result from puzzle spec2 input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput2)

    When("doMoveOnce(<^^)")
    val result = doMove(input, input.remainingMovements.take(3), input.robot, input.boxes)

    Then("Robot is unchanged")
    result.robot shouldBe Point2D(2, 1)
    result.boxes.toSeq.sortBy(p => (p.y, p.x)) shouldBe Seq(
      Point2D(3, 1), Point2D(5, 1),
      Point2D(4, 2),
      Point2D(4, 3),
      Point2D(4, 4),
      Point2D(4, 5),
    )
  }

  "doMoveOnce(step3)" should "find result from puzzle spec2 input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput2)

    When("doMoveOnce(<^^>)")
    val result = doMove(input, input.remainingMovements.take(4), input.robot, input.boxes)

    Then("Robot is unchanged")
    result.robot shouldBe Point2D(3, 1)
    result.boxes.toSeq.sortBy(p => (p.y, p.x)) shouldBe Seq(
      Point2D(4, 1), Point2D(5, 1),
      Point2D(4, 2),
      Point2D(4, 3),
      Point2D(4, 4),
      Point2D(4, 5),
    )
  }

  "doMoveOnce(step4)" should "find result from puzzle spec2 input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput2)

    When("doMoveOnce(<^^>>)")
    val result = doMove(input, input.remainingMovements.take(5), input.robot, input.boxes)

    Then("Robot is unchanged")
    result.robot shouldBe Point2D(4, 1)
    result.boxes.toSeq.sortBy(p => (p.y, p.x)) shouldBe Seq(
      Point2D(5, 1), Point2D(6, 1),
      Point2D(4, 2),
      Point2D(4, 3),
      Point2D(4, 4),
      Point2D(4, 5),
    )
  }

  "sumBoxesCoordinates(input)" should "find result from puzzle spec2 input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput2)

    When("sumBoxesCoordinates(input)")
    val result = sumBoxesCoordinates(input)

    Then("Result is 2028")
    result shouldBe 2028
  }

  "sumBoxesCoordinates(input)" should "find result from puzzle spec input" in {
    Given("Puzzle input")
    val input = parse(puzzleSpecInput)

    When("sumBoxesCoordinates(input)")
    val result = sumBoxesCoordinates(input)

    Then("Result is 10092")
    result shouldBe 10092
  }

  "sumBoxesCoordinates(input)" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)

    When("sumBoxesCoordinates(input)")
    val result = sumBoxesCoordinates(input)

    Then("Result is expected")
    result shouldBe 1490942
  }

  "extend(SpecInput3)" should "work for Spec input" in {
    val input = parse(puzzleSpecInput3)

    //TODO

    When("extend(input)")
    val result = extend(input)

    Then("Result is Spec Input")
    result.robot shouldBe Point2D(10, 3)

    // Walls
    result.walls.toSeq.sortBy(p => (p.left.y, p.left.x)).foreach(println)
    (0 to 13 by 2).foreach{x =>
      // Bottom
      result.walls should contain (BigPoint(Point2D(x, 0), Point2D(x + 1, 0)))
      // Up
      result.walls should contain (BigPoint(Point2D(x, 6), Point2D(x + 1, 6)))
    }
    (0 to 6 by 2).foreach{y =>
      result.walls should contain (BigPoint(Point2D(0, y), Point2D(1, y)))
      result.walls should contain (BigPoint(Point2D(12, y), Point2D(13, y)))
    }
    result.walls should contain (BigPoint(Point2D(8, 1), Point2D(9, 1)))
    result.walls should have size (14 + 10 + 1)

    result.boxes shouldBe Set(
      BigPoint(Point2D(6, 3), Point2D(7, 3)),
      BigPoint(Point2D(8, 3), Point2D(9, 3)),
      BigPoint(Point2D(6, 4), Point2D(7, 4)),
    )
    result.boxes should have size 3

    result.remainingMovements should have size 11
  }


  "doMoveOnce2(initial)" should "find result from puzzle spec3 input" in {
    Given("Puzzle input")
    val input = extend(parse(puzzleSpecInput3))

    When("doMoveOnce(<)")
    val result = doMove2(input, input.remainingMovements.take(1), input.robot, input.boxes)

    Then("Robot is unchanged")
    result.robot shouldBe Point2D(9, 3)
    result.walls shouldBe input.walls
    result.boxes.toSeq.sortBy(p => (p.left.y, p.left.x)) shouldBe Seq(
      BigPoint(Point2D(5, 3), Point2D(6, 3)),
      BigPoint(Point2D(7, 3), Point2D(8, 3)),
      BigPoint(Point2D(6, 4), Point2D(7, 4)),
    )
  }

  "doMoveOnce2(step1)" should "find result from puzzle spec3 input" in {
    Given("Puzzle input")
    val input = extend(parse(puzzleSpecInput3))

    When("doMoveOnce(<v)")
    val result = doMove2(input, input.remainingMovements.take(2), input.robot, input.boxes)

    Then("Robot is unchanged")
    result.robot shouldBe Point2D(9, 4)
    result.walls shouldBe input.walls
    result.boxes.toSeq.sortBy(p => (p.left.y, p.left.x)) shouldBe Seq(
      BigPoint(Point2D(5, 3), Point2D(6, 3)),
      BigPoint(Point2D(7, 3), Point2D(8, 3)),
      BigPoint(Point2D(6, 4), Point2D(7, 4)),
    )
  }

  "doMoveOnce2(step2)" should "find result from puzzle spec3 input" in {
    Given("Puzzle input")
    val input = extend(parse(puzzleSpecInput3))

    When("doMoveOnce(<vv)")
    val result = doMove2(input, input.remainingMovements.take(3), input.robot, input.boxes)

    Then("Robot is unchanged")
    result.robot shouldBe Point2D(9, 5)
    result.walls shouldBe input.walls
    result.boxes.toSeq.sortBy(p => (p.left.y, p.left.x)) shouldBe Seq(
      BigPoint(Point2D(5, 3), Point2D(6, 3)),
      BigPoint(Point2D(7, 3), Point2D(8, 3)),
      BigPoint(Point2D(6, 4), Point2D(7, 4)),
    )
  }

  "doMoveOnce2(step3)" should "find result from puzzle spec3 input" in {
    Given("Puzzle input")
    val input = extend(parse(puzzleSpecInput3))

    When("doMoveOnce(<vv<)")
    val result = doMove2(input, input.remainingMovements.take(4), input.robot, input.boxes)

    Then("Robot is unchanged")
    result.robot shouldBe Point2D(8, 5)
    result.walls shouldBe input.walls
    result.boxes.toSeq.sortBy(p => (p.left.y, p.left.x)) shouldBe Seq(
      BigPoint(Point2D(5, 3), Point2D(6, 3)),
      BigPoint(Point2D(7, 3), Point2D(8, 3)),
      BigPoint(Point2D(6, 4), Point2D(7, 4)),
    )
  }

  "doMoveOnce2(step4)" should "find result from puzzle spec3 input" in {
    Given("Puzzle input")
    val input = extend(parse(puzzleSpecInput3))

    When("doMoveOnce(<vv<<)")
    val result = doMove2(input, input.remainingMovements.take(5), input.robot, input.boxes)

    Then("Robot is unchanged")
    result.robot shouldBe Point2D(7, 5)
    result.walls shouldBe input.walls
    result.boxes.toSeq.sortBy(p => (p.left.y, p.left.x)) shouldBe Seq(
      BigPoint(Point2D(5, 3), Point2D(6, 3)),
      BigPoint(Point2D(7, 3), Point2D(8, 3)),
      BigPoint(Point2D(6, 4), Point2D(7, 4)),
    )
  }

  "doMoveOnce2(step5)" should "find result from puzzle spec3 input" in {
    Given("Puzzle input")
    val input = extend(parse(puzzleSpecInput3))

    When("doMoveOnce(<vv<<^)")
    val result = doMove2(input, input.remainingMovements.take(6), input.robot, input.boxes)

    Then("Robot is unchanged")
    result.robot shouldBe Point2D(7, 4)
    result.walls shouldBe input.walls
    result.boxes.toSeq.sortBy(p => (p.left.y, p.left.x)) shouldBe Seq(
      BigPoint(Point2D(5, 2), Point2D(6, 2)),
      BigPoint(Point2D(7, 2), Point2D(8, 2)),
      BigPoint(Point2D(6, 3), Point2D(7, 3)),
    )
  }

  "doMoveOnce2(step6)" should "find result from puzzle spec3 input" in {
    Given("Puzzle input")
    val input = extend(parse(puzzleSpecInput3))

    When("doMoveOnce(<vv<<^^)")
    val result = doMove2(input, input.remainingMovements.take(7), input.robot, input.boxes)

    Then("Robot is unchanged")
    result.robot shouldBe Point2D(7, 4)
    result.walls shouldBe input.walls
    result.boxes.toSeq.sortBy(p => (p.left.y, p.left.x)) shouldBe Seq(
      BigPoint(Point2D(5, 2), Point2D(6, 2)),
      BigPoint(Point2D(7, 2), Point2D(8, 2)),
      BigPoint(Point2D(6, 3), Point2D(7, 3)),
    )
  }

  "sumBoxesCoordinates2(extend(input))" should "find result from puzzle spec input" in {
    Given("Puzzle spec input")
    val input = parse(puzzleSpecInput)
    val extendedInput = extend(input)

    When("sumBoxesCoordinates2(extendedInput)")
    val result = sumBoxesCoordinates2(extendedInput)

    Then("Result is 9021")
    result shouldBe 9021
  }

  "sumBoxesCoordinates2(extend(input))" should "find result from puzzle input" in {
    Given("Puzzle input")
    val input = parse(puzzleInput)
    val extendedInput = extend(input)

    When("sumBoxesCoordinates2(extendedInput)")
    val result = sumBoxesCoordinates2(extendedInput)

    Then("Result is expected")
    result shouldBe 1519202
  }

}
