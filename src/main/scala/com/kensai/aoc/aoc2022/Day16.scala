package com.kensai.aoc.aoc2022

import scala.annotation.tailrec
import scala.collection.mutable

object Day16 {

  private val MaxMinutes = 30

  case class Valve(name: String, flowRate: Int, leadsTo: Seq[String])

  case class Step(minutesSpent: Int, currentFlow: Int, totalFlowSum: Int, currentPath: Seq[String], remaining: Set[String]) {

    def waitUntilEnd: Step = {
      val remainingMinutes = MaxMinutes - minutesSpent
      copy(minutesSpent = MaxMinutes, totalFlowSum = totalFlowSum + currentFlow * remainingMinutes)
    }

    def move(to: String, pathSize: Int): Step =
      copy(
        minutesSpent = minutesSpent + pathSize,
        totalFlowSum = totalFlowSum + currentFlow * pathSize,
        currentPath = to +: currentPath, // Prepend!
        remaining = remaining - to
      )

    def openValve(flow: Int): Step =
      copy(minutesSpent = minutesSpent + 1, currentFlow = currentFlow + flow, totalFlowSum = totalFlowSum + currentFlow)
  }

  private val lineRegex = """Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.+)""".r
  def parse(lines: Seq[String]): Map[String, Valve] =
    lines
      .collect {
        case str if str.nonEmpty => str.trim
      }
      .map {
        case lineRegex(valveName, flowRateStr, leadsToStr) =>
          val leadsTo = leadsToStr.split(", ").toSeq
          valveName -> Valve(valveName, flowRateStr.toInt, leadsTo)
        case str => throw new IllegalArgumentException(s"Can not parse line [$str]")
      }
      .toMap

  private def computeDirectPaths(valves: Map[String, Valve]): Map[(String, String), Int] = {
    val interestingValves = "AA" +: valves.values.filter(_.flowRate > 0).map(_.name).toSeq.sorted
    val allCouples        = generateAllPairs(interestingValves, Seq())
    allCouples.map { case (from, to) =>
      val bestPositions = mutable.Map(from -> 0)
      val nextPositions = mutable.PriorityQueue((from, 0))(Ordering.by(b => -b._2))
      val result        = doComputeBestPath(to, valves, bestPositions, nextPositions)
      (from, to) -> result
    }.toMap
  }

  @tailrec
  private def generateAllPairs(valveNames: Seq[String], acc: Seq[(String, String)]): Seq[(String, String)] = valveNames match {
    case Nil      => acc
    case _ :: Nil => acc
    case head :: tail =>
      val ro  = tail.map((head, _))
      val roo = acc ++ ro
      generateAllPairs(valveNames.tail, roo)
    case other => throw new IllegalArgumentException(s"Should not happen: $other")
  }

  // TODO: extract Dijkstra
  private def doComputeBestPath(
      target: String,
      valves: Map[String, Valve],
      bestPositions: mutable.Map[String, Int],
      nextPositions: mutable.PriorityQueue[(String, Int)]
    ): Int = {
    val (currentPos, pathSize) = nextPositions.dequeue()
    val newPathSize            = pathSize + 1
    if (currentPos == target)
      pathSize
    else if (bestPositions(currentPos) < pathSize)
      doComputeBestPath(target, valves, bestPositions, nextPositions)
    else {
      valves(currentPos).leadsTo
        .foreach { other =>
          bestPositions.put(other, newPathSize)
          nextPositions.enqueue((other, newPathSize))
        }
      doComputeBestPath(target, valves, bestPositions, nextPositions)
    }
  }

  def findBestReleasePressure(valves: Map[String, Valve]): Int = {
    val allPathSizes      = computeDirectPaths(valves)
    val filteredPathSizes = allPathSizes.view.filterKeys(_._1 != "AA").toMap

    val allRemainings = filteredPathSizes.keySet.flatMap(pair => Set(pair._1, pair._2))
    val initSteps     = doStep(valves, allPathSizes, Step(0, 0, 0, Seq("AA"), allRemainings))

    val results = doSteps(valves, filteredPathSizes, initSteps, Set())
    val result  = results.maxBy(_.totalFlowSum)
    result.totalFlowSum
  }

  @tailrec
  private def doSteps(valves: Map[String, Valve], pathSizes: Map[(String, String), Int], steps: Set[Step], stepsDone: Set[Step])
      : Set[Step] =
    if (steps.isEmpty)
      stepsDone
    else {
      val currentStep                  = steps.head
      val nextSteps                    = doStep(valves, pathSizes, currentStep)
      val (doneSteps, inProgressSteps) = nextSteps.partition(_.minutesSpent == MaxMinutes)
      doSteps(valves, pathSizes, steps.tail ++ inProgressSteps, stepsDone ++ doneSteps)
    }

  private def doStep(valves: Map[String, Valve], pathSizes: Map[(String, String), Int], currentStep: Step): Set[Step] =
    if (currentStep.remaining.isEmpty)
      Set(currentStep.waitUntilEnd)
    else {
      val currentValve = currentStep.currentPath.head
      val nextSteps = currentStep.remaining.map { otherValve =>
        val pathSize         = pathSizes.getOrElse((currentValve, otherValve), pathSizes((otherValve, currentValve)))
        val flow             = valves(otherValve).flowRate
        val remainingMinutes = MaxMinutes - currentStep.minutesSpent
        if (remainingMinutes - pathSize - 1 < 0)
          currentStep.waitUntilEnd
        else
          currentStep.move(otherValve, pathSize).openValve(flow)
      }
      nextSteps
    }

}
