package com.kensai.aoc.lib

class Graph[T] {

  private var adjacencyList: Map[T, Set[T]] = Map.empty

  def nodes: Iterable[T] =
    adjacencyList.keys

  def addNode(node: T): Unit = {
    if (!adjacencyList.contains(node)) {
      adjacencyList += (node -> Set.empty)
    }
  }

  def addEdge(node1: T, node2: T): Unit = {
    if (!adjacencyList.contains(node1))
      adjacencyList += (node1 -> Set())

    if (!adjacencyList.contains(node2))
      adjacencyList += (node2 -> Set())

    adjacencyList += (node1 -> (adjacencyList(node1) + node2))
    adjacencyList += (node2 -> (adjacencyList(node2) + node1))
  }

  def removeNode(node: T): Unit = {
    if (adjacencyList.contains(node)) {
      // Delete connexions
      adjacencyList(node).foreach { neighbor =>
        adjacencyList += (neighbor -> (adjacencyList(neighbor) - node))
      }

      // Delete node
      adjacencyList -= node
    }
  }

  def removeEdge(node1: T, node2: T): Unit = {
    if (adjacencyList.contains(node1) && adjacencyList.contains(node2)) {
      adjacencyList += (node1 -> (adjacencyList(node1) - node2))
      adjacencyList += (node2 -> (adjacencyList(node2) - node1))
    }
  }

  def neighbors(node: T): Set[T] = {
    adjacencyList.getOrElse(node, Set.empty)
  }

  def hasEdge(node1: T, node2: T): Boolean =
    adjacencyList.contains(node1) && adjacencyList(node1).contains(node2) ||
      (adjacencyList.contains(node2) && adjacencyList(node2).contains(node1))
}
