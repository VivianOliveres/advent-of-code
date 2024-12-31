package com.kensai.aoc.aoc2024

import com.kensai.aoc.lib.Graph

object Day23 {

  case class Triple(e1: String, e2: String, e3: String)
  object Triple {
    def apply(s1: String, s2: String, s3: String): Triple = {
      val xs = Seq(s1, s2, s3).sorted
      new Triple(xs.head, xs(1), xs(2))
    }
  }

  def parse(rows: Seq[String]): Graph[String] = {
    val graph = new Graph[String]()
    rows.foreach { row =>
      val splited = row.split("-")
      val first   = splited.head
      val second  = splited.tail.head
      graph.addEdge(first, second)
    }
    println(s"Graph of ${graph.nodes.size} nodes")
    graph
  }

  def extract(connections: Graph[String], startsWith: String): Seq[Triple] = {
    val keys = connections.nodes.filter(_.startsWith(startsWith))
    keys.flatMap { key =>
      val neighbors = connections.neighbors(key)
      neighbors
        .subsets(2)
        .map(t => (t.head, t.tail.head))
        .toSeq
        .filter { case (e1, e2) => connections.hasEdge(e1, e2) }
        .map { case (e1, e2) => Triple(key, e1, e2) }
    }.toSeq
  }

  def computePassword(connections: Graph[String]): String = {
    val results = scala.collection.mutable.ListBuffer.empty[Set[String]]
    bronKerbosch(Set(), connections.nodes.toSet, Set(), connections, results)
    val result = results.minBy(-_.size)
    result.toSeq.sorted.mkString(",")
  }

  // https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
  // Find all maximal cliques (ie set of nodes that are connected to all others).
  def bronKerbosch(
      acc: Set[String],
      todo: Set[String],
      excluded: Set[String],
      graph: Graph[String],
      cliques: scala.collection.mutable.ListBuffer[Set[String]]
    ): Unit =
    if (todo.isEmpty && excluded.isEmpty)
      cliques += acc
    else {
      var nextTodo     = todo
      var nextExcluded = excluded
      for (v <- todo) {
        bronKerbosch(
          acc + v,
          nextTodo.intersect(graph.neighbors(v)),
          nextExcluded.intersect(graph.neighbors(v)),
          graph,
          cliques
        )
        nextTodo -= v
        nextExcluded += v
      }
    }
}
