package lib

import scala.collection.mutable

object Graphs {

  object AStar {
    def apply[A](start: A, goal: A)(cf: (A, A) => Int)(nf: A => List[A]): Map[A, Int] = {
      val open  = mutable.ListBuffer[(A, Int)]((start, 0))
      val costs = mutable.Map[A, Int](start -> 0)
      while (open.nonEmpty) {
        val current = open.minBy(_._2)
        open -= current
        if (current._1 == goal) open.clear
        else {
          val newCost   = costs(current._1) + 1
          val neighbors = nf(current._1)
          for (next <- neighbors) {
            if (!costs.contains(next) || newCost < costs(next)) {
              costs(next) = newCost
              val priority = newCost + cf(goal, next)
              open += ((next, priority))
            }
          }
        }
      }
      costs.toMap
    }
  }

}
