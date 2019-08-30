import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object divide_futuristically {

  def main(args: Array[String]): Unit = {
    val M = 100 * 1000
    val N = 50
    Thread.sleep(3210) // let launcher settle down
    for (it <- 0 until 25) {
      val method = it % 5
      val start = System.currentTimeMillis()
      val result = divide(M, N, method)
      val elapsed = System.currentTimeMillis() - start
      assert(result == M / N)
      val methods = Array(
        "ordinary",
        "fast parallel",
        "nice parallel (sequence)",
        "nice parallel (foldLeft))",
        "nice parallel (reduceLeft)"
      )
      val name = methods(method)
      println(f"$name%30s: $elapsed ms")
    }
  }

  def is_multiple_of(m: Int, n: Int): Boolean = {
    val result = !(1 until n).map(_ + (m / n) * n).toSet.contains(m)
    assert(result == (m % n == 0)) // yes, a less crazy implementation exists
    result
  }

  def divide(m: Int, n: Int, method: Int): Int = {
    method match {
      case 0 =>
        (1 to m).count(is_multiple_of(_, n))
      case 1 =>
        (1 to m)
          .map { x =>
            Future { is_multiple_of(x, n) }
          }
          .count(Await.result(_, Duration.Inf))
      case 2 =>
        Await.result(divide_futuristically2(m, n), Duration.Inf)
      case 3 =>
        Await.result(divide_futuristically3(m, n), Duration.Inf)
      case 4 =>
        Await.result(divide_futuristically4(m, n), Duration.Inf)
    }
  }

  def divide_futuristically2(m: Int, n: Int): Future[Int] = {
    val futures = (1 to m).map { x =>
      Future { is_multiple_of(x, n) }
    }
    Future.sequence(futures).map(_.count(identity))
  }
  def divide_futuristically3(m: Int, n: Int): Future[Int] = {
    val futures = (1 to m).map { x =>
      Future { is_multiple_of(x, n) }
    }
    Future.foldLeft(futures)(0) { (count, flag) =>
      { if (flag) { count + 1 } else { count } }
    }
  }
  def divide_futuristically4(m: Int, n: Int): Future[Int] = {
    val futures = (1 to m).map { x =>
      Future { if (is_multiple_of(x, n)) 1 else 0 }
    }
    Future.reduceLeft(futures)(_ + _)
  }
}
