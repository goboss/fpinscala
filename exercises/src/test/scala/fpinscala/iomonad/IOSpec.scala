package fpinscala.iomonad

import java.nio.channels.AsynchronousFileChannel
import java.nio.file.{Paths, StandardOpenOption}
import java.util.concurrent.Executors

import fpinscala.parallelism.Nonblocking.Par
import org.scalatest.{FlatSpec, Matchers}

class IOSpec extends FlatSpec with Matchers {
  import fpinscala.iomonad.IO3._

  // Exercise 2

  "IO Monad" should "run trampolined Function0" in {
    val f: IO3.Free[Function0, Int] = IO3.Return(42)
    val free = List.fill(1000000)(f).foldLeft(f) { (f1, f2) =>
      f1.flatMap(_ => f2)
    }

    runTrampoline(free) shouldBe 42
  }

  // Exercise 3

  it should "run a Monad" in {
    val m = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)
      override def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = a.flatMap(f)
    }

    IO3.run[List, Int](IO3.Return(42))(m) shouldBe List(42)
    IO3.run[List, Int](IO3.Suspend(List(1, 2, 3)))(m) shouldBe List(1, 2, 3)
    IO3.run[List, Int](IO3.FlatMap(IO3.Suspend(List(1, 2, 3)), (i: Int) => Return(i + 1)))(m) shouldBe List(2, 3, 4)
  }

  // Exercise 5

  it should "read a file" in {
    val path = Paths.get(getClass.getClassLoader.getResource("data.txt").toURI)
    val ch = AsynchronousFileChannel.open(path, StandardOpenOption.READ)

    Par.run(Executors.newCachedThreadPool())(IO3.read(ch, 0, 255)).fold(
      err => fail(err),
      res => res should contain theSameElementsAs Array('a'.toInt, 'b'.toInt, 'c'.toInt)
    )
  }

}
