import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
  
  // Exercise 7.3: fix the implementation of map2 so that it respects the contract of timeouts on Future.
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      
      new Future[C] {
        def isDone: Boolean = af.isDone && bf.isDone
        def isCancelled: Boolean = af.isCancelled || bf.isCancelled
        def cancel(evenIfRunning: Boolean): Boolean = {
          af.cancel(evenIfRunning)
          bf.cancel(evenIfRunning)
        }
        def get(): C = f(af.get, bf.get)
        def get(timeout: Long, unit: TimeUnit): C = {
          val startedMs = System.currentTimeMillis
          val a = af.get(timeout, unit)
          val passedMs = System.currentTimeMillis - startedMs

          val b = bf.get(unit.toMillis(timeout) - passedMs, TimeUnit.MILLISECONDS)
          
          f(a, b)
        }
      }
    }

  // Execise 7.4: using lazyUnit, write a function to convert any function A => B to one that evaluates its
  // result asynchronously.
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))
  
  // Exercise 7.5: write the function sequence.
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A])) { (p, par) =>
      map2(p, par)(_ :: _)
    }

  // Exercise 7.6: implement parFilter, which filters elements of a list in parallel
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork {
      val ps = as.map(asyncF((a: A) => if(f(a)) List(a) else List()))
      map(sequence(ps))(_.flatten)
    }

  def fork[A](a: => Par[A]): Par[A] = 
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = 
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
