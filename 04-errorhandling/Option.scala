object errorhandling {

  sealed trait Option[+A] {
    // Exercise 4.1: implement basic functions on Option
    def map[B](f: A => B): Option[B] = this match {
      case Some(v) => Some(f(v))
      case _ => None
    }
    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(v) => v
      case _ => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = 
      map(Some.apply).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
      flatMap(v => if(f(v)) Some(v) else None)
      
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

    // Exercise 4.3: write a generic function map2 that combines two Option values using a binary function
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
      a.flatMap(va => b.map(vb => f(va, vb)))
    
    // Exercise 4.4: write a function that combines a list of Options into an Option of list
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldLeft(Some(List.empty[A]): Option[List[A]])((acc, o) => acc.flatMap(a => o.map(v => v +: a)))

    // Exercise 4.5: write a function traverse and implement sequence using it
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
      a.foldLeft(Some(List.empty[B]): Option[List[B]])((acc, a) =>
        for {
          t <- acc
          b <- f(a)
        } yield (b +: t)
      )

    def sequence2[A](a: List[Option[A]]): Option[List[A]] = 
      traverse(a)(identity)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  // Exercise 4.2: implement variance function in terms of flatMap
  def mean(xs: Seq[Double]): Option[Double] = 
    if(xs.nonEmpty) Some(xs.sum / xs.length)
    else None

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

}