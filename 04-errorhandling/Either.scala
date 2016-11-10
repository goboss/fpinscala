object errorhandling {

  sealed trait Either[+E, +A] {
  
    // Exercise 4.6: implement map, flatMap, orElse and map2 on Either that operate on the Right value
    def map[B](f: A => B): Either[E, B] = 
      flatMap(a => Right(f(a)))

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case _ => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      flatMap(aa => b.map(bb => f(aa, bb)))
  }

  object Either {
    // Exercise 4.7: implement sequence and traverse for Either. These should return the first error
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
      traverse(es)(identity)

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as.foldLeft(Right(List.empty): Either[E, List[B]])((acc, a) =>
        for {
          t <- acc
          b <- f(a)
        } yield (b +: t)
      )
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

}
