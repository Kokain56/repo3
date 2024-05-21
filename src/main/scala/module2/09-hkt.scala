package module2


object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}


  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit bind: Bindable[F]): F[(A, B)] =
    bind.flatMap(fa)(a => bind.map(fb)((a, _)))


  trait Bindable[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  implicit val optBindable: Bindable[Option] =
    new Bindable[Option] {

      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    }

  implicit val listBindable: Bindable[List] =
    new Bindable[List] {

      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }






  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  //lazy val r3 = println(tupleBindable(optBindable(optA), optBindable(optB)))
  //lazy val r4 = println(tupleBindable(listBindable(list1), listBindable(list2)))

  lazy val r1 = println(tuplef(optA, optB))
  lazy val r2 = println(tuplef(list1, list2))

}