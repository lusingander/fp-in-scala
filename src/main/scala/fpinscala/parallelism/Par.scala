package fpinscala.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (s: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true
    def get(timeout: Long, unit: TimeUnit): A = get
    def isCancelled: Boolean = false
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def fork[A](a: => Par[A]): Par[A] =
    s => s.submit(new Callable[A] { def call = a(s).get })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // 7.1
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (s: ExecutorService) => {
      val fa = pa(s)
      val fb = pb(s)
      UnitFuture(f(fa.get, fb.get))
    }

  def map[A, B](p: Par[A])(f: A => B): Par[B] =
    map2(p, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((a, b) => map2(a, b)(_ :: _))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = l.map(asyncF(f))
    sequence(fbs)
  }

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val g: A => Par[Option[A]] = asyncF(a => if (f(a)) Some(a) else None)
    val z: List[Par[Option[A]]] = as.map(g)
    val x: Par[List[Option[A]]] = sequence(z)
    map(x)(_.filter(o => o.isDefined).map(o => o.get))
  }
  def parFilter_[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val z: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else Nil))
    val x: Par[List[List[A]]] = sequence(z)
    map(x)(_.flatten)
  }

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    s => fa(s)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    s => if (run(s)(cond).get) t(s) else f(s)

  // 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    s => choices(run(s)(n).get)(s)

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(if (_) 0 else 1))(List(t, f))

  // 7.12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    s => choices(run(s)(key).get)(s)

  // 7.13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    s => {
      val a = run(s)(pa).get
      run(s)(choices(a))
    }

  def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices(_))

  def choice3[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if (_) t else f)

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    chooser(pa)(choices)

  // 7.14
  def join[A](a: Par[Par[A]]): Par[A] =
    s => {
      val p = run(s)(a).get
      run(s)(p)
    }

  def flatMap2[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(choices))

  def join2[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }
  }
}
