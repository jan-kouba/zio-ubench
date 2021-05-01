package dev.koubic.ubench

import dev.koubic.ubench.Benchmark.{Result, StepFunc}
import zio._
import zio.clock._
import zio.duration._

sealed trait Benchmark[-R, -I, +O] { self =>
  def step: StepFunc[R, I, O]

  def map[O2](func: O => O2): Benchmark[R, I, O2] =
    mapM { o => ZIO.succeed(func(o)) }

  def mapM[R1 <: R, O2](func: O => URIO[R1, O2]): Benchmark[R1, I, O2] =
    Benchmark {
      def loop(stepFunc: StepFunc[R, I, O]): StepFunc[R1, I, O2] = { (dur, i) =>
        for {
          r1 <- stepFunc(dur, i)
          r <-
            r1 match {
              case Result.HasOutput(out, nextFunc) =>
                func(out).map { r2 =>
                  Result.HasOutput(r2, loop(nextFunc))
                }
              case Result.NeedsMore(nextFunc) =>
                ZIO.succeed(Result.NeedsMore(loop(nextFunc)))
            }
        } yield r
      }

      loop(this.step)
    }

  def tap[R1 <: R](f: O => URIO[R1, Any]): Benchmark[R1, I, O] =
    mapM(o => f(o) as o)

  def &&[R1 <: R, I1 <: I, O1](that: Benchmark[R1, I1, O1]): Benchmark[R1, I1, (O, O1)] =
    new Benchmark[R1, I1, (O, O1)] {
      def step: StepFunc[R1, I1, (O, O1)] = {
        def loop(
          thisStep: StepFunc[R, I, O],
          thatStep: StepFunc[R1, I1, O1]
        ): StepFunc[R1, I1, (O, O1)] = { (dur, i) =>
          for {
            r1 <- thisStep(dur, i)
            r2 <- thatStep(dur, i)
          } yield {
            val nextStep = loop(r1.nextStep, r2.nextStep)

            (r1, r2) match {
              case (Result.HasOutput(out1, nextStep1), Result.HasOutput(out2, nextStep2)) =>
                Result.HasOutput((out1, out2), nextStep)

              case (_, _) =>
                Result.NeedsMore(nextStep)
            }
          }
        }

        loop(self.step, that.step)
      }
    }

  def <&[R1 <: R, I1 <: I](that: Benchmark[R1, I1, Any]) =
    (this && that).map(_._1)

  def &>[R1 <: R, I1 <: I, O1](that: Benchmark[R1, I1, O1]) =
    (this && that).map(_._2)

  def ||[R1 <: R, I1 <: I, O1 >: O](that: Benchmark[R1, I1, O1]): Benchmark[R1, I1, O1] =
    or(that)

  def or[R1 <: R, I1 <: I, O1 >: O](that: Benchmark[R1, I1, O1]): Benchmark[R1, I1, O1] =
    orEither(that).map(_.merge)

  def orEither[R1 <: R, I1 <: I, O1](
    that: Benchmark[R1, I1, O1]
  ): Benchmark[R1, I1, Either[O, O1]] =
    Benchmark {
      def loop(
        thisStep: StepFunc[R, I, O],
        thatStep: StepFunc[R1, I1, O1]
      ): StepFunc[R1, I1, Either[O, O1]] = { (dur, i) =>
        for {
          thisR <- thisStep(dur, i)
          thatR <- thatStep(dur, i)
        } yield {
          val nextStep = loop(thisR.nextStep, thatR.nextStep)

          thisR match {
            case Result.HasOutput(thisV, _) =>
              Result.HasOutput(
                Left(thisV),
                nextStep
              )

            case Result.NeedsMore(_) =>
              thatR match {
                case Result.HasOutput(thatV, _) =>
                  Result.HasOutput(
                    Right(thatV),
                    nextStep
                  )
                case Result.NeedsMore(_) =>
                  Result.NeedsMore(nextStep)
              }
          }
        }
      }

      loop(self.step, that.step)
    }

  def >>>[R1 <: R, O1](that: Benchmark[R1, O, O1]): Benchmark[R1, I, O1] =
    Benchmark {
      def loop(thisStep: StepFunc[R, I, O], thatStep: StepFunc[R1, O, O1]): StepFunc[R1, I, O1] = {
        (dur, i) =>
          for {
            r1 <- thisStep(dur, i)
            r <-
              r1 match {
                case Result.HasOutput(out, _) =>
                  thatStep(dur, out).map { r2 =>
                    r2.withNextStep(loop(r1.nextStep, r2.nextStep)): Result[R1, I, O1]
                  }

                case Result.NeedsMore(thisNextStep) =>
                  ZIO.succeed(
                    Result.NeedsMore(
                      loop(thisNextStep, thatStep)
                    ): Result[R1, I, O1]
                  )
              }
          } yield r
      }

      loop(self.step, that.step)
    }

  def untilOutput(func: O => Boolean): Benchmark[R, I, O] =
    self.until[I] { (_, o) => func(o) }

  def until[I1 <: I](f: (I1, O) => Boolean): Benchmark[R, I1, O] =
    untilM[R, I1] { (i, o) => ZIO.succeed(f(i, o)) }

  def untilM[R1 <: R, I1 <: I](func: (I1, O) => URIO[R1, Boolean]): Benchmark[R1, I1, O] =
    Benchmark {
      def loop(thisStep: StepFunc[R, I, O]): StepFunc[R1, I1, O] = { (dur, i) =>
        thisStep(dur, i).flatMap {
          case Result.NeedsMore(nextFunc) =>
            ZIO.succeed(Result.NeedsMore(loop(nextFunc)))
          case Result.HasOutput(out, nextFunc) =>
            func(i, out).map { done =>
              if (done) Result.HasOutput(out, nextFunc)
              else Result.NeedsMore(loop(nextFunc))
            }
        }
      }

      loop(self.step)
    }

  /** Runs until a local minimum is not improved for n steps.
    */
  def untilLocalMinimum[O1 >: O](n: Int)(implicit ord: Ordering[O1]): Benchmark[R, I, O] = {
    val min =
      Benchmark.unfoldInput[Option[(O, Int)], O](None) {
        case (Some((min, itemsSinceMin)), i) =>
          if (ord.lt(i, min)) Some(i -> 0)
          else Some(min -> (itemsSinceMin + 1))

        case (None, i) =>
          Some(i -> 0)
      }

    val stopper =
      min
        .map(_.get)
        .untilOutput { case (_, since) => since > n }

    (this >>> stopper).map(_._1)
  }
}

object Benchmark {
  type StepFunc[-R, -I, +O] = (Duration, I) => ZIO[R, Nothing, Result[R, I, O]]

  sealed trait Result[-R, -I, +O] {
    def withNextStep[R1, I1, O1 >: O](nextStep: StepFunc[R1, I1, O1]): Result[R1, I1, O1]

    val nextStep: StepFunc[R, I, O]
  }

  object Result {
    final case class NeedsMore[-R, -I, +O](
      nextStep: StepFunc[R, I, O]
    ) extends Result[R, I, O] {
      def withNextStep[R1, I1, O1 >: O](nextStep: StepFunc[R1, I1, O1]): Result[R1, I1, O1] =
        copy(nextStep = nextStep)
    }

    final case class HasOutput[-R, -I, +O](
      value: O,
      nextStep: StepFunc[R, I, O]
    ) extends Result[R, I, O] {
      def withNextStep[R1, I1, O1 >: O](nextStep: StepFunc[R1, I1, O1]): Result[R1, I1, O1] =
        copy(nextStep = nextStep)
    }
  }

  def apply[R, I, O](stepFunc: StepFunc[R, I, O]): Benchmark[R, I, O] =
    new Benchmark[R, I, O] {
      val step: StepFunc[R, I, O] = stepFunc
    }

  def fromFunc[A, B](f: A => B): Benchmark[Any, A, B] = apply {
    def loop: StepFunc[Any, A, B] = { (_, i) =>
      ZIO.succeed(
        Result.HasOutput(
          f(i),
          loop
        )
      )
    }
    loop
  }

  def id[I]: Benchmark[Any, I, (Duration, I)] =
    apply {
      def loop: StepFunc[Any, I, (Duration, I)] = { (dur, i) =>
        ZIO.succeed(
          Result.HasOutput(
            (dur, i),
            loop
          )
        )
      }

      loop
    }

  def unfold[S, I](init: S)(next: (S, I, Duration) => S): Benchmark[Any, I, S] =
    unfoldM(init) { (s, i, dur) => ZIO.succeed(next(s, i, dur)) }

  def unfoldDur[S](init: S)(next: (S, Duration) => S): Benchmark[Any, Any, S] =
    unfold[S, Any](init) { (s, _, d) => next(s, d) }

  def unfoldInput[S, I](init: S)(next: (S, I) => S): Benchmark[Any, I, S] =
    unfold[S, I](init) { (s, i, _) => next(s, i) }

  def unfoldM[R, S, I](init: S)(next: (S, I, Duration) => URIO[R, S]): Benchmark[R, I, S] = {
    apply {
      def loop(state: S): StepFunc[R, I, S] = { (dur, i) =>
        next(state, i, dur).map { nextState =>
          Result.HasOutput(
            nextState,
            loop(nextState)
          )
        }
      }

      loop(init)
    }
  }

  def keepLastN(n: Int): Benchmark[Any, Any, Vector[Duration]] =
    unfoldDur[Vector[Duration]](Vector()) { (s, dur) => (dur +: s).take(n) }

  def startTime: Benchmark[Clock, Any, Duration] =
    unfoldM[clock.Clock, Option[Duration], Any](None) {
      case (s @ Some(_), _, _) => ZIO.succeed(s)
      case (None, _, dur) => clock.nanoTime.map(nt => Some(nt.nanos minus dur))
    }
      .map(_.get)

  def measurementDuration: Benchmark[Any, Any, Duration] =
    id[Any].map(_._1)

  def totalDuration: Benchmark[Clock, Any, Duration] =
    startTime.mapM { st =>
      clock.nanoTime.map(nt => nt.nanos minus st)
    }

  def minRunTime(minDuration: Duration): Benchmark[Clock, Any, Duration] =
    totalDuration.untilOutput(_ >= minDuration)

  def measurementCount: Benchmark[Any, Any, Int] = unfoldDur(0) { (s, _) => s + 1 }

  def minMeasurements(n: Int): Benchmark[Any, Any, Int] =
    measurementCount.untilOutput(_ >= n)

  def minMeasurementDuration: Benchmark[Any, Any, Duration] =
    unfoldDur[Option[Duration]](None) {
      case (None, d) => Some(d)
      case (Some(m), d) => Some(m min d)
    }
      .map(_.get)

  def mseNs: Benchmark[Any, Vector[Duration], Double] =
    fromFunc { (measurements: Vector[Duration]) =>
      val measurementsNs = measurements.map(_.toNanos.toDouble)
      val avg = (measurementsNs.sum / measurements.size).round

      measurementsNs.map { m => (m - avg) * (m - avg) }.sum / measurementsNs.size
    }

  def avg: Benchmark[Any, Vector[Duration], Duration] =
    fromFunc { (s: Vector[Duration]) =>
      val sNs = s.map(_.toNanos.toDouble)
      (sNs.sum / sNs.size).round.nanos
    }

  def untilLowRmse(maxRmse: Duration, keepLastItems: Int): Benchmark[Any, Any, Duration] = {
    val maxMse = maxRmse.toNanos * maxRmse.toNanos
    keepLastN(keepLastItems) >>> (mseNs.untilOutput(_ < maxMse) &> avg)
  }

  def untilLocalMin(window: Int): Benchmark[Any, Any, Duration] =
    measurementDuration.untilLocalMinimum(window)

  def execute[R <: Clock, I, E, O](
    strategy: Benchmark[R, Any, O],
    io: ZIO[R, E, Any]
  ): ZIO[R, E, O] = {
    def loop(step: StepFunc[R, Any, O]): ZIO[R, E, O] = {
      for {
        e <- io.timed
        (dur, _) = e
        o <- step(dur, ())
        r <- o match {
          case Result.HasOutput(v, _) => ZIO.succeed(v)
          case Result.NeedsMore(nextStep) => loop(nextStep)
        }
      } yield r
    }

    loop(strategy.step)
  }
}
