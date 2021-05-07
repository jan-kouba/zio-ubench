package zio.ubench

import zio.ubench.Benchmark.{Result, StepFunc}
import zio._
import zio.clock._
import zio.duration._

/** A description of how to benchmark something.
  * Measures the duration of the benchmarked thing and calls the `step` function with the duration
  * and some input as parameters.
  * The `step` function returns some output value or indicates that one more measurement is needed.
  * The output should keep improving with more measurements.
  *
  * @tparam R The environment the benchmark needs.
  * @tparam I Input of the benchmark.
  * @tparam O Output of the benchmark.
  */
sealed trait Benchmark[-R, -I, +O] { self =>
  protected def step: StepFunc[R, I, O]

  /** Transforms output using the function `func`.
    */
  def map[O2](func: O => O2): Benchmark[R, I, O2] =
    mapM { o => ZIO.succeed(func(o)) }

  /** Transforms the output using function `func` returning a ZIO effect
    */
  def mapM[R1 <: R, O2](func: O => URIO[R1, O2]): Benchmark[R1, I, O2] =
    Benchmark {
      def loop(stepFunc: StepFunc[R, I, O]): StepFunc[R1, I, O2] = { (dur, i) =>
        stepFunc(dur, i).flatMap {
          case Result.HasOutput(out, nextFunc, minI) =>
            func(out).map { r2 =>
              Result.HasOutput(r2, loop(nextFunc), minI)
            }
          case Result.NeedsMore(nextFunc, minI) =>
            ZIO.succeed(Result.NeedsMore(loop(nextFunc), minI))
        }
      }

      loop(this.step)
    }

  /** For every output runs the function `f` for it's side effects
    */
  def tap[R1 <: R](f: O => URIO[R1, Any]): Benchmark[R1, I, O] =
    mapM(o => f(o) as o)

  /** Creates a new benchmark that is done when both this and `that` benchmarks are done.
    */
  def &&[R1 <: R, I1 <: I, O1](that: Benchmark[R1, I1, O1]): Benchmark[R1, I1, (O, O1)] =
    Benchmark {
      def loop(
        thisStep: StepFunc[R, I, O],
        thatStep: StepFunc[R1, I1, O1]
      ): StepFunc[R1, I1, (O, O1)] = { (dur, i) =>
        for {
          r1 <- thisStep(dur, i)
          r2 <- thatStep(dur, i)
        } yield {
          val nextStep = loop(r1.nextStep, r2.nextStep)
          val minI = r1.nextMinInvocationCount max r2.nextMinInvocationCount

          (r1, r2) match {
            case (Result.HasOutput(out1, _, _), Result.HasOutput(out2, _, _)) =>
              Result.HasOutput((out1, out2), nextStep, minI)

            case (_, _) =>
              Result.NeedsMore(nextStep, minI)
          }
        }
      }

      loop(self.step, that.step)
    }

  /** Like `&&` but keeps only the output of `this`.
    */
  def <&[R1 <: R, I1 <: I](that: Benchmark[R1, I1, Any]) =
    (this && that).map(_._1)

  /** Like `&&` but keeps only the output of `that`.
    */
  def &>[R1 <: R, I1 <: I, O1](that: Benchmark[R1, I1, O1]) =
    (this && that).map(_._2)

  /** Alias for `or`.
    */
  def ||[R1 <: R, I1 <: I, O1 >: O](that: Benchmark[R1, I1, O1]): Benchmark[R1, I1, O1] =
    or(that)

  /** Like `orEither` but merges the outputs into a single value.
    */
  def or[R1 <: R, I1 <: I, O1 >: O](that: Benchmark[R1, I1, O1]): Benchmark[R1, I1, O1] =
    orEither(that).map(_.merge)

  /** Creates a new benchmark that is done when `this` or `that` is done.
    * If both benchmarks are done, outputs the output of `this` and drops the output of `that`.
    */
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
          val nextMinI = thisR.nextMinInvocationCount max thatR.nextMinInvocationCount

          thisR match {
            case Result.HasOutput(thisV, _, _) =>
              Result.HasOutput(
                Left(thisV),
                nextStep,
                nextMinI
              )

            case Result.NeedsMore(_, _) =>
              thatR match {
                case Result.HasOutput(thatV, _, _) =>
                  Result.HasOutput(
                    Right(thatV),
                    nextStep,
                    nextMinI
                  )
                case Result.NeedsMore(_, _) =>
                  Result.NeedsMore(nextStep, nextMinI)
              }
          }
        }
      }

      loop(self.step, that.step)
    }

  /** Sends the output of `this` to the input of `that`.
    * The returned benchmark is done when both `this` and `that` are done.
    *
    * Keep in mind that values are fed to `that` only after `this` starts to produce values.
    */
  def >>>[R1 <: R, O1](that: Benchmark[R1, O, O1]): Benchmark[R1, I, O1] =
    Benchmark {
      def loop(thisStep: StepFunc[R, I, O], thatStep: StepFunc[R1, O, O1]): StepFunc[R1, I, O1] = {
        (dur, i) =>
          thisStep(dur, i).flatMap {
            case Result.HasOutput(out, nextStep1, minI1) =>
              thatStep(dur, out).map { r2 =>
                r2.withNextStep(loop(nextStep1, r2.nextStep), minI1 max r2.nextMinInvocationCount)
              }

            case Result.NeedsMore(thisNextStep, minI) =>
              ZIO.succeed(
                Result.NeedsMore(
                  loop(thisNextStep, thatStep),
                  minI
                )
              )
          }
      }

      loop(self.step, that.step)
    }

  /** Runs `this` benchmark to completion, then uses the output value to construct a new benchmark
    * and runs that to completion.
    */
  def andThenM[R1 <: R, I1 <: I, O2](
    that: O => ZIO[R1, Nothing, Benchmark[R1, I1, O2]]
  ): Benchmark[R1, I1, (O, O2)] = {
    Benchmark {
      def loop(step: StepFunc[R, I, O]): StepFunc[R1, I1, (O, O2)] = { (dur, i) =>
        for {
          rThis <- step(dur, i)
          r <-
            rThis match {
              case Result.NeedsMore(nextStep, _) =>
                ZIO.succeed(loop(nextStep))
              case Result.HasOutput(out, nextStep, _) =>
                that(out).map { thatB =>
                  loopThat(nextStep, thatB.step)
                }
            }
        } yield Result.NeedsMore(r, rThis.nextMinInvocationCount)
      }

      def loopThat(
        thisStep: StepFunc[R, I, O],
        thatStep: StepFunc[R1, I1, O2]
      ): StepFunc[R1, I1, (O, O2)] = { (dur, i) =>
        for {
          r1 <- thisStep(dur, i)
          r2 <- thatStep(dur, i)
        } yield {
          val nextStep = loopThat(r1.nextStep, r2.nextStep)
          val nextMinI = r1.nextMinInvocationCount max r2.nextMinInvocationCount

          (r1, r2) match {
            case (Result.HasOutput(out1, _, _), Result.HasOutput(out2, _, _)) =>
              Result.HasOutput((out1, out2), nextStep, nextMinI)

            case _ =>
              Result.NeedsMore(nextStep, nextMinI)
          }
        }
      }

      loop(step)
    }
  }

  /** See `andThenM`.
    */
  def andThen[R1 <: R, I1 <: I, O2](that: O => Benchmark[R1, I1, O2]): Benchmark[R1, I1, (O, O2)] =
    andThenM { o => ZIO.succeed(that(o)) }

  /** Like until but works only on the output of the benchmark.
    */
  def untilOutput(func: O => Boolean): Benchmark[R, I, O] =
    self.until[I] { (_, o) => func(o) }

  /** A benchmark that keeps requiring new values until `this` is done and `pred` returns `true`.
    */
  def until[I1 <: I](pred: (I1, O) => Boolean): Benchmark[R, I1, O] =
    untilM[R, I1] { (i, o) => ZIO.succeed(pred(i, o)) }

  /** A benchmark that keeps requiring new values until `this` is done and `pred` returns `true`.
    */
  def untilM[R1 <: R, I1 <: I](pred: (I1, O) => URIO[R1, Boolean]): Benchmark[R1, I1, O] =
    Benchmark {
      def loop(thisStep: StepFunc[R, I, O]): StepFunc[R1, I1, O] = { (dur, i) =>
        thisStep(dur, i).flatMap {
          case Result.NeedsMore(nextFunc, minI) =>
            ZIO.succeed(Result.NeedsMore(loop(nextFunc), minI))
          case Result.HasOutput(out, nextFunc, minI) =>
            pred(i, out).map { done =>
              if (done) Result.HasOutput(out, nextFunc, minI)
              else Result.NeedsMore(loop(nextFunc), minI)
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

  /** The result of one step of a benchmark.
    */
  sealed trait Result[-R, -I, +O] {
    def withNextStep[R1, I1, O1 >: O](
      nextStep: StepFunc[R1, I1, O1],
      nextMinInvocationCount: Int
    ): Result[R1, I1, O1]

    val nextStep: StepFunc[R, I, O]
    val nextMinInvocationCount: Int
  }

  object Result {

    /** Indicates that the benchmark is not able to produce a value yet and that it needs more input.
      * `nextStep` is the function to call for the next step,
      * `nextMinInvocationCount` is at least how many times the measured IO should be called the next time.
      */
    final case class NeedsMore[-R, -I, +O](
      nextStep: StepFunc[R, I, O],
      nextMinInvocationCount: Int
    ) extends Result[R, I, O] {
      def withNextStep[R1, I1, O1 >: O](
        nextStep: StepFunc[R1, I1, O1],
        nextMinInvocationCount: Int
      ): Result[R1, I1, O1] =
        NeedsMore(
          nextStep,
          nextMinInvocationCount
        )
    }

    /** Indicates that the benchmark was fed enough values so that it can produce the `output`.
      * `nextStep` is a function for next steps of the benchmark,
      * `nextMinInvocationCount` is at least how many times the measured IO should be called the next time.
      */
    final case class HasOutput[-R, -I, +O](
      value: O,
      nextStep: StepFunc[R, I, O],
      nextMinInvocationCount: Int
    ) extends Result[R, I, O] {
      def withNextStep[R1, I1, O1 >: O](
        nextStep: StepFunc[R1, I1, O1],
        nextMinInvocationCount: Int
      ): Result[R1, I1, O1] =
        HasOutput(
          value,
          nextStep,
          nextMinInvocationCount
        )
    }
  }

  /** Creates a benchmark from the step function.
    */
  def apply[R, I, O](stepFunc: StepFunc[R, I, O]): Benchmark[R, I, O] =
    new Benchmark[R, I, O] {
      val step: StepFunc[R, I, O] = stepFunc
    }

  /** Creates a benchmark that just transforms its input and that is immediately done.
    */
  def fromFunc[A, B](f: A => B): Benchmark[Any, A, B] =
    apply {
      def loop: StepFunc[Any, A, B] = { (_, i) =>
        ZIO.succeed(
          Result.HasOutput(
            f(i),
            loop,
            1
          )
        )
      }
      loop
    }

  /** Alias for `minMeasurementDuration`.
    * Use this if you want to end the benchmark immediately and produce "something meaningful".
    */
  def nop =
    minMeasurementDuration

  /** A benchmark that just forwards its input.
    */
  def id[I]: Benchmark[Any, I, (Duration, I)] =
    apply {
      lazy val step: StepFunc[Any, I, (Duration, I)] =
        (dur, i) => ZIO.succeed(Result.HasOutput((dur, i), step, 1))

      step
    }

  /** See `constM()`.
    */
  def const[O](value: O): Benchmark[Any, Any, O] =
    constM(ZIO.succeed(value))

  /** Output a value produced by running the effect `value`.
    * The `value` effect is run only once.
    */
  def constM[R, O](value: ZIO[R, Nothing, O]): Benchmark[R, Any, O] =
    apply {
      def loopTail(v: O): StepFunc[R, Any, O] = {
        lazy val l: StepFunc[R, Any, O] = (_, _) =>
          ZIO.succeed(
            Result.HasOutput(v, l, 1)
          )
        l
      }

      { (_: Duration, _: Any) =>
        value.map { v =>
          Result.HasOutput(v, loopTail(v), 1)
        }
      }
    }

  /** Like `unfoldM`.
    */
  def unfold[S, I](init: S)(next: (S, I, Duration) => S): Benchmark[Any, I, S] =
    unfoldM(init) { (s, i, dur) => ZIO.succeed(next(s, i, dur)) }

  /** Like `unfold` but uses only the durations of the measurements.
    */
  def unfoldDur[S](init: S)(next: (S, Duration) => S): Benchmark[Any, Any, S] =
    unfold[S, Any](init) { (s, _, d) => next(s, d) }

  /** Like `unfold` but uses only the input.
    */
  def unfoldInput[S, I](init: S)(next: (S, I) => S): Benchmark[Any, I, S] =
    unfold[S, I](init) { (s, i, _) => next(s, i) }

  /** Unfolds a value `init` using the input and durations of the measurements.
    */
  def unfoldM[R, S, I](init: S)(next: (S, I, Duration) => URIO[R, S]): Benchmark[R, I, S] = {
    apply {
      def loop(state: S): StepFunc[R, I, S] = { (dur, i) =>
        next(state, i, dur).map { nextState =>
          Result.HasOutput(
            nextState,
            loop(nextState),
            1
          )
        }
      }

      loop(init)
    }
  }

  /** Collects durations of last N measurements.
    */
  def collectLastNDurations(n: Int): Benchmark[Any, Any, Vector[Duration]] =
    unfoldDur[Vector[Duration]](Vector()) { (s, dur) => (dur +: s).take(n) }
      .untilOutput(_.size >= n)

  /** Outputs when benchmark started.
    */
  def startTime: Benchmark[Clock, Any, Duration] =
    unfoldM[clock.Clock, Option[Duration], Any](None) {
      case (s @ Some(_), _, _) => ZIO.succeed(s)
      case (None, _, dur) => clock.nanoTime.map(nt => Some(nt.nanos minus dur))
    }
      .map(_.get)

  /** Outputs duration of each measurement.
    */
  def measurementDuration: Benchmark[Any, Any, Duration] =
    id[Any].map(_._1)

  /** Outputs total duration of the benchmark so far.
    */
  def totalDuration: Benchmark[Clock, Any, Duration] =
    startTime.mapM { st =>
      clock.nanoTime.map(nt => nt.nanos minus st)
    }

  /** A benchmark that needs to be run at least `minDuration`.
    * Outputs benchmark total duration.
    */
  def untilTotalDuration(minDuration: Duration): Benchmark[Clock, Any, Duration] =
    totalDuration.untilOutput(_ >= minDuration)

  /** Counts number of measurements.
    */
  def measurementCount: Benchmark[Any, Any, Int] = unfoldDur(0) { (s, _) => s + 1 }

  /** A benchmark that needs at least `n` measurements.
    * Outputs the count of measurements so far.
    */
  def minMeasurements(n: Int): Benchmark[Any, Any, Int] =
    measurementCount.untilOutput(_ >= n)

  /** Outputs the minimal duration of all measurements.
    */
  def minMeasurementDuration: Benchmark[Any, Any, Duration] =
    unfoldDur(Duration.Infinity)(_ min _)

  /** Outputs the normalized root-mean-square error of the durations received on input.
    *  NRMSE is calculated as RMSE / AVG
    *
    * Typical usage is `collectLastNDurations(20) >>> nrmse`.
    */
  def nrmse: Benchmark[Any, Vector[Duration], Double] =
    fromFunc { (measurements: Vector[Duration]) =>
      val measurementsNs = measurements.map(_.toNanos.toDouble)
      val avg = (measurementsNs.sum / measurements.size).round

      val mse = measurementsNs.map { m => (m - avg) * (m - avg) }.sum / measurementsNs.size
      val rmse = math.sqrt(mse)
      rmse / avg
    }

  /** Outputs the average of the values received on input.
    *
    * Typical usage is `collectLastNDurations(20) >>> avgDuration`.
    */
  def avgDuration: Benchmark[Any, Vector[Duration], Duration] =
    fromFunc { (s: Vector[Duration]) =>
      val sNs = s.map(_.toNanos.toDouble)

      (sNs.sum / sNs.size).round.nanos
    }

  /** Runs until the durations of last `keepLastItems` measurements
    * have normalized root-mean-square error lower than `maxRmse`.
    * The benchmark outputs the average of the `keepLastItems` measurements.
    */
  def untilLowNrmse(maxNrmse: Double, keepLastItems: Int): Benchmark[Any, Any, Duration] = {
    collectLastNDurations(keepLastItems) >>> (nrmse.untilOutput(_ < maxNrmse) &> avgDuration)
  }

  /** Runs until the minimum of durations of measurements was not improved for `window` steps.
    * Returns the local minimum.
    */
  def untilLocalDurationMin(window: Int): Benchmark[Any, Any, Duration] =
    measurementDuration.untilLocalMinimum(window)

  /** Benchmarks the effect `io` using `bench`.
    */
  def execute[R <: Clock, I, E, O](
    bench: Benchmark[R, Any, O],
    io: ZIO[R, E, Any]
  ): ZIO[R, E, O] = {
    def measureN(io: ZIO[R, E, Any], n: Int) = {
      require(n > 0)
      io.repeatN(n - 1)
        .timed
        .map(_._1)
    }

    def loop(step: StepFunc[R, Any, O], nextInvocationCount: Int): ZIO[R, E, O] = {
      for {
        ioDur <- measureN(io, nextInvocationCount)
        nopDur <- measureN(ZIO.unit, nextInvocationCount)
        dur = (ioDur minus nopDur) dividedBy nextInvocationCount
        r <- step(dur, ()).flatMap {
          case Result.HasOutput(v, _, _) => ZIO.succeed(v)
          case Result.NeedsMore(nextStep, minI) => loop(nextStep, minI)
        }
      } yield r
    }

    loop(bench.step, 1)
  }
}
