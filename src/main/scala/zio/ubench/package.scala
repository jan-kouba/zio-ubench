package zio

import zio.ubench.UBench.Defaults
import zio.clock.Clock
import zio.duration.Duration

package object ubench {
  implicit class ZIOSyntax[R, E, A](private val io: ZIO[R, E, A]) extends AnyVal {

    /** Benchmarks this effect.
      *
      * First preheats the effect by running the benchmark `preheatStrategy`.
      * Then uses the `benchmarkingStrategy` to benchmark the effect
      * `io.repeatN(approxRunDuration / minPreheatDuration - 1)`.
      *
      * Returns the estimated duration of a single run of the effect `io`.
      *
      *  {{{
      *  // Run with default parameters
      *  val r1 = io.benchmark()
      *
      *  // Run fully customized
      *  val r2 = io.benchmark(
      *    Benchmark.minRunTime(100.millis),
      *    1.milli,
      *    (
      *      Benchmark.untilLocalMin(30) ||
      *      Benchmark.untilLowRmse(5.millis, 30)
      *    ).map { case (r1, r2) => r1 min r2 }
      *  )
      *  }}}
      */
    def benchmark[R1 <: R with Clock](
      preheatStrategy: Benchmark[R1, Any, Any] = Defaults.Preheat,
      approxRunDuration: Duration = Defaults.ApproxRunDuration,
      benchmarkingStrategy: Benchmark[R1, Any, Duration] = Defaults.Measure
    ): ZIO[R1, E, Duration] =
      UBench.benchmark(io, preheatStrategy, approxRunDuration, benchmarkingStrategy)
  }
}
