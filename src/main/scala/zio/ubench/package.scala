package zio

import zio.clock.Clock

package object ubench {
  implicit class ZIOSyntax[R, E, A](private val io: ZIO[R, E, A]) extends AnyVal {

    /** Benchmarks this effect using benchmark `bench`.
      *
      * {{{
      * // Run the default benchmark
      * val r1 = io.benchmark()
      *
      * // Run benchmark for 1 second and then take the duration of the fastest run.
      * val r2 = io.benchmark(
      *   Benchmark.untilTotalDuration(1.second) &> Benchmark.minMeasurementDuration
      * )
      *
      * // Preheat the io for 100 milliseconds and then run the benchmark until minimal duration
      * // is not improved for 30 steps or until the normalized root-mean-square error
      * // of last 10 measurements drops below 0.05.
      * // Return the minimal measurement duration.
      * val r3 = io.benchmark(
      *   (
      *     (Benchmark.untilLocalDurationMin(30) || Benchmark
      *       .untilLowNrmse(0.05, 10)) &> Benchmark.minMeasurementDuration
      *   ).withPreheat(Benchmark.untilTotalDuration(100.millis), 5.millis)
      * )
      * }}}
      */
    def benchmark[R1 <: R, O](
      bench: Benchmark[R1, Any, O] = Benchmark.default
    ): ZIO[Clock with R1, E, O] =
      Benchmark.execute(bench, io)
  }
}
