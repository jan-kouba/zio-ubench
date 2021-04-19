package dev.koubic.ubench

import zio._
import zio.clock.Clock
import zio.duration._

object UBench {
  private case class Step(
    iterCount: Int,
    totalDuration: Duration
  )

  object Defaults {
    val Preheat: Benchmark[Clock, Any, Duration] = Benchmark.minRunTime(100.millis)
    val ApproxRunDuration: Duration = 5.millis
    val Measure: Benchmark[Any, Any, Duration] = Benchmark.untilLocalMin(20)
  }

  def benchmark[R <: Clock, E](
    io: ZIO[R, E, Any],
    preheatStrategy: Benchmark[R, Any, Any] = Defaults.Preheat,
    approxRunDuration: Duration = Defaults.ApproxRunDuration,
    benchmarkingStrategy: Benchmark[R, Any, Duration] = Defaults.Measure
  ): ZIO[R, E, Duration] = {
    for {
      preheatDur <- Benchmark.execute(
        preheatStrategy &> Benchmark.minMeasurementDuration,
        io
      )
      iterCount = (approxRunDuration.toNanos.toDouble / preheatDur.toNanos).round.toInt max 1
      nopDur <- Benchmark.execute(benchmarkingStrategy, ZIO.unit.repeatN(iterCount - 1))
      ioDur <- Benchmark.execute(benchmarkingStrategy, io.repeatN(iterCount - 1))
    } yield (ioDur minus nopDur).dividedBy(iterCount)
  }
}
