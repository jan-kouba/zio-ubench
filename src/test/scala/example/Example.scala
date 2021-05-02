package example

import zio.ubench._
import zio._
import zio.duration._

object Example1 extends App {
  def test1 = {
    val io = ZIO.effectTotal {
      (0 until 1_000_000)
        .map(_ + 1)
        .last
    }

    val ioDur = io.benchmark(
      benchmarkingStrategy =
        (
          (Benchmark.untilLocalDurationMin(30) <& Benchmark.untilTotalDuration(1.second)) ||
          Benchmark.untilLowNrmse(0.05, 10)
        ) &> Benchmark.minMeasurementDuration
    )

    ioDur.flatMap { d =>
      console.putStrLn(s"duration of execution of io: ${d}")
    }
  }

  def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    test1 as ExitCode.success
  }
}
