package example

import zio.ubench._
import zio._
import zio.duration._

object Example extends App {
  def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val io = ZIO.effectTotal {
      (0 until 1000000)
        .map(_ + 1)
        .last
    }

    // Run the default benchmark
    val r1 = io.benchmark()

    // Run benchmark for 1 second and then take the duration of the fastest run.
    val r2 = io.benchmark(
      Benchmark.untilTotalDuration(1.second) &> Benchmark.minMeasurementDuration
    )

    // Preheat the io for 100 milliseconds and then run the benchmark until minimal duration
    // is not improved for 30 steps or until the normalized root-mean-square error
    // of last 10 measurements drops below 0.05.
    // Return the minimal measurement duration.
    val r3 = io.benchmark(
      (
        (Benchmark.untilLocalDurationMin(30) || Benchmark
          .untilLowNrmse(0.05, 10)) &> Benchmark.minMeasurementDuration
      ).withPreheat(Benchmark.untilTotalDuration(100.millis), 5.millis)
    )

    (show(r1) zip show(r2) zip show(r3)) as ExitCode.success
  }

  private def show[R, E](io: ZIO[R, E, Duration]) =
    io.flatMap { d =>
      console.putStrLn(s"duration of execution of io: ${d}")
    }
}
