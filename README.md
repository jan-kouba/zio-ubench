# zio-ubench

[![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases]
[![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots]

Micro-benchmark your ZIO effects!

```sbt
// build.sbt
libraryDependencies += "dev.koubic" %% "zio-ubench" % "0.2.3"
```

## How does it work?

```scala
import zio.ubench._
import zio._
import zio.duration._

val io = ZIO.effectTotal {
  (0 until 1_000_000)
    .map(_ + 1)
    .last
}

val ioDur = io.benchmark()

ioDur.flatMap { d =>
  console.putStrLn(s"duration of execution of io: ${d}")
}
```

Outputs:

```
duration of execution of io: PT0.02105794S
```

---- 

The `benchmark()` method has several parameters that can be used to tune the benchmarking process:

```scala
val ioDur = io.benchmark(
  benchmarkingStrategy = Benchmark.untilLocalDurationMin(30)
)
```
This stops the benchmark after the run time of the `io` was not improved for 30 steps.

We can also require the benchmark to run for at least 1 second:

```scala
val ioDur = io.benchmark(
  benchmarkingStrategy = 
    Benchmark.untilLocalDurationMin(30) <& Benchmark.untilTotalDuration(1.second))
)
```

We can also stop the benchmark if NRMSE is low and return the minimal run time observed:

```scala
val ioDur = io.benchmark(
  benchmarkingStrategy =
    (
      (Benchmark.untilLocalDurationMin(30) <& Benchmark.untilTotalDuration(1.second)) ||
      Benchmark.untilLowNrmse(0.05, 10)
    ) &> Benchmark.minMeasurementDuration
)
```

For more details see the scaladoc of the `Benchmark` class.

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.koubic/zio-ubench_2.13.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.koubic/zio-ubench_2.13.svg "Sonatype Snapshots"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/dev/koubic/zio-ubenchc_2.13/ "Sonatype Snapshots"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/dev/koubic/zio-ubenchc_2.13/ "Sonatype Releases"
