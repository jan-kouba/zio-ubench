package zio.ubench

import zio.ZIO
import zio.clock._
import zio.console.Console
import zio.duration._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.Live

object UBenchTests {
  def measureSleepCorrectlyTest(
    sleepDur: Duration
  ): ZSpec[Live with PreciseSleeper.Module with Clock with ZTestEnv with Annotations, Nothing] =
    testM(s"should measure sleep correctly (${sleepDur})") {
      val tolerance = 1.micros max (sleepDur.toNanos * 0.01).toLong.nanos

      assertM(
        PreciseSleeper
          .sleep(sleepDur)
          .benchmark()
          .map(_.toNanos)
      )(approximatelyEquals(sleepDur.toNanos, tolerance.toNanos))
    } @@ TestAspect.timeout(10.seconds) @@ TestAspect.flaky(3)
}

object UBenchSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("UBench")(
      suite("when using real clock")(
        UBenchTests.measureSleepCorrectlyTest(100.millis),
        UBenchTests.measureSleepCorrectlyTest(1.millis),
        UBenchTests.measureSleepCorrectlyTest(1.micros)
      ) @@ TestAspect.sequential
    )
      .provideSomeLayer[Live with ZTestEnv with Annotations](
        environment.live(ZIO.environment[Clock with Console]).toLayerMany ++
          PreciseSleeper.layer
      )
//      .provideSomeLayer[Live with Clock with ZTestEnv](PreciseSleeper.liveLayer)
}
