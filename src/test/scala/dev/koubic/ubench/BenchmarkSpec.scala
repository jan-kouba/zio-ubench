package dev.koubic.ubench

import dev.koubic.ubench.{Benchmark => B}
import dev.koubic.ubench.BenchmarkTests._
import zio._
import zio.duration._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestClock
import zio.clock.Clock
import zio.test.environment.Live

object BenchmarkTests {
  def testMinRunTime: ZSpec[Clock with Clock with TestClock, NoSuchElementException] =
    testM("minRunTime") {
      val minDur = 5.millis
      val sleepDur = 1.milli

      for {
        (totalTime, (benchRes, count)) <-
          B.execute(
            B.untilTotalDuration(minDur) && B.measurementCount,
            clock.sleep(sleepDur)
          ).timed <& TestClock.adjust(100.millis)
      } yield {
        assert(benchRes)(equalTo(minDur)) &&
        assert(totalTime)(equalTo(minDur)) &&
        assert(count.toLong)(equalTo(minDur dividedBy sleepDur))
      }
    }

  def testOrEither
    : Spec[Has[Clock.Service] with Has[TestClock.Service], TestFailure[Nothing], TestSuccess] = {
    def exec(dur: Duration, count: Int) =
      B.execute(
        B.untilTotalDuration(dur) orEither B.minMeasurements(count),
        clock.sleep(1.milli)
      ) raceFirst TestClock.adjust(10.millis).forever

    suite("orEither")(
      testM("right stops")(
        assertM(exec(5.millis, 3))(isRight(equalTo(3)))
      ),
      testM("left stops")(
        assertM(exec(3.millis, 5))(isLeft(equalTo(3.millis)))
      )
    )
  }
}

object BenchmarkSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Has[Clock.Service] with Has[TestClock.Service] with Live, Any] =
    suite("Benchmark")(
      testMinRunTime,
      testOrEither
    ) @@ TestAspect.timeout(5.second)
}
