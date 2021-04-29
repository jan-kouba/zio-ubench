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
            B.minRunTime(minDur) && B.measurementCount,
            clock.sleep(sleepDur)
          ).timed <& TestClock.adjust(100.millis)
      } yield {
        assert(benchRes)(equalTo(minDur)) &&
        assert(totalTime)(equalTo(minDur)) &&
        assert(count.toLong)(equalTo(minDur dividedBy sleepDur))
      }
    }

  def test_|| = {
    def exec(dur: Duration, count: Int) =
      B.execute(
        B.minRunTime(dur) || B.minMeasurements(count),
        clock.sleep(1.milli)
      ) raceFirst TestClock.adjust(10.millis).forever

    suite("||")(
      testM("right stops")(
        assertM(exec(5.millis, 3))(equalTo((3.millis, 3)))
      ),
      testM("left stops")(
        assertM(exec(3.millis, 5))(equalTo((3.millis, 3)))
      )
    )
  }
}

object BenchmarkSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Has[Clock.Service] with Has[TestClock.Service] with Live, Any] =
    suite("Benchmark")(
      testMinRunTime,
      test_||
    ) @@ TestAspect.timeout(5.second)
}
