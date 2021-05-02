package zio.ubench

import zio.duration.Duration
import zio.{Has, UIO, ZIO, ZLayer}

import scala.annotation.tailrec

trait PreciseSleeper {
  def sleep(dur: Duration): UIO[Any]
}

object PreciseSleeper {
  type Module = Has[PreciseSleeper]

  def sleep(dur: Duration): ZIO[Module, Nothing, Any] =
    ZIO.accessM[PreciseSleeper.Module](_.get.sleep(dur))

  lazy val layer: ZLayer[Any, Nothing, Has[PreciseSleeper]] = {
    def doPreciseSleep(d: Duration) = {
      ZIO.effectTotal {
        // System.nanoTime() is used directly, so that the sleep is precise.
        // Using zio.console.nanoTime has too much overhead
        val start = System.nanoTime()
        val waitUntilNanos = start + d.toNanos

        @tailrec
        def wait(): Unit = {
          if (System.nanoTime < waitUntilNanos) wait()
        }

        wait()
      }
    }

    ZLayer.succeed(
      new PreciseSleeper {
        def sleep(dur: Duration): UIO[Any] =
          doPreciseSleep(dur)
      }
    )
  }
}
