import zio.test.Assertion._
import zio.test._
import zio.ZIO

object ExampleTests {
  val oneTest: ZSpec[Any, Throwable] = testM("one equals one") {
    assertM(ZIO(1))(equalTo(1))
  }
}

object ExampleSpec extends DefaultRunnableSpec {
  def spec: Spec[Any, TestFailure[Throwable], TestSuccess] =
    suite("Example")(
      ExampleTests.oneTest
    )
}
