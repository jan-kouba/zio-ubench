import sbt._
import sbt.Keys.{name, organization}

import com.typesafe.tools.mima.plugin.MimaKeys._
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object MimaSettings {
  lazy val bincompatVersionToCompare = "0.2.1"

  def mimaSettings(failOnProblem: Boolean) =
    Seq(
      mimaPreviousArtifacts := Set(organization.value %% name.value % bincompatVersionToCompare),
      mimaReportSignatureProblems := true,
//      mimaBinaryIssueFilters ++= Seq(),
      mimaFailOnProblem := failOnProblem
    )
}
