#!/bin/bash

set -eu


sbtn scalafmtCheckAll
sbtn 'scalafixAll --check'
sbtn mimaReportBinaryIssues
sbtn '+ test'
