#!/bin/bash

set -eu


sbtn '; scalafmtCheckAll ; scalafixAll --check; + test'
