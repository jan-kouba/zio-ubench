#!/bin/bash

set -eu


./test.sh

# Publishes artifacts into a local staging directory
sbtn '+ publishSigned'

# Uploads artifacts to Sonatype and releases them
sbtn sonatypeBundleRelease
