#!/bin/sh
set -e
set -x

docker run --rm \
  -v ${PWD}/buildtasks/artifacts:/opt/artifacts \
  minionsapi_app \
  cp /opt/minionsApi/dist/build/minionsApi/minionsApi /opt/artifacts/

docker build -t minionsapi_bin ./buildtasks
