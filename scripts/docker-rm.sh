#!/bin/sh

docker ps -a | grep 'minions' | awk '{print $1}' | xargs docker rm -f
