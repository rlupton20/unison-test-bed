#!/usr/bin/env nix-shell
#!nix-shell -p python27Packages.docker_compose -i sh

docker-compose scale node=$1
docker-compose up
