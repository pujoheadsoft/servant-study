#!/bin/bash

docker build -t servant-study-db .
docker run -p 5432:5432 --name servant-study-db --rm -d servant-study-db