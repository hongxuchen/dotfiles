#!/bin/bash
jar tf $1 | grep class$ | perl -pe "s|/[^/]*class$||;s|/|.|g" | sort | uniq
