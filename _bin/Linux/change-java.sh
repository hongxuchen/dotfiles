#!/usr/bin/env bash


if [ "$#" -lt 1 ]; then
  echo "$#"
  echo "usage: $0 6|7|8"
  exit 1
fi

version=$1

update-alternatives --set java /usr/lib/jvm/java-${version}-oracle/jre/bin/java
update-alternatives --set javac /usr/lib/jvm/java-${version}-oracle/bin/javac
update-alternatives --set javap /usr/lib/jvm/java-${version}-oracle/bin/javap
update-alternatives --set javah /usr/lib/jvm/java-${version}-oracle/bin/javah

java -version
javac -version
