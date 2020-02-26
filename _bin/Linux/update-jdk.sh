if [ -z ${JAVA_VERSION} ]; then
  echo "export JAVA_VERSION=6|7|8 first!"
  return
else
  echo "JAVA_VERSION=${JAVA_VERSION}"
fi

version=${JAVA_VERSION}

# export J2SDKDIR=/usr/lib/jvm/java-${version}-oracle
# export J2REDIR=/usr/lib/jvm/java-${version}-oracle/jre
# export PATH=/usr/lib/jvm/java-${version}-oracle/bin:/usr/lib/jvm/java-${version}-oracle/db/bin:/usr/lib/jvm/java-${version}-oracle/jre/bin:${PATH}
# export JAVA_HOME=/usr/lib/jvm/java-${version}-oracle
# export DERBY_HOME=/usr/lib/jvm/java-${version}-oracle/db

export JAVA_HOME=

sudo update-alternatives --set java /usr/lib/jvm/java-${version}-oracle/jre/bin/java
sudo update-alternatives --set javac /usr/lib/jvm/java-${version}-oracle/bin/javac
sudo update-alternatives --set javap /usr/lib/jvm/java-${version}-oracle/bin/javap
sudo update-alternatives --set javah /usr/lib/jvm/java-${version}-oracle/bin/javah

java -version
javac -version
