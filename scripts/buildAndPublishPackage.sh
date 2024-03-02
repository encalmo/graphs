#!/bin/sh

help() {
    echo "Usage: buildAndPublishPackage.sh folder ghtoken sonuser sonpass version name"
    exit 2
}

run() {

    echo "Configuring credentials ..."
    scala-cli --power config publish.credentials s01.oss.sonatype.org "value:${sonuser}" "value:${sonpass}" --password-value

    echo "Building ${folder} ..."
    scala-cli --power version
    scala-cli --power compile .
    scala-cli --power test .

    echo "Publishing ..."
    scala-cli --power publish setup . --publish-repository central-s01 --project-version "$version"
    scala-cli --power publish . \
        --publish-repository central-s01 \
        --project-version "$version"

    #cs fetch "org.encalmo:${name}_3:${version}"
}

if [ $# -eq 5 ]; then

    ghtoken=$1
    sonuser=$2
    sonpass=$3
    version=$4
    name=$5

    run

else
    help
fi
