#!/bin/sh

help() {
    echo "Usage: buildAndPublishPackage.sh folder ghtoken sonuser sonpass gpgsecretkey version name"
    exit 2
}

run() {

    echo "Configuring credentials ..."
    scala-cli --power config github.token "value:${ghtoken}" --password-value
    scala-cli --power config publish.secretKey "value:${gpgsecretkey}" --password-value
    scala-cli --power config publish.credentials s01.oss.sonatype.org "value:${sonuser}" "value:${sonpass}" --password-value

    echo "Building ${folder} ..."
    scala-cli --power version
    scala-cli --power compile .
    scala-cli --power test .

    echo "Publishing ..."
    scala-cli --power publish setup . --publish-repository central-s01 --project-version "$version"
    scala-cli --power publish . \
        --publish-repository central-s01 \
        --project-version "$version" \
        --secret-key "value:${gpgsecretkey}"

    #cs fetch "org.encalmo:${name}_3:${version}"
}

if [ $# -eq 6 ]; then

    ghtoken=$1
    sonuser=$2
    sonpass=$3
    gpgsecretkey=$4
    version=$5
    name=$6

    run

else
    help
fi
