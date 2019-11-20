#!/bin/bash

BUILD_DIR="build"
APP_JS="elm.js"
INDEX_HTML="index.html"
STATIC="static"


if [ -z ${BUILD_DIR} ]; then
    echo "BUILD_DIR incorrectly defined"
    exit 1
fi
if [ -d ${BUILD_DIR} ]; then
    rm -rf ${BUILD_DIR}
fi

# Replace the build_dir with the static files
cp -r ${STATIC}     ${BUILD_DIR}

elm make src/Main.elm --output=${BUILD_DIR}/${APP_JS} --debug

cp ${INDEX_HTML}    ${BUILD_DIR}

http-server build