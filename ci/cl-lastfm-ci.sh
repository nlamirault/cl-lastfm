#!/bin/bash
#
# cl-lastfm continuous integration tool
#

SCRIPT_HOME=`dirname $0`
PROJECT_HOME=$(dirname $SCRIPT_HOME)
PROJECT="cl-lastfm"
TEST_ENV=$PROJECT_HOME/.clenv
QUICKLISP_FILE="http://beta.quicklisp.org/quicklisp.lisp"

cleanup() {
    rm -fr $TEST_ENV
    mkdir $TEST_ENV
}

init() {
    cp ci/init.lisp $TEST_ENV
    cd $TEST_ENV
    wget -q $QUICKLISP_FILE -O quicklisp.lisp
    sbcl --script init.lisp
    ln -s $PWD/.. ./.quicklisp/local-projects/$PROJECT
    cd ..
}

ci() {
    sbcl --script ci/$PROJECT-ci.lisp
}

cleanup
init
ci
