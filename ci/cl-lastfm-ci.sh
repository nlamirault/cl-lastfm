#!/bin/bash
#
# cl-lastfm continuous integration tool
#
# This file, part of cl-lastfm, is Copyright (c) 2009 by Nicolas Lamirault
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. cl-lastfm users are granted the rights to distribute and use this software
# as governed by the terms of the MIT License :
# http://www.opensource.org/licenses/mit-license.php
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
    sbcl --script ci/$PROJECT-ci.lisp >> output.logs
    cat output.logs
    grep "| 0 failed" output.logs
}

cleanup
init
ci
