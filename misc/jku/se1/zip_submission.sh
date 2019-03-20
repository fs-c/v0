#!/bin/bash

if [ $# -eq 0 ]; then
	echo "usage: $0 <exercise number>"

	exit 1;
fi

DIR=$1

if [ ! -d "$DIR" ]; then
	echo "error: directory $DIR does not exist"

	exit 1;
fi

# This is an ugly solution
cd $DIR
touch -t 000101010000 *
zip -r -t 2015-01-01  "../k11804751_Übung$DIR.zip" .
cd ..
