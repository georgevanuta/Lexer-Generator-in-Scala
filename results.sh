#!/bin/bash

function run_scala() {
	bonus=1.25
	result_op="$(sbt test | grep "+" | grep -o "[0-9]\+p" | grep -o "[0-9]\+" | tr '\n' '+')0"
	score=$(echo $result_op | bc)
}

function run_python() {
	bonus=1
	result_op="$(python3 -m unittest 2> /dev/null | grep "[0-9]\+p" | grep -o "[0-9]\+" | tr '\n' '+')0"

	score=$(echo $result_op | bc)
}

MAX_SCORE=100

# TODO find a way to check which language the student implemented the assignment in (one way to do this is to have 2 different vmck assignments)
run_scala

echo $score/$MAX_SCORE
echo "scale = 2; $score*$bonus/$MAX_SCORE" | bc