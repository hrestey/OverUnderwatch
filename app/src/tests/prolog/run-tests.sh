#!/bin/sh

swipl -q -s standings-tests.pl -t run_tests
swipl -q -s matches-tests.pl -t run_tests
swipl -q -s season-tests.pl -t run_tests
