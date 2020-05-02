#! /bin/bash

echo 'Boris Ermakov-Spektor and Austin Kee'
echo 'COP 4020'
echo 'Pascal Interpreter Testing Harness for Project 3'

echo "----"
echo 'Compiling Pascal interpreter'
stack build

if [ $? -ne 0 ]; then
    echo "ERROR: Compilation failed!"
    exit
fi

echo "----"
echo "Beginning unit tests for interpreter"
stack test

echo "----"
echo "Beginning integration tests"

src_extension=".pas"
in_extension=".in"
out_extension=".out"
passed=0
total=0

test_dir="test/code_test"

declare -a test_folders=("." "p1_tests")

for folder in "${test_folders[@]}"; do
    for i in "$test_dir"/"$folder"/*"$src_extension"; do
        testname=${i//"$test_dir/"/}
        infile=${i//$src_extension/$in_extension}
        outfile=${i//$src_extension/$out_extension}
        
        if [ -f "$infile" ]; then
            result=$(stack run "${i}" <"$infile")
        else
            result=$(stack run "${i}")
        fi
        
        if [ ! -f "$outfile" ]; then
            echo "ERROR: No outfile for $testname"
            exit
        fi
        
        expected_result=$(cat "$outfile")
        
        total=$((total + 1))
        
        echo "----"
        
        if [ "$result" == "$expected_result" ]; then
            passed=$((passed + 1))
            echo "$testname PASSED!"
        else
            echo "$testname FAILED!"
            echo "DIFF FOR $testname"
            diff -u <(echo "$result") <(echo "$expected_result")
        fi
        
    done
done

failed=$((total - passed))
echo "----"
echo "$passed TESTS PASSED"
echo "$failed TESTS FAILED"

if [ $failed -ne 0 ]; then
    return 1
fi
