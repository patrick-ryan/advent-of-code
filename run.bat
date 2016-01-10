@REM run.bat

@REM start python advent-of-code.py

@REM cd advent-of-code-clj
@REM start lein run --filename "../day-4.txt"
@REM cd ..

cd advent-of-code-scala
start sbt "run ../day-6.txt"
cd ..