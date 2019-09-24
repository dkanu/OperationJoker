@echo off
cd "PATH TO OPERATION JOKER PROJECT"
"PATH TO python.exe" joker_dv360_get_report.py%* -g QUERY_ID
"PATH TO RScript.exe" "PATH TO OperationJoker\main.R" QUERY_ID.csv > "PATH TO WHERE YOU WANT TO STORE \test.Rout"
pause
