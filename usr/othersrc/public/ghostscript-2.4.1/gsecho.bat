@echo off
if '%1'=='-a' goto add
if exist %1 erase %1
goto put
:add
shift
:put
echo %2 %3 %4 %5 %6 %7 %8 %9 >>%1
