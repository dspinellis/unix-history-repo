@echo off
goto %1
rem ---------------- dev ----------------
:-dev
shift
if '%1'=='' goto done
echo device_(gs_%1_device) >>_temp_.dh
goto -dev
rem ---------------- include ----------------
:-include
shift
if '%1'=='' goto done
copy _temp_.bat _temp_.dbt
copy %1.dev _temp_.bat
command /c _temp_.bat
copy _temp_.dbt _temp_.bat
goto -include
rem ---------------- lib ----------------
:-lib
shift
if '%1'=='' goto done
echo %1+ >>_temp_.dlb
goto -lib
rem ---------------- obj ----------------
:-obj
shift
if '%1'=='' goto done
echo %1+ >>_temp_.dob
echo FILE %1 >>_temp_.dow
goto -obj
rem ---------------- oper ----------------
:-oper
shift
if '%1'=='' goto done
echo oper_(%1_op_defs) >>_temp_.dop
goto -oper
rem ---------------- ps ----------------
:-ps
shift
if '%1'=='' goto done
echo psfile_("%1.ps") >>_temp_.dps
goto -ps
rem - - - - - - - - done - - - - - - - -
:done
