echo off
cls
echo %0 %1
pas1 %1,,%1;
if not errorlevel 3 pas2
if not errorlevel 3 if exist bindit.bat bindit.bat
if not errorlevel 3 link c:\-ibmpas\initseg+%1,%1,%1,c:\-ibmpas\pascal+\-ibmpas\userlib
