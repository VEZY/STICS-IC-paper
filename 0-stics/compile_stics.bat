ECHO OFF

REM initial dir
SET INIT_DIR=%CD%

REM set version
SET VER=%1
IF "%1"=="" (
     echo "Missing first input argument for model version !"
     exit 1
)

REM set stics dir
SET STICS_DIR=%2
SET DEBUG_DIR=%3
IF "%2"=="" (
    SET DEBUG_DIR=Debug
    SET STICS_DIR="."
)

REM getting year onth day
set BYEAR=%DATE:~6,4%
set BMONTH=%DATE:~3,2%
set BDAY=%DATE:~0,2%

set BUILD_DATE=%BYEAR%-%BMONTH%-%BDAY%

SET DATEVER=%3
IF "%3"=="" (
    SET DATEVER=%BUILD_DATE%
)

REM Stics dir
echo Stics directory %STICS_DIR%

REM version file creation
echo Informations about Stics model build version > %STICS_DIR%\stics_version.txt
echo Build date : %BUILD_DATE% (%TIME%) >> %STICS_DIR%\stics_version.txt
echo Version : %VER% >> %STICS_DIR%\stics_version.txt
echo Version date : %DATEVER% >> %STICS_DIR%\stics_version.txt
echo OS name : Windows (32bits) >> %STICS_DIR%\stics_version.txt

SET TMP_DIR=%STICS_DIR%\inputs\tmp
REM if dir name needed
IF NOT EXIST %TMP_DIR% (
 	echo "Invalid directory : %TMP_DIR%"
        echo "Set src parent directory name as script second input argument"
        exit 1
)

REM Creating F90 file with version and date
if EXIST %TMP_DIR%\call_num_version.f90 (
   del /F %TMP_DIR%\call_num_version.f90
)

type %TMP_DIR%\call_num_version_head.tmp > %TMP_DIR%\call_num_version.f90
echo         nomversion='%VER%' >> %TMP_DIR%\call_num_version.f90
echo         dateversion='%DATEVER%' >> %TMP_DIR%\call_num_version.f90
type %TMP_DIR%\call_num_version_foot.tmp >> %TMP_DIR%\call_num_version.f90
move /Y %TMP_DIR%\call_num_version.f90 %STICS_DIR%\inputs


REM moving to Debug folder for compilation
REM cd %DEBUG_DIR%
REM IF EXIST Stics.exe (
REM    del /F Stics.exe
REM )

echo %DEBUG_DIR%
make all -C %DEBUG_DIR%
REM mingw32-make all

IF NOT "%ERRORLEVEL%"=="0" (
     exit 1
)

IF NOT EXIST Stics.exe (
   exit 1
)


REM back to initial dir
cd %INIT_DIR%
