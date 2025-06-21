@rem
@rem Copyright 2015 the original author or authors.
@rem
@rem Licensed under the Apache License, Version 2.0 (the "License");
@rem you may not use this file except in compliance with the License.
@rem You may obtain a copy of the License at
@rem
@rem      https://www.apache.org/licenses/LICENSE-2.0
@rem
@rem Unless required by applicable law or agreed to in writing, software
@rem distributed under the License is distributed on an "AS IS" BASIS,
@rem WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
@rem See the License for the specific language governing permissions and
@rem limitations under the License.
@rem
@rem SPDX-License-Identifier: Apache-2.0
@rem

@if "%DEBUG%"=="" @echo off
@rem ##########################################################################
@rem
@rem  chess-oofp-bonus startup script for Windows
@rem
@rem ##########################################################################

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal

set DIRNAME=%~dp0
if "%DIRNAME%"=="" set DIRNAME=.
@rem This is normally unused
set APP_BASE_NAME=%~n0
set APP_HOME=%DIRNAME%..

@rem Resolve any "." and ".." in APP_HOME to make it shorter.
for %%i in ("%APP_HOME%") do set APP_HOME=%%~fi

@rem Add default JVM options here. You can also use JAVA_OPTS and CHESS_OOFP_BONUS_OPTS to pass JVM options to this script.
set DEFAULT_JVM_OPTS=

@rem Find java.exe
if defined JAVA_HOME goto findJavaFromJavaHome

set JAVA_EXE=java.exe
%JAVA_EXE% -version >NUL 2>&1
if %ERRORLEVEL% equ 0 goto execute

echo. 1>&2
echo ERROR: JAVA_HOME is not set and no 'java' command could be found in your PATH. 1>&2
echo. 1>&2
echo Please set the JAVA_HOME variable in your environment to match the 1>&2
echo location of your Java installation. 1>&2

goto fail

:findJavaFromJavaHome
set JAVA_HOME=%JAVA_HOME:"=%
set JAVA_EXE=%JAVA_HOME%/bin/java.exe

if exist "%JAVA_EXE%" goto execute

echo. 1>&2
echo ERROR: JAVA_HOME is set to an invalid directory: %JAVA_HOME% 1>&2
echo. 1>&2
echo Please set the JAVA_HOME variable in your environment to match the 1>&2
echo location of your Java installation. 1>&2

goto fail

:execute
@rem Setup the command line

set CLASSPATH=%APP_HOME%\lib\chess-oofp-bonus.jar;%APP_HOME%\lib\processing-core-4-4.3.jar;%APP_HOME%\lib\scala-library-2.13.14.jar;%APP_HOME%\lib\javafx-controls-19.0.2.1.jar;%APP_HOME%\lib\jogl-all-main-2.5.0-rc-20230523.jar;%APP_HOME%\lib\gluegen-rt-main-2.5.0-rc-20230523.jar;%APP_HOME%\lib\batik-svggen-1.16.jar;%APP_HOME%\lib\batik-dom-1.16.jar;%APP_HOME%\lib\itext-2.1.7.jar;%APP_HOME%\lib\javafx-graphics-19.0.2.1.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-android-aarch64.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-linux-amd64.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-linux-armv6hf.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-linux-aarch64.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-macosx-universal.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-windows-amd64.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-android-aarch64.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-linux-amd64.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-linux-armv6hf.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-linux-aarch64.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-macosx-universal.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-windows-amd64.jar;%APP_HOME%\lib\antlr-2.7.7.jar;%APP_HOME%\lib\batik-awt-util-1.16.jar;%APP_HOME%\lib\batik-css-1.16.jar;%APP_HOME%\lib\batik-xml-1.16.jar;%APP_HOME%\lib\batik-util-1.16.jar;%APP_HOME%\lib\batik-ext-1.16.jar;%APP_HOME%\lib\batik-constants-1.16.jar;%APP_HOME%\lib\batik-i18n-1.16.jar;%APP_HOME%\lib\batik-shared-resources-1.16.jar;%APP_HOME%\lib\xml-apis-ext-1.3.04.jar;%APP_HOME%\lib\bcmail-jdk14-138.jar;%APP_HOME%\lib\bcprov-jdk14-138.jar;%APP_HOME%\lib\javafx-base-19.0.2.1.jar;%APP_HOME%\lib\xmlgraphics-commons-2.7.jar;%APP_HOME%\lib\bctsp-jdk14-1.38.jar;%APP_HOME%\lib\commons-io-1.3.1.jar;%APP_HOME%\lib\commons-logging-1.0.4.jar;%APP_HOME%\lib\bcmail-jdk14-1.38.jar;%APP_HOME%\lib\bcprov-jdk14-1.38.jar


@rem Execute chess-oofp-bonus
"%JAVA_EXE%" %DEFAULT_JVM_OPTS% %JAVA_OPTS% %CHESS_OOFP_BONUS_OPTS%  -classpath "%CLASSPATH%" chess.ChessGame %*

:end
@rem End local scope for the variables with windows NT shell
if %ERRORLEVEL% equ 0 goto mainEnd

:fail
rem Set variable CHESS_OOFP_BONUS_EXIT_CONSOLE if you need the _script_ return code instead of
rem the _cmd.exe /c_ return code!
set EXIT_CODE=%ERRORLEVEL%
if %EXIT_CODE% equ 0 set EXIT_CODE=1
if not ""=="%CHESS_OOFP_BONUS_EXIT_CONSOLE%" exit %EXIT_CODE%
exit /b %EXIT_CODE%

:mainEnd
if "%OS%"=="Windows_NT" endlocal

:omega
