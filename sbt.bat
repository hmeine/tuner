@echo off

set R_HOME=C:\Program Files\R\R-2.13.0
set JRI=%R_HOME%\library\rJava\jri\x64;%R_HOME%\library\rJava\jri

set OPENGL=C:\Users\tom\Projects\tuner\lib\opengl\windows64
set PATH=%PATH%;%R_HOME%\bin;%R_HOME%\bin\x64;%OPENGL%;C:\Program Files\Java\jdk1.6.0_26\bin

set SCRIPT_DIR=%~dp0
java -Xmx512M -jar "%SCRIPT_DIR%sbt-launch.jar" %*


