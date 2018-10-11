@echo off
title Verteilte Systeme Praktikum - CMEM
echo --- Starting VSP CMEM
rem Get local ip address stored in %IP%
for /f "skip=1 delims={}, " %%A in ('wmic nicconfig get ipaddress') do for /f "tokens=1" %%B in ("%%~A") do set "IP=%%~B"
rem Start erlang node with -name containing local ip address as host
werl -name cmem@%IP%