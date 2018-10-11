@echo off
title Verteilte Systeme Praktikum - Compile
echo --- Compiling Server
erl -compile hbq
echo --- Compiling HBQ
erl -compile server
echo --- Compiling DLQ
erl -compile dlq
echo --- Compiling CMEM
erl -compile cmem