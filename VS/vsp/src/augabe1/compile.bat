@echo off
title Verteilte Systeme Praktikum - Compile
echo --- Compiling server
erl -compile server
echo --- Compiling dlq
erl -compile dlq