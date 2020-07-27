#!/bin/bash
#
gfortran -c analemma.f
if [ $? -ne 0 ]; then
  echo "Errors compiling analemma.f"
  exit
fi
#
gfortran analemma.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading analemma.o"
  exit
fi
#
rm analemma.o
#
chmod ugo+x a.out
mv a.out ~/binf77/analemma
#
echo "Executable installed as ~/binf77/analemma"
