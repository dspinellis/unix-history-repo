# Copyright (C) 1989 Free Software Foundation, Inc.
# written by Douglas C. Schmidt (schmidt@ics.uci.edu)
# 
# This file is part of GNU GPERF.
# 
# GNU GPERF is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
# 
# GNU GPERF is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU GPERF; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. 

SHELL = /bin/sh
GPERF = ../gperf
CC = gcc
CFLAGS = -O

all: test

test:
	@echo "performing some tests of the perfect hash generator"   
	$(CC) -c $(CFLAGS) test.c
	$(GPERF) -p -c -l -S1 -C -o c.gperf > cinset.c
	$(CC) $(CFLAGS) -o cout cinset.c test.o
	@echo "testing ANSI C reserved words, all items should be found in the set"
	./cout -v < c.gperf > output.c
	-diff -b expected.c output.c
	$(GPERF) -k1,4,'$$' ada.gperf > adainset.c
# double '$$' is only there since make gets confused; program wants only 1 '$'
	$(CC) $(CFLAGS) -o aout adainset.c test.o
	@echo "testing Ada reserved words,all items should be found in the set"
	./aout -v < ada.gperf > output.ada-res
	-diff -b expected.ada-res output.ada-res
	$(GPERF) -p -D -k1,'$$' -s 2 -o adadefs.gperf > preinset.c
	$(CC) $(CFLAGS) -o preout preinset.c test.o
	@echo "testing Ada predefined words, all items should be found in the set"
	./preout -v < adadefs.gperf > output.ada-pred
	-diff -b expected.ada-pred output.ada-pred
	$(GPERF) -k1,2,'$$' -o modula3.gperf > m3inset.c
	$(CC) $(CFLAGS) -o m3out m3inset.c test.o
	@echo "testing Modula3 reserved words, all items should be found in the set"
	./m3out -v < modula3.gperf > output.modula
	-diff -b expected.modula output.modula
	$(GPERF) -o -S2 -p < pascal.gperf > pinset.c
	$(CC) $(CFLAGS) -o pout pinset.c test.o
	@echo "testing Pascal reserved words, all items should be found in the set"
	./pout -v < pascal.gperf > output.pascal
	-diff -b expected.pascal output.pascal
# these next 5 are demos that show off the generated code
	$(GPERF) -p -j1 -g -o -t -N is_reserved_word -k1,3,'$$' c-parse.gperf > test.out.1
	-diff -b expected.out.1 test.out.1
	$(GPERF) -n -k1-8 -l modula2.gperf > test.out.2
	-diff -b expected.out.2 test.out.2
	$(GPERF) -p -j 1 -o -a -g -t -k1,4,$$ gplus.gperf > test.out.3
	-diff -b expected.out.3 test.out.3
	$(GPERF) -D -p -t < c-parse.gperf > test.out.4
	-diff -b expected.out.4 test.out.4
	$(GPERF) -g -o -j1 -t -p -N is_reserved_word gpc.gperf > test.out.5
	-diff -b expected.out.5 test.out.5
# prints out the help message
	-$(GPERF) -h > test.out.6 2>&1
	-diff -b expected.out.6 test.out.6
	@echo "only if, do, for, case, goto, else, while, and return should be found "
	./aout -v < c.gperf > test.out.7
	-diff -b expected.out.7 test.out.7

clean: 
	-rm -f *.o core *~ *inset.c test.out.* *out #*#
