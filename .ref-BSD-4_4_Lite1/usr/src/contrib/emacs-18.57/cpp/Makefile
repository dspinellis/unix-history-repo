# Makefile for cccp in the Emacs distribution only.
# Here we assume that you are using sys V.0
# (since cccp is used by Emacs only to deal with shortnames lossage).
# Therefore, we add -DEMACS to CFLAGS and add -lPW to linking.

CFLAGS=-g -I. -DEMACS
cpp: cccp
	-rm cpp
	ln cccp cpp
cccp: cccp.o y.tab.o
	cc -o cccp -g cccp.o y.tab.o -lPW
testexp: y.tab.c
	cc -g -DTEST_EXP_READER y.tab.c -o testexp
y.tab.c: cexp.y
	echo expect 40 shift/reduce conflicts
	yacc cexp.y
cccp.o: cccp.c
y.tab.o: y.tab.c
lint:	y.tab.c
	lint -p y.tab.c cccp.c
