YFLAGS=-d	# create y.tab.h
CFLAGS=-g 
OBJS =	reduce.o index.o
DOBJS =	duphead.o index.o
UOBJS =	unduphead.o index.o

unduphead:	${UOBJS}
	cc $(UOBJS) -ly -ll -o unduphead

unduphead.o: reduce.h pagesizes.h
	yacc -d	 unduphead.y
	cc -g  -c y.tab.c
#	rm y.tab.c
	mv y.tab.o unduphead.o

duphead:	${DOBJS}
	cc $(DOBJS) -ly -ll -o duphead

duphead.o: reduce.h pagesizes.h
	yacc -d	 duphead.y
	cc -g  -c y.tab.c
#	rm y.tab.c
	mv y.tab.o duphead.o

reduce:	${OBJS}
	cc $(OBJS) -ly -ll -o reduce

reduce.o: reduce.h pagesizes.h
	yacc -d	 reduce.y
	cc -g  -c y.tab.c
#	rm y.tab.c
	mv y.tab.o reduce.o

index.o:	index.h
	lex  index.l
	cc $(CFLAGS) -c lex.yy.c
#	rm lex.yy.c
	mv lex.yy.o index.o
