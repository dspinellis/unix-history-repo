# @(#)makefile	1.1	%G%
# 
# Makefile for the SUN Gremlin picture editor.
# 
# Mark Opperman (opcode@monet.BERKELEY)

CC = 		cc

CFLAGS =	-fsingle

OBJ =		db.o graphics.o graphics2.o pix.o icondata.o sun.o\
		menu.o point.o undodb.o long1.o long2.o short.o help.o\
        	display.o path.o text.o strings.o startup.o main.o

all:            gremlin

gremlin:        ${OBJ}
		${CC} ${OBJ} ${CFLAGS} -lm -lsuntool -lsunwindow -lpixrect \
		-o gremlin
