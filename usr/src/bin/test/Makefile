# Copyright (C) 1989 by Kenneth Almquist.  All rights reserved.
# This file is part of ash, which is distributed under the terms specified
# by the Ash General Public License.  See the file named LICENSE.


PROG= test
CFLAGS = -O
CC= cc
OBJS= error.o stalloc.o operators.o 

$(PROG): test.c operators.h extern.h $(OBJS)
	$(CC) $(CFLAGS) -o $@ test.c operators.o error.o stalloc.o
	-rm -f '['
	ln test '['

operators.c operators.h: unary_op binary_op mkops
	./mkops

operators.o: operators.h

clean:
	rm -f core.* *.o $(PROG) operators.h operators.c

tags:
	ctags -t -d *.[ch]
