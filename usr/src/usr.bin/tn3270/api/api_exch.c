#include <stdio.h>

#include "api_exch.h"

static int sock;		/* Socket number */

static char ibuffer[40], *ibuf_next, *ibuf_last;
#define	IBUFADDED(i)		ibuf_last += (i)
#define	IBUFAVAILABLE()		(ibuf_last -ibuf_next)
#define	IBUFFER()		ibuffer
#define	IBUFGETCHAR()		(*ibuf_next++)
#define	IBUFGETSHORT()		((*ibuf_next++<<8)|(*ibuf_next++&0xff))
#define	IBUFRESET()		(ibuf_next = ibuf_last = ibuffer)

char obuffer[40], *obuf_next;
#define	OBUFADDBYTES(w,l)	{ memcpy(obuf_next, w, l); obuf_next += l; }
#define	OBUFADDCHAR(c)		(*obuf_next++ = c)
#define	OBUFADDSHORT(s)		{*obuf_next++ = (s)>>8; *obuf_next++ = s; }
#define	OBUFAVAILABLE()		(obuf_next - obuffer)
#define	OBUFFER()		obuffer
#define	OBUFRESET()		obuf_next = obuffer
#define	OBUFROOM()		(obuffer+sizeof obuffer-obuf_next)


static int
outflush()
{
    int length = OBUFAVAILABLE();

    if (length != 0) {
	if (write(sock, OBUFFER(), length) != length) {
	    perror("writing to API client");
	    return -1;
	}
	OBUFRESET();
    }
    return 0;				/* All OK */
}


static int
infill(count)
int count;
{
    int i;

    if (OBUFAVAILABLE()) {
	if (outflush() == -1) {
	    return -1;
	}
    }
    if (ibuf_next == ibuf_last) {
	IBUFRESET();
    }
    while (count) {
	if ((i = read(sock, IBUFFER(), count)) < 0) {
	    perror("reading from API client");
	    return -1;
	}
	count -= i;
	IBUFADDED(i);
    }
    return 0;
}

int
api_exch_inbyte()
{
    if (IBUFAVAILABLE() < 1) {
	if (infill(1) == -1) {
	    return -1;
	}
    }
    return IBUFGETCHAR();
}


int
api_exch_incommand(command)
int command;
{
    int i;

    if (IBUFAVAILABLE() < 1) {
	if (infill(1) == -1) {
	    return -1;
	}
    }
    i = IBUFGETCHAR();
    if (i != command) {
	fprintf(stderr, "Expected API command 0x%x, got API command 0x%x.\n",
				command, i);
	return -1;
    }
    return 0;
}


int
api_exch_outcommand(command)
int command;
{
    if (OBUFROOM() < 1) {
	if (outflush() == -1) {
	    return -1;
	}
    }
    OBUFADDCHAR(command);
    return 0;
}


int
api_exch_outtype(type, length, location)
int
    type,
    length;
char
    *location;
{
    int netleng = htons(length);

    if (OBUFROOM() < 3) {
	if (outflush() == -1) {
	    return -1;
	}
    }
    OBUFADDCHAR(type);
    OBUFADDSHORT(netleng);
    if (OBUFROOM() > length) {
	OBUFADDBYTES(location, length);
    } else {
	if (outflush() == -1) {
	    return -1;
	}
	if (write(sock, location, length) != length) {
	    perror("writing to API client");
	    return -1;
	}
    }
}


int
api_exch_intype(type, length, location)
int
    type,
    length;
char
    *location;
{
    int i, netleng = htons(length);

    if (IBUFAVAILABLE() < 3) {
	if (infill(3) == -1) {
	    return -1;
	}
    }
    if ((i = IBUFGETCHAR()) != type) {
	fprintf(stderr, "Expected type 0x%x, got type 0x%x.\n", type, i);
	return -1;
    }
    if ((i = IBUFGETSHORT()) != netleng) {
	fprintf(stderr, "Type 0x%x - expected length %d, received length %d.\n",
		type, length, ntohs(i));
	return -1;
    }
    while (length) {
	if ((i = read(sock, location, length)) < 0) {
	    perror("reading from API client");
	    return -1;
	}
	length -= i;
	location += i;
    }
    return 0;
}

int
api_exch_init(sock_number)
int sock_number;
{
    sock = sock_number;

    IBUFRESET();
    OBUFRESET();

    return 0;
}
