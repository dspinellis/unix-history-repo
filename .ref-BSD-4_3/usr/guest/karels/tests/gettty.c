/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getttyent.c	5.3 (Berkeley) 5/19/86";
#endif LIBC_SCCS and not lint

#include <stdio.h>
#include <strings.h>
#include <ttyent.h>

main()
{
	struct ttyent *t;

	while (t = getttyent()) {
		printf("name \"%s\", ", t->ty_name);
		printf("getty \"%s\", ", t->ty_getty);
		printf("type \"%s\", ", t->ty_type);
		if (t->ty_window)
			printf("window \"%s\", ", t->ty_window);
		if (t->ty_status & TTY_ON)
			printf("on ");
		if (t->ty_status & TTY_SECURE)
			printf("secure ");
		if (t->ty_comment)
			printf("comment \"%s\", ", t->ty_comment);
		printf("\n");
	}
}
