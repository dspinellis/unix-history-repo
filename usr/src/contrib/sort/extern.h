/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Peter McIlroy.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

void	 append  __P((u_char **, int, int, FILE *, void (*)(), struct field *));
void	 concat __P((FILE *, FILE *));
length_t enterkey __P((struct recheader *,
	    DBT *, int, struct field *));
void	 fixit __P((int *, char **));
void	 fldreset __P((struct field *));
FILE	*ftmp __P((void));
void	 fmerge __P((int, union f_handle,
	    int, int (*)(), FILE *, void (*)(), struct field *));
void	 fsort __P((int, int, union f_handle, int, FILE *, struct field *));
int	 geteasy __P((int, union f_handle,
	    int, struct recheader *, u_char *, struct field *));
int	 getnext __P((int, union f_handle,
	    int, struct recheader *, u_char *, struct field *));
int	 makekey __P((int, union f_handle,
	    int, struct recheader *, u_char *, struct field *));
int	 makeline __P((int, union f_handle,
	    int, struct recheader *, u_char *, struct field *));
void	 merge __P((int, int, int (*)(), FILE *, void (*)(), struct field *));
void	 num_init __P((void));
void	 onepass __P((u_char **, int, long, long *, u_char *, FILE *));
int	 optval __P((int, int));
void	 order __P((union f_handle, int (*)(), struct field *));
void	 putline __P((struct recheader *, FILE *));
void	 putrec __P((struct recheader *, FILE *));
void	 rd_append __P((int, union f_handle, int, FILE *, u_char *, u_char *));
int	 seq __P((FILE *, DBT *, DBT *));
int	 setfield __P((char *, struct field *, int));
void	 settables __P((int));
