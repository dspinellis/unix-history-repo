/* $Id: util.h,v 3.0 1991/09/09 20:27:37 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

EXT bool waiting INIT(FALSE);	
			/* are we waiting for subprocess (in doshell)? */
EXT int len_last_line_got INIT(0);
			/* strlen of some_buf after */
			/*  some_buf = get_a_line(bufptr,buffersize,fp) */
EXT MEM_SIZE buflen_last_line_got INIT(0);

/* is the string for makedir a directory name or a filename? */

#define MD_DIR 	0
#define MD_FILE 1

void	util_init _((void));
int	doshell _((char*,char*));
char	*safemalloc _((MEM_SIZE));
char	*saferealloc _((char*,MEM_SIZE));
char	*safecpy _((char*,char*,int));
char	*safecat _((char*,char*,int));
char	*cpytill _((char*,char*,int));
char	*instr _((char*,char*,bool_int));
#ifdef SETUIDGID
int	eaccess _((char*,int));
#endif
char	*getwd _((char*));
char	*get_a_line _((char*,int,FILE*));
char	*savestr _((char*));
int	makedir _((char*,int));
void	setenv _((char*,char*));
int	envix _((char*));
void	notincl _((char*));
char	*getval _((char*,char*));
void	growstr _((char**,int*,int));
void	setdef _((char*,char*));
#ifndef HAS_STRFTIME
size_t	strftime _((char*, size_t, CONST char*, CONST struct tm*));
#endif
