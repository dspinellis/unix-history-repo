/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 * 
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction, 
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

/* @(#)libfind.c 1.10 69/12/31 */

/*
 * Copyright (c) 1987, 1991 by Sun Microsystems, Inc.
 */


#include <stdio.h>
#include <values.h>
#include <sys/param.h>
#include <sys/dir.h>

/*
 * At batch time if we have been asked which version number of some 
 * library then we follow the search path until we found it else
 * if no version is found then we take the highest version in the
 * first directory where a macthing library is found.
 */
#define LIBSTR 255
#define SPECIFIC 1

extern stol();
extern char *calloc();

getshlib(dir, lib, mj, mn)
	char *dir;
	char *lib;
	int *mj;
	int *mn;
{
	char *p;
	char *version;
	char *file;
	int len = 0;
	int lib_len = 0;
	DIR *dirp;
	register struct direct *dp;
	int maj = -MAXINT;
	int min = -MAXINT;
	int still_looking = 1;
	int cmp = 0;
	int flag = 0;
	int timeslooked =0;
	/*
	 * determine whether a specific version of the library is wanted here.
	 */
	version = calloc(LIBSTR, 1);
	file = calloc(LIBSTR, 1);
	p = lib;
	while (*p != '\0') {
		len++;
		if (*p != '.')
			p++;
		else
			break;
	}
	if (*p == '.') {
		if (strncmp(++p, "so", 2))
			error(1, "expected libx.so(.major.minor.xxx)\n");
		else {	 
			flag = SPECIFIC;
			lib_len =  p - lib + 3;
			if (!stol(p + 3,'.', &p, &maj)) 
				error(1, "expected libx.so(.major.minor.xxx)\n");
                    	if (!stol(++p, '.', &p, &min))
				error(1, "expected libx.so(.major.minor.xxx)\n");
		} 
	} else strcat(lib, ".so.");
	sprintf(version,"%d.%d", maj, min);
	len = strlen(lib);
	/*
	 * open the directory
	 */
	dirp = opendir(dir);
	if (dirp == NULL)
		return (-1);
	
	for (dp = readdir(dirp); dp != NULL && still_looking; dp = readdir(dirp)) {
		if (!strncmp(lib , dp->d_name, len)) {

			if (flag == SPECIFIC) {
				strncpy(file, dp->d_name, lib_len);
				still_looking = 0; 
			} else {
				if (((cmp = verscmp(dp->d_name + len, version)) > 0) ||
				    ((cmp == 0) && (*file == NULL))) {
					strncpy(file, dp->d_name, len);
					*version = '\0';
					strcpy(version, dp->d_name + len);
				}; 
			} 
		} 
	}
	closedir(dirp);
	if (file[0] != NULL) {
		strcat(dir, "/");
		strcat(dir, file);
		strcat(dir, version);
		stol(version, '.', &version, &maj);
		stol(++version, '.', &version, &min);
		*mj = maj;
		*mn = min;
		return 0;
	} else 
		return -1;
}
