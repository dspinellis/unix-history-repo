/* Copyright (C) 1982, 1988, 1989 Walter Tichy
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Walter Tichy.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Report all problems and direct all questions to:
 *   rcs-bugs@cs.purdue.edu
 * 







*/

/*******************************************************************
 *                     curdir: get current directory
 *******************************************************************
 * returns full pathname of working (current) directory.
 * This is an adaptation of pwd, and works for grafted directories.
 * Unlike pwd, returns to current directory after it is finished.
 * Uses stdio buffering for directory reads.
 */

 static char rcsid[]=
 "$Header: /usr/src/local/bin/rcs/src/RCS/curdir.c,v 3.3 89/05/01 15:11:49 narten Exp $";

/*******************************************************************
 *      $Log:	curdir.c,v $
 * Revision 3.3  89/05/01  15:11:49  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 3.2  87/10/18  10:21:49  narten
 * Updating version numbers. Changes relative to 1.1 are actually 
 * relative to 3.2
 * 
 * Revision 1.1  84/01/23  14:50:01  kcs
 * Initial revision
 * 
 * Revision 3.2  82/12/24  15:41:51  wft
 * Changed curdir() such that it returns a pointer to the pathname of
 * the current working directory, just as Berkeley's getcwd().
 * 
 * Revision 3.1  82/10/18  21:16:21  wft
 * curdir() now uses stdio buffering for directory reads,
 * returns to working directory after done, and closes the directories.
 * A testprogram was also added.
 * 
 *******************************************************************/
 

#include        "rcsbase.h"
#include        <sys/param.h>
#include        <sys/stat.h>
#include        <sys/dir.h>
#define dot     "."
#define dotdot  ".."


static char cwd[NCPPN];

char * curdir()
/* Function: places the pathname of the current directory into cwd
 * and returns a pointer to it. Returns NULL on failure.
 */
{
        FILE    *file;
        struct  stat    d, dd;
        struct  direct  dir;

        int rdev, rino;
        int off;
        register i,j;

        cwd[off= 0] = '/';
        cwd[1] = '\0';
        stat("/", &d);
        rdev = d.st_dev;
        rino = d.st_ino;
        for (;;) {
                if (stat(dot, &d)<0) return NULL;
                if (d.st_ino==rino && d.st_dev==rdev) {
                        if (cwd[off] == '/') cwd[off] = '\0';
                        chdir(cwd); /*change back to current directory*/
                        return cwd;
                }
                if ((file = fopen(dotdot,"r")) == NULL) return NULL;
                if (fstat(fileno(file), &dd)<0) goto fail;
                chdir(dotdot);
                if(d.st_dev == dd.st_dev) {
                        if(d.st_ino == dd.st_ino) {
                            if (cwd[off] == '/') cwd[off] = '\0';
                            chdir(cwd); /*change back to current directory*/
                            fclose(file);
                            return cwd;
                        }
                        do {
                            if (fread((char *)&dir, sizeof(dir), 1, file) !=1)
                                goto fail;
                        } while (dir.d_ino != d.st_ino);
                }
                else do {
                        if(fread((char *)&dir, sizeof(dir), 1, file) != 1) {
                            goto fail;
                        }
                        stat(dir.d_name, &dd);
                } while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
                fclose(file);

                /* concatenate file name */
                i = -1;
                while (dir.d_name[++i] != 0);
                for(j=off+1; j>0; --j)
                        cwd[j+i+1] = cwd[j];
                off=i+off+1;
                cwd[i+1] = '/';
                for(--i; i>=0; --i)
                        cwd[i+1] = dir.d_name[i];
        } /* end for */

fail:   fclose(file);
        return NULL;
}


#ifdef TEST
main ()
{
        printf ("pwd = %s\n", curdir());
}
#endif TEST

