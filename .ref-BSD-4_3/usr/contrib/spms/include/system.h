/* $Header$ */

/*
 * System-dependent definitions
 *
 * Author: Peter J. Nicklin
 */
#define FILEXIST(file)		(access(file,0) ? 0 : 1)

#ifdef V4BSD
#include <sys/file.h>
#define CREATE(name,flags,mode)	open(name,flags|FCREAT,mode)
#define FORK()			vfork()
#define MK_DIR(name)		((mkdir(name, 0777) != 0)?pperror(name),1:0)
#define OPEN(name,flags,mode)	open(name,flags,mode)
#define RENAME(from,to)		rename(from,to)
#define RM_DIR(name)		rm_dir(name)
#else
#define O_RDONLY		000
#define O_WRONLY		001
#define O_RDWR			002
#define CREATE(name,flags,mode)	creat(name,mode)
#define FORK()			fork()
#define MK_DIR(name)		mk_dir(name)
#define OPEN(name,flags,mode)	open(name,flags)
#define RENAME(from,to)		unlink(to); link(from,to); unlink(from)
#define RM_DIR(name)		rm_dir(name)
#endif
