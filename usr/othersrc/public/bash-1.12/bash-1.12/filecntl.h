/* filecntl.h - Definitions to set file descriptors to close-on-exec. */

#if !defined (_FILECNTL_H_)
#define _FILECNTL_H_

#include <fcntl.h>

/* Definitions to set file descriptors to close-on-exec, the Posix way. */
#if !defined (FD_CLOEXEC)
#define FD_CLOEXEC     1
#endif

#define FD_NCLOEXEC    0

#define SET_CLOSE_ON_EXEC(fd)  (fcntl ((fd), F_SETFD, FD_CLOEXEC))
#define SET_OPEN_ON_EXEC(fd)   (fcntl ((fd), F_SETFD, FD_NCLOEXEC))

#endif /* ! _FILECNTL_H_ */
