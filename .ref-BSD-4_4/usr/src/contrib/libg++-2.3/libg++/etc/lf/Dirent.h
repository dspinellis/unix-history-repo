/* Define a portable UNIX directory-entry manipulation interface. 

   This code is heavily based upon Doug Gwyn's public domain directory-access
   routines.  Hacked into C++ conformance by Doug Schmidt (schmidt@ics.uci.edu). */

#include <builtin.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#ifdef rewinddir
#undef rewinddir
#endif

class Dirent
{
private:
  DIR *dirp;

public:
                 Dirent (char *dirname);
                ~Dirent (void);
  struct dirent *readdir (void);
  void           opendir (char *filename);
  void           closedir (void);
  long           telldir (void);
  void           seekdir (long loc);
  void           rewinddir (void);
};

// error handlers

extern void  verbose_Dirent_error_handler(const char*);
extern void  quiet_Dirent_error_handler(const char*);
extern void  fatal_Dirent_error_handler(const char*);
extern one_arg_error_handler_t Dirent_error_handler;
extern one_arg_error_handler_t set_Dirent_error_handler(one_arg_error_handler_t);

// OPTIMIZE

#ifdef __OPTIMIZE__

inline 
Dirent::Dirent (char *dirname) 
{
  if ((dirp = ::opendir (dirname)) == 0)
    (*Dirent_error_handler) ("Dirent::Dirent");
}

inline 
Dirent::~Dirent (void)
{
  ::closedir (dirp);
}

inline void
Dirent::opendir (char *dirname) 
{
  if ((dirp = ::opendir (dirname)) == 0)
    (*Dirent_error_handler) ("Dirent::Dirent");
}

inline struct dirent *
Dirent::readdir (void)
{
  return ::readdir (dirp);
}

inline void
Dirent::closedir (void)
{
 ::closedir (dirp);
}

inline void
Dirent::rewinddir (void)
{
  ::seekdir (dirp, long (0));
}

inline void
Dirent::seekdir (long loc)
{
  ::seekdir (dirp, loc);
}

inline long
Dirent::telldir (void)
{
  return ::telldir (dirp);
}

#endif // __OPTIMIZE__
