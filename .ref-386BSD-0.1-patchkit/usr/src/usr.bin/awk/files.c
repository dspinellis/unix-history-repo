
/********************************************
files.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*$Log:	files.c,v $
 * Revision 5.1  91/12/05  07:56:00  brennan
 * 1.1 pre-release
 * 
*/

/* files.c */

#include "mawk.h"
#include "files.h"
#include "memory.h"
#include "fin.h"


#ifdef  V7
#include  <sgtty.h>    /* defines FIOCLEX */
#endif
  

#if  HAVE_FCNTL_H  

#include <fcntl.h>
#define  CLOSE_ON_EXEC(fd)   (void) fcntl(fd, F_SETFD, 1)

#else
#define  CLOSE_ON_EXEC(fd) ioctl(fd, FIOCLEX, (PTR) 0)
#endif


/* We store dynamically created files on a linked linear
   list with move to the front (big surprise)  */

typedef struct file {
struct file *link ;
STRING  *name ;
short type ;
int pid ;  /* we need to wait() when we close an out pipe */
           /* holds temp file index under MSDOS */
PTR   ptr ;  /* FIN*   or  FILE*   */
}  FILE_NODE ;

static FILE_NODE *file_list ;

void set_stderr()
{
  file_list = ZMALLOC(FILE_NODE) ;
  file_list->link = (FILE_NODE*) 0 ;
  file_list->type = F_TRUNC ;
  file_list->name = new_STRING("/dev/stderr") ;
  file_list->ptr = (PTR) stderr ;
}

PTR  file_find( sval, type )
  STRING *sval ;
  int type ;
{ register FILE_NODE *p = file_list ;
  FILE_NODE *q = (FILE_NODE *) 0 ;
  char *name = sval->str ;
  char *ostr ;

  while (1)
  {
    if ( !p )   /* open a new one */
    {
      p = (FILE_NODE*) zmalloc(sizeof(FILE_NODE)) ;
      switch( p->type = type )
      {
        case  F_TRUNC :
#if MSDOS && NO_BINMODE==0
            ostr = (binmode()&2) ? "wb" : "w" ;
#else
            ostr = "w" ;
#endif
            if ( !(p->ptr = (PTR) fopen(name, ostr)) )
                goto out_failure ;
            break ;

        case  F_APPEND :
#if MSDOS && NO_BINMODE==0
            ostr = (binmode()&2) ? "ab" : "a" ;
#else
            ostr = "a" ;
#endif
            if ( !(p->ptr = (PTR) fopen(name, ostr)) )
                goto out_failure ;
            break ;

        case  F_IN  :
            if ( !(p->ptr = (PTR) FINopen(name, 0)) )
            { zfree(p, sizeof(FILE_NODE)) ; return (PTR) 0 ; }
            break ;

        case  PIPE_OUT :
        case  PIPE_IN :

#if    HAVE_REAL_PIPES || HAVE_FAKE_PIPES 

            if ( !(p->ptr = get_pipe(name, type, &p->pid)) )
                if ( type == PIPE_OUT ) goto out_failure ;
                else
                { zfree(p, sizeof(FILE_NODE) ) ;
                  return (PTR) 0 ;
                }
#else
         rt_error("pipes not supported") ;
#endif
            break ;

#ifdef  DEBUG
        default :
            bozo("bad file type") ;
#endif
      }
      /* successful open */
      p->name = sval ;
      sval->ref_cnt++ ;
      break ; /* while loop */
    }

    if ( strcmp(name, p->name->str) == 0 )
    { /* no distinction between F_APPEND and F_TRUNC here */
      if ( p->type != type && 
           (p->type < F_APPEND || type < F_APPEND))  goto type_failure ;
      if ( !q )  /*at front of list */
          return  p->ptr ;
      /* delete from list for move to front */
      q->link = p->link ;
      break ;
    }
    q = p ; p = p->link ;
  }

  /* put p at the front of the list */
  p->link = file_list ;
  return  (PTR) (file_list = p)->ptr ;

out_failure:
  errmsg(errno, "cannot open \"%s\" for output", name) ;
  mawk_exit(1) ;

type_failure :
  rt_error("use of file \"%s\"\n\tis inconsistent with previous use",
           name) ;
}


/* close a file and delete it's node from the file_list */

int  file_close( sval )
  STRING *sval ;
{ register FILE_NODE *p = file_list ;
  FILE_NODE *q = (FILE_NODE *) 0 ; /* trails p */
  char *name = sval->str ;
  int retval = 0 ;

  while ( p )
        if ( strcmp(name,p->name->str) == 0 ) /* found */
        { 
          switch( p->type )
          {
            case  F_TRUNC :
            case  F_APPEND :    
                (void) fclose((FILE *) p->ptr) ;
                break ;

            case  PIPE_OUT :
                (void) fclose((FILE *) p->ptr) ;

#if  HAVE_REAL_PIPES
                retval =  wait_for(p->pid) ;
#endif
#if  HAVE_FAKE_PIPES
                retval = close_fake_outpipe(p->name->str,p->pid) ;
#endif
                break ;

            case F_IN  :
                FINclose((FIN *) p->ptr) ;
                break ;

            case PIPE_IN :
                FINclose((FIN *) p->ptr) ;

#if  HAVE_REAL_PIPES
                retval = wait_for(p->pid) ;
#endif
#if  HAVE_FAKE_PIPES
                (void) unlink(tmp_file_name(p->pid)) ;
#endif
                break ;
          }

          free_STRING(p->name) ;
          if ( q )  q->link = p->link ;
          else  file_list = p->link ;

          zfree(p, sizeof(FILE_NODE)) ;
          return retval ;
        }
        else { q = p ; p = p->link ; }

  /* its not on the list */
  return -1 ;
}

/* When we exit, we need to close and wait for all output pipes */


#if   HAVE_REAL_PIPES

void close_out_pipes()
{ register FILE_NODE *p = file_list ;

  while ( p )
  { if ( p->type == PIPE_OUT )
    { (void) fclose((FILE *) p->ptr) ;  (void) wait_for(p->pid) ; }
    p = p->link ;
  }
}

#else
#if  HAVE_FAKE_PIPES  /* pipes are faked with temp files */

void  close_fake_pipes()
{ register FILE_NODE *p = file_list ;

  /* close input pipes first to free descriptors for children */
  while ( p )
  {
    if ( p->type == PIPE_IN )
    { FINclose((FIN *) p->ptr) ;
      (void) unlink(tmp_file_name(p->pid)) ; 
    }
    p = p->link ;
  }
  /* doit again */
  p = file_list ;
  while ( p )
  {
    if ( p->type == PIPE_OUT )
    {
      (void) fclose(p->ptr) ;
      (void) close_fake_outpipe(p->name->str,p->pid) ;
    }
    p = p->link ;
  }
}
#endif
#endif

/* hardwire to /bin/sh for portability of programs */
char *shell = "/bin/sh" ;

#if  HAVE_REAL_PIPES

PTR get_pipe( name, type, pid_ptr)
  char *name ;
  int type ;
  int *pid_ptr ;
{ int the_pipe[2], local_fd, remote_fd ;

  if ( pipe(the_pipe) == -1 )  return (PTR) 0 ;
  local_fd = the_pipe[type == PIPE_OUT] ;
  remote_fd = the_pipe[type == PIPE_IN ] ;
  /* to keep output ordered correctly */
  fflush(stdout) ; fflush(stderr) ;

  switch( *pid_ptr = fork() )
  { case -1 :  
      (void) close(local_fd) ;
      (void) close(remote_fd) ;
      return (PTR) 0 ;

    case  0 :
        (void) close(local_fd) ;
        (void) close(type == PIPE_IN) ;
        (void) dup( remote_fd ) ;
        (void) close( remote_fd ) ;
        (void) execl(shell, shell, "-c", name, (char *) 0 ) ;
        errmsg(errno, "failed to exec %s -c %s" , shell, name) ;
        fflush(stderr) ;
        _exit(128) ;

    default :
        (void) close(remote_fd) ;
        /* we could deadlock if future child inherit the local fd ,
           set close on exec flag */
        CLOSE_ON_EXEC(local_fd) ;
        break ;
  }

  return  type == PIPE_IN ? (PTR) FINdopen(local_fd, 0) : 
                            (PTR)  fdopen(local_fd, "w")  ;
}
  


/*------------ children ------------------*/

/* we need to wait for children at the end of output pipes to
   complete so we know any files they have created are complete */

/* dead children are kept on this list */

static struct child {
int pid ;
int exit_status ;
struct child *link ;
}  *child_list ;

static  void  add_to_child_list(pid, exit_status)
  int pid, exit_status ;
{ register struct child *p = 
          (struct child *) zmalloc(sizeof(struct child)) ;

  p->pid = pid ; p->exit_status = exit_status ;
  p->link = child_list ; child_list = p ;
}

static struct child *remove_from_child_list(pid)
  int pid ;
{ register struct child *p = child_list ;
  struct child *q = (struct child *) 0 ;

  while ( p )
    if ( p->pid == pid )
    {
        if ( q ) q->link = p->link ;
        else child_list = p->link ;
        break ;
    }
    else { q = p ; p = p->link ; }

  return p ;  /* null return if not in the list */
}
    

/* wait for a specific child to complete and return its 
   exit status 

   If pid is zero, wait for any single child
*/

int wait_for(pid)
  int pid ;
{ int exit_status ;
  struct child *p ;
  int id ;

  if ( pid == 0 )
  {
    id = wait(&exit_status) ;
    add_to_child_list(id, exit_status) ;
  }
  else
  /* see if an earlier wait() caught our child */
  if ( p = remove_from_child_list(pid) ) 
  { exit_status = p->exit_status ;
    ZFREE(p) ;
  }
  else /* need to really wait */
    while ( (id = wait(&exit_status)) != pid )
        if ( id == -1 ) /* can't happen */  bozo("wait_for") ;
        else
        { /* we got the exit status of another child
             put it on the child list and try again */
          add_to_child_list(id, exit_status ) ;
        }

  if ( exit_status & 0xff ) 
       exit_status = 128 + (exit_status & 0xff) ;
  else  exit_status = (exit_status & 0xff00)>>8 ;

  return exit_status ;
}
        
#endif  /* HAVE_REAL_PIPES */
