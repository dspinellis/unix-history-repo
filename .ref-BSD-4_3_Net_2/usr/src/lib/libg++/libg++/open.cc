#include <open.h>
#pragma implementation

int open_cmd_arg(io_mode i, access_mode a) // decode modes
{
  int arg;
  switch(i)
  {
  case io_readonly:   arg = sio_read;                   break;
  case io_writeonly:  arg = sio_write;                  break;
  case io_readwrite:  arg = sio_readwrite;              break;
  case io_appendonly: arg = sio_append | sio_write;     break;
  case io_append:     arg = sio_append | sio_readwrite; break;
  default:            return -1;
  };
  switch(a)
  {
  case a_createonly:  return arg | sa_createonly;
  case a_create:      return arg | sa_create | sa_truncate;
  case a_useonly:     return arg;
  case a_use:         return arg | sa_create;
  default:            return -1;
  }
}

char* fopen_cmd_arg(io_mode i)
{
  switch(i)
  {
  case io_readonly:  return "r";
  case io_writeonly: return "w";
  case io_readwrite: return "r+";
  case io_appendonly:return "a";
  case io_append:    return "a+";
  default:           return 0;
  }
}

int open_cmd_arg(const char* m)
{
  if (m == 0) return -1;
  else if (m[0] == 'r') return (m[1] == '+')? io_readwrite : io_readonly;
  else if (m[0] == 'w') return (m[1] == '+')? io_readwrite : io_writeonly;
  else if (m[0] == 'a') return (m[1] == '+')? io_append : io_appendonly;
  else  return -1;
}
