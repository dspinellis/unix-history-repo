/* fake portable I/O routines, for those
    sites so backward as to not have the
     port. library */

int cin, cout;
extern int fin, fout;

copen( s, c ) char *s; {
  int f;

  if( c == 'r' ){
    fin = f = open( s, 0 );
    }

  else if( c == 'a' ){
    f = open( s, 1 );
    seek( f, 0, 2 );
    }

  else {  /* c == w */
    f = creat( s, 0666 );
    }

  return( f );
  }

cflush(x){ /* fake! sets file to x */
  flush();
  fout = x;
  }

system(){
  error( "The function \"system\" is called" );
  }

cclose(i){
  close(i);
  }

cexit(i){
  flush();
  exit(i);
  }
