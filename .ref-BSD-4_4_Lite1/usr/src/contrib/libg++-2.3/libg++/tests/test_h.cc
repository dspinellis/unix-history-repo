// Use all the g++ headerfiles

// $Author: bothner $
// $Revision: 1.10 $
// $Date: 1992/06/17 23:56:23 $

#include <_G_config.h>
// If we have the old iostream library, it defines _OLD_STREAMS
#include <stream.h>

#include <std.h>

#include <sys/types.h>
#include <sys/time.h>
#if _G_HAVE_SYS_RESOURCE
#include <sys/resource.h>
#endif
#include <sys/file.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/signal.h>
#if _G_HAVE_SYS_SOCKET
#include <sys/socket.h>
#endif
#include <sys/stat.h>
#include <sys/times.h>

#ifdef _OLD_STREAMS
#include <PlotFile.h>
#include <File.h>
#include <Filebuf.h>
#include <Fmodes.h>
#include <filebuf.h>
#include <SFile.h>
#endif

#include <ACG.h>
#include <Fix.h>
#include <MLCG.h>
#include <AllocRing.h>
#include <Binomial.h>
#include <BitSet.h>
#include <BitString.h>
#include <Complex.h>
#include <DiscUnif.h>
#include <Erlang.h>
#include <GetOpt.h>
#include <Fix16.h>
#include <Fix24.h>
#include <Geom.h>
#include <Rational.h>
#include <HypGeom.h>
#include <Integer.h>
#include <Incremental.h>
#include <LogNorm.h>
#include <fcntl.h>
#include <NegExp.h>
#include <Normal.h>
#include <Obstack.h>
#include <Pix.h>
#include <SmplHist.h>
#include <Poisson.h>
#include <RNG.h>
#include <Random.h>
#include <SmplStat.h>
#include <Regex.h>
#include <RndInt.h>
#include <builtin.h>
#include <String.h>
#include <Uniform.h>
#include <Weibull.h>

#include <assert.h>
#include <libc.h>
#include <compare.h>
#include <complex.h>
#include <ctype.h>
#include <errno.h>
#include <generic.h>
#include <grp.h>
#include <istream.h>
#include <getpagesize.h>
#include <time.h>
#include <math.h>
#include <memory.h>
#include <minmax.h>
#include <new.h>
#include <osfcn.h>
#include <ostream.h>
#include <pwd.h>
#include <regex.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <strclass.h>
#include <streambuf.h>
#include <string.h>
#include <swap.h>
#include <unistd.h>
#include <values.h>

main()
{
    cout << "Could include all g++-include files\n";
    exit (0);
}
