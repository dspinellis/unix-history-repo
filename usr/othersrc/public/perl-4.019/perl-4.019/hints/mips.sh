cmd_cflags='optimize="-g"'
perl_cflags='optimize="-g"'
tcmd_cflags='optimize="-g"'
tperl_cflags='optimize="-g"'
d_volatile=undef
d_castneg=undef
cc=cc
libpth="/usr/lib/cmplrs/cc $libpth"
groupstype=int
nm_opts='-B'
case $PATH in
*bsd*:/bin:*) cat <<END
NOTE:  Some people have reported having much better luck with Mips CC than
with the BSD cc.  Put /bin first in your PATH if you have difficulties.
END
;;
esac
