echo " "
echo "NOTE: regression test op.read may fail due to an NFS bug in HP/UX."
echo "If so, don't worry about it."
case `(uname -r) 2>/dev/null` in
*3.1*) d_syscall=$undef ;;
*2.1*) libswanted=`echo $libswanted | sed 's/ malloc / /'` ;;
esac
d_index=define
