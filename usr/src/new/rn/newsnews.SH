case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting newsnews (with variable substitutions)"
cat >newsnews <<!GROK!THIS!
			*** NEWS NEWS ***

Welcome to rn.  There are more options to rn than you want to think about, so
we won't list them here.  If you want to find out about them, read the manual
page(s).  There are some important things to remember, though:

  * Rn is not a modified readnews.  Don't expect the commands to be identical.
  * Rn runs in cbreak mode.  This means you don't have to type carriage return
    on single character commands.  (There are some multi-character commands.)
  * At ANY prompt in rn, you may type 'h' for help.  There are many different
    help menus, depending on where you are in rn.  Typing <esc>h in the
    middle of a multi-character command will list escape substitutions.
  * Typing a space to any prompt means to do the normal thing.  You could
    spend all day reading news and never hit anything but the space bar.

This particular message comes from $rnlib/newsnews.  You will only
see it once.  You news administrator should feel free to substitute his or
her own message whenever something new happens to rn, and then the file
will again be displayed, just once for each person.

Larry Wall	sdcrdcf!lwall
!GROK!THIS!
$eunicefix newsnews
