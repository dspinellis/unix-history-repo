setenv BLOCKSIZE 1k
setenv HOME ~root

stty erase ^H crt 
tset \?$TERM

umask 022

echo "Don't login as root, use the su command."
