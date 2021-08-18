export WATCOM=/opt/watcom
export PATH=$WATCOM/binl64:$WATCOM/binl:$PATH
export EDPATH=$WATCOM/eddat
export INCLUDE=$WATCOM/h
export WIPFC=$WATCOM/wipfc

# works
wcl386 -bt=dos -ldos4g dos_main.c

#owcc -b dos4g dos_main.c -march=i386
