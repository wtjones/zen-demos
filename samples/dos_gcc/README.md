# DJGPP DOS Sample

## DJGPP Setup

### Get DJGPP

Obtain DJGPP in some manner such as https://github.com/andrewwutw/build-djgpp.

Extract somewhere such as `/opt/djgpp`.

Set variable `DJGPP_PREFIX` in shell or profile:
```
export DJGPP_PREFIX="/opt/djgpp"
```

#### Activate DJGPP

This is optional when using `./run.sh`.

### Enable GCC environment

```
source $DJGPP_PREFIX/setenv
```

### Disable GCC environment

```
source /etc/environment
unset GCC_EXEC_PREFIX
```

## Demo Setup

Create folder ./build if it does not exist.
Place `CWSDPMI.EXE` from https://www.delorie.com/pub/djgpp/current/v2misc/csdpmi7b.zip into folder ./build.

## Run Demo

`./run.sh`
