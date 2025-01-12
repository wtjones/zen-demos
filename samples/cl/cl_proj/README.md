Run from REPL

```
(load "sample.asd")
(ql:quickload "sample")
(sample:main)
```

Use asdf

```
(load "sample.asd")
(asdf:load-system :sample)
(sample:main "foo" "bar" 444)
```

Use shell script

```
$ /run.sh "foo" 'bar' 444
$ Starting up...
$ Received arguments: (foo bar 444)
```

