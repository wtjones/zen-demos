Demonstrates process forking with a non-blocking read.

```
 docker build --rm -f Dockerfile -t ubuntu:dev .
```

```
docker run --rm -it -v $(pwd):/usr/proj -w /usr/proj ubuntu:dev ./build.sh
docker run --rm -it -v $(pwd):/usr/proj -w /usr/proj ubuntu:dev ./run.sh
```

## Resources

https://stackoverflow.com/a/36674490/107161
