FROM ubuntu:latest

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential cmake ninja-build

ENV PATH="/usr/bin/cmake/bin:${PATH}"
RUN mkdir -p /usr/proj
WORKDIR /usr/proj
ADD run.sh /usr/proj
CMD ["./run.sh"]
