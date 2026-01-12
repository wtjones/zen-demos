FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN set -ex; \
	apt-get update; \
	apt-get install -y --no-install-recommends \
	build-essential \
	ninja-build \
    cmake \
    xz-utils \
    unzip

RUN set -ex; \
	apt-get install -y --no-install-recommends \
	python3 \
	python3-pip \
	python3-venv \
	ca-certificates \
	wget \
	&& rm -rf /ver/lib/apt/lists/*

WORKDIR /opt

# Download and install the correct MIPS toolchain (mipsel-none-elf)
RUN wget https://github.com/Lameguy64/PSn00bSDK/releases/download/v0.24/PSn00bSDK-0.24-Linux.zip && \
   unzip PSn00bSDK-0.24-Linux.zip -d /opt && \
   rm PSn00bSDK-0.24-Linux.zip

ENV PATH="/opt/PSn00bSDK-0.24-Linux/bin:/opt/PSn00bSDK-0.24-Linux/include:${PATH}" \
   PSN00BSDK_LIBS="/opt/PSn00bSDK-0.24-Linux/lib/libpsn00b"

WORKDIR /work
COPY tools/psx/tools/requirements.txt tools/requirements.txt
RUN python3 -m venv env && \
    env/bin/pip install --upgrade pip && \
    env/bin/pip install -r tools/requirements.txt

ENV PATH ="/work/env/bin:$PATH"

CMD ["/bin/bash"]
