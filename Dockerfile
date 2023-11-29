##
# crazyzlj/swat:alpine
#
# Usage: 
#   > cd SWAT
#   > docker build -t <tag> -f docker/Dockerfile .
#   or build and push for amd64 and arm64 platforms simultanously
#   > docker buildx build --platform linux/amd64,linux/arm64 --push -t crazyzlj/swat:alpine -f docker/Dockerfile .
#
# Copyright 2022 Liang-Jun Zhu <zlj@lreis.ac.cn>

# Use alpine as the build container
ARG ALPINE_VERSION=3.15
FROM alpine:${ALPINE_VERSION} as builder

LABEL maintainer="Liang-Jun Zhu <zlj@lreis.ac.cn>"

# Replace alpine repository source cdn to accelarate access speed; Setup build environment
# RUN sed -i 's/dl-cdn.alpinelinux.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apk/repositories
RUN sed -i 's/dl-cdn.alpinelinux.org/mirrors.aliyun.com/g' /etc/apk/repositories \
    && apk update && apk upgrade \
    && apk add --no-cache cmake gfortran make musl-dev

# Copy source directory
WORKDIR /SWAT
COPY . .

# # Build for release
ARG INSTALL_DIR=/SWAT/dist
RUN cd /SWAT \
    && mkdir build \
    && cd build \
    && cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} \
    && make -j 2 \
    && make install \
    && cd ..

# # Build final image
FROM alpine:${ALPINE_VERSION} as runner

# Replace alpine repository source cdn; Add GNU gfortran library
# RUN sed -i 's/dl-cdn.alpinelinux.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apk/repositories
RUN sed -i 's/dl-cdn.alpinelinux.org/mirrors.aliyun.com/g' /etc/apk/repositories \
    && apk update && apk upgrade \
    && apk add --no-cache libgfortran

# Order layers starting with less frequently varying ones
ARG INSTALL_DIR=/SWAT/dist
COPY --from=builder ${INSTALL_DIR}/bin/ /usr/bin/
