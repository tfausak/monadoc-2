FROM amazonlinux:2
ARG STACK_VERSION=2.1.3
WORKDIR /tmp/stack
RUN yum update --assumeyes && yum install --assumeyes gcc gmp-devel gzip make perl postgresql-devel tar xz zip zlib-devel
RUN curl --location --output stack.tgz "https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz"
RUN tar --extract --file stack.tgz --strip-components 1 --wildcards '*/stack'
RUN mv stack /usr/local/bin
