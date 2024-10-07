# Stage 1: Base dependencies
FROM ubuntu:22.04 AS base

ENV DEBIAN_FRONTEND=noninteractive
ENV DOTNET_ROOT=/usr/share/dotnet

# Install common dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    wget \
    gnupg \
    ca-certificates \
    unzip \
    software-properties-common \
    libc6 \
    && rm -rf /var/lib/apt/lists/*

# Stage 2: Python
FROM base AS python

RUN apt-get update && apt-get install -y python3 python3-venv python3-pip \
    && python3 -m venv /opt/venv \
    && /opt/venv/bin/pip install --no-cache-dir matplotlib cirq qiskit vyper

ENV PATH="/opt/venv/bin:$PATH"

# Stage 3: Node.js
FROM base AS node

RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g typescript @types/node graphql express express-graphql

ENV PATH="$PATH:/usr/local/bin"

# Stage 4: Go
FROM base AS golang

# Install Go from the official binary distribution
RUN wget https://golang.org/dl/go1.20.6.linux-amd64.tar.gz \
    && tar -C /usr/local -xzf go1.20.6.linux-amd64.tar.gz \
    && rm go1.20.6.linux-amd64.tar.gz

ENV GOROOT=/usr/local/go
ENV GOPATH=/go
ENV PATH=$PATH:$GOROOT/bin:$GOPATH/bin

# Stage 5: Dart
FROM base AS dart

RUN wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | gpg --dearmor > /usr/share/keyrings/dart.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/dart.gpg] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main" > /etc/apt/sources.list.d/dart_stable.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends dart \
    && rm -rf /var/lib/apt/lists/*

# Stage 6: Final image
FROM base

# Install git in the final stage
RUN apt-get update && apt-get install -y --no-install-recommends git \
    && rm -rf /var/lib/apt/lists/*

# Copy installations from previous stages
COPY --from=python /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

COPY --from=node /usr/local/ /usr/local/
COPY --from=node /usr/bin/node /usr/bin/node
COPY --from=node /usr/bin/npm /usr/bin/npm
COPY --from=node /usr/lib/node_modules /usr/lib/node_modules
ENV PATH="/usr/local/bin:$PATH"

COPY --from=golang /usr/local/go /usr/local/go
COPY --from=golang /go /go
ENV GOROOT=/usr/local/go
ENV GOPATH=/go
ENV PATH=$PATH:$GOROOT/bin:$GOPATH/bin

COPY --from=dart /usr/lib/dart /usr/lib/dart
ENV PATH="$PATH:/usr/lib/dart/bin"

# Install Java
RUN apt-get update && apt-get install -y --no-install-recommends openjdk-11-jdk \
    && rm -rf /var/lib/apt/lists/*

# Install Rust
RUN apt-get update && apt-get install -y --no-install-recommends rustc cargo \
    && rm -rf /var/lib/apt/lists/*

# Install Ruby
RUN apt-get update && apt-get install -y --no-install-recommends ruby-full \
    && rm -rf /var/lib/apt/lists/*

# Install PHP
RUN apt-get update && apt-get install -y --no-install-recommends php \
    && rm -rf /var/lib/apt/lists/*

# Install Perl
RUN apt-get update && apt-get install -y --no-install-recommends perl \
    && rm -rf /var/lib/apt/lists/*

# Install R
RUN apt-get update && apt-get install -y --no-install-recommends r-base \
    && rm -rf /var/lib/apt/lists/*

# Install Haskell
RUN apt-get update && apt-get install -y --no-install-recommends ghc cabal-install \
    && rm -rf /var/lib/apt/lists/*

# Install Elixir and Erlang
RUN apt-get update && apt-get install -y --no-install-recommends elixir erlang \
    && rm -rf /var/lib/apt/lists/*

# Install Lua
RUN apt-get update && apt-get install -y --no-install-recommends lua5.3 liblua5.3-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Swift
RUN wget https://download.swift.org/swift-6.0.1-release/ubuntu2204/swift-6.0.1-RELEASE/swift-6.0.1-RELEASE-ubuntu22.04.tar.gz \
    && tar -xvzf swift-6.0.1-RELEASE-ubuntu22.04.tar.gz \
    && mv swift-6.0.1-RELEASE-ubuntu22.04 /opt/swift \
    && ln -s /opt/swift/usr/bin/swift /usr/local/bin/swift \
    && rm swift-6.0.1-RELEASE-ubuntu22.04.tar.gz

# Install Kotlin
RUN wget https://github.com/JetBrains/kotlin/releases/download/v1.9.10/kotlin-compiler-1.9.10.zip \
    && unzip kotlin-compiler-1.9.10.zip \
    && rm kotlin-compiler-1.9.10.zip \
    && mv kotlinc /usr/local/kotlinc \
    && ln -s /usr/local/kotlinc/bin/kotlinc /usr/local/bin/kotlinc \
    && ln -s /usr/local/kotlinc/bin/kotlin /usr/local/bin/kotlin

# Install Scala and sbt
RUN apt-get update && apt-get install -y --no-install-recommends gnupg2 curl \
    && echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list \
    && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x99E82A75642AC823" | apt-key add - \
    && apt-get update \
    && apt-get install -y --no-install-recommends scala sbt \
    && rm -rf /var/lib/apt/lists/*

# Install .NET SDK
RUN wget https://dot.net/v1/dotnet-install.sh -O dotnet-install.sh \
    && chmod +x dotnet-install.sh \
    && ./dotnet-install.sh --install-dir /usr/share/dotnet --channel 6.0 \
    && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet \
    && rm dotnet-install.sh

# Install Q# SDK
RUN dotnet new -i Microsoft.Quantum.ProjectTemplates

# Install Julia
RUN wget https://julialang-s3.julialang.org/bin/linux/x64/1.9/julia-1.9.3-linux-x86_64.tar.gz \
    && tar -xvzf julia-1.9.3-linux-x86_64.tar.gz \
    && mv julia-1.9.3 /opt/julia \
    && ln -s /opt/julia/bin/julia /usr/local/bin/julia \
    && rm julia-1.9.3-linux-x86_64.tar.gz

# Install V language
RUN apt-get update && apt-get install -y libssl-dev libgc-dev \
    && rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/vlang/v.git /opt/vlang \
    && cd /opt/vlang \
    && make \
    && ln -s /opt/vlang/v /usr/local/bin/v

# Install PowerShell
RUN wget -q https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    && apt-get update \
    && apt-get install -y --no-install-recommends powershell \
    && rm packages-microsoft-prod.deb \
    && rm -rf /var/lib/apt/lists/*

# Install EiffelStudio
RUN apt-get update && apt-get install -y wget bzip2 xdg-utils libgtk2.0-0 libxt6 libcanberra-gtk-module \
    && rm -rf /var/lib/apt/lists/*
RUN wget https://ftp.eiffel.com/pub/download/24.05/Eiffel_24.05_rev_107822-linux-x86-64.tar.bz2 \
    && tar -xjf Eiffel_24.05_rev_107822-linux-x86-64.tar.bz2 \
    && mv Eiffel_24.05 /opt/eiffelstudio \
    && ln -s /opt/eiffelstudio/studio/spec/linux-x86-64/bin/ec /usr/local/bin/ec \
    && rm Eiffel_24.05_rev_107822-linux-x86-64.tar.bz2

ENV ISE_EIFFEL=/opt/eiffelstudio
ENV ISE_PLATFORM=linux-x86-64
ENV PATH="$PATH:$ISE_EIFFEL/studio/spec/$ISE_PLATFORM/bin"

# Install Objective-C Compiler
RUN apt-get update && apt-get install -y gcc gobjc gnustep gnustep-devel \
    && rm -rf /var/lib/apt/lists/*

# Install F#
RUN apt-get update && apt-get install -y fsharp \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y rlwrap \
    && rm -rf /var/lib/apt/lists/*

# Install Clojure
RUN curl -O https://download.clojure.org/install/linux-install-1.11.1.1149.sh \
    && chmod +x linux-install-1.11.1.1149.sh \
    && ./linux-install-1.11.1.1149.sh \
    && rm linux-install-1.11.1.1149.sh

# Install SAS (Note: SAS is proprietary; we can install GNU PSPP as an alternative)
RUN apt-get update && apt-get install -y pspp \
    && rm -rf /var/lib/apt/lists/*

# Install MySQL client for SQL implementation
RUN apt-get update && apt-get install -y default-mysql-client sqlite3 \
    && rm -rf /var/lib/apt/lists/*

# Install Verilator for SystemVerilog
RUN apt-get update && apt-get install -y verilator \
    && rm -rf /var/lib/apt/lists/*

# Install other required languages and tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    gnat \
    nasm \
    ocaml \
    fp-compiler \
    swi-prolog \
    gfortran \
    gforth \
    sbcl \
    octave \
    micropython \
    racket \
    tcl \
    valac \
    iverilog \
    ghdl \
    mono-complete \
    default-jre \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /app

# Define volumes
VOLUME ["/app/languages", "/app/scripts"]

# Set the entry point
ENTRYPOINT ["/bin/bash", "-c"]
CMD ["/app/scripts/run_all.sh", "1000000"]
