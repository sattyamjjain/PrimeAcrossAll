# Use Ubuntu as the base image
FROM ubuntu:20.04

# Set environment variables to avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive
ENV GO111MODULE=on

# Install necessary tools and languages
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    wget \
    gpg \
    git \
    clang \
    python3 \
    python3-pip \
    openjdk-11-jdk \
    gcc \
    g++ \
    nodejs \
    npm \
    ruby-full \
    php \
    perl \
    r-base \
    rustc \
    cargo \
    golang-go \
    haskell-platform \
    mono-complete \
    fsharp \
    elixir \
    erlang \
    gnupg \
    libcurl4 \
    libpython3.8 \
    libicu66 \
    libtinfo5 \
    libssl1.1 \
    gnustep \
    gnustep-devel \
    tzdata \
    unzip \
    lua5.3 \
    liblua5.3-dev \
    time \
    gnat \
    nasm \
    ocaml \
    fp-compiler \
    clojure \
    rlwrap \
    clinfo \
    ocl-icd-opencl-dev \
    ocaml \
    ocamlbuild \
    opencl-headers \
    ocl-icd-libopencl1 \
    swi-prolog \
    gfortran \
    gforth \
    sbcl \
    octave \
    micropython \
    racket \
    mysql-server \
    verilator \
    tcl \
    valac \
    iverilog \
    ghdl \
    && rm -rf /var/lib/apt/lists/*

RUN pip3 install matplotlib cirq qiskit vyper

RUN go install github.com/hyperledger/fabric-contract-api-go@latest

# Install Node.js 18.x, TypeScript, and type definitions for Node.js
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g typescript @types/node graphql express express-graphql

# Install Dart
RUN wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | gpg --dearmor > /usr/share/keyrings/dart-archive-keyring.gpg \
    && sh -c 'echo "deb [signed-by=/usr/share/keyrings/dart-archive-keyring.gpg] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main" > /etc/apt/sources.list.d/dart_stable.list' \
    && apt-get update \
    && apt-get install -y dart

# Install Julia
RUN wget https://julialang-s3.julialang.org/bin/linux/x64/1.8/julia-1.8.1-linux-x86_64.tar.gz \
    && tar -xvzf julia-1.8.1-linux-x86_64.tar.gz \
    && mv julia-1.8.1 /opt/julia \
    && ln -s /opt/julia/bin/julia /usr/local/bin/julia \
    && rm julia-1.8.1-linux-x86_64.tar.gz

# Install V
RUN wget https://github.com/vlang/v/releases/download/0.4.8/v_linux.zip \
    && unzip v_linux.zip \
    && rm v_linux.zip \
    && mv v /opt/v \
    && ln -s /opt/v/v /usr/local/bin/v

# Install Swift
RUN wget https://download.swift.org/swift-6.0.1-release/ubuntu2004/swift-6.0.1-RELEASE/swift-6.0.1-RELEASE-ubuntu20.04.tar.gz \
    && tar -xvzf swift-6.0.1-RELEASE-ubuntu20.04.tar.gz \
    && rm swift-6.0.1-RELEASE-ubuntu20.04.tar.gz \
    && mv swift-6.0.1-RELEASE-ubuntu20.04 /opt/swift \
    && ln -s /opt/swift/usr/bin/swift /usr/local/bin/swift

# Install Kotlin
RUN wget https://github.com/JetBrains/kotlin/releases/download/v2.0.20/kotlin-compiler-2.0.20.zip \
    && unzip kotlin-compiler-2.0.20.zip \
    && rm kotlin-compiler-2.0.20.zip \
    && mv kotlinc /usr/local/kotlinc \
    && ln -s /usr/local/kotlinc/bin/kotlinc /usr/local/bin/kotlinc \
    && ln -s /usr/local/kotlinc/bin/kotlin /usr/local/bin/kotlin

# Install Scala and sbt
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list \
    && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x99E82A75642AC823" | apt-key add \
    && apt-get update && apt-get install -y --no-install-recommends scala sbt \
    && rm -rf /var/lib/apt/lists/*

# Install Clojure
RUN curl -O https://download.clojure.org/install/linux-install-1.11.1.1149.sh && \
    chmod +x linux-install-1.11.1.1149.sh && \
    ./linux-install-1.11.1.1149.sh && \
    rm linux-install-1.11.1.1149.sh

# Install EiffelStudio
RUN wget https://ftp.eiffel.com/pub/download/20.05/Eiffel_20.05_gpl_94553-linux-x86-64.tar.bz2 \
    && tar -xjf Eiffel_20.05_gpl_94553-linux-x86-64.tar.bz2 \
    && mv Eiffel_20.05 /opt/eiffelstudio \
    && ln -s /opt/eiffelstudio/studio/spec/linux-x86-64/bin/ec /usr/local/bin/ec \
    && rm Eiffel_20.05_gpl_94553-linux-x86-64.tar.bz2

# Install J software
RUN wget https://www.jsoftware.com/download/j903/install/j903_linux64.tar.gz && \
    tar -xzf j903_linux64.tar.gz && \
    rm j903_linux64.tar.gz && \
    mv j64-901 /opt/j64-903 && \
    ln -s /opt/j64-903/bin/jconsole /usr/local/bin/jconsole

# Install PowerShell
RUN apt-get update && apt-get install -y wget apt-transport-https software-properties-common \
    && wget -q "https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb" \
    && dpkg -i packages-microsoft-prod.deb \
    && apt-get update && apt-get install -y powershell \
    && rm -rf /var/lib/apt/lists/*

# Install .NET SDK for Q#
RUN wget https://dot.net/v1/dotnet-install.sh -O dotnet-install.sh \
    && chmod +x ./dotnet-install.sh \
    && ./dotnet-install.sh --install-dir /usr/share/dotnet --channel 6.0 \
    && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet \
    && dotnet --version

# Install Q# SDK
RUN dotnet new -i Microsoft.Quantum.ProjectTemplates


# Set the working directory inside the container
WORKDIR /app

# Define the volume where the source code will be mounted
VOLUME ["/app/languages", "/app/scripts"]

# Make the benchmark_all.sh or run_all.sh script executable (from the mounted volume)
ENTRYPOINT ["/bin/bash", "-c"]
CMD ["/app/scripts/run_all.sh", "1000000"]
