# Use Ubuntu as the base image
FROM ubuntu:20.04

# Set environment variables to avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Install necessary tools and languages
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    wget \
    git \
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
    libcurl4 \
    libpython3.8 \
    libicu66 \
    libtinfo5 \
    libssl1.1 \
    tzdata \
    unzip \
    && rm -rf /var/lib/apt/lists/*

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

# Set the working directory inside the container
WORKDIR /app

# Define the volume where the source code will be mounted
VOLUME ["/app/languages", "/app/scripts"]

# Make the run_all.sh script executable (from the mounted volume)
CMD ["/bin/bash", "/app/scripts/run_all.sh"]
