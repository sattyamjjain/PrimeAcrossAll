#!/bin/bash

# Name of the Docker image
IMAGE_NAME="prime-across-all-env"
DOCKERFILE="Dockerfile"

# File to store the Dockerfile's previous hash
HASH_FILE=".dockerfile_hash"

# Function to calculate the hash of the Dockerfile
calculate_dockerfile_hash() {
    sha256sum $DOCKERFILE | awk '{print $1}'
}

# Function to build the Docker image
build_docker_image() {
    echo "Building Docker image with support for all languages..."
    docker build -t $IMAGE_NAME .
    if [ $? -eq 0 ]; then
        echo "Docker image built successfully."
        # Store the new hash after successful build
        calculate_dockerfile_hash > $HASH_FILE
    else
        echo "Error building Docker image."
        exit 1
    fi
}

# Function to delete the old Docker image and clean the build cache
delete_old_image() {
    echo "Deleting old Docker image..."
    docker rmi $IMAGE_NAME

    # Clean the Docker build cache
    echo "Cleaning Docker build cache..."
    docker builder prune -f
}

# Check if the Dockerfile exists
if [ ! -f "$DOCKERFILE" ]; then
    echo "Error: Dockerfile not found!"
    exit 1
fi

# Check if the Docker image exists
image_exists=$(docker images -q $IMAGE_NAME)

# Calculate the current Dockerfile hash
current_hash=$(calculate_dockerfile_hash)

# Check if the hash file exists
if [ -f "$HASH_FILE" ]; then
    # Read the stored hash
    stored_hash=$(cat $HASH_FILE)

    # Compare hashes to see if Dockerfile has changed or if image is missing
    if [ "$current_hash" != "$stored_hash" ] || [ -z "$image_exists" ]; then
        echo "Dockerfile has changed or Docker image is missing."
        if docker images | grep -q $IMAGE_NAME; then
            delete_old_image
        fi
        build_docker_image
    else
        echo "Dockerfile has not changed. Using existing Docker image."
    fi
else
    echo "No previous Dockerfile hash found or Docker image is missing. Building Docker image for the first time."
    build_docker_image
fi

# Run Docker container with mounted volumes
echo "Running the Docker container with the mounted code..."
docker run --rm -v $(pwd)/languages:/app/languages -v $(pwd)/scripts:/app/scripts $IMAGE_NAME
