#!/bin/bash

# Create output directory if it doesn't exist
mkdir -p output

# Build the Docker image if needed
docker-compose build

# Run the Docker container with the provided arguments
docker-compose run --rm pdfmaker "$@"

echo "Output files can be found in the './output' directory" 