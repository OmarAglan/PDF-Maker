FROM haskell:9.2.8-slim AS build

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    libgmp-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Build the project with Stack
RUN stack setup
RUN stack build --copy-bins

# Create a slim runtime image
FROM debian:bullseye-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgmp10 \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    && rm -rf /var/lib/apt/lists/*

# Copy the executable from the build stage
COPY --from=build /root/.local/bin/mediawiki2latex /usr/local/bin/

# Copy required runtime files
COPY --from=build /app/latex /app/latex
COPY --from=build /app/document /app/document
COPY --from=build /app/src/babel /app/src/babel

# Set working directory
WORKDIR /data

# Set environment variables
ENV PATH="/usr/local/bin:${PATH}"

# Default command
ENTRYPOINT ["mediawiki2latex"]
CMD ["--help"] 