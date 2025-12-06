# OpenCog Unified - Multi-stage Docker Build
# Optimized for size and build performance

# Stage 1: Build Environment
FROM ubuntu:22.04 AS builder

# Prevent interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Install build dependencies
RUN apt-get update && apt-get install -y \
    cmake \
    ninja-build \
    ccache \
    build-essential \
    cxxtest \
    binutils-dev \
    libiberty-dev \
    libboost-all-dev \
    guile-3.0-dev \
    libgmp-dev \
    librocksdb-dev \
    cython3 \
    python3-dev \
    python3-pip \
    python3-nose \
    git \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Set up ccache
ENV CCACHE_DIR=/ccache
ENV PATH="/usr/lib/ccache:$PATH"

# Create workspace
WORKDIR /workspace

# Copy source code
COPY . /workspace/

# Configure and build
RUN cmake -B build -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DCMAKE_C_COMPILER_LAUNCHER=ccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
    -DBUILD_TESTING=OFF

RUN cmake --build build -j$(nproc)

# Install to staging directory
RUN DESTDIR=/staging cmake --install build

# Stage 2: Runtime Environment
FROM ubuntu:22.04 AS runtime

# Install runtime dependencies only
RUN apt-get update && apt-get install -y \
    libboost-filesystem1.74.0 \
    libboost-program-options1.74.0 \
    libboost-system1.74.0 \
    libboost-thread1.74.0 \
    libboost-regex1.74.0 \
    guile-3.0 \
    librocksdb6.11 \
    python3 \
    && rm -rf /var/lib/apt/lists/*

# Copy built artifacts from builder
COPY --from=builder /staging/usr/local /usr/local

# Update library cache
RUN ldconfig

# Create non-root user
RUN useradd -m -s /bin/bash opencog && \
    chown -R opencog:opencog /usr/local

USER opencog
WORKDIR /home/opencog

# Set environment
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
ENV GUILE_LOAD_PATH=/usr/local/share/guile/site/3.0:$GUILE_LOAD_PATH

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD [ "test", "-f", "/usr/local/lib/libcogutil.so" ]

# Default command
CMD ["/bin/bash"]

# Stage 3: Development Environment (optional)
FROM builder AS development

# Install development tools
RUN apt-get update && apt-get install -y \
    gdb \
    valgrind \
    clang-format \
    clang-tidy \
    cppcheck \
    doxygen \
    graphviz \
    vim \
    && rm -rf /var/lib/apt/lists/*

# Install Python development tools
RUN pip3 install --no-cache-dir \
    pylint \
    flake8 \
    black \
    mypy \
    pytest

WORKDIR /workspace

CMD ["/bin/bash"]

# Metadata
LABEL maintainer="OpenCog Unified Team"
LABEL description="OpenCog Unified - Advanced Cognitive Computing Framework"
LABEL version="1.0.0"
