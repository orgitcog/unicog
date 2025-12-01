#!/bin/bash
# OpenCog Unified Build Script
# Comprehensive build automation with intelligent defaults

set -e  # Exit on error
set -u  # Exit on undefined variable

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
BUILD_TYPE="${BUILD_TYPE:-Release}"
BUILD_DIR="${BUILD_DIR:-build}"
INSTALL_PREFIX="${INSTALL_PREFIX:-/usr/local}"
ENABLE_TESTS="${ENABLE_TESTS:-ON}"
ENABLE_COVERAGE="${ENABLE_COVERAGE:-OFF}"
PARALLEL_JOBS="${PARALLEL_JOBS:-$(nproc)}"
USE_CCACHE="${USE_CCACHE:-ON}"
USE_NINJA="${USE_NINJA:-ON}"

# Functions
print_header() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}"
}

print_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

check_dependencies() {
    print_header "Checking Dependencies"
    
    local missing_deps=()
    
    # Check for required tools
    command -v cmake >/dev/null 2>&1 || missing_deps+=("cmake")
    command -v g++ >/dev/null 2>&1 || missing_deps+=("g++")
    command -v pkg-config >/dev/null 2>&1 || missing_deps+=("pkg-config")
    
    # Check for optional tools
    if [ "$USE_NINJA" = "ON" ]; then
        command -v ninja >/dev/null 2>&1 || print_warning "Ninja not found, falling back to Make"
    fi
    
    if [ "$USE_CCACHE" = "ON" ]; then
        command -v ccache >/dev/null 2>&1 || print_warning "CCache not found, builds will be slower"
    fi
    
    if [ ${#missing_deps[@]} -ne 0 ]; then
        print_error "Missing required dependencies: ${missing_deps[*]}"
        echo ""
        echo "Install with:"
        echo "  Ubuntu/Debian: sudo apt-get install ${missing_deps[*]}"
        exit 1
    fi
    
    print_success "All required dependencies found"
}

configure_build() {
    print_header "Configuring Build"
    
    # Determine generator
    local generator="Unix Makefiles"
    if [ "$USE_NINJA" = "ON" ] && command -v ninja >/dev/null 2>&1; then
        generator="Ninja"
    fi
    
    # Determine compiler launcher
    local compiler_launcher=""
    if [ "$USE_CCACHE" = "ON" ] && command -v ccache >/dev/null 2>&1; then
        compiler_launcher="-DCMAKE_C_COMPILER_LAUNCHER=ccache -DCMAKE_CXX_COMPILER_LAUNCHER=ccache"
    fi
    
    print_info "Build Type: $BUILD_TYPE"
    print_info "Generator: $generator"
    print_info "Parallel Jobs: $PARALLEL_JOBS"
    print_info "Install Prefix: $INSTALL_PREFIX"
    
    # Create build directory
    mkdir -p "$BUILD_DIR"
    
    # Run CMake configuration
    cmake -B "$BUILD_DIR" -G "$generator" \
        -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
        -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
        -DBUILD_TESTING="$ENABLE_TESTS" \
        -DENABLE_COVERAGE="$ENABLE_COVERAGE" \
        -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
        $compiler_launcher
    
    print_success "Configuration complete"
}

build_project() {
    print_header "Building Project"
    
    cmake --build "$BUILD_DIR" --config "$BUILD_TYPE" -j "$PARALLEL_JOBS"
    
    print_success "Build complete"
}

run_tests() {
    if [ "$ENABLE_TESTS" = "ON" ]; then
        print_header "Running Tests"
        
        cd "$BUILD_DIR"
        ctest --output-on-failure --timeout 300 -j "$PARALLEL_JOBS" || {
            print_warning "Some tests failed"
            return 1
        }
        cd ..
        
        print_success "Tests passed"
    else
        print_info "Tests disabled"
    fi
}

install_project() {
    print_header "Installing Project"
    
    if [ "$EUID" -ne 0 ] && [ "$INSTALL_PREFIX" = "/usr/local" ]; then
        print_warning "Installing to $INSTALL_PREFIX requires sudo"
        sudo cmake --install "$BUILD_DIR"
    else
        cmake --install "$BUILD_DIR"
    fi
    
    print_success "Installation complete"
}

generate_coverage() {
    if [ "$ENABLE_COVERAGE" = "ON" ] && [ "$BUILD_TYPE" = "Debug" ]; then
        print_header "Generating Coverage Report"
        
        if command -v lcov >/dev/null 2>&1; then
            cd "$BUILD_DIR"
            lcov --capture --directory . --output-file coverage.info
            lcov --remove coverage.info '/usr/*' --output-file coverage.info
            lcov --list coverage.info
            
            if command -v genhtml >/dev/null 2>&1; then
                genhtml coverage.info --output-directory coverage-html
                print_success "Coverage report generated in $BUILD_DIR/coverage-html"
            fi
            cd ..
        else
            print_warning "lcov not found, skipping coverage report"
        fi
    fi
}

print_summary() {
    print_header "Build Summary"
    
    echo "Build Configuration:"
    echo "  Type: $BUILD_TYPE"
    echo "  Directory: $BUILD_DIR"
    echo "  Install Prefix: $INSTALL_PREFIX"
    echo "  Parallel Jobs: $PARALLEL_JOBS"
    echo ""
    
    if [ -f "$BUILD_DIR/compile_commands.json" ]; then
        print_success "Compile commands database generated"
    fi
    
    if [ "$USE_CCACHE" = "ON" ] && command -v ccache >/dev/null 2>&1; then
        echo ""
        echo "CCache Statistics:"
        ccache -s
    fi
    
    echo ""
    print_success "Build process completed successfully!"
    echo ""
    echo "Next steps:"
    echo "  - Run tests: cd $BUILD_DIR && ctest"
    echo "  - Install: ./build.sh install"
    echo "  - Clean: rm -rf $BUILD_DIR"
}

# Main execution
main() {
    print_header "OpenCog Unified Build System"
    
    # Parse command line arguments
    case "${1:-all}" in
        clean)
            print_info "Cleaning build directory..."
            rm -rf "$BUILD_DIR"
            print_success "Clean complete"
            exit 0
            ;;
        configure)
            check_dependencies
            configure_build
            ;;
        build)
            build_project
            ;;
        test)
            run_tests
            ;;
        install)
            install_project
            ;;
        coverage)
            generate_coverage
            ;;
        all)
            check_dependencies
            configure_build
            build_project
            run_tests
            generate_coverage
            print_summary
            ;;
        *)
            echo "Usage: $0 {all|configure|build|test|install|coverage|clean}"
            echo ""
            echo "Commands:"
            echo "  all       - Full build pipeline (default)"
            echo "  configure - Configure build system"
            echo "  build     - Build project"
            echo "  test      - Run tests"
            echo "  install   - Install project"
            echo "  coverage  - Generate coverage report"
            echo "  clean     - Remove build directory"
            echo ""
            echo "Environment Variables:"
            echo "  BUILD_TYPE      - Release|Debug (default: Release)"
            echo "  BUILD_DIR       - Build directory (default: build)"
            echo "  INSTALL_PREFIX  - Installation prefix (default: /usr/local)"
            echo "  ENABLE_TESTS    - ON|OFF (default: ON)"
            echo "  ENABLE_COVERAGE - ON|OFF (default: OFF)"
            echo "  PARALLEL_JOBS   - Number of parallel jobs (default: nproc)"
            echo "  USE_CCACHE      - ON|OFF (default: ON)"
            echo "  USE_NINJA       - ON|OFF (default: ON)"
            exit 1
            ;;
    esac
}

main "$@"
