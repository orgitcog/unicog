#!/bin/bash
set -euo pipefail

# Comprehensive CMake improvements for OpenCog Unified
# Adds modern CMake practices, better error handling, and optimization

echo "🔨 Improving CMake configurations..."

# Function to add modern CMake practices to CMakeLists.txt
improve_cmake_file() {
    local file="$1"
    local backup="${file}.backup"
    
    # Create backup
    cp "$file" "$backup"
    
    # Add CMake version requirement if missing
    if ! grep -q "cmake_minimum_required" "$file"; then
        sed -i '1i cmake_minimum_required(VERSION 3.16 FATAL_ERROR)' "$file"
        echo "  ✅ Added CMake version requirement to $(basename $file)"
    fi
    
    # Add project description if missing
    if grep -q "^project(" "$file" && ! grep -q "DESCRIPTION" "$file"; then
        sed -i 's/project(\([^)]*\))/project(\1 DESCRIPTION "OpenCog Unified Component" LANGUAGES CXX)/' "$file"
        echo "  ✅ Enhanced project() call in $(basename $file)"
    fi
    
    # Add C++ standard if missing
    if ! grep -q "CMAKE_CXX_STANDARD" "$file"; then
        echo "" >> "$file"
        echo "# Set C++ standard" >> "$file"
        echo "set(CMAKE_CXX_STANDARD 17)" >> "$file"
        echo "set(CMAKE_CXX_STANDARD_REQUIRED ON)" >> "$file"
        echo "set(CMAKE_CXX_EXTENSIONS OFF)" >> "$file"
        echo "  ✅ Added C++17 standard to $(basename $file)"
    fi
    
    # Add compiler warnings if missing
    if ! grep -q "CMAKE_CXX_FLAGS.*-Wall" "$file"; then
        echo "" >> "$file"
        echo "# Enable comprehensive warnings" >> "$file"
        echo 'if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")' >> "$file"
        echo '    add_compile_options(-Wall -Wextra -Wpedantic)' >> "$file"
        echo 'endif()' >> "$file"
        echo "  ✅ Added compiler warnings to $(basename $file)"
    fi
}

# Find and improve root CMakeLists.txt
if [ -f "CMakeLists.txt" ]; then
    improve_cmake_file "CMakeLists.txt"
fi

# Improve component CMakeLists.txt files
find . -name "CMakeLists.txt" -type f ! -path "*/build/*" ! -path "*/.git/*" | while read -r cmake_file; do
    if [ -f "$cmake_file" ]; then
        improve_cmake_file "$cmake_file"
    fi
done

echo ""
echo "============================================================"
echo "✅ CMake Improvement Complete!"
echo "============================================================"
echo "Enhanced CMake files with:"
echo "  - Modern CMake version requirements"
echo "  - Project descriptions"
echo "  - C++17 standard enforcement"
echo "  - Comprehensive compiler warnings"
