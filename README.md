# brachypode-results

This repository contains the results of the analyses of the [brachypode](https://github.com/rscherrer/brachypode) project (both with deterministic tools and simulations).

## Specifications

The simulations present in this repository were generated with brachypode version 1.0.

The executable was compiled on Ubuntu LTS 20.04 using the GNU C++ compiler (g++) 9.4.0 and CMake 3.16.3, with build options specified in the following CMake configuration file: 

```cmake
# CMakeLists.txt

# CMake
cmake_minimum_required(VERSION 3.16)

# Project name
project(brachypode)

# C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED)

# Boilerplate
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR})
set(CMAKE_INSTALL_PREFIX ${CMAKE_SOURCE_DIR})

# Source code
add_subdirectory(src)
```

See the [repository](https://github.com/rscherrer/brachypode) of the source code for an example setup on how to build the program according to these specifications. 
