# brachypode-results

This repository contains the results of the analyses of the [brachypode](https://github.com/rscherrer/brachypode) project.

## Prerequisites

To reproduce this study the following are needed:

* R version ...

### R packages

* tidyverse
* patchwork
* [brachypoder](https://github.com/rscherrer/brachypoder)
* ...

## Data

The simulations present in this repository were generated with **brachypode version 1.0**.

The executable was compiled on with the following specifications:

* Plaform: Ubuntu LTS 20.04
* Compiler: GNU C++ compiler (g++) 9.4.0
* Build system: CMake 3.16.3

The build options were as specified in the following CMake configuration file: 

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
