# Unofficial collection of SWAT code

[![Build by GCC(gfortran) on Linux/macOS](https://github.com/WatershedModels/SWAT/actions/workflows/cmake_build_gcc.yml/badge.svg?branch=master)](https://github.com/WatershedModels/SWAT/actions/workflows/cmake_build_gcc.yml)
[![Build by IntelFortran(ifort) on Linux](https://github.com/WatershedModels/SWAT/actions/workflows/cmake_build_ifort.yml/badge.svg?branch=master)](https://github.com/WatershedModels/SWAT/actions/workflows/cmake_build_ifort.yml)

## 1. About SWAT (Soil & Water Assessment Tool)

> The Soil & Water Assessment Tool is a small watershed to river basin-scale model used to simulate the quality and quantity of surface and ground water and predict the environmental impact of land use, land management practices, and climate change. SWAT is widely used in assessing soil erosion prevention and control, non-point source pollution control and regional management in watersheds.

Official site: https://swat.tamu.edu/

Official source code: https://bitbucket.org/blacklandgrasslandmodels/swat_development/src

## 2. About this repository

This is **not** an official repository of SWAT. It exists mostly because of personal interests and the lack of official public one. There used to be a excellent unofficial [repository](https://github.com/mlt/swat) created by [mlt](https://github.com/mlt). Unfortunately, it seems to have stopped updating since Apr. 2013.

This repository is an attempt to bring historical releases from official develop team and various enhancement or improvement from scientific community, i.e., the unofficial collection of SWAT code. 

CMake build system is used for cross-platform compiling. The [cmake_fortran_template](https://github.com/SethMMorton/cmake_fortran_template) created by [SethMMorton](https://github.com/SethMMorton) is adopted.

Routine testing is done with gfortran compiler within MinGW64 on MS Windows and Ubuntu GNU/Linux, and Intel Fortran compiler integrated with MS Visual Studio 2013.

1. Windows 10-64bit with Visual Studio 2013, Intel Compiler 17.0 Update 4 (ifort 17.0.4.210)

2. Windows 10-64bit with mingw64 (GCC-9.1.0), gfortran-9.1.0

3. Ubuntu 16.04 LTS with GCC-5.4.0, gfortran-5.4.0

   For the convenience of end-users, executables of debug and release versions under x86 and x64 architectures compiled by VS2013+IntelFortran17.0 can be found in [releases](https://github.com/WatershedModels/SWAT/releases).

Since I do not have enough test data and the associated SWAT input files, I have to say, I only compiled various SWAT versions successfully, but cannot guarantee the validity of running them. So, if you have qualified test data, welcome to contact me (zlj@lreis.ac.cn) for model development and validation and any other purposes.

### 2.1 Branches

+ **master**: Branch of revised official code. Once a new version of SWAT source code available, I will create a new branch and merge it to the master branch! Revisions are only made in typos and cross-platform compilations.
+ **\<MAJOR\>rev\<MINOR\>[.\<PATCH\>]**: SWAT versions, i.e., `2012rev670`, `2012rev622.omp`, etc.

### 2.2. Prerequisite

+ CMake2.8+
+ Windows:
  + Microsoft Visual Studio 2010+ and Intel Fortran compiler (ifort) 12.0+
  + or CLion and mingw64 (with gfortran 4.8+)
+ Linux/macOS:
  + GCC (with gfortran installed) 4.8+
  + ifort 12.0+

### 2.3. Compile procedure

+ common commands

  ```shell
  cd <path to SWAT>
  mkdir build
  cd build
  cmake ..
  make && make install
  ```

+ More details can be found in [wiki](https://github.com/WatershedModels/SWAT/wiki).

### 2.4. Docker images

The SWAT images have been built for linux:arm64 and linux:amd64, and pushed to
the [Docker hub](https://hub.docker.com/r/crazyzlj/swat/tags).

```shell
docker pull crazyzlj/swat:apline-<VERSION_MAJOR>.<VERSION_MINOR>
docker run -it -v /path/to/your/swat-model-data:/swat_data swat<VERSION_MAJOR>.<VERSION_MINOR>.gfort.rel

# For example,
docker pull crazyzlj/swat:apline-2012.682
docker run -it -v /Users/ljzhu/Documents/tmp/swatdata:/swat_data crazyzlj/swat:alpine-2012.682 swat2012.682.gfort.rel
```

# 3. References
+ [Compile and Debug SWAT with gfortran and Eclipse](https://zhiqiangyu.wordpress.com/2014/10/01/compile-and-debug-swat-with-gfortran-and-eclipse/) by [Dr. Zhiqiang Yu](https://github.com/hawklorry)
+ https://github.com/mlt/swat
