# ----------------------------------------------------------------------------------------------
#  Root CMake file for SWAT.
#     Soil and Water Assessment Tool,
#       a public domain model jointly developed by USDA Agricultural Research Service (USDA-ARS)
#       and Texas A&M AgriLife Research, part of The Texas A&M University System.
#     http://swat.tamu.edu/
#
#     Please from the off-tree build directory, invoke:
#       $ cmake <path/to/SWAT>
#
#     This is an unofficial project maintained by Dr. Junzhi Liu and Liangjun Zhu
#       for non-commercial, educational, and scientific purpose.
#
#  Routine testing platforms and compilers include:
#     1. Windows 10 with Visual Studio 2013, Intel Compiler 17.0 Update 4 (ifort 17.0.4.210)
#     2. Windows 10 with mingw64 (GCC-4.9.3), gfortran-4.9.3
#     3. CentOS 6.2 (cluster) with GCC-4.8.4, gfortran-4.8.4
#     4. Red Hat Server 6.2 (cluster), Intel Compiler 12 SP1 (ifort 12.1.0)
#
#  Created by Liangjun Zhu with the help of @SethMMorton (https://github.com/SethMMorton)
#  Contact: zlj@lreis.ac.cn
#  Latest updated: Nov. 1, 2017
# ----------------------------------------------------------------------------------------------
cmake_minimum_required(VERSION 2.8.5)

### SWAT version
set(SWAT_VER_MAJOR 2012)
set(SWAT_VER_MINOR 664)

### Project name and language
set(prjname SWAT${SWAT_VER_MAJOR}rev${SWAT_VER_MINOR})
project(${prjname} Fortran)

### Set cmake find path
set(CMAKE_MODULE_PATH
        ${CMAKE_CURRENT_SOURCE_DIR}/cmake
        ${CMAKE_MODULE_PATH}
        )
INCLUDE(Utils)

### Set default or specified installation directory.
IF (INSTALL_PREFIX)
    SET(CMAKE_INSTALL_PREFIX ${INSTALL_PREFIX} CACHE PATH "Set specified install path" FORCE)
ELSEIF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    SET(CMAKE_INSTALL_PREFIX ${CMAKE_SOURCE_DIR}/bin CACHE PATH "Set default install path" FORCE)
ENDIF ()
SET(INSTALL_DIR ${CMAKE_INSTALL_PREFIX})

### In case of Makefiles if the user does not setup CMAKE_BUILD_TYPE, assume it's Release.
IF(CMAKE_GENERATOR MATCHES "Makefiles|Ninja" AND "${CMAKE_BUILD_TYPE}" STREQUAL "")
    SET(CMAKE_BUILD_TYPE Release)
ENDIF()

### Break in case of popular CMake configuration mistakes.
IF(NOT CMAKE_SIZEOF_VOID_P GREATER 0)
    MESSAGE(FATAL_ERROR "CMake fails to determine the bitness of the target platform.
  Please check your CMake and compiler installation.")
ENDIF()

### Required the support of Fortran 90
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

### Set the compile flags for DEBUG, RELEASE, and TESTING.
INCLUDE(SetFortranFlags)

### One of MPI and OpenMP is allowed to set for parallel computation
OPTION(USE_MPI "Use the MPI library for parallelization" OFF)
OPTION(USE_OPENMP "Use OpenMP for parallelization" OFF)
INCLUDE(SetParallelizationLibrary)

### Use statically or dynamically linkage?
### Global flag to cause add_library to create shared libraries if on, otherwise static library.
OPTION(BUILD_SHARED_LIBS "Build shared libraries (DLLs)." OFF)
### libraries built with hunter link against the dynamic CRT on windows
OPTION(BUILD_WITH_STATIC_CRT "Build against dynamic CRT on windows." ON)
IF(MSVC)
    INCLUDE(CRTLinkage)
ELSE()
    OPTION(ENABLE_STATIC_LINKING "Enable static libraries linking" ON)
    INCLUDE(StaticLinking)
ENDIF()

### There is an error in CMAKE with this flag for pgf90.  Unset it
GET_FILENAME_COMPONENT(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
IF(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
ENDIF(FCNAME STREQUAL "pgf90")

############################################################
# Define the actual files and folders that make up the build
############################################################

### Add subdirectories.
ADD_SUBDIRECTORY(swat_src/src)

############################################################
# Build information
############################################################

STATUS("")
STATUS("  Platform:")
IF(NOT CMAKE_VERSION VERSION_LESS 2.8.11 AND NOT BUILD_INFO_SKIP_TIMESTAMP)
    STRING(TIMESTAMP TIMESTAMP "" UTC)
    IF(TIMESTAMP)
        STATUS("    Timestamp:"    ${TIMESTAMP})
    ENDIF()
ENDIF()
STATUS("    Host:"             ${CMAKE_HOST_SYSTEM_NAME} ${CMAKE_HOST_SYSTEM_VERSION} ${CMAKE_HOST_SYSTEM_PROCESSOR})
IF(CMAKE_CROSSCOMPILING)
    STATUS("    Target:"         ${CMAKE_SYSTEM_NAME} ${CMAKE_SYSTEM_VERSION} ${CMAKE_SYSTEM_PROCESSOR})
ENDIF()
STATUS("    CMake:"            ${CMAKE_VERSION})
STATUS("    CMake generator:"  ${CMAKE_GENERATOR})
STATUS("    CMake build tool:" ${CMAKE_BUILD_TOOL})
IF(MSVC)
    STATUS("    MSVC:"           ${MSVC_VERSION})
ENDIF()
IF(CMAKE_GENERATOR MATCHES Xcode)
    STATUS("    Xcode:"          ${XCODE_VERSION})
ENDIF()
IF(NOT CMAKE_GENERATOR MATCHES "Xcode|Visual Studio")
    STATUS("    Configuration:"  ${CMAKE_BUILD_TYPE})
ENDIF()

### C/C++ options.
IF(CMAKE_CXX_COMPILER_VERSION)
    SET(GEO_COMPILER_STR "${CMAKE_Fortran_COMPILER} ${CMAKE_Fortran_COMPILER_ARG1} (ver ${CMAKE_Fortran_COMPILER_VERSION})")
ELSE()
    SET(GEO_COMPILER_STR "${CMAKE_Fortran_COMPILER} ${CMAKE_Fortran_COMPILER_ARG1}")
ENDIF()
STRING(STRIP "${GEO_COMPILER_STR}" GEO_COMPILER_STR)

STATUS("")
STATUS("  Fortran:")
STATUS("    Fortran Compiler:"           ${GEO_COMPILER_STR})
STATUS("    Fortran flags (Release):"    ${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_RELEASE})
STATUS("    Fortran flags (Debug):"      ${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_DEBUG})
IF(WIN32)
    STATUS("    Linker flags (Release):" ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_EXE_LINKER_FLAGS_RELEASE})
    STATUS("    Linker flags (Debug):"   ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_EXE_LINKER_FLAGS_DEBUG})
ELSE()
    STATUS("    Linker flags (Release):" ${CMAKE_SHARED_LINKER_FLAGS} ${CMAKE_SHARED_LINKER_FLAGS_RELEASE})
    STATUS("    Linker flags (Debug):"   ${CMAKE_SHARED_LINKER_FLAGS} ${CMAKE_SHARED_LINKER_FLAGS_DEBUG})
ENDIF()
### Auxiliary.
STATUS("")
STATUS("  Install path:" ${CMAKE_INSTALL_PREFIX})
STATUS("")