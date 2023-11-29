### Set primary name of the executable SWAT
set(swatexe swat)

### Get source file lists of different formats
file(GLOB F77SRCS *.f)
file(GLOB F90SRCS *.f90)

### Set Fortran line format for source file

GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if(${Fortran_COMPILER_NAME} MATCHES "ifort.*" OR ${Fortran_COMPILER_NAME} MATCHES "icl.*")
    # ifort, reference https://software.intel.com/en-us/node/678225
    if(MSVC) # ifort integrated in Microsoft Visual Studio
        set(FLAG_L72 /4L72)
        set(FLAG_L80 /4L80)
        set(FLAG_LLONG /4L132)
    else()
        set(FLAG_L72 -72)
        set(FLAG_L80 -80)
        set(FLAG_LLONG -132)
    endif()
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Cray") 
    set(FLAG_L72 -N72)
    set(FLAG_L80 -N80)
    set(FLAG_LLONG -N132)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Flang")
    set(FLAG_L72 -Mextend)
    set(FLAG_L80 -Mextend)
    set(FLAG_LLONG -Mextend)
else() # gfortran tested only
    set(FLAG_L72 -ffixed-line-length-72)
    set(FLAG_L80 -ffixed-line-length-80)
    set(FLAG_LLONG -ffixed-line-length-none)
endif()

## Special sources that have fixed length of 72
set(LEN72_SRCS "grow" "tran")
## Special sources that have long length of 132 but retained the suffix of "*.f"
set(LENLONG_SRCS "subbasin")
foreach(F77FILE ${F77SRCS})
    get_filename_component(CORENAME ${F77FILE} NAME_WE)
    list(FIND LEN72_SRCS ${CORENAME} _FOUND_LEN72)
    list(FIND LENLONG_SRCS ${CORENAME} _FOUND_LENLONG)
    if(${_FOUND_LEN72} GREATER -1)
        set_source_files_properties(${F77FILE} PROPERTIES COMPILE_FLAGS ${FLAG_L72})
    elseif(${_FOUND_LENLONG} GREATER -1)
        set_source_files_properties(${F77FILE} PROPERTIES COMPILE_FLAGS ${FLAG_LLONG})
    else()
        set_source_files_properties(${F77FILE} PROPERTIES COMPILE_FLAGS ${FLAG_L80})
    endif()
endforeach()

foreach(F90FILE ${F90SRCS})
    set_source_files_properties(${F90FILE} PROPERTIES COMPILE_FLAGS ${FLAG_LLONG})
endforeach()

### Customize compile process according to versions of 'main.f' for gfortran
file(READ ${CMAKE_SOURCE_DIR}/src/main.f First_Line_Of_Main_File OFFSET 0 LIMIT 26)
## Situation 1: The first line of main.f is "include 'modparm.f'"
if((NOT MSVC) AND (${First_Line_Of_Main_File} MATCHES "      include 'modparm.f'"))
    if (${CMAKE_BUILD_TYPE} STREQUAL "RELEASE")
        separate_arguments(Compile_Flags_List UNIX_COMMAND "${CMAKE_Fortran_FLAGS_RELEASE}")
    else ()
        separate_arguments(Compile_Flags_List UNIX_COMMAND "${CMAKE_Fortran_FLAGS_DEBUG}")
    endif ()
    # Build main.f first which includes modparm.f (i.e., parm.mod)
    add_custom_command(OUTPUT main.o
                       COMMAND ${CMAKE_Fortran_COMPILER} ${Compile_Flags_List} ${FLAG_L80} -c
                               ${CMAKE_CURRENT_SOURCE_DIR}/modparm.f ${CMAKE_CURRENT_SOURCE_DIR}/main.f)
    # Build other source files that depend on main.o
    foreach(SRCFILE ${F77SRCS} ${F90SRCS})
        get_filename_component(CORENAME ${SRCFILE} NAME_WE)
        get_filename_component(ext ${SRCFILE} EXT)
        # Excludes main.f and modparm.f
        if(${CORENAME} STREQUAL "main" OR ${CORENAME} STREQUAL "modparm")
            continue()
        endif()
        # Set compile flag according to Fortran line format. These should be consistent with settings above.
        if(${ext} STREQUAL ".f")
            set(Format_Flag ${FLAG_L80})
        else()
            set(Format_Flag ${FLAG_LLONG})
        endif()
        list(FIND LEN72_SRCS ${CORENAME} _FOUND_LEN72)
        list(FIND LENLONG_SRCS ${CORENAME} _FOUND_LENLONG)
        if(${_FOUND_LEN72} GREATER -1)
            set(Format_Flag ${FLAG_L72})
        elseif(${_FOUND_LENLONG} GREATER -1)
            set(Format_Flag ${FLAG_LLONG})
        endif()
        add_custom_command(OUTPUT ${CORENAME}.o
                           COMMAND ${CMAKE_Fortran_COMPILER} ${Compile_Flags_List} ${Format_Flag} -c
                           ${SRCFILE} DEPENDS main.o)
        list(APPEND swat_obj ${CORENAME}.o)
    endforeach()
    list(SORT swat_obj)
    # Add executable built by objects
    add_executable(${swatexe} ${swat_obj} main.o)
## Situation 2: The first line of main.f is not "include 'modparm.f'", the command is same for ifort(MSVC) and gfortran
else()
    add_executable(${swatexe} ${F77SRCS} ${F90SRCS})
endif()

### Set exact name of SWAT executable according to versions and build type
set_target_properties(${swatexe} PROPERTIES
        LINKER_LANGUAGE Fortran
        RELEASE_POSTFIX "${SWAT_VER_STR}.${TOOL_CHAIN}.rel${CMAKE_BUILD_BIT}"
        DEBUG_POSTFIX "${SWAT_VER_STR}.${TOOL_CHAIN}.dbg${CMAKE_BUILD_BIT}"
        PGO_POSTFIX "${SWAT_VER_STR}.${TOOL_CHAIN}.relpgo${CMAKE_BUILD_BIT}"
        IPGO_POSTFIX "${SWAT_VER_STR}.${TOOL_CHAIN}.relpgo${CMAKE_BUILD_BIT}"
        )

### Set installation directory
install(TARGETS ${swatexe} RUNTIME DESTINATION ${INSTALL_DIR}/bin)

        