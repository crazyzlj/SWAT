cmake_minimum_required(VERSION 2.8)
file(GLOB SWAT_SRCS *.f *.f90)

if (Fortran_COMPILER_NAME MATCHES "ifort.*")
    # ifort

    # reference https://software.intel.com/en-us/node/678225
    IF (MSVC)
        set_source_files_properties(
                ${SWAT_SRCS}
                PROPERTIES COMPILE_FLAGS /4L132)
    ELSE ()
        set_source_files_properties(
                ${SWAT_SRCS}
                PROPERTIES COMPILE_FLAGS -132)
    ENDIF ()
else ()
    set_source_files_properties(
            ${SWAT_SRCS}
            PROPERTIES COMPILE_FLAGS -ffree-line-length-none
            COMPILE_FLAGS -ffixed-line-length-none)
endif ()
add_executable(swat ${SWAT_SRCS})
#set(PATCH ${SWAT_PKG_VER_PATCH})

set_target_properties(swat PROPERTIES
        RELEASE_POSTFIX "${SWAT_VER_MAJOR}Rev${SWAT_VER_MINOR}Rel"
        DEBUG_POSTFIX "${SWAT_VER_MAJOR}Rev${SWAT_VER_MINOR}Dbg"
        PGO_POSTFIX "${SWAT_VER_MAJOR}Rev${SWAT_VER_MINOR}RelPGO"
        IPGO_POSTFIX "${SWAT_VER_MAJOR}Rev${SWAT_VER_MINOR}RelPGO"
        )

install(TARGETS swat RUNTIME DESTINATION ${INSTALL_DIR})

# Profile guided optimization

if (CMAKE_BUILD_TYPE MATCHES "PGO")
    if (CMAKE_BUILD_TYPE MATCHES "I")
        set(PROF_DIR "-prof-dir")
        set(PROF_GEN "-prof-gen")
        set(PROF_USE "-prof-use")
    else ()# GCC
        set(PROF_DIR "-fprofile-dir")
        set(PROF_GEN "-fprofile-generate")
        set(PROF_USE "-fprofile-use")
    endif ()

    set(EXTRA_LINK_FLAGS "${PROF_DIR}=${CMAKE_BINARY_DIR}/profile")
    set(EXTRA_COMPILE_FLAGS "${EXTRA_LINK_FLAGS}")

    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/profile)

    add_executable(swat0 ${SWAT_SRCS})
    set_target_properties(swat0 PROPERTIES
            COMPILE_FLAGS "${EXTRA_COMPILE_FLAGS} ${PROF_GEN}"
            LINK_FLAGS "${EXTRA_LINK_FLAGS} ${PROF_GEN}")

    add_custom_target(swat0_run
            COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/profile/CMakeFiles/swat0.dir
            COMMAND ${CMAKE_CURRENT_BINARY_DIR}/swat0
            COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_BINARY_DIR}/profile/CMakeFiles/swat.dir
            COMMAND ${CMAKE_COMMAND} -E rename ${CMAKE_BINARY_DIR}/profile/CMakeFiles/swat0.dir ${CMAKE_BINARY_DIR}/profile/CMakeFiles/swat.dir
            DEPENDS swat0
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/TxtInOut)

    set_target_properties(swat PROPERTIES
            COMPILE_FLAGS "${EXTRA_COMPILE_FLAGS} ${PROF_USE}"
            LINK_FLAGS "${EXTRA_LINK_FLAGS} ${PROF_USE}")
    add_dependencies(swat swat0_run)

endif ()
