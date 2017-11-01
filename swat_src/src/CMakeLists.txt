file(GLOB SWAT_SRCS *.f *.f90)

IF (${CMAKE_Fortran_COMPILER} MATCHES "ifort.*")
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
ELSE ()
    set_source_files_properties(
            ${SWAT_SRCS}
            PROPERTIES COMPILE_FLAGS -ffree-line-length-none
            COMPILE_FLAGS -ffixed-line-length-none)
ENDIF ()

add_executable(swat ${SWAT_SRCS})

set_target_properties(swat PROPERTIES
        RELEASE_POSTFIX "${SWAT_VER_MAJOR}Rev${SWAT_VER_MINOR}Rel"
        DEBUG_POSTFIX "${SWAT_VER_MAJOR}Rev${SWAT_VER_MINOR}Dbg"
        PGO_POSTFIX "${SWAT_VER_MAJOR}Rev${SWAT_VER_MINOR}RelPGO"
        IPGO_POSTFIX "${SWAT_VER_MAJOR}Rev${SWAT_VER_MINOR}RelPGO"
        )

install(TARGETS swat RUNTIME DESTINATION ${INSTALL_DIR})

### For CLion to implement the "make install" command
add_custom_target(install_${PROJECT_NAME}
        $(MAKE) install
        DEPENDS swat
        COMMENT "Installing ${PROJECT_NAME}")