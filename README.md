# Compile SWAT with CMake, VS, and IVF
Compile SWAT model from FORTRAN source code using CMake and Visual Studio IDE with Intel Fortran compiler.

## Prerequisite

+ CMake2.8+
+ Microsoft Visual Studio 2010+
+ Intel Fortran compiler 13.0+

## Compile procedure

 Take VS2013+Intel_Parallel_Studio_XE_2017 (Intel 17.0.4) as an example.

+ Open "Intel compiler 17.0 Update 4 Intel(R) 64 Visual Studio 2013" from start menu.
+ cd to the compile destination folder. e.g., `cd D:/compile/SWAT_ifort`
+ Run `cmake <path to SWAT src path>`. e.g., `cmake C:\z_code\Hydro\SWAT`
  + Compile 64-bit version: `cmake -G "Visual Studio 12 2013 Win64" C:\z_code\Hydro\SWAT`
+ Open the project `SWAT.sln`, and do what you want!

## Update with the latest SWAT source (For administrator only)

In order to make it easier to track code changes and keep up with newer revisions, I use subtree to manage the SWAT source code from tamu.

+ Set subtree as `swat_src`(only do once at the very beginning of this repository)

  ```bash
   git remote add -f swat_src git@github.com:crazyzlj/swat_src_tamu.git
   git subtree add --prefix=swat_src swat_src master --squash
  ```

+ Do any modification to the `swat_src` to meet our specific needs.

+ If the SWAT source is updated we should pull the latest code from [swat_src_tamu](https://github.com/crazyzlj/swat_src_tamu) and merge it.

  ```shell
  git fetch swat_src master
  git subtree pull --prefix=swat_src swat_src master --squash
  ```

## Changelog

+ 9/10/2017: Update to rev.664 and test compilation on VS2013+IVF 17.0


# Reference
+ [Compile and Debug SWAT with gfortran and Eclipse by Dr. Zhiqiang Yu](https://zhiqiangyu.wordpress.com/2014/10/01/compile-and-debug-swat-with-gfortran-and-eclipse/)
  + If you cannot access [google drive](https://drive.google.com/file/d/0B16YhFB_9MejSG15ai0zYS1fMkU/edit?usp=sharing) for the `Makefile` or [google docs](https://docs.google.com/document/d/16Do2U1_v4mZZBOV0hmcs6Gh1UvAUAXOMidpB-SE203A/edit?usp=sharing) for the single document guide, these files are located in `SWAT/posts_by_zhiqiang`.
+ https://github.com/mlt/swat