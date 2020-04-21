+ 04/21/2020: Reorganize the repo

  ----

+ 7/1/2019: Reorganize the repo and create a newly revised tag of rev.664.

+ 9/11/2017: Bugs fixed or code improvement. Cross-platform compilation.

  + Code improvement, e.g.:

    ``` fortran
    !! 1. Error: Symbol 'XXX' at (1) has no IMPLICIT type
    !! 2. Function 'XXXX' at (1) has no IMPLICIT type
    ```

  + Bugs, e.g.:

    ```fortran
    !! 1. Typo
    !! e.g., hhwatqual.f
    if (orgpin < 1.e-6) orgnpin = 0.0
    !! should be
    if (orgpin < 1.e-6) orgpin = 0.0
    !! 2. Float equal
    if (rg_orifice(i,kk)==1) then
    !! changed to
    if (abs(rg_orifice(i,kk)-1.)<1.e-5) then
    ```

+ 9/10/2017: Update to rev.664 and test compilation on VS2013+IVF 17.0
