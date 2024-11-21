# Irregular Terrain Model (ITM) (Longley-Rice)

A C# port of the C++ code (itself a port of FORTRAN code) found at https://www.its.bldrdoc.gov/resources/radio-propagation-software/itm/itm.aspx

A refactored version is also present which attempts to clean up the code a little bit and avoid the use of unsafe code.

The C++ version is included for unit testing to validate results. Unit tests cover 94% of the refactored version. Uncovered lines include old error handling flags which have been turned into argument exceptions on usage.

