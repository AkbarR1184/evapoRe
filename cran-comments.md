## R CMD check results
  
0 ERRORS
0 WARNINGS
0 NOTES (new resubmission after archiving at 2023-11-27)
  
## This is a resubmission, after archiving on 2023-11-27

## Comments by Victoria Wimmer on 2023-11-27

**Issue:**
Thanks, Please do not start the description with "Functions for", "This package", package name, title, or similar.

**Answer:**
Done. Updated description: An R-based application for exploratory data analysis of global EvapoTranspiration (ET) datasets. 'evapoRe' enables users to download, validate, visualize, and analyze multi-source ET data across various spatio-temporal scales. Also, the package offers calculation methods for estimating potential ET (PET), including temperature-based and radiation-based approaches. 'evapoRe' supports hydrological modeling, climate studies, agricultural research, and other data-driven fields by facilitating access to ET data and offering powerful analysis capabilities. Users can seamlessly integrate the package into their research applications and explore diverse ET data at different resolutions.

**Issue:**
Please always write package names, software names, and API (application programming interface) names in single quotes in the title and description. e.g: --> 'evapoRe' Please note that package names are case-sensitive.

**Answer:**
Done. e.g., 'evapoRe' instead of evapoRe.

**Issue:**
Additionally, we still see: Size of tarball: 5009359 bytes A CRAN package should not be larger than 5 MB. Please reduce the size a bit more if possible.

**Answer:**
Size now reduced to below 5mb.

**Issue:**
\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.

**Answer:**
Fixed with the following solution for the examples with donotrun wrapped with 'if (interactive()){}'

**Issue:**
Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir(). e.g.: R/download_bess.R ; ...

**Answer:**
Done. Fixed and now all functions do not write by default to the user file space.


## This is a resubmission, after archiving on 2023-11-28

Comments by Benjamin Altmann on 2023-11-28

**Issue:**
Only functions which are supposed to run interactively (e.g. shiny) should be wrapped in if(interactive()).
Please put functions which download data and/or take longer than 5 sec to run in \donttest{}.
All other examples can be unwrapped.

If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in
quotes: "Title")

Please fix and resubmit.

Best,
Benjamin Altmann

**Answer:**
Description file now updated. Also, examples updated with donttest{}.
