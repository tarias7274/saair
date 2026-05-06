# saair (development version)

## Milestones

* Initial CRAN submission.

## New Features

* Added get_sensor_data() function
* Added load_padata() function
* Added read_excel_allsheets() function
* Added get_api_key() function for API call purposes
* Added set_api_key() function for responsible user key-setting
* Added %notin% infix function
* Added coerce_date_sequence function
* Changed coerce_date_sequence to ignore potentially-conflicting start/end_date
timezones in favor of the value of the explicit time_zone argument
* Added optimize_path function
* Added argument to coerce_date_sequence which allows violation of time paradox
to create date sequences going into the future
* Added argument to coerce_date_sequence which allows user to change default
behavior when using a small date difference to create an initial interval
* Modified load_padata to catch improperly-supplied sensor indexes
* Added load_padata catch to prevent it from erroring when finding no sensor matches
* Added ability to multiple partial non-case-sensitive name selection filters

## Bug Fixes

* Accounted for entire date range outside limit edge case for
coerce_date_sequence function
