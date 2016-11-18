# Medicare 0.2.0
- New features:
    - price_deflate() function: deflates spending from specific Medicare sectors (i.e. inpatient, physician, hospice) within the range 2002:2014.
- Bug fixes:
    - Fixed row / column subsetting in cr_extract(), which wasn't working correctly for tibbles.
    - Fixed typos in vignettes.

# Medicare 0.1.0
- Initial release: functions for manipulating cost reports and provider of services data.