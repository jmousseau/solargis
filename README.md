# solargis
Request [SolarGIS](solargis.info) for the United States and Canada.

## Overview
Install `solargis` using the [`devtools`](https://github.com/hadley/devtools) 
package.
```R
devtools::install_github("jmousseau/solargis")
```

### Known Issues

Let `A` be an existing location with data from 2015-01-01 to 2016-01-01. If a 
user requests location `A` data from 2013-01-01 to 2014-01-01, the `meta.csv`
record will say that location `A` data exits from 2013-01-01 to 2016-01-01
even though 2014 data was not fetched. To fix this issue, a new data range
representation is required in the `meta.csv` file. For example, the two
ranges above could be stored as `"2013-01-01:2014-01-01 2015-01-01:2016-01-01"`.

## Getting Started

Request data for a location. In this example, the `store/` directory will be
used to keep a single point of truth for request history.
``` R
data_file_path <- solargis::request("store/", "0001", 41.8, -87.6, "2017-03-08",
                                    "2018-11-20", "api-key")
```

Interface with the 
[Usage API](https://wiki.solargis.com/display/public/Usage+API). All routes are
[supported](R/usage.R).
```R
solargis::usage_calls("api-key")
```
