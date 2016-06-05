
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<img src="icon.png" align="right"/>

googlecalendar: Google Calendar for R
=====================================

**googlecalendar** makes Google Calendar easily accessible through R. The package offers a collection of functions for accessing and modifying calendars, events, and UI settings via Google's [Calendar API](https://developers.google.com/google-apps/calendar/). While not a verbatim port of available methods, the package aims to provide intuitive commands for interacting with the Calendar service.

This project is inspired in part by [googlesheets](https://github.com/jennybc/googlesheets), a Google Sheets interface for R. googlesheets is a wonderful project, and this package attempts to mirror its ease of use. Most notably, googlecalendar has been designed with a similar ambition for compatability with the [magrittr](https://github.com/smbache/magrittr) `%>%` pipe operator.

Under the hood, googlecalendar uses the [httr](https://github.com/hadley/httr) and [jsonlite](https://github.com/jeroenooms/jsonlite) packages for administering HTTP requests and coercing responses.

Development Note
----------------

This project is still very much in an early development stage. Not only is ~~a great deal of~~ most functionaly still missing, but virtually no tests have been formalized. All the same, feel free to share any initial feedback on the project via the [GitHub Issues](https://github.com/benjcunningham/googlecalendar/issues) page while we work to stabilize the package API and round out its basic functionality.

Installation & Setup
--------------------

To install the current development version:

``` r
devtools::install_github("benjcunningham/googlecalendar")
```

To use googlecalendar, you will need access to a Google Calendar API client key and secret. To obtain these, visit the [Google Developers Console](https://console.developers.google.com/) and register a new application. Once you have generated a key and secret, you can set them for persistent use in an `.Rprofile` file:

``` r
options(
  googlecalendar.client_key = "<KEY>",
  googlecalendar.client_secret = "<SECRET>"
)
```

Every function in this package that sends a request to the Google Calendar API must also include an authorization token. Fortunately, in almost all cases, the OAuth 2.0 process is evoked internally. However, you can use the following method to manually initialize the process:

``` r
library(googlecalendar)
gc_auth(new_user = TRUE)
```

From here, you will be automatically redirected to your web browser and asked to sign-in and grant permission for googlecalendar to access the Google Calendar API on your behalf. A similar mechanism will trigger when a network function is called without previously authorizing the application. When an access token becomes stale, the next function to make a request to the Calendar service will automatically handle fetching a new one without reinitializing the full authentication procedure (i.e. browser-based logins should happen only once per session). Note that googlecalendar currently only handles credentialling of one user at a time.

To deauthorize the current user and permanently remove their cached credentials:

``` r
gc_deauth(clear_cache = TRUE)
```

Usage
-----

*If you haven't already, make sure to read the section above on the basics of setting up the package. Recall that to get started you ought to have stored your newly-registered Google application client credentials in your `.Rprofile`.*

Let's just jump right in and create a new calendar for demonstrating some core functionality. No need to worry about authenticating ahead of time — `gc_new()` will take care of redirecting you to a browser if necessary.

``` r
library(googlecalendar)
cal <- gc_new("useR Meetups", location = "University of Iowa")
```

    ## Successfully created calendar: "useR Meetups"

And here is what that new calendar looks like, saved as a `googlecalendar` object:

    ## Calendar ID: ecng334gan2bgtrleid9r2tfl0@group.calendar.google.com
    ## 
    ##       Title: useR Meetups
    ## Description: NA
    ##    Location: University of Iowa
    ##   Time Zone: UTC
    ## Permissions: owner
    ##        ETag: "1465166679530000"

Don't worry if that doesn't look like much. The `print()` dispatch for this object class intentionally hides a lot of non-essential information. To see what lies beneath the surface, we can use `str(cal)`.

    ## List of 18
    ##  $ kind                : chr "calendar#calendarListEntry"
    ##  $ etag                : chr "\"1465166679530000\""
    ##  $ id                  : chr "ecng334gan2bgtrleid9r2tfl0@group.calendar.google.com"
    ##  $ summary             : chr "useR Meetups"
    ##  $ description         : chr NA
    ##  $ location            : chr "University of Iowa"
    ##  $ timeZone            : chr "UTC"
    ##  $ summaryOverride     : chr NA
    ##  $ colorId             : chr "15"
    ##  $ backgroundColor     : chr "#9fc6e7"
    ##  $ foregroundColor     : chr "#000000"
    ##  $ hidden              : logi FALSE
    ##  $ selected            : logi TRUE
    ##  $ accessRole          : chr "owner"
    ##  $ defaultReminders    :List of 2
    ##   ..$ method : chr NA
    ##   ..$ minutes: int NA
    ##  $ notificationSettings:List of 1
    ##   ..$ notifications:List of 2
    ##   .. ..$ type  : chr NA
    ##   .. ..$ method: chr NA
    ##  $ primary             : logi NA
    ##  $ deleted             : logi FALSE
    ##  - attr(*, "class")= chr [1:2] "googlecalendar" "list"

While not perfect, googlecalendar attempts to faithfully represent objects according to the structure of their originating Google Calendar API resource. Generally, properties are named and typed identically to how they are returned by the service.

Taking a closer look at `cal`, it looks like we forgot to pass along a few useful properties in our original creation method. We can add these now using:

``` r
cal <- gc_edit(
  cal,
  description = "Iowa City's useR Group",
  timeZone = "America/Chicago",
  colorId = "20"
)
```

    ## Successfully edited calendar: "useR Meetups"

For good measure, let's convince ourselves that the changes were made and returned:

    ## List of 18
    ##  $ kind                : chr "calendar#calendarListEntry"
    ##  $ etag                : chr "\"1465166680757000\""
    ##  $ id                  : chr "ecng334gan2bgtrleid9r2tfl0@group.calendar.google.com"
    ##  $ summary             : chr "useR Meetups"
    ##  $ description         : chr "Iowa City's useR Group"
    ##  $ location            : chr "University of Iowa"
    ##  $ timeZone            : chr "America/Chicago"
    ##  $ summaryOverride     : chr NA
    ##  $ colorId             : chr "20"
    ##  $ backgroundColor     : chr "#cabdbf"
    ##  $ foregroundColor     : chr "#000000"
    ##  $ hidden              : logi FALSE
    ##  $ selected            : logi TRUE
    ##  $ accessRole          : chr "owner"
    ##  $ defaultReminders    :List of 2
    ##   ..$ method : chr NA
    ##   ..$ minutes: int NA
    ##  $ notificationSettings:List of 1
    ##   ..$ notifications:List of 2
    ##   .. ..$ type  : chr NA
    ##   .. ..$ method: chr NA
    ##  $ primary             : logi NA
    ##  $ deleted             : logi FALSE
    ##  - attr(*, "class")= chr [1:2] "googlecalendar" "list"

Like all functions in the package, `gc_edit()` attempts to return a useful object (in this case the updated calendar) that can be piped into another function. Accordingly, we overwrite `cal` so that we keep the current version of the calendar in our environment. We'll cover more on chaining package functions later.

Finally, we can go ahead and delete our demonstration calendar:

``` r
gc_delete(cal)
```

    ## Calendar "useR Meetups" was successfully deleted.

*More to come soon (and eventually be dumped in a vignette)...*

Functions Overview
------------------

| Function          | Description                         | Return                                       |
|:------------------|:------------------------------------|:---------------------------------------------|
| gc\_auth          | Authorize the package               | Token2.0, Token, R6                          |
| gc\_deauth        | Deauthorize the package             | logical                                      |
| gc\_ls            | List available calendars            | googlecalendar\_ls, tbl\_df, tbl, data.frame |
| gc\_new           | Create a new calendar               | googlecalendar, list                         |
| gc\_id            | Retrieve a calendar by ID           | googlecalendar, list                         |
| gc\_summary       | Retrieve a calendar by title        | googlecalendar, list                         |
| gc\_edit          | Edit metadata for a calendar        | googlecalendar, list                         |
| gc\_delete        | Delete a calendar                   | logical                                      |
| gc\_event\_ls     | List events scheduled on a calendar | event\_ls, tbl\_df, tbl, data.frame          |
| gc\_event\_new    | Create a new event                  | event, list                                  |
| gc\_event\_id     | Retrieve an event by ID             | event, list                                  |
| gc\_event\_query  | Retrieve an event by expanded query | event, list                                  |
| gc\_event\_edit   | Edit metadata for an event          |                                              |
| gc\_event\_delete | Delete an event                     | logical                                      |
