# iso-date: A simple library for working with ISO (YYYY-MM-DD) dates in Emacs

ISO dates (such as `2025-07-28`), being as they are a well-known standard in software, have the potential to connect your computing environment. This library aims to provide utilities that makes working with ISO dates in Emacs simple and fun, both for users and developers.

## For users

This library offers some commands which let you use ISO dates with several Emacs packages:

- iso-date-show-calendar
- iso-date-show-org-agenda
- iso-date-show-diary
- iso-date-send-to-calc

They are particularly useful as `embark` actions (see [this pull request](https://github.com/oantolin/embark/pull/769) for context).

There are also commands to insert or manipulate ISO dates:

- iso-date-insert
- iso-date-at-point-day-up
- iso-date-at-point-day-down
- iso-date-at-point-do-shift

Or display information about them:

- iso-date-echo-difference
- iso-date-pretty-print

## For developers

This library provides the `iso-date` function. Without arguments, it returns an ISO date string for current day:

```elisp
(iso-date)
=> e.g. "2025-07-28"
```

It can also take keywords which modify the date returned.

```elisp
(iso-date :day +3)
=> e.g. "2025-07-31"
```

Valid keywords are :day, :month and :year. This does not simply manipulates the string; it parses the date, uses `decoded-time-add`, and then reconverts it to ISO. So you never end up with an invalid date.

`iso-date` can also take the :start-day keyword, which changes the starting point relative to which modifications are applied.

```elisp
(iso-date :start-date "2000-12-18" :day +7)
=> "2000-12-25"
```

Apart from this, the library provides other functions which may be useful for packages dealing with ISO dates.

For converting from/back to ISO:

- iso-date-to-calendar
- iso-date-to-calendar-lax
- iso-date-from-calendar
- iso-date-to-internal
- iso-date-from-internal

For validating dates:

- iso-date-valid-p

For extracting values:

- iso-date-year
- iso-date-month
- iso-date-day

Others:

- iso-date-between-dates-p
- iso-date-list-dates-between

Last but not least, this library can add `date` as a thing recognized by the `thingatpt` library. Just add the following to your init file:

```elisp
(iso-date-configure-thingatpt)
```

The `iso-date-at-point` and `iso-date-bounds` functions are already provided. They configure `thingatpt` when first used.

------

**NOTE**: `iso-date` is a high-level library. If you just need to parse an ISO date, use the built-in `iso8601` library, or its wrapper `time-date`.
