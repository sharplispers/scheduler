
# Instinct Engine Scheduler

Module is designed for regular tasks which need to be performed on
specific date or on regular basis. For now main component client is
reporting module, but it may be used for scheduling calculation
refresh, regular cleanups and similar.

## Specifying task

Task should be added in a format similar to the one used
in [cron tables](https://www.pantz.org/software/cron/croninfo.html).
There are a few note-worthy differences. Each entry is composed of
six fields separated by space:

MINUTE HOUR DAY-OF-MONTH MONTH DAY-OF-WEEK SEXPRESSION

- MINUTE        Minutes within the hour (0–59)
- HOUR          The hour of the day (0–23)
- DAY-OF-MONTH  The day of the month (1–31)
- MONTH         The month (1–12)
- DAY-OF-WEEK   The day of the week (0–7) where 0 and 7 are Sunday
- SEXPRESSION   The Lisp form to be evaluated at given time.

To specify multiple values for one field, the following operators are
available. In the order of precedence,

- `*` specifies all valid values
- `M-N` specifies a range of values
- `M-N/X` or `*/X` steps by intervals of X through the specified range or whole valid range
- `A,B,...,Z` enumerates multiple values
- `H` specifies random value chosen from all valid values for the field (or specified as argument)

Examples:

```scheduler
# make report every fifteen minutes (perhaps at :07, :22, :37, :52)
H/15 * * * * (ie-reports:make-report "foo" "bar" "zzzur")

# refresh data every ten minutes in the first half of every hour (three times, perhaps at :04, :14, :24)
H(0-29)/10 * * * * (grc:refresh-data)

# once every two hours at 45 minutes past the hour starting at 9:45 AM and finishing at 3:45 PM every weekday.
45 9-16/2 * * 1-5 (grc:restart)

# once in every two hours slot between 9 AM and 5 PM every weekday (perhaps at 10:38 AM, 12:38 PM, 2:38 PM, 4:38 PM)
H H(9-16)/2 * * 1-5 (grc:restart)

# once a day on the 1st and 15th of every month except December
H H 1,15 1-11 * nil
```

## Managing tasks from the scheduler

ADD-SCHEDULER-TASK                                      [function]
args: cron-specs

