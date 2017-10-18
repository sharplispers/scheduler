
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

## Using scheduler

Using scheduler is fairly simple. First user has to create a scheduler
instance of desired class[^1]. Then add some tasks to it. The last
task is to stop the scheduler in the first or in the second minute
(depends on whenever we have started it on even or odd minute).

```
> (defvar *scheduler* (make-instance 'scheduler:in-memory-scheduler))
> (scheduler:create-scheduler-task *scheduler* "@reboot (print :reboot)")
> (scheduler:create-scheduler-task *scheduler* "@shutdown (print :shutdown)")
> (scheduler:create-scheduler-task *scheduler* "* * * * * (print :minute)")
> (scheduler:create-scheduler-task *scheduler* "*/2 * * * * (scheduler:stop-scheduler *scheduler*)")
```

Important note regarding the last task. It assumes, that
`start-scheduler` is run in the same package as `*scheduler*` is
defined. Listing tasks is also very easy. Same goes for starting the
scheduler loop:

```
> (scheduler:list-scheduler-tasks *scheduler*)
(#<SCHEDULER-IMPLEMENTATION::SCHEDULER-TASK {10035E67C3}>
 #<SCHEDULER-IMPLEMENTATION::SCHEDULER-TASK {10035E16F3}>
 #<SCHEDULER-IMPLEMENTATION::SCHEDULER-TASK {1003501F03}>
 #<SCHEDULER-IMPLEMENTATION::SCHEDULER-TASK {10034FF883}>)
> (scheduler:start-scheduler *scheduler*)
;; Starting scheduler.
;; :REBOOT
;; :MINUTE
;; :SHUTDOWN
;; Exiting scheduler
:STOPPED
```

Since we have started scheduler in the same thread we should see
output in the same console we have started it. If it were started in a
separate thread `*standard-output*` would be bound to wherever
`*inferior-lisp*` output is.

Remember, to add package prefixes to commands. Also keep in mind, that
we have no means in the cron specification to set a second of
occurance (this may be done with `update-scheduler-task`) and by
default seconds are assigned to a random value between 0-59 (to
distribute tasks on the timeline a little).

Now let us update the task which stops the scheduler so it ends in 5
minutes from now:

```
> (let ((task (find "(scheduler:stop-scheduler *scheduler*)"
                    (scheduler:list-scheduler-tasks *scheduler*)
                    :key #'scheduler:command :test #'equal)))
    (scheduler:update-scheduler-task
     *scheduler* task
     :start-at (local-time:timestamp+ (local-time:now) 5 :minute)))
#<SCHEDULER-IMPLEMENTATION::SCHEDULER-TASK {10032FD393}>
> (scheduler:start-scheduler *scheduler*)
;; Starting scheduler.
;; :REBOOT
;; :MINUTE
;; :MINUTE
;; :MINUTE
;; :MINUTE
;; :SHUTDOWN
;; Exiting scheduler.
:STOPPED
```

This illustrates a capability of the scheduler to postpone the first
occurance of the event. After shutdown is triggered, next occurance is
recomputed from the stored time specification.

Another interesting aspect of having next occurance precomputed is a
capability of running tasks, which was missed (i.e computer was down,
or scheduler wasn't running). If the scheduler encounters task with
its activation time set in the past it will start it at some point in
the future[^2].

Note, that the missing task will be performed only once even if more
calls were missed. With our scheduler, if we have waited at least two
minutes after last invocation and start the scheduler once more it
stopped immedietely after start:

```
> (scheduler:start-scheduler *scheduler*)
;; Starting scheduler.
;; ; missed (scheduler:stop-scheduler *scheduler*) is called
;; :MINUTE ; <- another missed task, we go through full loop iteration before exit
;; :REBOOT ; <- "ordinary" reboot task
;; :SHUTDOWN ; <- "ordinary" shutdown task
;; Exiting scheduler.
:STOPPED
```

In this case scheduler loop will exit immedietely. It is an important
clue, that it is unwise to schedule `stop-scheduler` call in
scheduler, but rather call this function asynchronously.

## Extending Scheduler

This is extensible software. Scheduler is arranged around a simple
protocol based on CRUD(L) principle. Reference implementation
`in-memory-scheduler` is provided for conveniance and testing. More
advanced backends may be written by subclassing `scheduler` class and
implementing methods `create-scheduler-task`, `read-scheduler-task`,
`update-scheduler-task`, `delete-scheduler-task` and
`list-scheduler-tasks`. Methods implementation should be fairly
straightforward.

## Reference manual

```
SCHEDULER:SCHEDULER                                              [class]

Abstract class from which each driver has to inherit. Thread-safety of
CRUDL operations is implemented in :AROUND methods. Class should not
have direct instances.
```

```
SCHEDULER:IN-MEMORY-SCHEDULER                                   [class]
base-class: SCHEDULER

Reference implementation of a scheduler. Data is kept in a
memory. Class may be instationalized.
```

```
SCHEDULER:SCHEDULER-TASK                                         [class]
readers: TIME-SPECS, COMMAND, LAST-OCCURANCE, NEXT-OCCURANCE

Class representing a single task. All scheduler entries added by
CREATE-SCHEDUELR-TASK should be instances of this class.
```

```
SCHEDULER:CREATE-SCHEDULER-TASK                       [generic function]
args: SCHEDULER CRON-ENTRY

Creates and returns scheduler task instance of class SCHEDULER-TASK.
```

```
SCHEDULER:READ-SCHEDULER-TASK                         [generic function]
args: SCHEDULER TASK

Look for a task in scheduler entries. Programmer may specialize second
argument on other types like UUID.
```

```
SCHEDULER:UPDATE-SCHEDULER-TASK                      [generic function]
args: SCHEDULER TASK &key CRON-ENTRY START-AT &allow-other-keys

Updates TASK. CRON-ENTRY contains TIME-SPECS and command as described
earlier in this document. START-AT is LOCAL-TIME:TIMESTAMP instance
used to set a date of the first task invocation.

If a key argument is not supplied then slot is not updated. Remember,
that you shouldn't use direct slot write, because scheduler driver may
store data in a database or in a file. Always update tasks with this
function.
```

```
SCHEDULER:DELETE-SCHEDULER-TASK                      [generic function]
args: SCHEDULER TASK

Removes task from the scheduler so it is not invoked when it's next
occurance time comes.
```

```
SCHEDULER:LIST-SCHEDULER-TASKS                       [generic function]
args: SCHEDULER

Returns a list of all tasks associated with the scheduler. List
elements are instances of class SCHEDULER-TASK.
```

```
SCHEDULER:START-SCHEDULER                                    [function]
args: SCHEDULER

Starts scheduler in a loop. Function blocks until STOP-SCHEDULER is
called, so it may be wise to start it in another thread. All tasks are
evaluated in the package this function is invoked in. NB: if function
is started with

  (bt:make-thread (lambda () (scheduler:start-scheduler *s*)))
  
the package is CL-USER because that is how BORDEAUX-THREADS is
implemented.

If scheduler loop encounters task which was missed (it was meant to be
started in the past) it will schedule this tasks by itself in the near
future (when exactly is not specified). After each task activation
next activation date is recomputed automatically and updated with
UPDATE-SCHEDULER-TASK.
```

```
SCHEDULER:STOP-SCHEDULER                                     [function]
args: SCHEDULER

Stops scheduler. Scheduler main function should stop after finishing
currently pending task loop iteration.
```

[^1]: Currently only reference scheduler is implemented called
`in-memory-scheduler`. It should be fairly usable for scheduling tasks
in running software, but for persistance it may be necessary to write
a new backend.

[^2]: In this case - immedietely, but it is not specifed to allow task
distribution to balance the load after restart.
