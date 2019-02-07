# purescript-activity-monitor
Its pretty standard on modern websites to log a user out once they've been
inactive for a sufficient period of time. This tiny library provides
the plumbing to quickly add this functionality to pages.

## Key Features
While logging out a user was the inspriation for this library, the actual
implementation allows you to run an arbitrary `Effect` once the countdown
has elapsed. Multi-tab is provided via local storage.
