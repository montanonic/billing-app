# Using the website to add calendar events.

One of the things about Google Calendar is that it doesn't really have much of a
tagging system. The client-facing UI includes a title, a description, and a
color, but little else. Hence, in attempting to find the events that a user
wants to count the hours of, we may end up with unrelated events and
double-count.

There are two ways to prevent this issue.

## Put the burden on the user

Encourage users to create a calendar for billable events *only*, and to ensure
that the unique identifying keywords that they gave the app to search by (like,
"Longo") are not used anywhere in other events. This is not the *best* system,
but it would work. And of course, users can delete any duplicates when looking
over their invoice before sending it.

See `In-Calendar markup` within `Calendar.md` for an enhancement of this idea.

## Encourage users to add events using the web client itself

I could create an API with a client-facing UI to add events to their calendar,
ensuring that they are uniquely identified. I could even store that data
locally, so that a GAPI query wouldn't even need to happen upon creating the
invoice (but of course, I could also store data after each API query to decrease
request sizes, but I'd have to figure out which method would be more
cost-effective, especially given that GAPI query sizes would mostly be limited
to about a months worth of events each query).

The drawback of this system is that it is decidedly less simple than users
sticking to their Calendar (even though they may misspell names and such and
throw off the search).
