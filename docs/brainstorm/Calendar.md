## Single events vs. recurring events.

Recurring events allow for estimation dates of when the user can bill, although
relying upon manually created events doesn't lose out on that much, and is
simpler to implement, and will work for recurring events too, it just won't take
advantage of the knowledge gained by knowing something is recurrent.

## In-Calendar markup

I may want to have an extremely simple language that a user can write in within
their event descriptions that would have some protocol for ensuring that their
event would be recorded properly, along with potentially adding event-specific
data. For example:

Let's say "Longo" is a keyword associated with one of the user's billing/invoice
profiles, and it takes every calendar event with that keyword in it and uses it.

The user should have provided a default hourly rate within that invoice profile,
and all the hours associated with that event would use that rate in their
invoice calculation. But say for one particular day, their rate for that client
was different, and instead of manually fixing that upon creating the invoice,
they could type in `hourly-rate = 30.55`, for setting the rate to thirty
dollars and fifty-five cents in their default currency (or whatever currency is
associated with the invoice profile).

When the invoice is created later on, the web-based display of it would use
different colored fields for events that were altered in this way, and contain
either mouse-over or icon-based info indicating what changed compared to the
defaults, so that the user would know exactly which events were different, and
how they were different.

Here's an example of what it could look like:

($ hourly-rate = 40 ; invoice-client = Longo )

Invoice-client would be a way to force the event to be recognized as something
that bills one client in particular, to avoid any overlapping if the description
or title uses keywords that refers to the search fields in a user's other
invoice profiles. The shorthand for such a field could be something like:

$client=Longo

In case it's something a user wants to but in every event they create, to avoid
any potential for error.



### Suggested markup symbol
Pound sign and equals: #=
