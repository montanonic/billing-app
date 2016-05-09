CalendarEvent and Event are now mostly identical structures, with the difference
being that CalendarEvent is used by Persistent, and thus more valuable. Fix
the EventsList dependency on Event, switching to CalendarEvent instead.

Orphan instance should be gotten rid of, and JSON instance defined in the Model
file.
