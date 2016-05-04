To create an invoice from Calendar information, we have the user tell the app
which event fields to look for, so that we can find the correct events. The user
can have multiple Invoice profiles for different clients, that search their
calendar for different event identifiers based upon which client they are
creating an invoice for, or even search multiple calendars for their associated
event identifiers.

There are three ways for a user to get an invoice:
 * Manually: they can specify a period of time in which their Calendar will be
 searched and analyzed according to the given "Invoice profile", and have an
 invoice generated from that.

 * They can set a schedule for when their invoices should be processed, such as
 at the beginning/end of each month, or on specific dates throughout the year,
 to be repeated each year. They can either schedule this using the app, or by
 adding a specifically tagged event-type to the calendar they want to associate
 this invoice profile with.

There are three ways to respond to an automatic invoice:
  * Add a Calendar event notification that tells a user that their invoices
  are ready to send.

  * Email the user that they have an invoice or invoices that are ready to send,
  including both a link to their current invoice page (to edit, if desired), and
  a pdf file of the invoice printed as is.

  * Send an invoice on the user's behalf. The sent message will include a link
  to the website, and explain that the content of the invoice is automatically
  generated per the user's requests, but that they should contact the user if
  there are any discrepancies.


## Invoice Profile
An Invoice Profile, or Billing Profile, is a data structure that exists on the
level of the app. The invoice profile contains all the information needed to
generate an invoice for a particular client, and possibly information on how the
user wants to approach Invoicing (manually, reminder-based, or automated).

Many aspects of an Invoice will be repeated throughout profiles, and so we
should probably have a default Invoice entity that the user creates, to set the
default values for each Invoice Profile that they create. The default Invoice
can even be automatically created based upon the first Invoice Profile they
create (excluding any fields that obviously only apply to a specific invoice),
and
