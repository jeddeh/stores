PRINTHTML Command Line Usage
----------------------------

SYNTAX:
PRINTHTML.EXE [options]

OPTIONS:
html          Specify the HTML text to print. You can use HTML encoding to
              encode special characters.
printername   Name of the printer to you want to use. It will use the default
              printer if nothing is specified.
file          Print the HTML stored in a file.
              This parameter can be used multiple times.



url           Load the HTML from this URL and print it. This p
              This parameter can be used multiple times.
directurl     Print this URL. Can only be specified once and will overwrite
              previously set buffer content.
title         Title of print job. This can be used in headers and footers.
header        Header text of print job. See header and footer syntax below.
footer        Footer text of print job. See header and footer syntax below.
leftmargin    Left margin.
rightmargin   Right margin.
topmargin     Top margin.
bottommargin  Bottom margin

ADVANCED OPTIONS:
filename      File name of print job.
nopreserve    Keep header, footer, and margins in registry.
eventlog      Write success and error messages in system event log.

HEADER AND FOOTER SYNTAX:
When you specify a header or footer you can use the following codes.

&&            Single ampersand '&'.
&b            Separate left and right aligned text.
&d            Short date formatted according to regional settings.
&D            Long date formatted according to regional settings.
&p            Current page number.
&P            Total number of pages in print job.
&u            Page URL.
&w            Page title.
&t            Time formatted according to regional settings
&T            24 hour time format.
Any Text      You can write custom text and combine it with the codes.
(blank)       Leaves the header or footer blank if you don't specify any text

EXAMPLES:

Print 'Hello World' to default printer. 'World' will be printed in bold.
PRINTHTML.EXE html="Hello <b>World</b>"

Print the web page from printhtml.com to the installed Bullzip PDF Printer.
PRINTHTML.EXE directurl="www.printhtml.com" printername="Bullzip PDF Printer"

More examples and documentation at www.printhtml.com.