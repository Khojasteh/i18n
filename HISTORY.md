VERSION HISTORY
===============

### Version 1.10.1 (July 18, 2022)
- Increased the maximum supported date for the Jalali (Persian) calendar.

### Version 1.10 (January 25, 2014)
- Added x64 compilation support by *arhangelsoft*.
- Fixed bug in the Hebrew Calendar, which resulting an incorrect month name when the selected language was also Hebrew. Thanks to *JHarding* for reporting this bug.

### Version 1.9 (November 03, 2013)**
- Added the `APIKey` and `UserIP` properties to the `TGoogleTranslator` class. These properties are required for having a valid Google service call.

### Version 1.8 (September 12, 2013)
- Added the `AdjustDays` property and the `OnAdjustment` event to the `THijriCalendar` class, so that the Hijri dates can be adjusted.

### Version 1.7 (June 9, 2013)
- Fixed bug in parsing the ternary operator of the C-like expressions.
- Reduced expression tree of the C-like expressions by pre-calculating the constant sub-expressions.

### Version 1.6 (April 26, 2013)
- Added the missing piece of code that saves design-time value of the `Culture` and `CalendarType` properties of the `IntlDateTimeLabel` control into DFM.

### Version 1.5 (January 5, 2013)
- Fixed the compatibility issues with Delphi XE2 and XE3.

### Version 1.4 (December 2, 2012)
- Fixed the bug in applying translation on Delphi XE3. Thanks to *David Esperalta* for fixing the bug.
- Handled the exceptions that may occur while getting translatable properties of an OLE control. Thanks to *jjLopaz* for reporting this problem.

### Version 1.3 (November 4, 2012)
- Fixed the bug in the `TDBIntlDatePicker` control that was causing the dataset switch to edit mode. Thanks to JHarding for reporting this bug.
- Fixed the issue with the popup position of the `TIntlDatePicker` control on multi-monitor systems. Thanks to *Keeny* for reporting this problem.

### Version 1.2 (August 19, 2012)
- Fixed the bug causing the invisible forms become visible after translation. Thanks to *Ahell* for reporting and fixing this bug.

### Version 1.1 (June 13, 2011)
- Made the package compatible with Delphi 2009. The package has some problems in Delphi 2009. Because of a few bugs in the `Controls` unit, mirroring the page and tab controls causes the child controls of 
these controls go off the screen. Some other common controls may also show odd behavior after toggling the layout's direction.
- Fixed display problem of the `TIntlDatePicker` control on Windows XP.
- Some minor tweaks.

### Version 1.0 (June 11, 2011)
- First public release
