# VERSION HISTORY

### Version 1.12 (January 11, 2026)
- Improved the Persian calendar:
  * Used a more precise astronomical calculation for Nowruz (Tehran time). The new algorithm provides Vernal Equinox time with sub-minute precision,
  ensuring that leap years align closely with actual astronomical events.This change allows reliable date conversions up to Jalali year 2379 (Gregorian year 3000).
  * Added support for the Imperial era alongside the Hijri era. The Hijri era remains the default.
- Updated some captions and translations for better clarity.

### Version 1.11 (December 30, 2025)
- Fixed Persian calendar by replacing the arithmetic leap-year rule with an astronomical Nowruz calculation (Tehran time).
- Updated the Japanese calendar to include the latest era changes.
- Resolved compiler warnings in Delphi 12.

### Version 1.10.1 (July 18, 2022)
- Increased the maximum supported date for the Jalali (Persian) calendar.

### Version 1.10 (January 25, 2014)
- Added x64 compilation support, contributed by *arhangelsoft*.
- Fixed a bug in the Hebrew Calendar that resulted in an incorrect month name when the selected language was Hebrew. Thanks to *JHarding* for reporting this bug.

### Version 1.9 (November 03, 2013)
- Added the `APIKey` and `UserIP` properties to the `TGoogleTranslator` class. These properties are required to make valid Google service calls.

### Version 1.8 (September 12, 2013)
- Added the `AdjustDays` property and the `OnAdjustment` event to the `THijriCalendar` class so that Hijri dates can be adjusted.

### Version 1.7 (June 9, 2013)
- Fixed a bug in parsing the ternary operator in C-like expressions.
- Reduced the size of expression trees for C-like expressions by pre-calculating constant sub-expressions.

### Version 1.6 (April 26, 2013)
- Added missing code to save the design-time values of the `Culture` and `CalendarType` properties of the `IntlDateTimeLabel` control into the DFM.

### Version 1.5 (January 5, 2013)
- Fixed compatibility issues with Delphi XE2 and XE3.

### Version 1.4 (December 2, 2012)
- Fixed a bug with applying translations on Delphi XE3. Thanks to *David Esperalta* for reporting this bug.
- Handled exceptions that could occur while retrieving translatable properties of an OLE control. Thanks to *jjLopaz* for reporting this problem.

### Version 1.3 (November 4, 2012)
- Fixed a bug in the `TDBIntlDatePicker` control that caused the dataset switch to enter edit mode. Thanks to *JHarding* for reporting this bug.
- Fixed the popup position issue of the `TIntlDatePicker` control on multi-monitor systems. Thanks to *Keeny* for reporting this problem.

### Version 1.2 (August 19, 2012)
- Fixed a bug that caused invisible forms to become visible after translation. Thanks to *Ahell* for reporting and fixing this bug.

### Version 1.1 (June 13, 2011)
- Made the package compatible with Delphi 2009. There are some known issues in Delphi 2009: because of a few bugs in the `Controls` unit, mirroring page and tab controls can cause their child controls to go off the screen. Some other common controls may also exhibit odd behavior after toggling the layout direction.
- Fixed a display problem in the `TIntlDatePicker` control on Windows XP.
- Minor tweaks.

### Version 1.0 (June 11, 2011)
- First public release.
