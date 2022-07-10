DESCRIPTION
-----------

The i18n package is a  library  of Delphi components, classes, and procedures to 
fully and  easily localize  Delphi applications  targeting the Windows platform,
and to change the locale of the applications on the fly.

There  are many  software tools  for localizing a Delphi  application, but  they 
mostly  provide  support for  translation  of the user  interface  strings only. 
However, the i18n package not  only  enables  developers to  select translatable
properties  and  string constants/literals within the IDE but also takes care of 
plural forms, reading layout, formatting preferences, and calendar system of the
target language.

You may want that your application be ready for right-to-left languages, but you
do not  know which  language is right-to-left or even how it works? Don’t worry,
the i18n package handles  right-to-left  languages effortlessly.  When locale of 
application changes, and whenever needed, the i18n package automatically mirrors
the layout  of the  application’s forms  according to the  reading layout of the
selected language. You do not need to write a single line of code or even revise 
it.

Although the Gregorian calendar is the  most used  calendar in the world, but if 
your application supports only this calendar, it cannot  be truly  localized for
all countries/regions. The i18n package knows  which calendar  system is used by 
which country/region, and automatically formats date-time values in the calendar
system of the selected locale. You don't  need to modify  your code, because the
application still sees all date-time values in  Gregorian calendar as a standard
TDateTime value. 

The following major calendars are implemented in the i18n package:

- Gregorian Calendar
- Hebrew or Jewish Calendar (הלוח העברי)
- Hijri or Islamic Calendar (التقويم الهجري)
- Jalali or Persian Calendar (گاهشمار جلالی)
- Japanese Emperor Era Calendar
- Julian Calendar
- Korean Tangun Era Calendar
- Taiwan calendar
- Buddhist Calendar

I have tested the i18n package on Delphi 2009 and 2010 only,  but it should work
on the later versions of Delphi as well.


CONTENT
-------

You will have the following components after installation of the package:

- **TLocalizer:** \
  Provides the core functionality for localizing an application.

- **TTranslator:** \
  Maintains list of  translatable  strings for a form, frame, data module, and
  so on. This component also translates the user interface strings and handles
  bi-directional mirroring of the controls.

- **TGoogleTranslator:** \
  This is an interface to the online  Google translation service for automatic
  text translation.

- **TMessageDialog:** \
  Displays a localizable and customizable message dialog.

- **TInputQueryDialog:** \
  Displays a localizable input  dialog  box that lets the user enter a string,
  double, or integer.

- **TIntlDateTimeLabel:** \
  A label control that displays a fully localizable TDateTime value on a form.

- **TIntlMonthCalendar:** \
  A month calendar control to display a fully localizable calendar on a form.

- **TIntlDatePicker:** \
  A control that is designed specifically for entering localized dates.

- **TFlagImageList:** \
  A special image list component that provides country flag icons.

- **TImageLabel:** \
  A label control that can include an image on its face.

- **TCultureLabel:** \
  A label control to display name and  country flag of a culture (locale) on a
  form.

- **TCultureBox:** \
  A combo box control that represents a list of cultures (locales).

- **TCultureListBox:** \
  A list box control that represents a list of cultures (locales).

- **TCultureCheckListBox:** \
  A check list box control that represents a list of cultures (locales).

- **TTerritoryLabel:** \
  A label control that displays name and flag of a country/region on a form.

- **TTerritoryBox:** \
  A combo box control that represents a list of countries/regions.

- **TTerritoryListBox:** \
  A list box control that represents a list of countries/regions.

- **TTerritoryCheckListBox:** \
  A check list box control that represents a list of countries/regions.

- **TCurrencyLabel:** \
  A label control that displays name and country flag of a currency on a form.

- **TCurrencyBox:** \
  A combo box control that represents a list of currencies.

- **TCurrencyListBox:** \
  A list box control that represents a list of currencies.

- **TCurrencyCheckListBox:** \
  A check list box control that represents a list of currencies.

- **TDBImageLabel:** \
  A data-aware label control that displays value of a field plus a custom icon
  on a form.

- **TDBCultureLabel:** \
  A data-aware label control that displays value of a locale field.

- **TDBCultureBox:** \
  A data-aware combo box control that enable users to select value of a locale
  field from a list of cultures (locales).

- **TDBCultureListBox:** \
  A data-aware list box control that enable users to  select value of a locale
  field from a list of cultures (locales).

- **TDBTerritoryLabel:** \
  A data-aware label control that displays value of a country/region field.

- **TDBTerritoryBox:** \
  A data-aware  combo box  control  that  enable  users  to select  value of a
  country/region field from a list of territories.

- **TDBTerritoryListBox:** \
  A data-aware  list box  control  that  enable  users  to  select  value of a
  country/region field from a list of territories.

- **TDBCurrencyLabel:** \
  A data-aware label control that displays value of a currency symbol field.

- **TDBCurrencyBox:** \
  A data-aware  combo box  control  that  enable  users  to select  value of a
  currency symbol field from a list of currencies.

- **TDBCurrencyListBox:** \
  A data-aware  list box  control  that  enable  users  to  select  value of a
  currency symbol field from a list of currencies.

- **TDBIntlDateTimeLabel:** \
  A data-aware label control to display localized value of a date-time field.

- **TDBIntlDatePicker:** \
  A data-aware control  that  enables the user to enter a localized date for a
  date field.

In addition, the i18n package has some additional classes for:

- decoding/encoding JSON formatted strings
- setting formatted content of rich edit controls using BBCode styled tags
- parsing and evaluating mathematical expressions in C/C++ language syntax
- parsing Pascal code to extract string literals and string constants

The i18n package also contains  the source code  of a comprehensive Delphi
application for editing/translating exported localizable strings.


INSTALLATION
------------

To install the i18n package on Delphi:

1. Open 'i18nPackages.groupproj' that is located in the 'Packages' folder.
2. Build 'i18n' and 'i18nDB' packages.
3. Install 'i18nDesign' package.


HOW TO USE
----------

To localize an application using the i18n package, follow these steps:

1. Drop an instance of TLocalizer component on a data module or your main form.

2. On each  form, frame, data  module, and  report of  your  application  drop a
   TTranslator component.

3. Double click on each instance of TTranslator component and select  properties
   and  string literals/constants  that need to be translated.  Then, export the
   selection into a file.

4. Assign the file name of the exported translatable items  in step 3 to the URI
   property of the TLocalizer component (step 1).

5. Drop an instance of TCultureBox or TCultureListBox  control on a  form, where
   the user selects  the UI language of the application.  Set Localizer property
   of the control to the TLocalizer component in (step 1).

6. Wherever you  need to display or input a date,  use an appropriate control of 
   the i18n  package.  Remember to  set  Localizer  property of  the control  to 
   the TLocalizer component (step 1).

7. Wherever you  format a number or date-time  value in your application, use an
   appropriate formatting method of the TLocalizer component (step 1).

At this  moment, your  application is  localized.  You just need  to translate the
exported localizable strings (step 3) to target languages of your application.  To
do so, use **i18nEditor**, which  you can  find its source code in `Editor` folder 
of this repository.


VERSION HISTORY
---------------

- **Version 1.0 (June 11, 2011)** 
  - First public release
>
- **Version 1.1 (June 13, 2011)** 
  - Made the package compatible with Delphi 2009. However, the package has some
    problems on Delphi 2009: Because of a few bugs in Controls unit,  mirroring 
    page and tab controls for right-to-left languages  causes child controls of 
    these controls go off the screen. Some other common controls may  also show
    odd behavior when toggle the layout's direction.
  - Fixed display problem of the date picker control on Windows XP.
  - Some minor tweaks.
>
- **Version 1.2 (August 19, 2012)**
  - Fixed the bug causing the invisible forms become visible after translation.
    Thanks to *Ahell* for reporting and fixing this bug.
>
- **Version 1.3 (November 4, 2012)**
  - Fixed the bug in TDBIntlDatePicker that causing the dataset goes wrongly to
    edit mode. Thanks to JHarding for reporting this bug.
  - Fixed the issue with the popup position of TIntlDatePicker control on multi
    monitor systems. Thanks to Keeny for reporting this problem.
>
- **Version 1.4 (December 2, 2012)**
  - Fixed the bug in applying translation on Delphi XE3.
    Thanks to David Esperalta for fixing the bug.
  - Handled the exceptions that may occur while getting translatable properties 
    of an OLE control. Thanks to *jjLopaz* for reporting this problem.
>
- **Version 1.5 (January 5, 2013)**
  - Fixed the compatibility issues with Delphi XE2 and XE3.
>
- **Version 1.6 (April 26, 2013)**
  - Added the missing piece of code that saves design-time value of Culture and
    CalendarType properties of the IntlDateTimeLabel control into DFM.
>
- **Version 1.7 (June 9, 2013)**
  - Fixed bug in parsing ternary operator of C-like expressions.
  - Optimized parsed tree of C-like expressions by pre-calculating the constant
    subexpressions. 
>
- **Version 1.8 (September 12, 2013)**
  - Added AdjustDays property and OnAdjustment event to THijriCalendar class to 
    make adjustment of Hijri dates possible.
>
- **Version 1.9 November 03, 2013)**
  - Added APIKey and UserIP  properties to the  TGoogleTranslator class.  These 
    properties are required for having a valid Google service call.
>
- **Version 1.10 January 25, 2014)**
  - Added x64 compilation support by *arhangelsoft*.
  - Fixed bug in the Hebrew Calendar, which resulting wrong month name when the
    selected language was also Hebrew.
