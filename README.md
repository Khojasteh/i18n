# DESCRIPTION

The i18n package is a  library  of Delphi components, classes, and procedures to
fully and  easily localize  Delphi applications  targeting the Windows platform,
and to change the locale of the applications on the fly.

There  are many  software tools  for localizing a Delphi  application, but  they
mostly  provide  support for  translation  of the user  interface  strings only.
However, the i18n package not  only  enables  developers to  select translatable
properties  and  string constants/literals within the IDE but also takes care of
plural forms, reading layout, formatting preferences, and the calendar system of
the target language.

You may want your application to be ready  for right-to-left languages, but you
do not  know which  language is right-to-left or even how it works? Don’t worry,
the i18n package handles  right-to-left  languages effortlessly. When the locale
of the application changes, and whenever needed, the i18n  package automatically
mirrors the layout  of the  application’s forms  according to the reading layout
of the selected language. You do not need to write a single line of code or even
revise it.

The Gregorian calendar  is the  most  used calendar  system in the world, but if
your application supports only  this calendar, it cannot be  truly localized for
all countries/regions. The i18n package knows  which calendar  system is used by
which country/region, and automatically formats date-time values in the calendar
system of the selected locale. You don't  need to modify  your code, because the
application still  gets all the date-time values in the Gregorian calendar  as a
standard TDateTime value.

Supported calendar systems:

- Julian Calendar
- Gregorian Calendar
- Hebrew or Jewish Calendar (הלוח העברי)
- Hijri or Islamic Calendar (التقويم الهجري)
- Jalali or Persian Calendar (گاهشمار جلالی)
- Japanese Emperor Era Calendar (和暦)
- Korean Tangun Era Calendar (단기)
- Taiwan Calendar (民國紀年)
- Buddhist Calendar (พุทธศักราช)


# COMPONENTS

The package includes the following components:

- **TLocalizer:** \
  Provides the core functionality for localizing an application.

- **TTranslator:** \
  Maintains list of  translatable  strings for a form, frame, data module, and
  so on. This component also translates the user interface strings and handles
  bi-directional mirroring of the controls.

- **TGoogleTranslator:** \
  An interface to the Google Translation API for automatic text translation.

- **TMessageDialog:** \
  Displays a localized message dialog.

- **TInputQueryDialog:** \
  Displays a localized input dialog for entering string, double, or integer values.

- **TIntlDateTimeLabel:** \
  A label control for displaying localized TDateTime values.

- **TIntlMonthCalendar:** \
  A month calendar control for displaying localized calendars.

- **TIntlDatePicker:** \
  A control for entering localized dates.

- **TFlagImageList:** \
  An image list component that provides country flag icons.

- **TImageLabel:** \
  A label control that can include an image on its face.

- **TCultureLabel:** \
  A label control for displaying culture (locale) name and country flag.

- **TCultureBox:** \
  A combo box control that represents a list of cultures (locales).

- **TCultureListBox:** \
  A list box control that represents a list of cultures (locales).

- **TCultureCheckListBox:** \
  A check list box control that represents a list of cultures (locales).

- **TTerritoryLabel:** \
  A label control for displaying country/region name and flag.

- **TTerritoryBox:** \
  A combo box control that represents a list of countries/regions.

- **TTerritoryListBox:** \
  A list box control that represents a list of countries/regions.

- **TTerritoryCheckListBox:** \
  A check list box control that represents a list of countries/regions.

- **TCurrencyLabel:** \
  A label control for displaying currency name and country flag.

- **TCurrencyBox:** \
  A combo box control that represents a list of currencies.

- **TCurrencyListBox:** \
  A list box control that represents a list of currencies.

- **TCurrencyCheckListBox:** \
  A check list box control that represents a list of currencies.

- **TDBImageLabel:** \
  A data-aware label control for displaying field values with a custom icon.

- **TDBCultureLabel:** \
  A data-aware label control for displaying locale field values.

- **TDBCultureBox:** \
  A data-aware combo box control for selecting locale field values from a
  list of cultures (locales).

- **TDBCultureListBox:** \
  A data-aware list box control for selecting locale field values from a
  list of cultures (locales).

- **TDBTerritoryLabel:** \
  A data-aware label control for displaying country/region field values.

- **TDBTerritoryBox:** \
  A data-aware combo box control for selecting country/region field values
  from a list of territories.

- **TDBTerritoryListBox:** \
  A data-aware list box control for selecting country/region field values
  from a list of territories.

- **TDBCurrencyLabel:** \
  A data-aware label control for displaying currency symbol field values.

- **TDBCurrencyBox:** \
  A data-aware combo box control for selecting currency symbol field values
  from a list of currencies.

- **TDBCurrencyListBox:** \
  A data-aware list box control for selecting currency symbol field values
  from a list of currencies.

- **TDBIntlDateTimeLabel:** \
  A data-aware label control for displaying localized date-time field values.

- **TDBIntlDatePicker:** \
  A data-aware control for entering localized dates in date fields.

Additional classes are included for:

- JSON encoding and decoding
- Rich edit content formatting using BBCode tags
- C/C++ expression parsing and evaluation
- Pascal code parsing to extract string literals and constants

The package includes source code for an editor application (i18nEditor) for
managing and translating localizable strings.


# INSTALLATION

To install the i18n package on Delphi:

1. Open `i18nPackages.groupproj` that is located in the `Packages` folder.
2. Build the `i18n` and `i18nDB` packages.
3. Install the `i18nDesign` package.


# USAGE

To localize an application:

1. Add a `TLocalizer` component to a data module or the main form.

2. Add a `TTranslator` component to each form, frame, data module, and report.

3. Double-click each `TTranslator` instance and select properties and string
   literals/constants to be translated. Export the selection to a file.

4. Assign the exported file name to the `URI` property of the `TLocalizer`
   instance (from step 1).

5. Add a `TCultureBox` or `TCultureListBox` control where users will select the
   UI language. Set its `Localizer` property to the `TLocalizer` instance.

6. For date display or input, use the appropriate i18n controls and set their
   `Localizer` property to the `TLocalizer` instance.

7. Use the `TLocalizer` formatting methods for number and date-time formatting.

Translate the exported strings (step 3) using the **i18nEditor** application.
Source code is available in the `Editor` folder.
