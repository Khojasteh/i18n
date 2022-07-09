{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements the localization components.
unit i18nLocalizer;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls,
  i18nCore, i18nCatalog, i18nPlurals, i18nCalendar, i18nHashList;

type

  TLocalizer = class;
  TTranslator = class;

  {$region 'xmldoc'}
  /// <summary>
  /// TTranslatables maintaines the list of translatable properties and string
  /// literals.</summary>
  {$endregion}
  TTranslatables = class(TPersistent)
  private
    fTranslator: TTranslator;
    fProperties: TTextItems;
    fLiterals: TTextItems;
    function GetDomainName: String;
    function GetDomainOwner: TComponent;
    procedure ReadProperties(Reader: TReader);
    procedure WriteProperties(Writer: TWriter);
    procedure ReadLiterals(Reader: TReader);
    procedure WriteLiterals(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the content of <see cref="Properties"/> and <see cref="Literals"/>
    /// properties as if they were published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies the object that owns this object.</summary>
    /// <returns>
    /// Returns the object that owns this object</returns>
    {$endregion}
    function GetOwner: TPersistent; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Normalizes translatable properties by removing the invalid entries,
    /// and retrieving their original values.</summary>
    {$endregion}
    procedure NormalizeProperties;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies value of the translatable properties to the actual properties.</summary>
    {$endregion}
    procedure ApplyProperties;
    {$region 'xmldoc'}
    /// <summary>
    /// Reverts value of translatable items to their original values.</summary>
    {$endregion}
    procedure DiscardTranslations;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.</summary>
    /// <param name="ATranslator">
    /// The <see cref="TTranslator"/> component that owns this instance.</param>
    {$endregion}
    constructor Create(ATranslator: TTranslator);
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies another object to this object.</summary>
    /// <param name="Source">
    /// The source object.</param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the top level component that owns the translatable properties and
    /// string literals.</summary>
    /// <seealso cref="DomainName"/>
    {$endregion}
    property DomainOwner: TComponent read GetDomainOwner;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the name of the text domain that maintains the translatable properties
    /// and string literals.</summary>
    /// <seealso cref="DomainOwner"/>
    {$endregion}
    property DomainName: String read GetDomainName;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the translatable properties.</summary>
    /// <seealso cref="Literals"/>
    {$endregion}
    property Properties: TTextItems read fProperties;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the translatable string literals.</summary>
    /// <seealso cref="Properties"/>
    {$endregion}
    property Literals: TTextItems read fLiterals;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TTranslator"/> component that owns this object.</summary>
    {$endregion}
    property Translator: TTranslator read fTranslator;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the notifications of the <see cref="TLocalizer"/>
  /// component.</summary>
  {$endregion}
  TLocalizerNotification = (
    {$region 'xmldoc'}
    /// The content of the <see cref="TLocalizer"/> component is about to be changed.
    {$endregion}
    lnChanging,
    {$region 'xmldoc'}
    /// The content of the <see cref="TLocalizer"/> component is changed.
    {$endregion}
    lnChanged,
    {$region 'xmldoc'}
    /// The translation catalog of the <see cref="TLocalizer"/> component is about to be changed.
    {$endregion}
    lnCatalogChanging,
    {$region 'xmldoc'}
    /// The translation catalog of the <see cref="TLocalizer"/> component is changed.
    {$endregion}
    lnCatalogChanged,
    {$region 'xmldoc'}
    /// The <see cref="TLocalizer.Culture"/> property is about to be changed.
    {$endregion}
    lnCultureChanging,
    {$region 'xmldoc'}
    /// The <see cref="TLocalizer.Culture"/> property is changed.
    {$endregion}
    lnCultureChanged,
    {$region 'xmldoc'}
    /// The <see cref="TLocalizer.BiDiMode"/> property is about to be changed.
    {$endregion}
    lnBiDiModeChanging,
    {$region 'xmldoc'}
    /// The <see cref="TLocalizer.BiDiMode"/> property is changed.
    {$endregion}
    lnBiDiModeChanged,
    {$region 'xmldoc'}
    /// The <see cref="TLocalizer.Options"/> property is about to be changed.
    {$endregion}
    lnOptionsChanging,
    {$region 'xmldoc'}
    /// The <see cref="TLocalizer.Options"/> property is changed.
    {$endregion}
    lnOptionsChanged
  );

  {$region 'xmldoc'}
  /// <summary>
  /// ILocalizerLink interface lets an object to be notified about changes in one
  /// or more <see cref="TLocalizer"/> components.</summary>
  /// <remarks>
  /// Use ILocalizerLink interface when defining objects that can be linked to one
  /// or more instances of <see cref="TLocalizer"/> components.
  /// </remarks>
  /// <seealso cref="TLocalizer.RegisterListener"/>
  /// <seealso cref="TLocalizer.UnregisterListener"/>
  {$endregion}
  ILocalizerLink = interface
    {$region 'xmldoc'}
    /// <summary>
    /// Notifies that a linked <see cref="TLocalizer"/> component is changed.</summary>
    /// <param name="Sender">
    /// The <see cref="TLocalizer"/> component who originated the notification.</param>
    /// <param name="Reason">
    /// The reason of the notification.</param>
    {$endregion}
    procedure LocalizerNotify(Sender: TLocalizer; Reason: TLocalizerNotification);
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the ways to look for state of a flag that
  /// is also manageable by a <see cref="TLocalizer"/> component.</summary>
  {$endregion}
  TLocalizerSwicth = (
    {$region 'xmldoc'}
    /// The flag is only on if it is actiavted in the related <see cref="TLocalizer"/>
    /// component. Otherwise the flag is off.
    {$endregion}
    lsDefault,
    {$region 'xmldoc'}
    /// The flag is always on regardless of its state in the related
    /// <see cref="TLocalizer"/> component.
    {$endregion}
    lsAlways,
    {$region 'xmldoc'}
    /// The flag is always offregardless of its state in the related
    /// <see cref="TLocalizer"/> component.
    {$endregion}
    lsNever
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the possible options that can be selected
  /// in a <see cref="TLocalizer"/> component.</summary>
  {$endregion}
  TLocalizerOption = (
    {$region 'xmldoc'}
    /// Automatically selects the active culture based on the user's locale settings.
    {$endregion}
    loAutoSetectLanguage,
    {$region 'xmldoc'}
    /// Sets BiDiMode of the application according to the active culture.
    {$endregion}
    loAdjustApplicationBiDiMode,
    {$region 'xmldoc'}
    /// Translates format settings according to the active culture.
    {$endregion}
    loAdjustFormatSettings,
    {$region 'xmldoc'}
    /// Loads the approperiate keyboard layout according to the active culture.
    {$endregion}
    loAdjustKeyboardLayout,
    {$region 'xmldoc'}
    /// When formatting numbers and date-time values, uses native digits of
    /// the active culture.
    {$endregion}
    loUseNativeDigits,
    {$region 'xmldoc'}
    /// When formatting dates, uses the active culture's calendar system instead
    /// of the default Gregorian Calendar.
    {$endregion}
    loUseNativeCalendar
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This data type represents the set of options that are selected is a
  /// <see cref="TLocalizer"/> component.</summary>
  {$endregion}
  TLocalizerOptions = set of TLocalizerOption;

  {$region 'xmldoc'}
  /// <summary>
  /// TLocalizerNotifyEvent is the type for event handlers that respond when a
  /// <see cref="TLocalizer"/> component sends a notification.</summary>
  /// <param name="Sender">
  /// The object that generated the event.</param>
  /// <param name="Reason">
  /// The reason of the notification.</param>
  {$endregion}
  TLocalizerNotifyEvent = procedure(Sender: TObject; Reason: TLocalizerNotification) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TLocalizer provides the core functionality for localizing an application.</summary>
  /// <remarks>
  /// TLocalizer provides properties and methods to manage how deep an application
  /// should be localized.
  ///
  /// TLocalizer can control the formatting parameters, the calendar system, the
  /// keyboard layout, and the application's bi-directional mode based on the selected
  /// locale.
  ///
  /// TLocalizer loads the translation catalog from an application's resource, a file,
  /// or a directory. Then, it provides a lists of <see cref="TCultureInfo"/> objects
  /// that represent the available translation languages. By selecting one of these
  /// <see cref="TCultureInfo"/> objects, TLocalizer automatically updates the locale
  /// specific settings of the application and notifies the linked <see cref="TTranslator"/>
  /// components to update the user interface of the application according to the selected
  /// language.
  ///
  /// Each application needs only one instance of the TLocalizer component. However,
  /// you can have more than one instance whenever it is needed. For example, suppose
  /// an application that can print reports in a language different from the user
  /// interface language. In this case, the application needs one instance of the
  /// TLocalizer component for managing the user interface and another one for handling
  /// the reports.
  ///
  /// It is highly recomended to use formatting functions of TLocalizer component to
  /// format values in your application. In this way, you have full control on how the
  /// localization should affect the formatting of values by just adjusting the properties
  /// of the TLocalizer component.
  /// </remarks>
  /// <seealso cref="TTranslator"/>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TLocalizer = class(TComponent)
  private
    class var
      TheFirstInstance: TLocalizer;
  private
    fURI: String;
    fCatalog: TTranslationCatalog;
    fPluralForms: TPluralForms;
    fOptions: TLocalizerOptions;
    fCulture: TCultureInfo;
    fClients: TList;
    fOnNotification: TLocalizerNotifyEvent;
    fBiDiToggled: Boolean;
    fUpdateCount: Integer;
    function GetCultures: TReadonlyCultureList;
    procedure SetURI(const Value: String);
    procedure SetCulture(Value: TCultureInfo);
    function GetCultureIndex: Integer;
    procedure SetCultureIndex(Value: Integer);
    procedure SetOptions(Value: TLocalizerOptions);
    function GetBiDiMode: TBiDiMode;
    procedure ReadTargetCulture(Reader: TReader);
    procedure WriteTargetCulture(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="Culture"/> property as if it was published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Initializes the component when it is first loaded into memory.</summary>
    {$endregion}
    procedure Loaded; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Notifies the linked opjects with a <see cref="TLocalizerNotification"/> of
    /// lnChanging.</summary>
    {$endregion}
    procedure BeginUpdate; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Notifies the linked opjects with a <see cref="TLocalizerNotification"/> of
    /// lnChanged.</summary>
    {$endregion}
    procedure EndUpdate; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnNotification"/> event.</summary>
    /// <param name="Reason">
    /// The reason of the notification.</param>
    {$endregion}
    procedure DoNotification(Reason: TLocalizerNotification); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Loads the translation catalog from the location specified by the <see cref="URI"/>
    /// property.</summary>
    {$endregion}
    procedure ReloadCatalog; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Prforms the required actions when the active culture changes.</summary>
    /// <remarks>
    /// ApplyCultureChanges updates the prural rule of <see cref="PluralForms"/>
    /// object based on the currently active language and and notifies the linked
    /// objects with a <see cref="TLocalizerNotification"/> of lnCultureChanged.
    ///
    /// Depending on the value of the <see cref="Options"/> property, ApplyCultureChanges
    /// may also call <see cref="AdjustApplicationBiDi"/>, <see cref="AdjustKeyboardLayout"/>,
    /// and <see cref="AdjustFormatSettings"/> methods.</remarks>
    {$endregion}
    procedure ApplyCultureChanges; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets the BiDiMode property of the application according to the language's
    /// direction of the currently active culture.
    /// NOTE: This method is called automatically if loAdjustApplicationBiDiMode
    /// is toggled on in the component's <see cref="Options"/>.</summary>
    /// <seealso cref="AdjustKeyboardLayout"/>
    /// <seealso cref="AdjustFormatSettings"/>
    {$endregion}
    procedure AdjustApplicationBiDi; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Loads the proper keyboard layout based on the locale of the currently active
    /// culture.
    /// NOTE: This method is called automatically if loAdjustKeyboardLayout is
    /// toggled on in the component's <see cref="Options"/>.</summary>
    /// <seealso cref="AdjustApplicationBiDi"/>
    /// <seealso cref="AdjustFormatSettings"/>
    {$endregion}
    procedure AdjustKeyboardLayout; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the global format settings of the application according to the locale
    /// of the currently active culture.
    /// NOTE: This method is called automatically if loAdjustFormatSettings is
    /// toggled on in the component's <see cref="Options"/>.</summary>
    /// <seealso cref="AdjustApplicationBiDi"/>
    /// <seealso cref="AdjustKeyboardLayout"/>
    {$endregion}
    procedure AdjustFormatSettings; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds the <see cref="TTextTranslation"/> object of a given text string in the
    /// loaded translation catalog.</summary>
    /// <param name="Text">
    /// The text to be located. It is in the original language of the application.</param>
    /// <param name="DomainName">
    /// Determines the name of the domain where the translation is looked up.
    ///
    /// A text domain is a set of translatable strings in a translation catalog, and
    /// the domain name is usually the name of the container (e.g. form, frame, data
    /// module, etc.) where the text strings are referenced in.
    ///
    /// If the DomainName parameter is omitted or an empty string is passed, only
    /// the translatable strings of the domain related to the currently active form
    /// will be looked up.
    ///
    /// If '*' is used as the DomainName parameter, all the text domains will be
    /// looked up.</param>
    /// <returns>
    /// The <see cref="TTextTranslation"/> object of the text string or <see langword="nil"/>
    /// if the text is not found.</returns>
    {$endregion}
    function FindTranslation(const Text: String; const DomainName: String = ''): TTextTranslation; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the component can translate properties and string literals that
    /// are maintained by a specified <see cref="TTranslatables"/> object.</summary>
    /// <param name="Translatables">
    /// The <see cref="TTranslatables"/> object that maintains translatable properties
    /// and string literals.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the component is able to translate the specified
    /// translatable items. Otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="Translate"/>
    {$endregion}
    function CanTranslate(Translatables: TTranslatables): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Attempts to translate properties and string literals that are maintained by
    /// a specified <see cref="TTranslatables"/> object.</summary>
    /// <param name="Translatables">
    /// The <see cref="TTranslatables"/> object that maintains translatable properties
    /// and string literals.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the component was able to translate the
    /// specified translatable items. Otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="CanTranslate"/>
    {$endregion}
    function Translate(Translatables: TTranslatables): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Broadcasts a notification to the linked objects.</summary>
    /// <param name="Reason">
    /// The reason of the notification.</param>
    /// <seealso cref="ILocalizerLink"/>
    /// <seealso cref="RegisterListener"/>
    /// <seealso cref="UnregisterListener"/>
    {$endregion}
    procedure NotifyClients(Reason: TLocalizerNotification);
    {$region 'xmldoc'}
    /// <summary>
    /// Provides direct access to the <see cref="TTranslationCatalog"/> object that
    /// maintains the loaded translation catalog.</summary>
    {$endregion}
    property Catalog: TTranslationCatalog read fCatalog;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides direct access to the <see cref="PluralForms"/> object that decides
    /// which plural form of the active langauge is correct for expressing a noun or
    /// unit.</summary>
    {$endregion}
    property PluralForms: TPluralForms read fPluralForms;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the component.</summary>
    /// <param name="AOwner">
    /// The owner component.</param>
    {$endregion}
    constructor Create(AOwner: TComponent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the component and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the least recently created instance of the <see cref="TLocalizer"/>
    /// component that is not destroyed yet. Usually this instance is the only instance
    /// of the <see cref="TLocalizer"/> component in an application.</summary>
    /// <returns>
    /// The oldest instance of the <see cref="TLocalizer"/> component or <see langword="nil"/>
    /// if no instance is in memory right now.</returns>
    {$endregion}
    class function FirstInstance: TLocalizer; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Enables an object that implements the <see cref="ILocalizerLink"/> interface
    /// to be notified about changes in this instance of the component.</summary>
    /// <param name="Client">
    /// The object with <see cref="ILocalizerLink"/> interface that is interested
    /// in receiving the notifications.</param>
    /// <seealso cref="ILocalizerLink"/>
    /// <seealso cref="UnregisterListener"/>
    {$endregion}
    procedure RegisterListener(Client: ILocalizerLink);
    {$region 'xmldoc'}
    /// <summary>
    /// Stops sending the notifications to an object that was previously registered
    /// by a call to <see cref="RegisterListener"/> method.</summary>
    /// <param name="Client">
    /// The object with <see cref="ILocalizerLink"/> interface that is no more
    /// interested in receiving the notifications.</param>
    /// <seealso cref="ILocalizerLink"/>
    /// <seealso cref="RegisterListener"/>
    {$endregion}
    procedure UnregisterListener(Client: ILocalizerLink);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the component is ready to localize the application.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the component is fully loaded and its
    /// <see cref="Culture"/> property is assigned. Otherwise, returns
    /// <see langword="false"/>.</returns>
    {$endregion}
    function IsReady: Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Attempts to translate a text string into the currently active language, by
    /// looking up the translation in the loaded translation catalog.</summary>
    /// <param name="Text">
    /// The text to be translated. It is in the original language of the application.</param>
    /// <param name="DomainName">
    /// Determines the name of the domain where the translation is looked up.
    ///
    /// A text domain is a set of translatable strings in a translation catalog, and
    /// the domain name is usually the name of the container (e.g. form, frame, data
    /// module, etc.) where the text strings are referenced in.
    ///
    /// If the DomainName parameter is omitted or an empty string is passed, only
    /// the translatable strings of the domain related to the currently active form
    /// will be looked up.
    ///
    /// If '*' is used as the DomainName parameter, all the text domains will be
    /// looked up.</param>
    /// <returns>
    /// Returns the translated text if it is found. Otherwise, returns the origial
    /// text.</returns>
    /// <seealso cref="GetNText"/>
    {$endregion}
    function GetText(const Text: String; const DomainName: String = ''): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Attempts to translate a text string into the currently active language, by
    /// looking up the appropriate plural form of the translation in the translation
    /// data.</summary>
    /// <param name="TextVariants">
    /// The plural forms of the text to be located, in the original language of the
    /// application.
    ///
    /// The plural forms are grammatical variants of a text depending on a number. For
    /// example, The English language has two plural forms: singular and plural.</param>
    /// <param name="N">
    /// The number (in addition to the language) that determines which plural form
    /// should be selected.</param>
    /// <param name="DomainName">
    /// Determines the name of the domain where the translation is looked up.
    ///
    /// A text domain is a set of translatable strings in a translation catalog, and
    /// the domain name is usually the name of the container (e.g. form, frame, data
    /// module, etc.) where the text strings are referenced in.
    ///
    /// If the DomainName parameter is omitted or an empty string is passed, only
    /// the translatable strings of the domain related to the currently active form
    /// will be looked up.
    ///
    /// If '*' is used as the DomainName parameter, all the text domains will be
    /// looked up.</param>
    /// <returns>
    /// Returns the appropriate plural form of the translated text if it is found.
    /// Otherwise, returns the appropriate plural form of the origial text.</returns>
    /// <seealso cref="GetText"/>
    {$endregion}
    function GetNText(const TextVariants: array of String; N: Integer; const DomainName: String = ''): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a formatted string assembled from a format string in C# language style
    /// and an array of arguments.</summary>
    /// <remarks>
    /// FormatCS returns a string produced according to the formatting string <paramref name="Fmt"/>
    /// and currently active culture.
    ///
    /// NOTE: Regardless of value of the <see cref="Options"/> property, FormatCS always
    /// uses the native calendar system of the currently active culture to format a date
    /// value.</remarks>
    /// <param name="Fmt">
    /// The format string. See <see cref="i18nUtils.FormatCS"/> for details.</param>
    /// <param name="Args">
    /// The values to be formatted.</param>
    /// <returns>
    /// The formatted string.</returns>
    {$endregion}
    function FormatCS(const Fmt: String; const Args: array of Variant): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a formatted string assembled from a format string and an array of
    /// arguments.</summary>
    /// <remarks>
    /// Format method acts like Format function of Delphi, except that it uses the
    /// currently active culture's preferences for formatting the values.
    ///
    /// The <see cref="Options"/> property determines whether the numbers should be
    /// expressed in native digits or not.</remarks>
    /// <param name="Fmt">
    /// The format string as in used by Delphi's standard Format function.</param>
    /// <param name="Args">
    /// The array of arguments to apply to the format specifiers in the format string.</param>
    /// <returns>
    /// The formatted string.</returns>
    {$endregion}
    function Format(const Fmt: String; const Args: array of const): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Formats a floating point value.</summary>
    /// <remarks>
    /// FormatNumber method acts like FormatFloat function of Delphi, except that it
    /// uses the currentrly active culture's preferences for formatting the number.
    ///
    /// The <see cref="Options"/> property determines whether the number should be
    /// expressed in native digits or not.</remarks>
    /// <param name="Fmt">
    /// The format string.</param>
    /// <param name="Value">
    /// The floating point value to format.</param>
    /// <returns>
    /// The formatted string.</returns>
    {$endregion}
    function FormatNumber(const Fmt: String; const Value: Extended): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Formats a floating point value as a percentage.</summary>
    /// <remarks>
    /// FormatPercent represents a floating point value as a percentage using the
    /// percentage format of the currently active culture.
    ///
    /// The <see cref="Options"/> property determines whether the percentage should
    /// be expressed in native digits or not.</remarks>
    /// <param name="Value">
    /// The floating point value to format.</param>
    /// <param name="Decimals">
    /// The number of decimals.</param>
    /// <returns>
    /// The formatted string.</returns>
    {$endregion}
    function FormatPercent(const Value: Extended; Decimals: Integer = 2): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Formats a <see cref="TDateTime"/> value.</summary>
    /// <remarks>
    /// FormatDateTime formats the <see cref="TDateTime"/> value given by <paramref name="Value"/>
    /// using the format given by <paramref name="Fmt"/> and currently active culture.
    ///
    /// The <see cref="Options"/> property determines whether the native calendar
    /// system and the native digits should be used for expressing the date and
    /// time components.</remarks>
    /// <param name="Fmt">
    /// The format string. See <see cref="TCalendar.Format"/> method for details.</param>
    /// <param name="Value">
    /// The <see cref="TDateTime"/> value to format.</param>
    /// <returns>
    /// The formatted date-time string.</returns>
    /// <seealso cref="TryParseDateTime"/>
    /// <seealso cref="ParseDateTime"/>
    {$endregion}
    function FormatDateTime(const Fmt: String; const DateTime: TDateTime): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string to a <see cref="TDateTime"/> value using a specified format
    /// string.</summary>
    /// <remarks>
    /// The ParseDateTime method scans the string given by <paramref name="Str"/>
    /// using the format string given by <paramref name="Fmt"/> to extract its
    /// date and time values.
    ///
    /// The <see cref="Options"/> property determines whether the ParseDateTime
    /// method should use the native calendar system of the currently active culture
    /// or the Gregorian calendar to extract the date and time components from the
    /// input string.</remarks>
    /// <param name="Fmt">
    /// The format string. See the <see cref="TCalendar.Format"/> method for details.</param>
    /// <param name="Str">
    /// The date and time value as a string.</param>
    /// <returns>
    /// The <see cref="TDateTime"/> value.</returns>
    /// <exception cref="EConvertError">
    /// Occurs when the conversion is failed.</exception>
    /// <seealso cref="TryParseDateTime"/>
    /// <seealso cref="FormatDateTime"/>
    {$endregion}
    function ParseDateTime(const Fmt: String; const Str: String): TDateTime;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a string to a <see cref="TDateTime"/> value using a specified format
    /// string.</summary>
    /// <remarks>
    /// The TryParseDateTime method scans the string given by <paramref name="Str"/>
    /// using the format string given by <paramref name="Fmt"/> to extract its
    /// date and time values.
    ///
    /// The <see cref="Options"/> property determines whether the TryParseDateTime
    /// method should use the native calendar system of the currently active culture
    /// or the Gregorian calendar to extract the date and time components from the
    /// input string.</remarks>
    /// <param name="Fmt">
    /// The format string. See the <see cref="TCalendar.Format"/> method for details.</param>
    /// <param name="Str">
    /// The date and time value as a string.</param>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the conversion is succeeded, otherwise
    /// returns <see langword="false"/>.</returns>
    /// <seealso cref="ParseDateTime"/>
    /// <seealso cref="FormatDateTime"/>
    {$endregion}
    function TryParseDateTime(const Fmt: String; const Str: String; var DateTime: TDateTime): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a string that its embeded digits will always be displayed in native
    /// of the currently active culture, regardless of Windows settings.</summary>
    /// <remarks>
    /// Based on the user's selection (configurable via Windows control panel), Windows
    /// may display nominal digits (0-9) in different digit shapes.
    ///
    /// NativeDigits substitudes the nominal digits in the string with the native digits
    /// of the currently active culture and inserts appropriate Unicode control character
    /// in front of the string to prevent automatic digit substitution by Windows.</remarks>
    /// <param name="Str">
    /// The string to convert.</param>
    /// <returns>
    /// The string that its digits are fixed as native.</returns>
    {$endregion}
    function NativeDigits(const Str: String): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the available cultures that the application can be localized to.</summary>
    {$endregion}
    property Cultures: TReadonlyCultureList read GetCultures;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the BiDiMode of the currently active culture.</summary>
    {$endregion}
    property BiDiMode: TBiDiMode read GetBiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether direction of the currently active language differs from the
    /// direction of the application's original language.</summary>
    {$endregion}
    property IsBiDiToggled: Boolean read fBiDiToggled;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the index of currently active culture in the list of available
    /// cultures that the application can be localized to.</summary>
    {$endregion}
    property CultureIndex: Integer read GetCultureIndex write SetCultureIndex;
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the location of the translation catalog to load.</summary>
    /// <remarks>
    /// The URI property determines the location of the translation catalog that
    /// is required for translating the application's strings.
    ///
    /// The URI can be simply a path to a translation catalog file, but it can also
    /// address a directory or an application's resource.
    ///
    /// <list type="table">
    ///   <listheader>
    ///     <term>URI Format</term>
    ///     <description>Description</description>
    ///   </listheader>
    ///   <item>
    ///     <term>
    ///     res:&lt;ResName&gt;[.&lt;ResType&gt;]</term>
    ///     <description>
    ///     Loads the translation catalog from an application's resource.</description>
    ///     <example><code>
    ///     Localizer1.URI := 'res:translations';        // ResType is RCDATA
    ///     Localizer2.URI := 'res:translations.i18n';
    ///     </code></example>
    ///   </item>
    ///   <item>
    ///     <term>
    ///     dir:&lt;Directory&gt;\[FileMask]</term>
    ///     <description>
    ///     Merges the translation catalogs found in a specified directory, and
    ///     loads them.</description>
    ///     <example><code>
    ///     Localizer1.URI := 'dir:langs\';              // FileMask is *.i18n
    ///     Localizer2.URI := 'dir:langs\*.*';
    ///     </code></example>
    ///   </item>
    ///   <item>
    ///     <term>
    ///     dirs:&lt;Directory&gt;\[FileMask]</term>
    ///     <description>
    ///     Merges the translation catalogs found in a specified directory including
    ///     its sub-directories, and loads them.</description>
    ///     <example><code>
    ///     Localizer1.URI := 'dirx:langs\';             // FileMask is *.i18n
    ///     Localizer2.URI := 'dirx:langs\*.*';
    ///     </code></example>
    ///   </item>
    ///   <item>
    ///     <term>
    ///     [file:]&lt;FilePath&gt;</term>
    ///     <description>
    ///     Load the translation catalog from a specified file.</description>
    ///     <example><code>
    ///     Localizer1.URI := 'file:langs\MyApp.i18n';
    ///     Localizer2.URI := 'langs\MyApp.i18n';
    ///     </code></example>
    ///   </item>
    /// </list>
    ///
    /// The file and directory paths can use environment variables.
    /// <example><code>
    /// Localizer1.URI := 'dir:%ALLUSERSPROFILE%\MyApp\';
    /// Localizer2.URI := '%USERPROFILE%\MyApp\myapp.i18n';
    /// </code></example>
    ///
    /// NOTE: For convenience, a relative file or directory is considered ro be
    /// relative to the application's directory instead of the current directory.
    /// </remarks>
    /// <exception cref="ETranslationCatalogError">
    /// Occurs when a problem is encountered while loading the translation catalog.</exception>
    {$endregion}
    property URI: String read fURI write SetURI;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently active culture.</summary>
    {$endregion}
    property Culture: TCultureInfo read fCulture write SetCulture;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the component's preferences.</summary>
    /// <remarks>
    /// The Options property determines whether the component should automatically
    /// perform some tasks or not. It also specifies how the formatting methods
    /// should express the numbers and the components of dates.</remarks>
    {$endregion}
    property Options: TLocalizerOptions read fOptions write SetOptions default
      [loAdjustApplicationBiDiMode, loAdjustFormatSettings, loUseNativeDigits, loUseNativeCalendar];
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the component's properties change.</summary>
    {$endregion}
    property OnNotification: TLocalizerNotifyEvent read fOnNotification write fOnNotification;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TTranslator maintains the list of translatable strings for a form, frame,
  /// data module, and so on.</summary>
  /// <remarks>
  /// Use TTranslator to select which properties and literal strings are translatable.
  ///
  /// To translate controls' properties or messages of a form (or frame, data module,
  /// etc.), drop an instance of TTranslator component on the form. Then, double click
  /// on the component to bring up its component's editor.
  ///
  /// The component editor lets you to select which properties and literal strings
  /// should be translated. You can also export the selected items into a translation
  /// data file, so that you can translate the strings later using the translator
  /// application (i18nHelper.exe). After clicking on OK button of the component's
  /// editor, the source code will be updated automatically to reflect changes you
  /// made in the translatable strings.
  ///
  /// After selecting the translatable strings, the next and of course the last step
  /// is assigning an instance of <see cref="TLocalizer"/> component to the
  /// <see cref="Localizer"/> property. The <see cref="TLocalizer"/> component provides
  /// the translations of the strings and specifies what is the current user interface
  /// language of the application.
  ///
  /// TTranslator has some events to notify you about the changes in the user interface
  /// of the owner component. The <see cref="OnLoaded"/> event occurs only once when the
  /// component is loaded into memory. The other events occur whenever the user interface
  /// language is changed. The <see cref="OnBeforeFlipLayout"/> and <see cref="OnAfterFlipLayout"/>
  /// events occur only when the direction of the new language is different from the last
  /// one. However, the <see cref="OnBeforeTranslate"/> and <see cref="OnAfterTranslate"/>
  /// events always occur when the language changes. The order of the events is in the
  /// same order that they are mentioned here.
  ///
  /// NOTE: When you drop an instance of TTranslator component on a form (or frame), it
  /// resets the ParentBiDiMode of the owner form (or frame). The component manages the
  /// BiDiMode of the owner form (or frame) if <see cref="AutoBiDiLayout"/> property is
  /// set to <see langword="true"/>.</remarks>
  /// <seealso cref="TLocalizer"/>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTranslator = class(TComponent, ILocalizerLink)
  private
    fLocalizer: TLocalizer;
    fTranslatables: TTranslatables;
    fCurrentCulture: TCultureInfo;
    fAutoBiDiLayout: Boolean;
    fOnLoaded: TNotifyEvent;
    fOnBeforeFlipLayout: TNotifyEvent;
    fOnAfterFlipLayout: TNotifyEvent;
    fOnBeforeTranslate: TNotifyEvent;
    fOnAfterTranslate: TNotifyEvent;
    fLayoutFlipped: Boolean;
    fUpdateCount: Integer;
    fActiveControl: TWinControl;
    procedure SetLocalizer(Value: TLocalizer);
    procedure SetTranslatables(Value: TTranslatables);
    procedure SetAutoBiDiLayout(Value: Boolean);
    procedure Sync;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Notifies the component about changes in the linked <see cref="TLocalizer"/>
    /// component.</summary>
    /// <param name="Sender">
    /// The <see cref="TLocalizer"/> component who originated the notification.</param>
    /// <param name="Reason">
    /// The reason of the notification.</param>
    {$endregion}
    procedure LocalizerNotify(Sender: TLocalizer; Reason: TLocalizerNotification);
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to notifications that components are about to be created or destroyed.</summary>
    /// <param name="AComponent">
    /// The component, which is generated the notification.</param>
    /// <param name="Operation">
    /// Indicates whether the component is created or destroyed.</param>
    {$endregion}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Initializes the component when it is first loaded into memory.</summary>
    {$endregion}
    procedure Loaded; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Attempts to translates the translatable properties and literals using the
    /// information provided by the <see cref="Localizer"/> component. If the
    /// translation was successful, it then applies the translations to the actual
    /// properties of the owner component.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the translation is successful. Otherwise,
    /// returns <see langword="false"/>.</returns>
    {$endregion}
    function Translate: Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Horizontally flips content of the owner component.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the layout is flipped. Otherwise, returns
    /// <see langword="false"/>.</returns>
    /// <seealso cref="NeedsFlipLayout"/>
    /// <seealso cref="IsLayoutFlipped"/>
    {$endregion}
    function FlipLayout: Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the controls of the owner component should be flipped
    /// to reflect the direction of the currently active language.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the layout should be flipped. Otherwise,
    /// returns <see langword="false"/>.</returns>
    /// <seealso cref="FlipLayout"/>
    /// <seealso cref="IsLayoutFlipped"/>
    {$endregion}
    function NeedsFlipLayout: Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnLoaded"/> event.</summary>
    {$endregion}
    procedure DoLoaded; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnBeforeTranslate"/> event.</summary>
    {$endregion}
    procedure DoBeforeTranslate; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnAfterTranslate"/> event.</summary>
    {$endregion}
    procedure DoAfterTranslate; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnBeforeFlipLayout"/> event.</summary>
    {$endregion}
    procedure DoBeforeFlipLayout; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnAfterFlipLayout"/> event.</summary>
    {$endregion}
    procedure DoAfterFlipLayout; virtual;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the component.</summary>
    /// <param name="AOwner">
    /// The owner component.</param>
    {$endregion}
    constructor Create(AOwner: TComponent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the component and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents the control that owns this instance of TTranslator to draw itself
    /// on the screen.</summary>
    /// <seealso cref="EndUpdate"/>
    {$endregion}
    procedure BeginUpdate; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Reenables the control that owns this instance of TTranslator to draw itself
    /// on the screen.</summary>
    /// <seealso cref="BeginUpdate"/>
    {$endregion}
    procedure EndUpdate; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Attempts to translate a text string into the currently active language, by
    /// looking up the translation of the translatable string literals.</summary>
    /// <param name="Text">
    /// The text to be translated. It is in the original language of the application.</param>
    /// <returns>
    /// Returns the translated text if it is found. Otherwise, returns the origial
    /// text.</returns>
    /// <seealso cref="GetNText"/>
    {$endregion}
    function GetText(const Text: String): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Attempts to translate a text string into the currently active language, by
    /// looking up the appropriate plural form of the translation of the translatable
    /// string literals.</summary>
    /// <param name="TextVariants">
    /// The plural forms of the text to be located, in the original language of the
    /// application.
    ///
    /// The plural forms are grammatical variants of a text depending on a number. For
    /// example, The English language has two plural forms: singular and plural.</param>
    /// <param name="N">
    /// The number (in addition to the language) that determines which plural form
    /// should be selected.</param>
    /// <returns>
    /// Returns the appropriate plural form of the translated text if it is found.
    /// Otherwise, returns the appropriate plural form of the origial text.</returns>
    /// <seealso cref="GetText"/>
    {$endregion}
    function GetNText(const TextVariants: array of String; N: Integer): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the content the owner component is horizontally flipped
    /// to reflect the direction of the active language.</summary>
    {$endregion}
    property IsLayoutFlipped: Boolean read fLayoutFlipped;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the currently active culture, which determines the language of
    /// translations.</summary>
    {$endregion}
    property CurrentCulture: TCultureInfo read fCurrentCulture;
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the conponent should flip content of its owner
    /// windowed control to reflect the direction of the active language.</summary>
    {$endregion}
    property AutoBiDiLayout: Boolean read fAutoBiDiLayout write SetAutoBiDiLayout default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component's that provides the
    /// translation of the strings and determines the currently active language.</summary>
    {$endregion}
    property Localizer: TLocalizer read fLocalizer write SetLocalizer;
    {$region 'xmldoc'}
    /// <summary>
    /// Gives direct access to the list of translatable properties and string literals
    /// of the owner component.</summary>
    {$endregion}
    property Translatables: TTranslatables read fTranslatables write SetTranslatables;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the component is first loaded into memory.</summary>
    /// <remarks>
    /// Write an OnLoaded event handler to take specific action immediately after
    /// the component is loaded into memory. OnLoaded is particularly useful for
    /// taking initialization actions.</remarks>
    {$endregion}
    property OnLoaded: TNotifyEvent read fOnLoaded write fOnLoaded;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the screen layout is about to be flipped.</summary>
    /// <seealso cref="OnAfterFlipLayout"/>
    {$endregion}
    property OnBeforeFlipLayout: TNotifyEvent read fOnBeforeFlipLayout write fOnBeforeFlipLayout;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the screen layout is flipped.</summary>
    /// <seealso cref="OnBeforeFlipLayout"/>
    {$endregion}
    property OnAfterFlipLayout: TNotifyEvent read fOnAfterFlipLayout write fOnAfterFlipLayout;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the translatable strings are about to be translated.</summary>
    /// <seealso cref="OnAfterTranslate"/>
    {$endregion}
    property OnBeforeTranslate: TNotifyEvent read fOnBeforeTranslate write fOnBeforeTranslate;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the translatable strings are translated and applied on the screen.</summary>
    /// <seealso cref="OnBeforeTranslate"/>
    {$endregion}
    property OnAfterTranslate: TNotifyEvent read fOnAfterTranslate write fOnAfterTranslate;
  end;

{$region 'xmldoc'}
/// <summary>
/// Attempts to translate a text string into the currently active language, by
/// looking up the translation in the translation catalog of the least recentrly
/// created instance of the application's <see cref="TLocalizer"/> component.</summary>
/// <param name="Text">
/// The text to be translated. It is in the original language of the application.</param>
/// <param name="DomainName">
/// Determines the name of the domain where the translation is looked up.
///
/// A text domain is a set of translatable strings in a translation catalog, and
/// the domain name is usually the name of the container (e.g. form, frame, data
/// module, etc.) where the text strings are referenced in.
///
/// If the DomainName parameter is omitted or an empty string is passed, only
/// the translatable strings of the domain related to the currently active form
/// will be looked up.
///
/// If '*' is used as the DomainName parameter, all the text domains will be
/// looked up.</param>
/// <returns>
/// Returns the translated text if it is found. Otherwise, returns the origial
/// text.</returns>
/// <seealso cref="TLocalizer.GetText"/>
/// <seealso cref="TLocalizer.GetNText"/>
/// <seealso cref="GetNText"/>
{$endregion}
function GetText(const Text: String; const DomainName: String = ''): String;

{$region 'xmldoc'}
/// <summary>
/// Attempts to translate a text string into the currently active language, by
/// looking up the appropriate plural form of the translation in the translation
/// catalog of the least recentrly created instance of the application's
/// <see cref="TLocalizer"/> component.</summary>
/// <param name="TextVariants">
/// The plural forms of the text to be located, in the original language of the
/// application.
///
/// The plural forms are grammatical variants of a text depending on a number. For
/// example, The English language has two plural forms: singular and plural.</param>
/// <param name="N">
/// The number (in addition to the language) that determines which plural form
/// should be selected.</param>
/// <param name="DomainName">
/// Determines the name of the domain where the translation is looked up.
///
/// A text domain is a set of translatable strings in a translation catalog, and
/// the domain name is usually the name of the container (e.g. form, frame, data
/// module, etc.) where the text strings are referenced in.
///
/// If the DomainName parameter is omitted or an empty string is passed, only
/// the translatable strings of the domain related to the currently active form
/// will be looked up.
///
/// If '*' is used as the DomainName parameter, all the text domains will be
/// looked up.</param>
/// <returns>
/// Returns the appropriate plural form of the translated text if it is found.
/// Otherwise, returns the appropriate plural form of the origial text.</returns>
/// <seealso cref="TLocalizer.GetNText"/>
/// <seealso cref="TLocalizer.GetText"/>
/// <seealso cref="GetText"/>
{$endregion}
function GetNText(const TextVariants: array of String; N: Integer; const DomainName: String = ''): String;

{$region 'xmldoc'}
/// <summary>
/// Collects the list of translatable properties for a specified component and
/// children of the component.</summary>
/// <param name="Root">
/// The component to collect its translatable properties.</param>
/// <param name="Properties">
/// The <see cref="TTextItem"/> object that receives the translatable properties
/// of the specified component.</param>
/// <seealso cref="SetTranslatableProperties"/>
{$endregion}
procedure GetTranslatableProperties(Root: TComponent; Properties: TTextItems); overload;

{$region 'xmldoc'}
/// <summary>
/// Updates the specified properties of a component and children of the component
/// with the provided translations.</summary>
/// <param name="Root">
/// The component to translate its properties.</param>
/// <param name="Properties">
/// The <see cref="TTextItem"/> object that maintains the list of translatable
/// properties of the component, including their translations.</param>
/// <seealso cref="GetTranslatableProperties"/>
{$endregion}
procedure SetTranslatableProperties(Root: TComponent; Properties: TTextItems); overload;

implementation

uses
  TypInfo, Types, Forms, ComCtrls, Contnrs, StrUtils, ListActns, i18nMD5,
  i18nUtils, i18nZStrList, i18nBiDi, i18nUnicode, i18nCalGregorian;

var
  TheDefaultPluralForms: TPluralForms = nil;

{ Local Helper Functions }

function DefaultPluralForms: TPluralForms;
begin
  if not Assigned(TheDefaultPluralForms) then
    TheDefaultPluralForms := TPluralForms.Create(GetApplicationDefaultCulture.PluralRule);
  Result := TheDefaultPluralForms;
end;

function UniqueKeyOf(const Str: String): String; overload; inline;
begin
  Result := MD5(Str);
end;

function UniqueKeyOf(const Strs: array of String): String; overload;
begin
  Result := '#' + UniqueKeyOf(ZStrings.Construct(Strs));
end;

procedure WriteListViewItems(Stream: TStream; ListView: TListView);
var
  R, C: Integer;
begin
  with TWriter.Create(Stream, 1024) do
    try
      WriteListBegin;
      for R := 0 to ListView.Items.Count - 1 do
        with ListView.Items[R] do
        begin
          WriteString(Caption);
          WriteBoolean(Checked);
          WriteInteger(Indent);
          WriteInteger(GroupID);
          WriteInteger(ImageIndex);
          WriteInteger(OverlayIndex);
          WriteInteger(StateIndex);
          WriteListBegin;
          for C := 0 to SubItems.Count - 1 do
          begin
            WriteString(SubItems[C]);
            WriteInteger(SubItemImages[C]);
          end;
          WriteListEnd;
        end;
      WriteListEnd;
      WriteInteger(ListView.ItemIndex);
    finally
      Free;
    end;
end;

procedure ReadListViewItems(Stream: TStream; ListView: TListView);
var
  C: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    with TReader.Create(Stream, 1024) do
      try
        ReadListBegin;
        while not EndOfList do
          with ListView.Items.Add do
          begin
            Caption := ReadString;
            Checked := ReadBoolean;
            Indent := ReadInteger;
            GroupID := ReadInteger;
            ImageIndex := ReadInteger;
            OverlayIndex := ReadInteger;
            StateIndex := ReadInteger;
            ReadListBegin;
            while not EndOfList do
            begin
              C := SubItems.Add(ReadString);
              SubItemImages[C] := ReadInteger;
            end;
            ReadListEnd;
          end;
        ReadListEnd;
        ListView.ItemIndex := ReadInteger;
      finally
        Free;
      end;
  finally
    ListView.Items.EndUpdate;
  end;
end;

{ Global Helper Functions }

function GetText(const Text: String; const DomainName: String): String;
begin
  if TLocalizer.FirstInstance <> nil then
    Result := TLocalizer.FirstInstance.GetText(Text, DomainName)
  else
    Result := Text;
end;

function GetNText(const TextVariants: array of String; N: Integer;
  const DomainName: String): String;
var
  Index: Integer;
begin
  if Length(TextVariants) = 0 then
    Result := ''
  else if TLocalizer.FirstInstance <> nil then
    Result := TLocalizer.FirstInstance.GetNText(TextVariants, N, DomainName)
  else
  begin
    Index := DefaultPluralForms.IndexOf(N);
    if Index > High(TextVariants) then
      Index := High(TextVariants);
    Result := TextVariants[Index];
  end;
end;

procedure GetTranslatableProperties(Instance: TObject; const ParentPath: String;
  Properties: TTextItems); overload;

  procedure AddProperty(const PropPath, PropValue: String);
  begin
    if IsStringTranslatable(PropValue) then
      Properties.Add(PropPath).OriginalValue := PropValue;
  end;

var
  PropList: PPropList;
  SubInstance: TObject;
  PropertyCount, I: Integer;
  Node: TTreeNode;
begin
  if Instance is TStrings then
  begin
    if AnsiEndsText('.Lines', ParentPath) or AnsiEndsText('.SQL', ParentPath) then
      AddProperty(ParentPath + '.Text', TStrings(Instance).Text)
    else
      for I := 0 to TStrings(Instance).Count - 1 do
        AddProperty(ParentPath + '[' + IntToStr(I) + ']', TStrings(Instance).Strings[I]);
  end
  else if Instance is TCollection then
  begin
    for I := 0 to TCollection(Instance).Count - 1 do
      GetTranslatableProperties(TCollection(Instance).Items[I], ParentPath + '[' + IntToStr(I) + ']', Properties);
  end
  else if Instance is TListItems then
  begin
    TListItems(Instance).Handle; // underlining Window control is needed otherwise 'Items' can be empty
    for I := 0 to TListItems(Instance).Count - 1 do
      GetTranslatableProperties(TListItems(Instance).Item[I], ParentPath + '[' + IntToStr(I) + ']', Properties);
  end
  else if Instance is TListItem then
  begin
    AddProperty(ParentPath + '.Caption', TListItem(Instance).Caption);
    for I := 0 to TListItem(Instance).SubItems.Count - 1 do
      AddProperty(ParentPath + '.SubItems[' + IntToStr(I) + ']', TListItem(Instance).SubItems[I]);
  end
  else if Instance is TTreeNodes then
  begin
    Node := TTreeNodes(Instance).GetFirstNode;
    while Assigned(Node) do
    begin
      GetTranslatableProperties(Node, ParentPath + '[' + IntToStr(Node.Index) + ']', Properties);
      Node := Node.getNextSibling;
    end;
  end
  else if Instance is TTreeNode then
  begin
    AddProperty(ParentPath + '.Text', TTreeNode(Instance).Text);
    for I := 0 to TTreeNode(Instance).Count - 1 do
      GetTranslatableProperties(TTreeNode(Instance).Item[I], ParentPath + '[' + IntToStr(I) + ']', Properties);
  end
  else if (Instance is TPersistent) and not ((Instance is TLocalizer) or (Instance is TTranslator)) then
  begin
    PropertyCount := GetPropList(Instance, PropList);
    if PropertyCount = 0 then Exit;
    try
      for I := 0 to PropertyCount - 1 do
      begin
        case (PropList^[I].PropType^)^.Kind of
          tkString, tkLString:
            if not (SameText(String(PropList^[I].Name), 'Name') and (Instance is TComponent)) then
              try
                AddProperty(ParentPath + '.' + String(PropList^[I].Name), GetStrProp(Instance, PropList^[I]));
              except
                // ignore exceptions
              end;
          tkWString:
            if not (SameText(String(PropList^[I].Name), 'Name') and (Instance is TComponent)) then
              try
                AddProperty(ParentPath + '.' + String(PropList^[I].Name), GetWideStrProp(Instance, PropList^[I]));
              except
                // ignore exceptions
              end;
          tkUString:
            if not (SameText(String(PropList^[I].Name), 'Name') and (Instance is TComponent)) then
              try
                AddProperty(ParentPath + '.' + String(PropList^[I].Name), GetUnicodeStrProp(Instance, PropList^[I]));
              except
                // ignore exceptions
              end;
          tkClass:
          begin
            SubInstance := GetObjectProp(Instance, PropList^[I]);
            if Assigned(SubInstance) and (not (SubInstance is TComponent) or (csSubComponent in TComponent(SubInstance).ComponentStyle)) then
              GetTranslatableProperties(SubInstance, ParentPath + '.' + String(PropList^[I].Name), Properties);
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure GetTranslatableProperties(Root: TComponent; Properties: TTextItems); overload;
var
  I: Integer;
begin
  Properties.Clear;
  if Assigned(Root) then
  begin
    GetTranslatableProperties(Root, '', Properties);
    for I := 0 to Root.ComponentCount - 1 do
      if Root.Components[I].Name <> '' then
        GetTranslatableProperties(Root.Components[I], Root.Components[I].Name, Properties);
  end;
end;

procedure SetTranslatableProperties(Instance: TObject; const ParentPath: String;
  Properties: TTextItems); overload;

  function GetPropertyValue(const PropPath: String; out PropValue: String): Boolean;
  var
    Item: TTextItem;
  begin
    Item := Properties.Find(PropPath);
    if Assigned(Item) then
    begin
      PropValue := Item.Value;
      Result := True;
    end
    else
      Result := False;
  end;

var
  PropList: PPropList;
  SubInstance: TObject;
  PropertyCount, I: Integer;
  Node: TTreeNode;
  Value: String;
  ItemIndex, AutoSize: Integer;
begin
  if Instance is TStrings then
  begin
    if AnsiEndsText('.Lines', ParentPath) or AnsiEndsText('.SQL', ParentPath) then
    begin
      if GetPropertyValue(ParentPath + '.Text', Value) then
        TStrings(Instance).Text := Value;
    end
    else
    begin
      for I := 0 to TStrings(Instance).Count - 1 do
        if GetPropertyValue(ParentPath + '[' + IntToStr(I) + ']', Value) then
          TStrings(Instance).Strings[I] := Value;
    end;
  end
  else if Instance is TCollection then
  begin
    for I := 0 to TCollection(Instance).Count - 1 do
      SetTranslatableProperties(TCollection(Instance).Items[I], ParentPath + '[' + IntToStr(I) + ']', Properties);
  end
  else if Instance is TListItems then
  begin
    TListItems(Instance).Handle; // underlining Window control is needed otherwise 'Items' can be empty
    for I := 0 to TListItems(Instance).Count - 1 do
      SetTranslatableProperties(TListItems(Instance).Item[I], ParentPath + '[' + IntToStr(I) + ']', Properties);
  end
  else if Instance is TListItem then
  begin
    if GetPropertyValue(ParentPath + '.Caption', Value) then
      TListItem(Instance).Caption := Value;
    for I := 0 to TListItem(Instance).SubItems.Count - 1 do
      if GetPropertyValue(ParentPath + '.SubItems[' + IntToStr(I) + ']', Value) then
        TListItem(Instance).SubItems[I] := Value;
  end
  else if Instance is TTreeNodes then
  begin
    Node := TTreeNodes(Instance).GetFirstNode;
    while Assigned(Node) do
    begin
      SetTranslatableProperties(Node, ParentPath + '[' + IntToStr(Node.Index) + ']', Properties);
      Node := Node.getNextSibling;
    end;
  end
  else if Instance is TTreeNode then
  begin
    if GetPropertyValue(ParentPath + '.Text', Value) then
      TTreeNode(Instance).Text := Value;
    for I := 0 to TTreeNode(Instance).Count - 1 do
      SetTranslatableProperties(TTreeNode(Instance).Item[I], ParentPath + '[' + IntToStr(I) + ']', Properties);
  end
  else if (Instance is TPersistent) and not (Instance is TLocalizer) and not (Instance is TTranslator) then
  begin
    PropertyCount := GetPropList(Instance, PropList);
    if PropertyCount = 0 then Exit;
    try
      // save control's state
      ItemIndex := -1;
      AutoSize := 0;
      if Instance is TControl then
      begin
        if Instance is TCustomListControl then
          ItemIndex := TCustomListControl(Instance).ItemIndex;
        if IsPublishedProp(Instance, 'AutoSize') then
        begin
          AutoSize := GetOrdProp(Instance, 'AutoSize');
          if AutoSize <> 0 then
            SetOrdProp(Instance, 'AutoSize', 0);
        end;
      end;
      // apply values
      for I := 0 to PropertyCount - 1 do
        case (PropList^[I].PropType^)^.Kind of
          tkString, tkLString, tkWString, tkUString:
            if not (SameText(String(PropList^[I].Name), 'Name') and (Instance is TComponent)) then
            begin
              if GetPropertyValue(ParentPath + '.' + String(PropList^[I].Name), Value) then
                case (PropList^[I].PropType^)^.Kind of
                  tkWString: SetWideStrProp(Instance, PropList^[I], Value);
                  tkUString: SetUnicodeStrProp(Instance, PropList^[I], Value);
                else
                  SetStrProp(Instance, PropList^[I], Value);
                end;
            end;
          tkClass:
          begin
            SubInstance := GetObjectProp(Instance, PropList^[I]);
            if Assigned(SubInstance) and (not (SubInstance is TComponent) or (csSubComponent in TComponent(SubInstance).ComponentStyle)) then
              SetTranslatableProperties(SubInstance, ParentPath + '.' + String(PropList^[I].Name), Properties);
          end;
        end;
      // restore control's state
      if ItemIndex >= 0 then
        TCustomListControl(Instance).ItemIndex := ItemIndex;
      if AutoSize <> 0 then
        SetOrdProp(Instance, 'AutoSize', AutoSize);
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure SetTranslatableProperties(Root: TComponent; Properties: TTextItems); overload;
var
  I: Integer;
begin
  if Assigned(Root) then
  begin
    SetTranslatableProperties(Root, '', Properties);
    for I := 0 to Root.ComponentCount - 1 do
      if Root.Components[I].Name <> '' then
        SetTranslatableProperties(Root.Components[I], Root.Components[I].Name, Properties);
  end;
end;

{ TTranslatables }

constructor TTranslatables.Create(ATranslator: TTranslator);
begin
  inherited Create;
  fTranslator := ATranslator;
  fProperties := TTextItems.Create;
  fLiterals := TTextItems.Create;
end;

destructor TTranslatables.Destroy;
begin
  fLiterals.Free;
  fProperties.Free;
  inherited Destroy;
end;

function TTranslatables.GetDomainOwner: TComponent;
begin
  if Assigned(Translator) then
    Result := Translator.Owner
  else
    Result := nil;
end;

function TTranslatables.GetDomainName: String;
begin
  if DomainOwner <> nil then
  begin
    Result := DomainOwner.ClassName;
    if Result[1] = 'T' then
      Delete(Result, 1, 1);
  end
  else
    Result := '-DEFAULT-';
end;

function TTranslatables.GetOwner: TPersistent;
begin
  Result := Translator;
end;

procedure TTranslatables.ReadLiterals(Reader: TReader);
begin
  Literals.Read(Reader);
end;

procedure TTranslatables.WriteLiterals(Writer: TWriter);
begin
  Literals.Write(Writer);
end;

procedure TTranslatables.ReadProperties(Reader: TReader);
begin
  Properties.Read(Reader);
end;

procedure TTranslatables.WriteProperties(Writer: TWriter);
begin
  Properties.Write(Writer);
end;

procedure TTranslatables.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Properties', ReadProperties, WriteProperties, Properties.Count > 0);
  Filer.DefineProperty('Literals', ReadLiterals, WriteLiterals, Literals.Count > 0);
end;

procedure TTranslatables.Assign(Source: TPersistent);
begin
  if Source is TTranslatables then
  begin
    Properties.Assign(TTranslatables(Source).Properties);
    Literals.Assign(TTranslatables(Source).Literals);
  end
  else
    inherited Assign(Source);
end;

procedure TTranslatables.NormalizeProperties;
var
  AllProperties: TTextItems;
  Current, Origin, Obsolete: TTextItem;
begin
  AllProperties := TTextItems.Create;
  try
    GetTranslatableProperties(DomainOwner, AllProperties);
    Obsolete := nil;
    Current := Properties.First;
    while Current <> nil do
    begin
      Origin := AllProperties.Find(Current.ID);
      if Origin <> nil then
        Current.OriginalValue := Origin.OriginalValue
      else
        Obsolete := Current;
      Current := Current.Next;
      if Obsolete <> nil then
      begin
        Obsolete.Delete;
        Obsolete := nil;
      end;
    end;
  finally
    AllProperties.Free;
  end;
end;

procedure TTranslatables.ApplyProperties;
begin
  SetTranslatableProperties(DomainOwner, Properties);
end;

procedure TTranslatables.DiscardTranslations;
begin
  Literals.DiscardTranslations;
  Properties.DiscardTranslations;
end;

{ TLocalizer }

constructor TLocalizer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not Assigned(TheFirstInstance) then
    TheFirstInstance := Self;
  fCatalog := TTranslationCatalog.Create;
  fPluralForms := TPluralForms.Create(GetApplicationDefaultCulture.PluralRule);
  fClients := TList.Create;
  fOptions := [loAdjustApplicationBiDiMode, loAdjustFormatSettings, loUseNativeDigits, loUseNativeCalendar];
end;

destructor TLocalizer.Destroy;
begin
  fCatalog.Free;
  fPluralForms.Free;
  fClients.Free;
  if TheFirstInstance = Self then
    TheFirstInstance := nil;
  inherited Destroy;
end;

procedure TLocalizer.ReadTargetCulture(Reader: TReader);
begin
  Culture := CultureOf(Reader.ReadString);
end;

procedure TLocalizer.WriteTargetCulture(Writer: TWriter);
begin
  Writer.WriteString(Culture.Locale);
end;

procedure TLocalizer.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('TragetCulture', ReadTargetCulture, WriteTargetCulture,
    Assigned(Culture) and not (loAutoSetectLanguage in Options));
end;

procedure TLocalizer.Loaded;
begin
  if not (csDesigning in ComponentState) and (loAutoSetectLanguage in Options) then
    Culture := GetUserDefaultUICulture;
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    ReloadCatalog;
end;

procedure TLocalizer.DoNotification(Reason: TLocalizerNotification);
begin
  if Assigned(OnNotification) then
    OnNotification(Self, Reason);
end;

function TLocalizer.GetBiDiMode: TBiDiMode;
begin
  if Assigned(Culture) then
    Result := Culture.BiDiMode
  else
    Result := Application.BiDiMode;
end;

function TLocalizer.GetCultures: TReadonlyCultureList;
begin
  if ([csLoading, csDesigning] * ComponentState) = [] then
    Result := Catalog.Cultures
  else
    Result := World.Cultures;
end;

procedure TLocalizer.SetOptions(Value: TLocalizerOptions);
begin
  if Options <> Value then
  begin
    if (csDesigning in ComponentState) and (loAutoSetectLanguage in Value) then
      Culture := nil;
    NotifyClients(lnOptionsChanging);
    fOptions := Value;
    NotifyClients(lnOptionsChanged);
  end;
end;

procedure TLocalizer.SetURI(const Value: String);
begin
  if csLoading in ComponentState then
    fURI := Value
  else if URI <> Value then
  begin
    fURI := Value;
    ReloadCatalog;
  end;
end;

procedure TLocalizer.SetCulture(Value: TCultureInfo);
var
  OldBiDiMode, NewBiDiMode: TBiDiMode;
begin
  if csLoading in ComponentState then
    fCulture := Value
  else if Culture <> Value then
  begin
    if (csDesigning in ComponentState) and Assigned(Value) then
      Options := Options - [loAutoSetectLanguage];
    BeginUpdate;
    try
      NotifyClients(lnCultureChanging);
      try
        OldBiDiMode := BiDiMode;
        if Assigned(Value) then
          NewBiDiMode := Value.BiDiMode
        else
          NewBiDiMode := Application.BiDiMode;
        if OldBiDiMode <> NewBiDiMode then
        begin
          NotifyClients(lnBiDiModeChanging);
          fBiDiToggled := not IsBiDiToggled;
        end;
        fCulture := Value;
        ApplyCultureChanges;
        if OldBiDiMode <> NewBiDiMode then
          NotifyClients(lnBiDiModeChanged);
      finally
        NotifyClients(lnCultureChanged);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

function TLocalizer.GetCultureIndex: Integer;
begin
  Result := Cultures.IndexOf(Culture);
end;

procedure TLocalizer.SetCultureIndex(Value: Integer);
begin
  if Value < 0 then
    Culture := nil
  else if Value < Cultures.Count then
    Culture := Cultures[Value];
end;

class function TLocalizer.FirstInstance: TLocalizer;
begin
  Result := TheFirstInstance;
end;

procedure TLocalizer.NotifyClients(Reason: TLocalizerNotification);
var
  I: Integer;
begin
  if not (csLoading in ComponentState) then
  begin
    for I := fClients.Count - 1 downto 0 do
      ILocalizerLink(fClients[I]).LocalizerNotify(Self, Reason);
    DoNotification(Reason);
  end;
end;

procedure TLocalizer.RegisterListener(Client: ILocalizerLink);
begin
  fClients.Add(Pointer(Client));
end;

procedure TLocalizer.UnregisterListener(Client: ILocalizerLink);
begin
  if not (csDestroying in ComponentState) then
    fClients.Remove(Pointer(Client));
end;

function TLocalizer.IsReady: Boolean;
begin
  Result := not (csLoading in ComponentState) and Assigned(Culture);
end;

procedure TLocalizer.BeginUpdate;
begin
  Inc(fUpdateCount);
  if fUpdateCount = 1 then
    NotifyClients(lnChanging);
end;

procedure TLocalizer.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then
    NotifyClients(lnChanged);
end;

procedure TLocalizer.ReloadCatalog;
var
  OldCulture: TCultureInfo;
begin
  if ([csLoading, csDesigning] * ComponentState) = [] then
  begin
    BeginUpdate;
    try
      OldCulture := Culture;
      NotifyClients(lnCatalogChanging);
      try
        if URI <> '' then
        begin
          Catalog.Open(URI);
          if Catalog.Cultures.Exists(OldCulture) then
            fCulture := nil
          else
            OldCulture := Catalog.Cultures.FindNearest(OldCulture);
        end
        else
          Catalog.Clear;
      finally
        NotifyClients(lnCatalogChanged);
        Culture := OldCulture;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TLocalizer.ApplyCultureChanges;
var
  PostSetBiDiMode: Boolean;
begin
  if ([csLoading, csDesigning] * ComponentState) = [] then
  begin
    PluralForms.Rule := Catalog.PluralRuleOf(Culture);
    PostSetBiDiMode := (Application.BiDiMode = bdLeftToRight);
    if not PostSetBiDiMode and (loAdjustApplicationBiDiMode in Options) then
      AdjustApplicationBiDi;
    if loAdjustFormatSettings in Options then
      AdjustFormatSettings;
    if PostSetBiDiMode and (loAdjustApplicationBiDiMode in Options) then
      AdjustApplicationBiDi;
    if loAdjustKeyboardLayout in Options then
      AdjustKeyboardLayout;
  end;
end;

procedure TLocalizer.AdjustApplicationBiDi;
begin
  if Assigned(Culture) then
    Application.BiDiMode := Culture.BiDiMode
  else if IsBiDiToggled then
  begin
    if Application.BiDiMode = bdRightToLeft then
      Application.BiDiMode := bdLeftToRight
    else
      Application.BiDiMode := bdRightToLeft;
  end;
end;

procedure TLocalizer.AdjustFormatSettings;
begin
  if Assigned(Culture) then
    SetThreadLocale(Culture.LocaleID)
  else
    SetThreadLocale(GetUserDefaultUICulture.LocaleID);
  GetFormatSettings;
  DefaultCalendar.Settings.Prepare(GetThreadLocale);
end;

procedure TLocalizer.AdjustKeyboardLayout;
const
  MaxLayoutCount = 255;
var
  Layouts: packed array[1..MaxLayoutCount] of HKL;
  AutoBiDiLayout, NonAutoBiDiLayout: HKL;
  I: Integer;
begin
  Application.BiDiKeyboard := '';
  Application.NonBiDiKeyboard := '';
  if Assigned(Culture) then
  begin
    if BiDiMode = bdRightToLeft then
    begin
      AutoBiDiLayout := 0;
      NonAutoBiDiLayout := 0;
      for I := 1 to GetKeyboardLayoutList(MaxLayoutCount, Layouts) do
      begin
        if Windows.LANGID(Layouts[I]) = Culture.LangID then
        begin
          AutoBiDiLayout := Layouts[I];
          if NonAutoBiDiLayout <> 0 then
            Break;
        end
        else if NonAutoBiDiLayout = 0 then
        begin
          NonAutoBiDiLayout := Layouts[I];
          if AutoBiDiLayout <> 0 then
            Break;
        end;
      end;
      if (AutoBiDiLayout <> 0) and (NonAutoBiDiLayout <> 0) then
      begin
        Application.BiDiKeyboard := IntToHex(Windows.LANGID(AutoBiDiLayout), 8);
        Application.NonBiDiKeyboard := IntToHex(Windows.LANGID(NonAutoBiDiLayout), 8);
      end;
    end;
    LoadKeyboardLayout(PChar(IntToHex(Culture.LangID, 8)), KLF_ACTIVATE);
  end;
end;

function TLocalizer.CanTranslate(Translatables: TTranslatables): Boolean;
begin
  Result := Assigned(Culture)
        and (([csLoading, csDesigning] * ComponentState) = [])
        and Catalog.TextDomains.Exists(Translatables.DomainName);
end;

function TLocalizer.Translate(Translatables: TTranslatables): Boolean;
var
  TextDomain: TTextDomain;
begin
  Result := False;
  if Assigned(Culture) then
  begin
    TextDomain := Catalog.TextDomains.Find(Translatables.DomainName);
    if TextDomain <> nil then
    begin
      TextDomain.Properties.Translate(Translatables.Properties, Culture.Locale);
      TextDomain.Literals.Translate(Translatables.Literals, Culture.Locale);
      Result := True;
    end;
  end;
end;

function TLocalizer.FindTranslation(const Text: String;
  const DomainName: String): TTextTranslation;
var
  TextID: String;
  TextDomain: TTextDomain;
begin
  Result := nil;
  if Assigned(Culture) then
  begin
    TextID := UniqueKeyOf(Text);
    if DomainName = '*' then
    begin
      TextDomain := Catalog.TextDomains.First;
      while (TextDomain <> nil) and (Result = nil) do
      begin
        Result := TextDomain.Literals.TranslationOf(TextID, Culture.Locale);
        TextDomain := TextDomain.Next;
      end;
    end
    else
    begin
      if DomainName = '' then
      begin
        if Screen.ActiveForm <> nil then
          TextDomain := Catalog.TextDomains.Find(Screen.ActiveForm.Name)
        else
          TextDomain := nil;
      end
      else
        TextDomain := Catalog.TextDomains.Find(DomainName);
      if TextDomain <> nil then
        Result := TextDomain.Literals.TranslationOf(TextID, Culture.Locale);
    end;
  end;
end;

function TLocalizer.GetText(const Text: String; const DomainName: String): String;
var
  Translation: TTextTranslation;
begin
  Translation := FindTranslation(Text, DomainName);
  if Assigned(Translation) and Translation.IsApproved then
    Result := Translation.Value
  else
    Result := Text;
end;

function TLocalizer.GetNText(const TextVariants: array of String; N: Integer;
  const DomainName: String): String;
var
  Translation: TTextTranslation;
  Index: Integer;
begin
  if Length(TextVariants) = 0 then
    Result := ''
  else
  begin
    Translation := FindTranslation(ZStrings.Construct(TextVariants), DomainName);
    if Assigned(Translation) and Translation.IsApproved then
    begin
      Index := PluralForms.IndexOf(N);
      Result := Translation.Plurals[Index];
    end
    else
    begin
      Index := DefaultPluralForms.IndexOf(N);
      if Index > High(TextVariants) then
        Index := High(TextVariants);
      Result := TextVariants[Index];
    end
  end;
end;

function TLocalizer.FormatCS(const Fmt: String; const Args: array of Variant): String;
begin
  if not (loUseNativeDigits in Options) then
    Result := i18nUtils.FormatCS(UCC_NODS + Fmt, Args, Culture)
  else
    Result := i18nUtils.FormatCS(Fmt, Args, Culture);
end;

function TLocalizer.Format(const Fmt: String; const Args: array of const): String;
begin
  if Assigned(Culture) then
    Result := Culture.Format(Fmt, Args, loUseNativeDigits in Options)
  else
    Result := SysUtils.Format(Fmt, Args);
end;

function TLocalizer.FormatNumber(const Fmt: String; const Value: Extended): String;
begin
  if Assigned(Culture) then
    Result := Culture.FormatNumber(Fmt, Value, loUseNativeDigits in Options)
  else
    Result := SysUtils.FormatFloat(Fmt, Value);
end;

function TLocalizer.FormatPercent(const Value: Extended; Decimals: Integer): String;
begin
  if Assigned(Culture) then
    Result := Culture.FormatPercent(Value, Decimals, loUseNativeDigits in Options)
  else
    Result := SysUtils.Format('%.*f%%', [Abs(Decimals), Value * 100.0]);
end;

function TLocalizer.FormatDateTime(const Fmt: String; const DateTime: TDateTime): String;
begin
  if Assigned(Culture) and (loUseNativeCalendar in Options) then
    Result := Culture.FormatDateTime(Fmt, DateTime, loUseNativeDigits in Options)
  else
    Result := DefaultCalendar.Format(Fmt, DateTime);
  if Assigned(Culture) and (loUseNativeDigits in Options) then
    Result := Culture.FreezeDigits(Result, True);
end;

function TLocalizer.ParseDateTime(const Fmt, Str: String): TDateTime;
begin
  if Assigned(Culture) then
    if loUseNativeCalendar in Options then
      Result := Culture.ParseDateTime(Fmt, Str)
    else
      Result := DefaultCalendar.Parse(Fmt, Culture.UnfreezeDigits(Str))
  else
    Result := DefaultCalendar.Parse(Fmt, Str);
end;

function TLocalizer.TryParseDateTime(const Fmt, Str: String; var DateTime: TDateTime): Boolean;
begin
  if Assigned(Culture) then
    if loUseNativeCalendar in Options then
      Result := Culture.TryParseDateTime(Fmt, Str, DateTime)
    else
      Result := DefaultCalendar.TryParse(Fmt, Culture.UnfreezeDigits(Str), DateTime)
  else
    Result := DefaultCalendar.TryParse(Fmt, Str, DateTime);
end;

function TLocalizer.NativeDigits(const Str: String): String;
begin
  if Assigned(Culture) then
    Result := Culture.FreezeDigits(Str, True)
  else
    Result := Str;
end;

{ TTranslator }

type TWinControlHack = class(TWinControl);

constructor TTranslator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTranslatables := TTranslatables.Create(Self);
  fAutoBiDiLayout := True;
  if csDesigning in ComponentState then
  begin
    Localizer := TLocalizer.FirstInstance;
    if (Owner is TWinControl) and not (csLoading in Owner.ComponentState) then
    begin
      // ParentBiDiMode messes things up simetimes; better to be off!
      TWinControlHack(Owner).ParentBiDiMode := False;
    end;
  end;
end;

destructor TTranslator.Destroy;
begin
  Localizer := nil;
  fTranslatables.Free;
  inherited Destroy;
end;

procedure TTranslator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Localizer) then
    Localizer := nil;
end;

procedure TTranslator.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
  begin
    Translatables.NormalizeProperties;
    DoLoaded;
    Sync;
  end;
end;

procedure TTranslator.SetLocalizer(Value: TLocalizer);
begin
  if Localizer <> Value then
  begin
    if Assigned(Localizer) then
    begin
      Localizer.RemoveFreeNotification(Self);
      Localizer.UnregisterListener(Self);
    end;
    fLocalizer := Value;
    if Assigned(Localizer) then
    begin
      Localizer.FreeNotification(Self);
      Localizer.RegisterListener(Self);
    end;
    Sync;
  end;
end;

procedure TTranslator.SetTranslatables(Value: TTranslatables);
begin
  Translatables.Assign(Value);
end;

procedure TTranslator.SetAutoBiDiLayout(Value: Boolean);
begin
  if AutoBiDiLayout <> Value then
  begin
    fAutoBiDiLayout := Value;
    if (([csDesigning, csLoading] * ComponentState) = []) and NeedsFlipLayout then
      FlipLayout;
  end;
end;

function TTranslator.GetText(const Text: String): String;
var
  Literal: TTextItem;
begin
  Result := Text;
  if Text <> '' then
  begin
    Literal := Translatables.Literals.Find(UniqueKeyOf(Text));
    if Assigned(Literal) and Literal.IsTranslated then
      Result := Literal.Value;
  end;
end;

function TTranslator.GetNText(const TextVariants: array of String; N: Integer): String;
var
  Literal: TTextItem;
  Index: Integer;
begin
  if Length(TextVariants) = 0 then
    Result := ''
  else
  begin
    Literal := Translatables.Literals.Find(UniqueKeyOf(TextVariants));
    if Assigned(Literal) and Literal.IsTranslated and Assigned(Localizer) then
    begin
      Index := Localizer.PluralForms.IndexOf(N);
      Result := Literal.Plurals[Index];
    end
    else
    begin
      Index := DefaultPluralForms.IndexOf(N);
      if Index > High(TextVariants) then
        Index := High(TextVariants);
      Result := TextVariants[Index];
    end;
  end;
end;

procedure TTranslator.Sync;
begin
  if (([csDestroying, csLoading, csDesigning] * ComponentState) = []) and
     (Assigned(CurrentCulture) or (Assigned(Localizer) and Localizer.IsReady)) then
  begin
    if NeedsFlipLayout then
      FlipLayout;
    Translate;
  end;
end;

procedure TTranslator.LocalizerNotify(Sender: TLocalizer;
  Reason: TLocalizerNotification);
begin
  if ([csLoading, csDesigning] * ComponentState) = [] then
    case Reason of
      lnChanging:
        BeginUpdate;
      lnBiDiModeChanged:
        if NeedsFlipLayout then
          FlipLayout;
      lnCultureChanged:
        Translate;
      lnChanged:
        EndUpdate;
    end;
end;

procedure TTranslator.DoLoaded;
begin
  if Assigned(OnLoaded) then
    Onloaded(Self);
end;

procedure TTranslator.DoBeforeTranslate;
begin
  if Assigned(OnBeforeTranslate) then
    OnBeforeTranslate(Self);
end;

procedure TTranslator.DoAfterTranslate;
begin
  if Assigned(OnAfterTranslate) then
    OnAfterTranslate(Self);
end;

procedure TTranslator.DoBeforeFlipLayout;
begin
  if Assigned(OnBeforeFlipLayout) then
    OnBeforeFlipLayout(Self);
end;

procedure TTranslator.DoAfterFlipLayout;
begin
  if Assigned(OnAfterFlipLayout) then
    OnAfterFlipLayout(Self);
end;

procedure TTranslator.BeginUpdate;
begin
  if (fUpdateCount = 0) and (Owner is TWinControl) then
  begin
    if Owner is TCustomForm then
      fActiveControl := TCustomForm(Owner).ActiveControl;
    if TWinControl(Owner).HandleAllocated and TWinControl(Owner).Visible then
      TWinControl(Owner).Perform(WM_SETREDRAW, 0, 0);
  end;
  Inc(fUpdateCount);
end;

procedure TTranslator.EndUpdate;
begin
  Dec(fUpdateCount);
  if (fUpdateCount = 0) and (Owner is TWinControl) then
  begin
    if TWinControl(Owner).HandleAllocated and TWinControl(Owner).Visible then
    begin
      TWinControl(Owner).Perform(WM_SETREDRAW, 1, 0);
      RedrawWindow(TWinControl(Owner).Handle, nil, 0,
        RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_UPDATENOW);
    end;
    if (Owner is TCustomForm) and Assigned(fActiveControl) then
      TCustomForm(Owner).ActiveControl := fActiveControl;
  end;
end;

function TTranslator.Translate: Boolean;
begin
  Result := False;
  DoBeforeTranslate;
  try
    if Assigned(Localizer) and Localizer.Translate(Translatables) then
    begin
      fCurrentCulture := Localizer.Culture;
      Translatables.ApplyProperties;
      Result := True;
    end
    else if Assigned(CurrentCulture) then
    begin
      fCurrentCulture := nil;
      Translatables.DiscardTranslations;
      Translatables.ApplyProperties;
    end;
  finally
    DoAfterTranslate;
  end;
end;

function TTranslator.FlipLayout: Boolean;

  var
    ListViews: TList;
    Streams: TObjectList;

  // By chaging BiDiMode of a list view, it may lose its items.
  // The following procedure saves the items of all the list view controls, so
  // that we can restore them when needed.
  procedure StoreListViews;
  var
    I: Integer;
    ListView: TListView;
    Stream: TStream;
  begin
    ListViews := nil;
    Streams := nil;
    for I := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[I] is TCustomListView then
      begin
        ListView := TListView(Owner.Components[I]);
        if ListView.ParentBiDiMode and not ListView.OwnerData and
           not (ListView.Action is TCustomListAction) then
        begin
          ListView.HandleNeeded;
          if ListView.Items.Count <> 0 then
          begin
            Stream := TMemoryStream.Create;
            try
              WriteListViewItems(Stream, ListView);
              if not Assigned(ListViews) then
              begin
                ListViews := TList.Create;
                Streams := TObjectList.Create(True);
              end;
              ListViews.Add(ListView);
              Streams.Add(Stream);
            except
              Stream.Free;
            end;
          end;
        end;
      end;
  end;

  // The following procedure restores items of any list view control that has
  // lost its items.
  procedure RestoreListViews;
  var
    I: Integer;
    ListView: TListView;
    Stream: TStream;
  begin
    if Assigned(ListViews) then
      try
        for I := ListViews.Count - 1 downto 0 do
        begin
          ListView := TListView(ListViews[I]);
          ListView.HandleNeeded;
          if ListView.Items.Count = 0 then
          begin
            Stream := TStream(Streams[I]);
            Stream.Seek(0, soFromBeginning);
            ReadListViewItems(Stream, ListView);
          end;
        end;
      finally
        ListViews.Free;
        Streams.Free;
      end;
  end;

begin
  Result := False;
  if Owner is TWinControl then
  begin
    DoBeforeFlipLayout;
    try
      StoreListViews;
      try
        try
          if TWinControl(Owner).BiDiMode = bdLeftToRight then
            TWinControl(Owner).BiDiMode := bdRightToLeft
          else
            TWinControl(Owner).BiDiMode := bdLeftToRight;
          Application.ProcessMessages;
        except
          // ignore exceptions
        end;
        fLayoutFlipped := not IsLayoutFlipped;
        BiDi.FlipChildren(TWinControl(Owner));
      finally
        RestoreListViews;
      end;
      Result := True;
    finally
      DoAfterFlipLayout;
    end;
  end;
end;

function TTranslator.NeedsFlipLayout: Boolean;
begin
  Result := False;
  if Owner is TWinControl then
  begin
    if AutoBiDiLayout and Assigned(Localizer) and Localizer.IsBiDiToggled then
      Result := (TWinControl(Owner).BiDiMode <> Localizer.BiDiMode)
    else
      Result := IsLayoutFlipped;
  end;
end;

initialization

finalization
  if Assigned(TheDefaultPluralForms) then
    FreeAndNil(TheDefaultPluralForms);
end.
