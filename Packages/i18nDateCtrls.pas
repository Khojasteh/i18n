{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  Internationalization and Localization for Delphi                            }
{                                                                              }
{  Copyright (c) Kambiz Khojasteh                                              }
{  https://github.com/khojasteh/i18n                                           }
{                                                                              }
{------------------------------------------------------------------------------}

/// <summary>
/// This unit implements controls that represent fully localizable date and
/// time values.
/// </summary>
unit i18nDateCtrls;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls,
  i18nCore, i18nCalendar, i18nLocalizer, i18nCtrls, UxTheme;

type

  {$region 'xmldoc'}
  /// <summary>
  /// TDateTimeList maintains a sorted list of <see cref="TDateTime"/> values.
  /// </summary>
  /// <remarks>
  /// Use TDateTimeList to store and maintain a list of <see cref="TDateTime"/> values,
  /// which are sorted in ascending order. TDateTimeList provides properties and methods
  /// to add, delete, locate, and access the maintained <see cref="TDateTime"/>
  /// values.
  /// </remarks>
  {$endregion}
  TDateTimeList = class(TPersistent)
  private
    fList: array of TDateTime;
    fCount: Integer;
    fUpdateCount: Integer;
    fOnChanging: TNotifyEvent;
    fOnChange: TNotifyEvent;
    function GetCapacity: Integer; inline;
    procedure SetCapacity(Value: Integer);
    function GetItems(Index: Integer): TDate;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChanging"/> event.
    /// </summary>
    /// <seealso cref="Change"/>
    {$endregion}
    procedure Changing; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChange"/> event.
    /// </summary>
    /// <seealso cref="Changing"/>
    {$endregion}
    procedure Change; virtual;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the TDateTimeList instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the list of <see cref="TDateTime"/> values from another object.
    /// </summary>
    /// <param name="Source">
    /// The source list.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents generating of the <see cref="OnChanging"/> and <see cref="OnChange"/>
    /// events until the <see cref="EndUpdate"/> method is called.
    /// </summary>
    /// <seealso cref="EndUpdate"/>
    {$endregion}
    procedure BeginUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Reenables <see cref="OnChanging"/> and <see cref="OnChange"/> events generation
    /// that was turned off with the <see cref="BeginUpdate"/> method.
    /// </summary>
    /// <seealso cref="BeginUpdate"/>
    {$endregion}
    procedure EndUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes all the <see cref="TDateTime"/> values from the list.
    /// </summary>
    {$endregion}
    procedure Clear; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a <see cref="TDateTime"/> value to the list.
    /// </summary>
    /// <param name="ADateTime">
    /// The <see cref="TDateTime"/> value to add.
    /// </param>
    /// <returns>
    /// The index of newly added <see cref="TDateTime"/> value.
    /// </returns>
    {$endregion}
    function Add(const ADateTime: TDateTime): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes a <see cref="TDateTime"/> value from the list.
    /// </summary>
    /// <param name="ADateTime">
    /// The <see cref="TDateTime"/> value to remove.
    /// </param>
    /// <returns>
    /// The index of the removed <see cref="TDateTime"/> value or -1 if the
    /// <see cref="TDateTime"/> value is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Remove(const ADateTime: TDateTime): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a <see cref="TDateTime"/> value, which is specified by its index
    /// in the list.
    /// </summary>
    /// <param name="Index">
    /// The index of <see cref="TDateTime"/> value to remove.
    /// </param>
    /// <seealso cref="Remove"/>
    {$endregion}
    procedure Delete(Index: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified <see cref="TDateTime"/> value is in the list.
    /// </summary>
    /// <param name="ADateTime">
    /// The <see cref="TDateTime"/> value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is in the
    /// list, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IndexOf"/>
    /// <seealso cref="Find"/>
    {$endregion}
    function Exists(const ADateTime: TDateTime): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds the index of a specified <see cref="TDateTime"/> value in the list.
    /// </summary>
    /// <param name="ADateTime">
    /// The <see cref="TDateTime"/> value to locate.
    /// </param>
    /// <returns>
    /// Returns the index of the <see cref="TDateTime"/> value if it is in the list,
    /// otherwise returns -1.
    /// </returns>
    /// <seealso cref="Find"/>
    /// <seealso cref="Exists"/>
    {$endregion}
    function IndexOf(const ADateTime: TDateTime): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Locates the index of a specified <see cref="TDateTime"/> value in the list.
    /// </summary>
    /// <param name="ADateTime">
    /// The <see cref="TDateTime"/> value to locate.
    /// </param>
    /// <param name="Index">
    /// Receives the index of the <see cref="TDateTime"/> value in the list.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDateTime"/> value is in the
    /// list, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IndexOf"/>
    /// <seealso cref="Exists"/>
    {$endregion}
    function Find(const ADateTime: TDateTime; out Index: Integer): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the smallest <see cref="TDateTime"/> value in the list.
    /// </summary>
    /// <returns>
    /// Returns <see cref="Items"/>[0], or <see cref="TCalendar.NoDate"/> if the
    /// list is empty.
    /// </returns>
    /// <seealso cref="Last"/>
    {$endregion}
    function First: TDateTime;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the largest <see cref="TDateTime"/> value in the list.
    /// </summary>
    /// <returns>
    /// Returns <see cref="Items"/>[<see cref="Count"/> - 1], or <see cref="TCalendar.NoDate"/>
    /// if the list is empty.
    /// </returns>
    /// <seealso cref="First"/>
    {$endregion}
    function Last: TDateTime;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of <see cref="TDateTime"/> values in the list.
    /// </summary>
    {$endregion}
    property Count: Integer read fCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of <see cref="TDateTime"/> values the TDateTimeList
    /// object can hold.
    /// </summary>
    {$endregion}
    property Capacity: Integer read GetCapacity write SetCapacity;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TDateTime"/> values in the list.
    /// </summary>
    {$endregion}
    property Items[Index: Integer]: TDate read GetItems; default;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately after the list changes.
    /// </summary>
    {$endregion}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately before the list changes.
    /// </summary>
    {$endregion}
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomIntlDateTimeLabel is the base class for label controls that display a
  /// localized <see cref="TDateTime"/> value on a form.
  /// </summary>
  /// <remarks>
  /// <para>
  /// Use TCustomIntlDateTimeLabel as a base class when defining objects that display a
  /// localized <see cref="TDateTime"/> value.
  /// </para>
  /// <para>
  /// TCustomIntlDateTimeLabel can use different calendar systems for displaying the
  /// dates. The <see cref="CalendarType"/> property is the main property that specifies
  /// which calendar system should be used by the control. If the <see cref="CalendarType"/>
  /// property is not set, the <see cref="Culture"/> property can do this task. In addition,
  /// the <see cref="Culture"/> property provides the locale specific settings of the control.
  /// If none of these two properties are set the <see cref="DefaultCalendar"/> global
  /// variable determines the calendar system used by the control.
  /// </para>
  /// <para>
  /// TCustomIntlDateTimeLabel can be linked to a <see cref="TLocalizer"/> component.
  /// In this case, the <see cref="TLocalizer"/> component determines the localized strings
  /// and the calendar system that should be used for displaying the <see cref="TDateTime"/>
  /// value.
  /// </para>
  /// </remarks>
  {$endregion}
  TCustomIntlDateTimeLabel = class(TCustomImageLabel, ILocalizerLink)
  private
    fDateTime: TDateTime;
    fDateTimeFormat: String;
    fCalendarType: TCalendarClass;
    fCalendar: TCalendar;
    fCulture: TCultureInfo;
    fCultureCalendar: TLocalizerSwicth;
    fCultureDigits: TLocalizerSwicth;
    fLocalizer: TLocalizer;
    procedure SetDateTime(const Value: TDateTime);
    procedure SetDateTimeFormat(const Value: String);
    procedure SetCalendarType(Value: TCalendarClass);
    procedure SetCulture(Value: TCultureInfo);
    procedure SetCultureCalendar(Value: TLocalizerSwicth);
    procedure SetCultureDigits(Value: TLocalizerSwicth);
    procedure SetLocalizer(Value: TLocalizer);
    function GetUsingCultureCalendar: Boolean;
    function GetUsingCultureDigits: Boolean;
    function IsDateTimeStored: Boolean;
    procedure ReadCulture(Reader: TReader);
    procedure WriteCulture(Writer: TWriter);
    procedure ReadCalendarType(Reader: TReader);
    procedure WriteCalendarType(Writer: TWriter);
    procedure CalendarChanged(Sender: TObject);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Notifies the control about changes in the linked <see cref="TLocalizer"/>
    /// component.
    /// </summary>
    /// <param name="Sender">
    /// The <see cref="TLocalizer"/> component who originated the notification.
    /// </param>
    /// <param name="Reason">
    /// The reason of the notification.
    /// </param>
    {$endregion}
    procedure LocalizerNotify(Sender: TLocalizer; Reason: TLocalizerNotification);
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to notifications that components are about to be created or destroyed.
    /// </summary>
    /// <param name="AComponent">
    /// The component which generated the notification.
    /// </param>
    /// <param name="Operation">
    /// Indicates whether the component is created or destroyed.
    /// </param>
    {$endregion}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="Culture"/> and <see cref="CalendarType"/> properties
    /// as if they were published.
    /// </summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.
    /// </param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a text label to display on the control.
    /// </summary>
    /// <returns>
    /// The text label.
    /// </returns>
    {$endregion}
    function GetLabelText: String; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Validates the <see cref="Calendar"/> property to ensure it represents a proper
    /// calendar system with an appropriate locale settings.
    /// </summary>
    {$endregion}
    procedure ValidateCalendar; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TDateTime"/> value to be displayed on the control.
    /// </summary>
    {$endregion}
    property DateTime: TDateTime read fDateTime write SetDateTime stored IsDateTimeStored;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the format string that determines how the <see cref="DateTime"/> property
    /// should be displayed on the control. See <see cref="TCalendar.Format"/> for details.
    /// </summary>
    /// <seealso cref="TCalendar.Format"/>
    {$endregion}
    property DateTimeFormat: String read fDateTimeFormat write SetDateTimeFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCalendar"/> class that is used for manipulating dates.
    /// </summary>
    /// <seealso cref="Culture"/>
    {$endregion}
    property CalendarType: TCalendarClass read fCalendarType write SetCalendarType;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCultureInfo"/> object that provides the locale specific
    /// settings of the calendar.
    ///
    /// NOTE: If the <see cref="CalendarType"/> property is <see langword="nil"/>, depends
    /// on the values of <see cref="TLocalizer"/> and <see cref="CultureCalendar"/> properties,
    /// the Culture property may also determine the calendar system of the control.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="CultureCalendar"/>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="CultureDigits"/>
    /// <seealso cref="CalendarType"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property Culture: TCultureInfo read fCulture write SetCulture;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar system used by the control should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.
    /// NOTE: Value of this property has no effect when the <see cref="CalendarType"/>
    /// property is not <see langword="nil"/>.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureCalendar: TLocalizerSwicth read fCultureCalendar write SetCultureCalendar default lsDefault;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the way the control renders digits should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.</summary>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureDigits: TLocalizerSwicth read fCultureDigits write SetCultureDigits default lsDefault;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the control.
    /// </summary>
    {$endregion}
    property Localizer: TLocalizer read fLocalizer write SetLocalizer;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the control.
    /// </summary>
    /// <param name="AOwner">
    /// The owner component.
    /// </param>
    {$endregion}
    constructor Create(AOwner: TComponent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the component and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the control is managed by a <see cref="TLocalizer"/> component
    /// specifying the localized strings and the calendar system for displaying the
    /// <see cref="TDateTime"/> value.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if <see cref="Localizer"/> property is not
    /// <see langword="nil"/>, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Localizer"/>
    {$endregion}
    function IsManagedByLocalizer: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TCalendar"/> object that the control is using to
    /// manipulate and display dates.
    /// </summary>
    {$endregion}
    property Calendar: TCalendar read fCalendar;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the calendar system used by the control is determined by the
    /// <see cref="Culture"/> property.
    /// </summary>
    /// <seealso cref="CultureCalendar"/>
    /// <seealso cref="Culture"/>
    {$endregion}
    property UsingCultureCalendar: Boolean read GetUsingCultureCalendar;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the way that the control renders digits is determined by the
    /// <see cref="Culture"/> property.
    /// </summary>
    /// <seealso cref="CultureDigits"/>
    /// <seealso cref="Culture"/>
    {$endregion}
    property UsingCultureDigits: Boolean read GetUsingCultureDigits;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TIntlDateTimeLabel is a label control that displays a localized <see cref="TDateTime"/>
  /// value on a form.
  /// </summary>
  /// <remarks>
  /// Use TIntlDateTimeLabel to display a localized <see cref="TDateTime"/> value on a form.
  /// The <see cref="DateTime"/> and <see cref="DateTimeFormat"/> properties determine the
  /// content that will be displayed.
  ///
  /// TIntlDateTimeLabel can use different calendar systems for displaying the dates.
  /// The <see cref="CalendarType"/> property is the main property that specifies which
  /// calendar system should be used by the control. If the <see cref="CalendarType"/>
  /// property is not set, the <see cref="Culture"/> property can do this task. In addition,
  /// the <see cref="Culture"/> property provides the locale specific settings of the control.
  /// If none of these two properties are set the <see cref="DefaultCalendar"/> global
  /// variable determines the calendar system used by the control.
  ///
  /// TIntlDateTimeLabel can be linked to a <see cref="TLocalizer"/> component. In this case,
  /// the <see cref="TLocalizer"/> component determines the localized strings and the calendar
  /// system that should be used for displaying the <see cref="TDateTime"/> value.
  ///
  /// TIntlDateTimeLabel publishes many of the properties, events, and methods of
  /// <see cref="TCustomIntlDateTimeLabel"/>, but does not introduce any new behavior.
  /// </remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TIntlDateTimeLabel = class(TCustomIntlDateTimeLabel)
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control aligns within its container (parent control).
    /// </summary>
    {$endregion}
    property Align;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the horizontal placement of the text within the label.
    /// </summary>
    {$endregion}
    property Alignment;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether a control should be constrained by <see cref="Margins"/>.
    /// </summary>
    {$endregion}
    property AlignWithMargins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control is anchored to its parent.
    /// </summary>
    {$endregion}
    property Anchors;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control sizes itself automatically to accommodate
    /// its contents.
    /// </summary>
    {$endregion}
    property AutoSize;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the bidirectional mode for the control.
    /// </summary>
    {$endregion}
    property BiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the color of the control's border.
    /// </summary>
    {$endregion}
    property BorderColor;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the width of the control's border.
    /// </summary>
    {$endregion}
    property BorderWidth;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCalendar"/> class that is used for manipulating dates.
    /// </summary>
    /// <seealso cref="Culture"/>
    {$endregion}
    property CalendarType;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the background color of the control.
    /// </summary>
    {$endregion}
    property Color nodefault;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the size constraints for the control.
    /// </summary>
    {$endregion}
    property Constraints;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCultureInfo"/> object that provides the locale specific
    /// settings of the calendar.
    ///
    /// NOTE: If the <see cref="CalendarType"/> property is <see langword="nil"/>, depends
    /// on the values of <see cref="TLocalizer"/> and <see cref="CultureCalendar"/> properties,
    /// the Culture property may also determine the calendar system of the control.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="CultureCalendar"/>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="CultureDigits"/>
    /// <seealso cref="CalendarType"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property Culture;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar system used by the control should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.
    /// NOTE: Value of this property has no effect when the <see cref="CalendarType"/>
    /// property is not <see langword="nil"/>.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureCalendar;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the way the control renders digits should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.</summary>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureDigits;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TDateTime"/> value to be displayed on the control.
    /// </summary>
    {$endregion}
    property DateTime;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the format string that determines how the <see cref="TDateTime"/> value
    /// should be displayed on the control. See <see cref="TCalendar.Format"/> for details.
    /// </summary>
    {$endregion}
    property DateTimeFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the image used to represent the mouse pointer when it passes
    /// into the region covered by the control.
    /// </summary>
    {$endregion}
    property DragCursor;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control is being dragged normally or for docking.
    /// </summary>
    {$endregion}
    property DragKind;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control initiates drag-and-drop or drag-and-dock operations.
    /// </summary>
    {$endregion}
    property DragMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control responds to keyboard, mouse, and timer events.
    /// </summary>
    {$endregion}
    property Enabled;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets a windowed control associated with the label.
    /// </summary>
    {$endregion}
    property FocusControl;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the attributes of text written on the control.
    /// </summary>
    {$endregion}
    property Font;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the radius of the glow around the label.
    /// NOTE: This feature is only available in Windows Vista and later.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property GlowSize;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the text string that can appear when the user moves the mouse over
    /// the control.
    /// </summary>
    {$endregion}
    property Hint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which image is displayed as the icon on the label.
    /// </summary>
    {$endregion}
    property ImageIndex;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the images that can appear on the control.
    /// </summary>
    {$endregion}
    property Images;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the control.
    /// </summary>
    {$endregion}
    property Localizer;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the margins of the control.
    /// </summary>
    {$endregion}
    property Margins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the distance between the image and the text from the control's border.
    /// </summary>
    {$endregion}
    property Padding;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="BiDiMode"/>.
    /// </summary>
    {$endregion}
    property ParentBiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Color"/>.
    /// </summary>
    {$endregion}
    property ParentColor;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Font"/>.
    /// </summary>
    {$endregion}
    property ParentFont;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="ShowHint"/>.
    /// </summary>
    {$endregion}
    property ParentShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the pop-up menu associated with the control.
    /// </summary>
    {$endregion}
    property PopupMenu;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how an ampersand in the label text is displayed.
    /// </summary>
    {$endregion}
    property ShowAccelChar default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.
    /// </summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of pixels between the image and the text of the control.
    /// </summary>
    {$endregion}
    property Spacing;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the touch manager component associated with the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property Touch;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether controls that sit below the label on a form can be seen
    /// through the label.
    /// </summary>
    {$endregion}
    property Transparent;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the vertical placement of the text within the label.
    /// </summary>
    {$endregion}
    property Layout;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control appears onscreen.
    /// </summary>
    {$endregion}
    property Visible;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets Specifies whether the label text wraps when it is too long for the
    /// width of the label.
    /// </summary>
    {$endregion}
    property WordWrap;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user clicks the control.
    /// </summary>
    {$endregion}
    property OnClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user right-clicks the control or otherwise invokes the pop-up
    /// menu (such as using the keyboard).
    /// </summary>
    {$endregion}
    property OnContextPopup;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user double-clicks the left mouse button when the mouse pointer
    /// is over the control.
    /// </summary>
    {$endregion}
    property OnDblClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user drops an object being dragged on the control.
    /// </summary>
    {$endregion}
    property OnDragDrop;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user drags an object over the control.
    /// </summary>
    {$endregion}
    property OnDragOver;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dragging of an object ends, either by docking the object or
    /// by canceling the dragging.
    /// </summary>
    {$endregion}
    property OnEndDock;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dragging of an object ends, either by dropping the object or
    /// by canceling the dragging.
    /// </summary>
    {$endregion}
    property OnEndDrag;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when user performs a gesture associated with the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnGesture;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user presses a mouse button with the mouse pointer over the
    /// control and the parent form is not active.
    /// </summary>
    {$endregion}
    property OnMouseActivate;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user presses a mouse button with the mouse pointer over the
    /// control.
    /// </summary>
    {$endregion}
    property OnMouseDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse pointer while the mouse pointer is over
    /// the control.
    /// </summary>
    {$endregion}
    property OnMouseMove;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user releases a mouse button that was pressed with the mouse
    /// pointer over the control.
    /// </summary>
    {$endregion}
    property OnMouseUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse into the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnMouseEnter;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse outside of the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnMouseLeave;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated.
    /// </summary>
    {$endregion}
    property OnMouseWheel;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated downward.
    /// </summary>
    {$endregion}
    property OnMouseWheelDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated upward.
    /// </summary>
    {$endregion}
    property OnMouseWheelUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately after the control is resized.
    /// </summary>
    {$endregion}
    property OnResize;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user begins to drag the control with a <see cref="DragKind"/>
    /// of <see cref="DragKind.dkDock"/>.
    /// </summary>
    {$endregion}
    property OnStartDock;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user begins to drag the control with a <see cref="DragKind"/>
    /// of <see cref="DragKind.dkDrag"/>.
    /// </summary>
    {$endregion}
    property OnStartDrag;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the ways that users can select dates in a
  /// month calendar control.
  /// </summary>
  {$endregion}
  TIntlCalSelectionStyle = (
    {$region 'xmldoc'}
    /// User can select only a single date at a time.
    {$endregion}
    ssSingle,
    {$region 'xmldoc'}
    /// User can select only a single date range at a time.
    {$endregion}
    ssRange,
    {$region 'xmldoc'}
    // User can freely select multiple dates or range of dates.
    {$endregion}
    ssFree
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the date componenets that a user can select
  /// at a time on a month calendar control.
  /// calendar's page types.
  /// </summary>
  {$endregion}
  TIntlCalViewMode = (
    {$region 'xmldoc'}
    /// Displays days of a month.
    {$endregion}
    vmDay,
    {$region 'xmldoc'}
    /// Displays months of a year.
    {$endregion}
    vmMonth,
    {$region 'xmldoc'}
    /// Displays years of a decade.
    {$endregion}
    vmYear
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the sections of a month calendar control
  /// that a mouse pointer can be over it.
  /// </summary>
  {$endregion}
  TIntlCalHitTest = (
    {$region 'xmldoc'}
    /// Nowhere
    {$endregion}
    chtNowhere,
    {$region 'xmldoc'}
    /// Left navigation button
    {$endregion}
    chtArrowLeft,
    {$region 'xmldoc'}
    /// Right navigation button
    {$endregion}
    chtArrowRight,
    {$region 'xmldoc'}
    /// Text of the header
    {$endregion}
    chtHeaderText,
    {$region 'xmldoc'}
    /// Text of the footer
    {$endregion}
    chtFooter,
    {$region 'xmldoc'}
    /// Week numbers column
    {$endregion}
    chtWeekNumber,
    {$region 'xmldoc'}
    /// Days of week row
    {$endregion}
    chtDayOfWeek,
    {$region 'xmldoc'}
    /// A day of month cell
    {$endregion}
    chtDay,
    {$region 'xmldoc'}
    /// A month cell
    {$endregion}
    chtMonth,
    {$region 'xmldoc'}
    /// A year cell
    {$endregion}
    chtYear
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the ways an item in a month calendar
  /// control can be rendered.
  /// </summary>
  {$endregion}
  TIntlCalDrawState = (
    {$region 'xmldoc'}
    /// The drawing item is part of the header.
    {$endregion}
    cdsHeader,
    {$region 'xmldoc'}
    /// The drawing item is part of the footer.
    {$endregion}
    cdsFooter,
    {$region 'xmldoc'}
    /// The drawing item is part of the fixed row (day of week cells).
    {$endregion}
    cdsFixedRow,
    {$region 'xmldoc'}
    /// The drawing item is part of the fixed column (week numbers).
    {$endregion}
    cdsFixedCol,
    {$region 'xmldoc'}
    /// The drawing item represents today's information.
    {$endregion}
    cdsToday,
    {$region 'xmldoc'}
    /// The drawing item is in hot state.
    {$endregion}
    cdsHotLight,
    {$region 'xmldoc'}
    /// The drawing item is selected.
    {$endregion}
    cdsSelected,
    {$region 'xmldoc'}
    /// The drawing item has focus.
    {$endregion}
    cdsFocused,
    {$region 'xmldoc'}
    /// The drawing item is part of another page.
    {$endregion}
    cdsGrayed,
    {$region 'xmldoc'}
    /// Do not draw any content.
    {$endregion}
    cdsBlank
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This data type represents a set of <see cref="TIntlCalDrawState"/> values.
  /// </summary>
  {$endregion}
  TIntlCalDrawStates = set of TIntlCalDrawState;

  {$region 'xmldoc'}
  /// <summary>
  /// TDateHintEvent is the type for event handlers that respond when the hint
  /// string for a date is requested.
  /// </summary>
  /// <param name="Sender">
  /// The object that generated the event.
  /// </param>
  /// <param name="ADate">
  /// The <see cref="TDate"/> value that its hint string is requested.
  /// </param>
  /// <param name="HintStr">
  /// The hint string of the <see cref="TDate"/> value.
  /// </param>
  {$endregion}
  TDateHintEvent = procedure(Sender: TObject; const ADate: TDate;
    var HintStr: String) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TDateSelectableEvent is the type for event handlers that decide whether a
  /// date can be selected.
  /// </summary>
  /// <param name="Sender">
  /// The object that generated the event.
  /// </param>
  /// <param name="ADate">
  /// The <see cref="TDate"/> value to select.
  /// </param>
  /// <param name="CanSelect">
  /// Indicates whether the <see cref="TDate"/> value can be selected.
  /// </param>
  {$endregion}
  TDateSelectableEvent = procedure(Sender: TObject; const ADate: TDate;
    var CanSelect: Boolean) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDrawTextEvent is the type for event handlers that render a text.
  /// </summary>
  /// <param name="Sender">
  /// The object that generated the event.
  /// </param>
  /// <param name="Canvas">
  /// The canvas on which to draw the text.
  /// </param>
  /// <param name="ARect">
  /// The bounding rectangle on the canvas where the text should be drawn.
  /// </param>
  /// <param name="State">
  /// The state information that can affect the way the text is drawn.
  /// </param>
  /// <param name="AText">
  /// The text to draw.
  /// </param>
  /// <param name="DefaultDraw">
  /// Indicates whether the default rendering should proceed after the event handler exits.
  /// </param>
  {$endregion}
  TCustomDrawTextEvent = procedure(Sender: TObject; Canvas: TCanvas;
    const ARect: TRect; State: TIntlCalDrawStates;
    const AText: String; var DefaultDraw: Boolean) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDrawDayEvent is the type for event handlers that render an integer value
  /// as a day of the month.
  /// </summary>
  /// <param name="Sender">
  /// The object that generated the event.
  /// </param>
  /// <param name="Canvas">
  /// The canvas on which to draw the day of the month.
  /// </param>
  /// <param name="ARect">
  /// The bounding rectangle on the canvas where the day of the month should be drawn.
  /// </param>
  /// <param name="State">
  /// The state information that can affect the way the day of the month is drawn.
  /// </param>
  /// <param name="ADate">
  /// The <see cref="TDate"/> value that represents the day of the moneh.
  /// </param>
  /// <param name="ADay">
  /// The day of the month to draw.
  /// </param>
  /// <param name="DefaultDraw">
  /// Indicates whether the default rendering should proceed after the event handler exits.
  /// </param>
  {$endregion}
  TCustomDrawDayEvent = procedure(Sender: TObject; Canvas: TCanvas;
    const ARect: TRect; State: TIntlCalDrawStates;
    const ADate: TDate; ADay: Integer; var DefaultDraw: Boolean) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDrawDayOfWeekEvent is the type for event handlers that render name of
  /// a day of the week.
  /// </summary>
  /// <param name="Sender">
  /// The object that generated the event.
  /// </param>
  /// <param name="Canvas">
  /// The canvas on which to draw the day's name.
  /// </param>
  /// <param name="ARect">
  /// The bounding rectangle on the canvas where the day's name should be drawn.
  /// </param>
  /// <param name="State">
  /// The state information that can affect the way the day's name is drawn.
  /// </param>
  /// <param name="ADayOfWeek">
  /// The day of the week to draw its name.
  /// </param>
  /// <param name="DefaultDraw">
  /// Indicates whether the default rendering should proceed after the event handler exits.
  /// </param>
  {$endregion}
  TCustomDrawDayOfWeekEvent = procedure(Sender: TObject; Canvas: TCanvas;
    const ARect: TRect; State: TIntlCalDrawStates;
     ADayOfWeek: TDayOfWeek; var DefaultDraw: Boolean) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDrawDayEvent is the type for event handlers that render an integer value
  /// as a week number of the year.
  /// </summary>
  /// <param name="Sender">
  /// The object that generated the event.
  /// </param>
  /// <param name="Canvas">
  /// The canvas on which to draw the week number.
  /// </param>
  /// <param name="ARect">
  /// The bounding rectangle on the canvas where the week number should be drawn.
  /// </param>
  /// <param name="State">
  /// The state information that can affect the way the week number is drawn.
  /// </param>
  /// <param name="AWeek">
  /// The week of the year to draw.
  /// </param>
  /// <param name="DefaultDraw">
  /// Indicates whether the default rendering should proceed after the event handler exits.
  /// </param>
  {$endregion}
  TCustomDrawWeekNumberEvent = procedure(Sender: TObject; Canvas: TCanvas;
    const ARect: TRect; State: TIntlCalDrawStates;
    AWeek: Integer; var DefaultDraw: Boolean) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDrawDayOfWeekEvent is the type for event handlers that render name of
  /// a month.
  /// </summary>
  /// <param name="Sender">
  /// The object that generated the event.
  /// </param>
  /// <param name="Canvas">
  /// The canvas on which to draw the month's name.
  /// </param>
  /// <param name="ARect">
  /// The bounding rectangle on the canvas where the month's name should be drawn.
  /// </param>
  /// <param name="State">
  /// The state information that can affect the way the month's name is drawn.
  /// </param>
  /// <param name="AMonth">
  /// The month to draw its name.
  /// </param>
  /// <param name="DefaultDraw">
  /// Indicates whether the default rendering should proceed after the event handler exits.
  /// </param>
  {$endregion}
  TCustomDrawMonthEvent = procedure(Sender: TObject; Canvas: TCanvas;
    const ARect: TRect; State: TIntlCalDrawStates;
    AMonth: Integer; var DefaultDraw: Boolean) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDrawDayEvent is the type for event handlers that render an integer value
  /// as a year.
  /// </summary>
  /// <param name="Sender">
  /// The object that generated the event.
  /// </param>
  /// <param name="Canvas">
  /// The canvas on which to draw the year.
  /// </param>
  /// <param name="ARect">
  /// The bounding rectangle on the canvas where the year should be drawn.
  /// </param>
  /// <param name="State">
  /// The state information that can affect the way the year is drawn.
  /// </param>
  /// <param name="AYear">
  /// The year to draw.
  /// </param>
  /// <param name="DefaultDraw">
  /// Indicates whether the default rendering should proceed after the event handler exits.
  /// </param>
  {$endregion}
  TCustomDrawYearEvent = procedure(Sender: TObject; Canvas: TCanvas;
    const ARect: TRect; State: TIntlCalDrawStates;
    AYear: Integer; var DefaultDraw: Boolean) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomIntlMonthCalendar is the base class for controls that display a
  /// localized calendar on a form.
  /// </summary>
  /// <remarks>
  /// Use TCustomIntlMonthCalendar as a base class when defining objects that display a
  /// localized calendar.
  ///
  /// TCustomIntlMonthCalendar can use different calendar systems for manipulating and
  /// displaying the dates. The <see cref="CalendarType"/> property is the main property
  /// that specifies which calendar system should be used by the cotnrol. If the
  /// <see cref="CalendarType"/> property is not set, the <see cref="Culture"/> property
  /// can do this task. In addition, the <see cref="Culture"/> property provides the
  /// locale specific settings of the control. If none of these two properties are set,
  /// the <see cref="DefaultCalendar"/> global variable determines the calendar system
  /// used by the control.
  ///
  /// TCustomIntlDateTimeLabel can be linked to a <see cref="TLocalizer"/> component.
  /// In this case, the linked <see cref="TLocalizer"/> component can provide both the
  /// locale specific settings and the calendar system of the calendar control.
  /// </remarks>
  {$endregion}
  TCustomIntlMonthCalendar = class(TCustomControl, ILocalizerLink)
  protected
    type
      {$region 'xmldoc'}
      /// <summary>
      /// This enumeration type identifies the sections of a month calendar control.
      /// </summary>
      {$endregion}
      TIntlCalSection = (
        {$region 'xmldoc'}
        /// The section of the header items.
        {$endregion}
        csHeader,
        {$region 'xmldoc'}
        /// The section filled by the header text.
        {$endregion}
        csHeaderText,
        {$region 'xmldoc'}
        /// The section filled by the left navigation button.
        {$endregion}
        csArrowLeft,
        {$region 'xmldoc'}
        /// The section filled by the right navigation button.
        {$endregion}
        csArrowRight,
        {$region 'xmldoc'}
        /// The section of the footer items.
        {$endregion}
        csFooter,
        {$region 'xmldoc'}
        /// The empty area shared between row of day names and column of week numbers.
        {$endregion}
        csWeekSpace,
        {$region 'xmldoc'}
        /// The section of names of days.
        {$endregion}
        csDaysOfWeek,
        {$region 'xmldoc'}
        /// The section of week numbers.
        {$endregion}
        csWeekNumbers,
        {$region 'xmldoc'}
        /// The section, which display days of a month, months of a year, or years of a decade.
        {$endregion}
        csYMD
      );
      {$region 'xmldoc'}
      /// <summary>
      /// This data type represents a set of <see cref="TIntlCalSection"/> values.
      /// </summary>
      {$endregion}
      TIntlCalSections = set of TIntlCalSection;
      {$region 'xmldoc'}
      /// <summary>
      /// This enumeration type identifies different kind of names for days of week and
      /// months.
      /// </summary>
      {$endregion}
      TIntlCalNameKind = (
        {$region 'xmldoc'}
        /// Full name
        {$endregion}
        nkLong,
        {$region 'xmldoc'}
        /// Abbreviation
        {$endregion}
        nkShort,
        {$region 'xmldoc'}
        /// Shortest possible abbreviation
        {$endregion}
        nkShortest
      );
      {$region 'xmldoc'}
      /// <summary>
      /// This data type represents the internal metrics of a month calendar control.
      /// </summary>
      {$endregion}
      TIntlCalMetrics = record
        {$region 'xmldoc'}
        /// Full, abbreviation, or short name?
        {$endregion}
        NameKind: TIntlCalNameKind;
        {$region 'xmldoc'}
        /// Long or short date format for footer?
        {$endregion}
        LongFormat: Boolean;
        {$region 'xmldoc'}
        /// The client rectangle of the month calendar
        {$endregion}
        Bounds: TRect;
        {$region 'xmldoc'}
        /// The height of the header section
        {$endregion}
        HeaderHeight: Integer;
        {$region 'xmldoc'}
        /// The height of the footer section
        {$endregion}
        FooterHeight: Integer;
        {$region 'xmldoc'}
        /// The row's height for names of days
        {$endregion}
        FixedRowHeight: Integer;
        {$region 'xmldoc'}
        /// The column's width for week numbers
        {$endregion}
        FixedColWidth: Integer;
        {$region 'xmldoc'}
        /// The row's height for day, month, year, or decade
        {$endregion}
        RowHeight: Integer;
        {$region 'xmldoc'}
        /// The column's width for day, month, year, or decade
        {$endregion}
        ColWidth: Integer;
        {$region 'xmldoc'}
        /// The number of rows excluding the fixed row
        {$endregion}
        RowCount: Integer;
        {$region 'xmldoc'}
        /// The number of columns excluding the fixed column
        {$endregion}
        ColCount: Integer;
        {$region 'xmldoc'}
        /// The number of rows with extra height (1 pixel)
        {$endregion}
        StretchedRows: Integer;
        {$region 'xmldoc'}
        /// The number of columns with extra width (1 pixel)
        {$endregion}
        StretchedCols: Integer;
        {$region 'xmldoc'}
        /// The optimal width of the grid
        {$endregion}
        PrefferedWidth: Integer;
        {$region 'xmldoc'}
        /// The optimal height of the grid
        {$endregion}
        PrefferedHeight: Integer;
        {$region 'xmldoc'}
        /// The bounding rectangles of the month calendar's sections
        {$endregion}
        Sections: array[TIntlCalSection] of TRect;
      end;
  private
    fDate: TDate;
    fDateMin: TDate;
    fDateMax: TDate;
    fCalendarType: TCalendarClass;
    fCalendar: TCalendar;
    fCulture: TCultureInfo;
    fCultureCalendar: TLocalizerSwicth;
    fCultureDigits: TLocalizerSwicth;
    fWeekNumbers: Boolean;
    fCurrentView: TIntlCalViewMode;
    fSelectionStyle: TIntlCalSelectionStyle;
    fSelectionAnchor: TDate;
    fSelection: TDateTimeList;
    fShowHeader: Boolean;
    fShowFooter: Boolean;
    fFontHeader: TFont;
    fFontFooter: TFont;
    fFontDayOfWeek: TFont;
    fFontWeekNumbers: TFont;
    fEra, fYear, fMonth, fDay: Integer;
    fMonthsInYear, fDaysInYear, fDaysInMonth: Integer;
    fMonthFirstDate: TDate;
    fMonthFirstDayOfWeek: TDayOfWeek;
    fMonthStartCell: Integer;
    fFirstCellDayNumber: Integer;
    fFirstCellYear: Integer;
    fActualDateMin: TDate;
    fActualDateMax: TDate;
    fHotCell: Integer;
    fHotInfo: TIntlCalHitTest;
    fDraggingCell: Integer;
    fDraggingShiftState: TShiftState;
    fDraggingOffPage: Boolean;
    fNavigationTimer: Integer;
    fHintDate: TDate;
    fMetrics: TIntlCalMetrics;
    fLocalizer: TLocalizer;
    fPrefferedSize: TSize;
    fPrefferedSizeReady: Boolean;
    fPrefferedSizeSmallest: Boolean;
    fUpdateMetricsCount: Integer;
    fColorDivider: TColor;
    fColorToday: TColor;
    fTheme: HTHEME;
    fAnimate: Boolean;
    fSavedScreen: TBitmap;
    fSavedView: TIntlCalViewMode;
    fSavedDate: TDate;
    fSavedCellRect: TRect;
    fSaveFontHeader: Boolean;
    fSaveFontFooter: Boolean;
    fSaveFontDayOfWeek: Boolean;
    fSaveFontWeekNumbers: Boolean;
    fOnDateChange: TNotifyEvent;
    fOnPageChange: TNotifyEvent;
    fOnCalendarChange: TNotifyEvent;
    fOnSelectionChange: TNotifyEvent;
    fOnDateHint: TDateHintEvent;
    fOnDateSelectable: TDateSelectableEvent;
    fOnCustomDrawDayOfWeek: TCustomDrawDayOfWeekEvent;
    fOnCustomDrawWeekNumber: TCustomDrawWeekNumberEvent;
    fOnCustomDrawDay: TCustomDrawDayEvent;
    fOnCustomDrawMonth: TCustomDrawMonthEvent;
    fOnCustomDrawYear: TCustomDrawYearEvent;
    fOnCustomDrawHeaderText: TCustomDrawTextEvent;
    fOnCustomDrawFooterText: TCustomDrawTextEvent;
    procedure SetDate(const Value: TDate);
    procedure SetDateMin(const Value: TDate);
    procedure SetDateMax(const Value: TDate);
    procedure SetCalendarType(Value: TCalendarClass);
    procedure SetCulture(Value: TCultureInfo);
    procedure SetCultureCalendar(Value: TLocalizerSwicth);
    procedure SetCultureDigits(Value: TLocalizerSwicth);
    procedure SetWeekNumbers(Value: Boolean);
    procedure SetSelectionStyle(Value: TIntlCalSelectionStyle);
    procedure SetShowHeader(Value: Boolean);
    procedure SetShowFooter(Value: Boolean);
    procedure SetFontHeader(Value: TFont);
    procedure SetFontFooter(Value: TFont);
    procedure SetFontDayOfWeek(Value: TFont);
    procedure SetFontWeekNumbers(Value: TFont);
    procedure SetCurrentView(Value: TIntlCalViewMode);
    procedure SetLocalizer(Value: TLocalizer);
    procedure SetColorDivider(Value: TColor);
    procedure SetColorToday(Value: TColor);
    function GetSelectionStart: TDate;
    function GetSelectionEnd: TDate;
    function GetSelectionCount: Integer;
    function GetSelection(Index: Integer): TDate;
    function GetUsingCultureCalendar: Boolean;
    function GetUsingCultureDigits: Boolean;
    function GetMonthLastDate: TDate; inline;
    function GetFirstCellDate: TDate; inline;
    function GetLastCellDate: TDate; inline;
    function GetLastCellYear: Integer; inline;
    function GetFirstYearOnPage: Integer;
    function GetLastYearOnPage: Integer;
    function GetColCount: Integer; inline;
    function GetRowCount: Integer; inline;
    function GetCellCount: Integer; inline;
    function GetFocusedCell: Integer;
    function GetTheme: HTHEME;
    function IsDateStored: Boolean;
    function IsDateMinStored: Boolean;
    function IsDateMaxStored: Boolean;
    procedure ReadCulture(Reader: TReader);
    procedure WriteCulture(Writer: TWriter);
    procedure ReadCalendarType(Reader: TReader);
    procedure WriteCalendarType(Writer: TWriter);
    procedure FontHeaderChanged(Sender: TObject);
    procedure FontFooterChanged(Sender: TObject);
    procedure FontDayOfWeekChanged(Sender: TObject);
    procedure FontWeekNumbersChanged(Sender: TObject);
    procedure CalendarChanged(Sender: TObject);
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure ExtractDateInfo(const ADate: TDate);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies the duration of the control's animations, in milliseconds.
    /// </summary>
    {$endregion}
    const AnimateDuration = 400; {ms}
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies the interval between automatic navigations, in milliseconds.
    /// </summary>
    {$endregion}
    const AutoNavInterval = AnimateDuration + 100; {ms}
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies the identifier of forward automatic navigation timer.
    /// </summary>
    {$endregion}
    const AutoNavTimerForward = 100;
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies the identifier of backward automatic navigation timer.
    /// </summary>
    {$endregion}
    const AutoNavTimerBackward = 200;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Notifies the control about changes in the linked <see cref="TLocalizer"/>
    /// component.
    /// </summary>
    /// <param name="Sender">
    /// The <see cref="TLocalizer"/> component who originated the notification.
    /// </param>
    /// <param name="Reason">
    /// The reason of the notification.
    /// </param>
    {$endregion}
    procedure LocalizerNotify(Sender: TLocalizer; Reason: TLocalizerNotification);
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to notifications that components are about to be created or destroyed.
    /// </summary>
    /// <param name="AComponent">
    /// The component, which is generated the notification.
    /// </param>
    /// <param name="Operation">
    /// Indicates whether the component is created or destroyed.
    /// </param>
    {$endregion}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="Culture"/> and <see cref="CalendarType"/> properties
    /// as if they were published.
    /// </summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.
    /// </param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Makes any required adjustments when the control changes size.
    /// </summary>
    {$endregion}
    procedure Resize; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Renders the control.
    /// </summary>
    {$endregion}
    procedure Paint; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the control when it is first loaded into memory.
    /// </summary>
    {$endregion}
    procedure Loaded; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the control can resize itself automatically to accommodate
    /// its contents.
    /// </summary>
    /// <param name="NewWidth">
    /// Indicates the proposed new width of the control.
    /// </param>
    /// <param name="NewHeight">
    /// Indicates the proposed new height of the control.
    /// </param>
    /// <returns>
    /// Always returns <see langword="true"/>.
    /// </returns>
    {$endregion}
    function CanAutoSize(var NewWidth: Integer; var NewHeight: Integer): Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a mouse button down while the cursor's hotspot is
    /// over the control.
    /// </summary>
    /// <param name="Button">
    /// Determines which mouse button the user pressed.
    /// </param>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user pressed the mouse button.
    /// </param>
    /// <param name="X">
    /// The horizontal pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    /// <param name="Y">
    /// The vertical pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    {$endregion}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user moves the mouse pointer while the cursor's hotspot is
    /// over the control.
    /// </summary>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user moved the mouse pointer.
    /// </param>
    /// <param name="X">
    /// The horizontal pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    /// <param name="Y">
    /// The vertical pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    {$endregion}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user releases a previously pressed mouse button down while the
    /// cursor's hotspot is over the control.
    /// </summary>
    /// <param name="Button">
    /// Determines which mouse button the user released.
    /// </param>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user released the mouse button.
    /// </param>
    /// <param name="X">
    /// The horizontal pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    /// <param name="Y">
    /// The vertical pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    {$endregion}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user rotates the mouse wheel downward.
    /// </summary>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user rotated the mouse wheel.
    /// </param>
    /// <param name="MousePos">
    /// The pixel coordinates of the mouse pointer within the screen.
    /// </param>
    {$endregion}
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user rotates the mouse wheel upward.
    /// </summary>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user rotated the mouse wheel.
    /// </param>
    /// <param name="MousePos">
    /// The pixel coordinates of the mouse pointer within the screen.
    /// </param>
    {$endregion}
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key.
    /// </summary>
    /// <param name="Key">
    /// The key that was pressed. If the method changes Key to 0, processing of the keystroke stops.
    /// </param>
    /// <param name="Shift">
    /// Indicates the concurrent state of the Ctrl, Shift, and Alt keys.
    /// </param>
    {$endregion}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the control when it receives input focus.
    /// </summary>
    {$endregion}
    procedure DoEnter; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the control when it loses input focus.
    /// </summary>
    {$endregion}
    procedure DoExit; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnDateChange"/> event.
    /// </summary>
    {$endregion}
    procedure DoDateChange; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnPageChange"/> event.
    /// </summary>
    {$endregion}
    procedure DoPageChange; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnCalendarChange"/> event.
    /// </summary>
    {$endregion}
    procedure DoCalendarChange; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnSelectionChange"/> event.
    /// </summary>
    {$endregion}
    procedure DoSelectionChange; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnDateHint"/> event.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value that its hint string is requested.
    /// </param>
    /// <param name="HintStr">
    /// The hint string of the <see cref="TDate"/> value.
    /// </param>
    {$endregion}
    procedure DoDateHint(const ADate: TDate; var HintStr: String); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnDateSelectable"/> event.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDate"/> value can be selected,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DoDateSelectable(const ADate: TDate): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnCustomDrawDayOfWeek"/> event.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the day's name.
    /// </param>
    /// <param name="ARect">
    /// The bounding rectangle on the canvas where the day's name should be drawn.
    /// </param>
    /// <param name="State">
    /// The state information that can affect the way the day's name is drawn.
    /// </param>
    /// <param name="ADayOfWeek">
    /// The day of the week to draw its name.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the default rendering should proceed after
    /// the method exits, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DoCustomDrawDayOfWeek(Canvas: TCanvas; const ARect: TRect; State: TIntlCalDrawStates; ADayOfWeek: TDayOfWeek): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnCustomDrawWeekNumber"/> event.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the week number.
    /// </param>
    /// <param name="ARect">
    /// The bounding rectangle on the canvas where the week number should be drawn.
    /// </param>
    /// <param name="State">
    /// The state information that can affect the way the week number is drawn.
    /// </param>
    /// <param name="AWeek">
    /// The week of the year to draw.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the default rendering should proceed after
    /// the method exits, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DoCustomDrawWeekNumber(Canvas: TCanvas; const ARect: TRect; State: TIntlCalDrawStates; AWeek: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnCustomDrawDay"/> event.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the day of the month.
    /// </param>
    /// <param name="ARect">
    /// The bounding rectangle on the canvas where the day of the month should be drawn.
    /// </param>
    /// <param name="State">
    /// The state information that can affect the way the day of the month is drawn.
    /// </param>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value that represents the day of the moneh.
    /// </param>
    /// <param name="ADay">
    /// The day of the month to draw.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the default rendering should proceed after
    /// the method exits, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DoCustomDrawDay(Canvas: TCanvas; const ARect: TRect; State: TIntlCalDrawStates; const ADate: TDate; ADay: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnCustomDrawMonth"/> event.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the month's name.
    /// </param>
    /// <param name="ARect">
    /// The bounding rectangle on the canvas where the month's name should be drawn.
    /// </param>
    /// <param name="State">
    /// The state information that can affect the way the month's name is drawn.
    /// </param>
    /// <param name="AMonth">
    /// The month to draw its name.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the default rendering should proceed after
    /// the method exits, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DoCustomDrawMonth(Canvas: TCanvas; const ARect: TRect; State: TIntlCalDrawStates; AMonth: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnCustomDrawYear"/> event.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the year.
    /// </param>
    /// <param name="ARect">
    /// The bounding rectangle on the canvas where the year should be drawn.
    /// </param>
    /// <param name="State">
    /// The state information that can affect the way the year is drawn.
    /// </param>
    /// <param name="AYear">
    /// The year to draw.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the default rendering should proceed after
    /// the method exits, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DoCustomDrawYear(Canvas: TCanvas; const ARect: TRect; State: TIntlCalDrawStates; AYear: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnCustomDrawHeaderText"/> event.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the text.
    /// </param>
    /// <param name="ARect">
    /// The bounding rectangle on the canvas where the text should be drawn.
    /// </param>
    /// <param name="State">
    /// The state information that can affect the way the text is drawn.
    /// </param>
    /// <param name="AText">
    /// The text to draw.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the default rendering should proceed after
    /// the method exits, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DoCustomDrawHeaderText(Canvas: TCanvas; const ARect: TRect; State: TIntlCalDrawStates; const AText: String): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnCustomDrawFooterText"/> event.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the text.
    /// </param>
    /// <param name="ARect">
    /// The bounding rectangle on the canvas where the text should be drawn.
    /// </param>
    /// <param name="State">
    /// The state information that can affect the way the text is drawn.
    /// </param>
    /// <param name="AText">
    /// The text to draw.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the default rendering should proceed after
    /// the method exits, otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DoCustomDrawFooterText(Canvas: TCanvas; const ARect: TRect; State: TIntlCalDrawStates; const AText: String): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a string that its embedded nominal digits (0-9) are substituted with
    /// their corresponding digits in the langauge specified by the <see cref="Culture"/>
    /// property. NormalizeDigits also inserts appropriate Unicode control character in
    /// front of the string to prevent automatic digit substitution by Windows.
    /// </summary>
    /// <param name="Str">
    /// The string to convert.
    /// </param>
    /// <returns>
    /// The string that its digits are fixed as native.
    /// </returns>
    /// <seealso cref="Culture"/>
    {$endregion}
    procedure NormalizeDigits(var Str: String); inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the display name for a specified day of the week.
    /// </summary>
    /// <param name="DayOfWeek">
    /// The day of the week that its display name is requested.
    /// </param>
    /// <param name="NameKind">
    /// Determines which kind of display name is needed.
    /// </param>
    /// <returns>
    /// The display name of the specified day of the week.
    /// </returns>
    {$endregion}
    function GetDayName(DayOfWeek: TDayOfWeek; NameKind: TIntlCalNameKind): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the display name for a specified month.
    /// </summary>
    /// <param name="Month">
    /// The month that its display name is requested.
    /// </param>
    /// <param name="NameKind">
    /// Determines which kind of display name is needed.
    /// </param>
    /// <returns>
    /// The display name of the specified month.
    /// </returns>
    {$endregion}
    function GetMonthName(Month: Integer; NameKind: TIntlCalNameKind): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a specified year to string.
    /// </summary>
    /// <param name="AYear">
    /// The year.
    /// </param>
    /// <returns>
    /// The string representation of the specified year.
    /// </returns>
    {$endregion}
    function GetYearText(AYear: Integer): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a specified day of the month to string.
    /// </summary>
    /// <param name="ADay">
    /// The day of the month.
    /// </param>
    /// <returns>
    /// The string representation of the specified day of the month.
    /// </returns>
    {$endregion}
    function GetDayText(ADay: Integer): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a specified week of the year to string.
    /// </summary>
    /// <param name="ADay">
    /// The week of the year.
    /// </param>
    /// <returns>
    /// The string representation of the specified week of the year.
    /// </returns>
    {$endregion}
    function GetWeekText(AWeek: Integer): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the text of the calendar's header based on a specified view mode.
    /// </summary>
    /// <param name="ViewMode">
    /// Indicates whether the header text is going to be used.
    /// </param>
    /// <returns>
    /// The text that appears on the header.
    /// </returns>
    {$endregion}
    function GetHeaderText(ViewMode: TIntlCalViewMode): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the today's date as the footer text.
    /// </summary>
    /// <param name="LongFormat">
    /// Indicates which of long or short date formats should be used.
    /// </param>
    /// <returns>
    /// The text that appears on the footer.
    /// </returns>
    {$endregion}
    function GetFooterText(LongFormat: Boolean): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines that a point is in which section of the control.
    /// </summary>
    /// <param name="X">
    /// The point's horizontal coordinate in the control's coordinate space.
    /// </param>
    /// <param name="Y">
    /// The point's vertical coordinate in the control's coordinate space.
    /// </param>
    /// <param name="Cell">
    /// The index of the grid's cell, if the point is in the calendar's grid.
    /// </param>
    /// <returns>
    /// Returns a <see cref="TIntlCalHitTest"/> value.
    /// </returns>
    {$endregion}
    function GetHitTestInfoAt(X, Y: Integer; out Cell: Integer): TIntlCalHitTest; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether a point is in a cell of the calendar's grid that represents
    /// a day of a month.
    /// </summary>
    /// <param name="X">
    /// The point's horizontal coordinate in the control's coordinate space.
    /// </param>
    /// <param name="Y">
    /// The point's vertical coordinate in the control's coordinate space.
    /// </param>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value of the cell, if the point is in the calendar's grid and
    /// the grid is showing days of a month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the point is in the calendar's grid and the grid is
    /// showing days of a month, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="GetDateAtCell"/>
    {$endregion}
    function GetDateAt(X, Y: Integer; out ADate: TDate): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether a cell of the calendar's grid is representing a day of a month.
    /// </summary>
    /// <param name="Index">
    /// The index of the cell to examine.
    /// </param>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value of the cell, if the specified cell index is in range and
    /// the calendar's grid is showing days of a month.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the specified cell index is in range and the calendar's
    /// grid is showing days of a month, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="GetDateAt"/>
    {$endregion}
    function GetDateAtCell(Index: Integer; out ADate: TDate): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether a point is in a cell of the calendar's grid that represents
    /// a month of a year.
    /// </summary>
    /// <param name="X">
    /// The point's horizontal coordinate in the control's coordinate space.
    /// </param>
    /// <param name="Y">
    /// The point's vertical coordinate in the control's coordinate space.
    /// </param>
    /// <param name="AMonth">
    /// The month value of the cell, if the point is in the calendar's grid and the grid
    /// is showing months of a year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the point is in the calendar's grid and the grid is
    /// showing months of a year, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="GetMonthAtCell"/>
    {$endregion}
    function GetMonthAt(X, Y: Integer; out AMonth: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether a cell of the calendar's grid is representing a month of the year.
    /// </summary>
    /// <param name="Index">
    /// The index of the cell to examine.
    /// </param>
    /// <param name="AMonth">
    /// The month value of the cell, if the specified cell index is in range and
    /// the calendar's grid is showing months of a year.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the specified cell index is in range and the calendar's
    /// grid is showing months of a year, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="GetMonthAt"/>
    {$endregion}
    function GetMonthAtCell(Index: Integer; out AMonth: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether a point is in a cell of the calendar's grid that represents
    /// a year.
    /// </summary>
    /// <param name="X">
    /// The point's horizontal coordinate in the control's coordinate space.
    /// </param>
    /// <param name="Y">
    /// The point's vertical coordinate in the control's coordinate space.
    /// </param>
    /// <param name="AYear">
    /// The year value of the cell, if the point is in the calendar's grid and the grid
    /// is showing years of a decade.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the point is in the calendar's grid and the grid is
    /// showing years of a decade, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="GetYearAtCell"/>
    {$endregion}
    function GetYearAt(X, Y: Integer; out AYear: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether a cell of the calendar's grid is representing a year.
    /// </summary>
    /// <param name="Index">
    /// The index of the cell to examine.
    /// </param>
    /// <param name="AMonth">
    /// The year value of the cell, if the specified cell index is in range and
    /// the calendar's grid is showing years of a decade.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the specified cell index is in range and the
    /// calendar's grid is showing years of a decade, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="GetYearAt"/>
    {$endregion}
    function GetYearAtCell(Index: Integer; out AYear: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the bounding rectangle of the calendar's content within the client
    /// area of the control.
    /// </summary>
    /// <returns>
    /// The bounding rectangle of the calendar's content.
    /// </returns>
    {$endregion}
    function GetCalendarClientRect: TRect; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the bounding rectangle of a calendar's section within the client
    /// area of the control.
    /// </summary>
    /// <param name="Section">
    /// The section to get its bounding rectangle.
    /// </param>
    /// <returns>
    /// The bounding rectangle of the calendar's section.
    /// </returns>
    /// <seealso cref="GetSectionWidth"/>
    /// <seealso cref="GetSectionHeight"/>
    /// <seealso cref="CalcMetricsSections"/>
    {$endregion}
    function GetSectionRect(Section: TIntlCalSection): TRect; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the width of a calendar's section, in pixels.
    /// </summary>
    /// <param name="Section">
    /// The section to get its width.
    /// </param>
    /// <returns>
    /// The width of the calendar's section.
    /// </returns>
    /// <seealso cref="GetSectionWidth"/>
    /// <seealso cref="GetSectionRect"/>
    {$endregion}
    function GetSectionWidth(Section: TIntlCalSection): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the height of a calendar's section, in pixels.
    /// </summary>
    /// <param name="Section">
    /// The section to get its height.
    /// </param>
    /// <returns>
    /// The height of the calendar's section.
    /// </returns>
    /// <seealso cref="GetSectionHeight"/>
    /// <seealso cref="GetSectionRect"/>
    {$endregion}
    function GetSectionHeight(Section: TIntlCalSection): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the bounding rectangle of a cell of the calendar's grid, which is
    /// specified by its row and column coordinates.
    /// </summary>
    /// <param name="Row">
    /// The row number of the cell.
    /// </param>
    /// <param name="Col">
    /// The column number of the cell.
    /// </param>
    /// <returns>
    /// The bounding rectangle of the cell.
    /// </returns>
    {$endregion}
    function GetCellRect(Row, Col: Integer): TRect; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the bounding rectangle of a cell of the calendar's grid, which is
    /// specified by its index.
    /// </summary>
    /// <param name="Index">
    /// The index of the cell in the grid.
    /// </param>
    /// <returns>
    /// The bounding rectangle of the cell.
    /// </returns>
    {$endregion}
    function GetCellRect(Index: Integer): TRect; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether a vertical coordinate is within the calendar's grid.
    /// </summary>
    /// <param name="Y">
    /// The vertical coordinate in the control's coordinate space.
    /// </param>
    /// <returns>
    /// Returns the row number of the grid at the specified vertical coordinate, or -1
    /// if the coordinate is not within the calendar's grid.
    /// </returns>
    /// <seealso cref="GetColAt"/>
    {$endregion}
    function GetRowAt(Y: Integer): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether a horizontal coordinate is within the calendar's grid.
    /// </summary>
    /// <param name="Y">
    /// The horizontal coordinate in the control's coordinate space.
    /// </param>
    /// <returns>
    /// Returns the column number of the grid at the specified horizontal coordinate, or -1
    /// if the coordinate is not within the calendar's grid.
    /// </returns>
    /// <seealso cref="GetRowAt"/>
    {$endregion}
    function GetColAt(X: Integer): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Regarding to the value of <see cref="CurrentView"/> and <see cref="Date"/> properties,
    /// returns a date that represents next or previous month, year, or decade.
    /// </summary>
    /// <param name="Backward">
    /// Indicates whether the next or the previous date is needed.
    /// </param>
    /// <returns>
    /// The <see cref="TDate"/> value of the next or the previous date.
    /// </returns>
    {$endregion}
    function GetAdjacentPageDate(Backward: Boolean): TDate;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the preffered dimension of the control's client area to display
    /// the calendar's content.
    /// </summary>
    /// <param name="AWidth">
    /// The calculated width of the control's client area.
    /// </param>
    /// <param name="AHeight">
    /// The calculated height of the control's client area.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the calculated size is the smallest possible
    /// value that can support the entire content, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="GetPreferredControlSize"/>
    {$endregion}
    function GetPreferredClientSize(out AWidth, AHeight: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the preffered dimension of the control (including border and bevels)
    /// to display the calendar's content.
    /// </summary>
    /// <param name="AWidth">
    /// The calculated width of the control.
    /// </param>
    /// <param name="AHeight">
    /// The calculated height of the control.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the calculated size is the smallest possible
    /// value that can support the entire content, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="GetPreferredClientSize"/>
    {$endregion}
    function GetPreferredControlSize(out AWidth, AHeight: Integer): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the metrics for rendering the calendar.
    /// </summary>
    /// <param name="ViewMode">
    /// Determines metrics for which page type is requested.
    /// </param>
    /// <param name="Metrics">
    /// The record that stores the calculated metrics.
    /// </param>
    /// <seealso cref="CalcMetricsSections"/>
    /// <seealso cref="UpdateMetrics"/>
    {$endregion}
    procedure CalcMetrics(ViewMode: TIntlCalViewMode; out Metrics: TIntlCalMetrics); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates the bounding rectangle of calnedar's sections based on a specified metrics.
    /// </summary>
    /// <param name="Metrics">
    /// The metrics record that stores the bounding rectangle of sections.
    /// </param>
    /// <seealso cref="CalcMetrics"/>
    /// <seealso cref="UpdateMetrics"/>
    {$endregion}
    procedure CalcMetricsSections(var Metrics: TIntlCalMetrics); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Recalculates the metrics for rendering the calendar, and invalidates the control.
    /// </summary>
    /// <seealso cref="CalcMetrics"/>
    /// <seealso cref="CalcMetricsSections"/>
    {$endregion}
    procedure UpdateMetrics; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Reevaluates value of <see cref="ActualDateMin"/> and <see cref="ActualDateMax"/>
    /// properties.
    /// </summary>
    {$endregion}
    procedure UpdateMinMaxRanges; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Validates the <see cref="Calendar"/> property to ensure it represents a proper
    /// calendar system with an appropriate locale settings.
    /// </summary>
    {$endregion}
    procedure ValidateCalendar; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the control is using the Windows theming system.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the control is using the Windows theming system
    /// to draw itself, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Theme"/>
    {$endregion}
    function IsThemed: Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified year is somehow visible on the current page of the
    /// control.
    /// </summary>
    /// <param name="AYear">
    /// The year to be examined.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the year is visible on the current page, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function IsYearOnPage(AYear: Integer): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Initializes the animation that changes the current page.
    /// </summary>
    /// <seealso cref="AnimatePlay"/>
    /// <seealso cref="AnimateEnd"/>
    {$endregion}
    procedure AnimateBegin; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Plays a previously initialized animation using the <seealso cref="AnimateBegin"/> method.
    /// </summary>
    /// <seealso cref="AnimateBegin"/>
    /// <seealso cref="AnimateEnd"/>
    {$endregion}
    procedure AnimatePlay; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Finalizes a previously initialized animation using the <seealso cref="AnimateBegin"/> method.
    /// </summary>
    /// <seealso cref="AnimateBegin"/>
    /// <seealso cref="AnimatePlay"/>
    {$endregion}
    procedure AnimateEnd; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a bitmap that contains the current calendar's screen.
    /// </summary>
    /// <returns>
    /// The bitmap of the current calendar's screen.
    /// </returns>
    {$endregion}
    function TakeSnapshot: TBitmap;
    {$region 'xmldoc'}
    /// <summary>
    /// Renders background of a calendar's section.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the background.
    /// </param>
    /// <param name="Rect">
    /// The bounding rectangle on the canvas where the background should be drawn.
    /// </param>
    /// <param name="State">
    /// The state information that can affect the way the background is drawn.
    /// </param>
    /// <seealso cref="RenderText"/>
    {$endregion}
    procedure RenderBackground(Canvas: TCanvas; var Rect: TRect; State: TIntlCalDrawStates); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Renders a specified text.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the text.
    /// </param>
    /// <param name="Rect">
    /// The bounding rectangle on the canvas where the text should be drawn.
    /// </param>
    /// <param name="State">
    /// The state information that can affect the way the text is drawn.
    /// </param>
    /// <param name="Text">
    /// The text to draw.
    /// </param>
    /// <param name="Alignment">
    /// The horizontal alignment of the text in regard of its bounding rectangle.
    /// </param>
    /// <seealso cref="RenderBackground"/>
    {$endregion}
    procedure RenderText(Canvas: TCanvas; var Rect: TRect; State: TIntlCalDrawStates; const Text: String; Alignment: TAlignment); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Renders a left or right arrow.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the arrow.
    /// </param>
    /// <param name="Rect">
    /// The bounding rectangle on the canvas where the arrow should be drawn.
    /// </param>
    /// <param name="State">
    /// The state information that can affect the way the arrow is drawn.
    /// </param>
    /// <param name="Backward">
    /// Determines whether the arrow is pointed to left or right.
    /// </param>
    /// <seealso cref="RenderBackground"/>
    /// <seealso cref="RenderText"/>
    {$endregion}
    procedure RenderArrow(Canvas: TCanvas; var Rect: TRect; State: TIntlCalDrawStates; Backward: Boolean); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Renders a marker in a specified bounding rectangle to specify the today's date.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas on which to draw the marker.
    /// </param>
    /// <param name="Rect">
    /// The bounding rectangle on the canvas where the marker should be drawn.
    /// </param>
    {$endregion}
    procedure RenderTodayMarker(Canvas: TCanvas; var Rect: TRect); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Draws the entire calendar on a specified canvas.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas to draw.
    /// </param>
    {$endregion}
    procedure DrawCalendar(Canvas: TCanvas); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Draws the header section of the calendar on a specified canvas.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas to draw.
    /// </param>
    {$endregion}
    procedure DrawHeader(Canvas: TCanvas); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Draws the footer section of the calendar on a specified canvas.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas to draw.
    /// </param>
    {$endregion}
    procedure DrawFooter(Canvas: TCanvas); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Draws the empty area shared between row of day names and column of week
    /// numbers.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas to draw.
    /// </param>
    {$endregion}
    procedure DrawWeekSpace(Canvas: TCanvas); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Draws the section of the calendar that displays days of the week.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas to draw.
    /// </param>
    {$endregion}
    procedure DrawDaysOfWeek(Canvas: TCanvas); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Draws the section of the calendar that displays week numbers.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas to draw.
    /// </param>
    {$endregion}
    procedure DrawWeekNumbers(Canvas: TCanvas); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Draws the calendar's grid with days of a month.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas to draw.
    /// </param>
    {$endregion}
    procedure DrawDays(Canvas: TCanvas); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Draws the calendar's grid with months of a year.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas to draw.
    /// </param>
    {$endregion}
    procedure DrawMonths(Canvas: TCanvas); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Draws the calendar's grid with years of a decade.
    /// </summary>
    /// <param name="Canvas">
    /// The canvas to draw.
    /// </param>
    {$endregion}
    procedure DrawYears(Canvas: TCanvas); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for a specified set of calendar's sections.
    /// </summary>
    /// <param name="Sections">
    /// The set of sections to repaint.
    /// </param>
    /// <seealso cref="InvalidateDateDependentSections"/>
    /// <seealso cref="FocusDate"/>
    /// <seealso cref="Date"/>
    {$endregion}
    procedure InvalidateSections(Sections: TIntlCalSections); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for those calendar's sections that need to be
    /// updated when the <see cref="Date"/> property changes.
    /// </summary>
    /// <seealso cref="InvalidateSections"/>
    /// <seealso cref="Date"/>
    {$endregion}
    procedure InvalidateDateDependentSections; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for a cell of calendar's grid that is specified by
    /// its index.
    /// </summary>
    /// <param name="Index">
    /// The index of the cell in the grid.
    /// </param>
    {$endregion}
    procedure InvalidateCell(Index: Integer); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for any part of the calendar that represents a
    /// cpmponent of a specified date.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value to repaint.
    /// </param>
    /// <seealso cref="InvalidateDates"/>
    /// <seealso cref="InvalidateDateRange"/>
    {$endregion}
    procedure InvalidateDate(const ADate: TDate); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for those days in the calendar's grid that are
    /// in a specified dates.
    /// </summary>
    /// <param name="ADates">
    /// The list of <see cref="TDate"/> values to repaint.
    /// </param>
    /// <seealso cref="InvalidateDate"/>
    /// <seealso cref="InvalidateDateRange"/>
    {$endregion}
    procedure InvalidateDates(ADates: TDateTimeList); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for those days in the calendar's grid that are
    /// in a specified range.
    /// </summary>
    /// <param name="ADate1">
    /// The <see cref="TDate"/> value that indicates start of the range.
    /// </param>
    /// <param name="ADate2">
    /// The <see cref="TDate"/> value that indicates end of the range.
    /// </param>
    /// <seealso cref="InvalidateDate"/>
    /// <seealso cref="InvalidateDates"/>
    {$endregion}
    procedure InvalidateDateRange(const ADate1, ADate2: TDate); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for those part of the calendar that are in hot state.
    /// </summary>
    /// <seealso cref="InvalidateSelection"/>
    {$endregion}
    procedure InvalidateHotLight; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for selected cells of the calendar's grid.
    /// </summary>
    /// <seealso cref="InvalidateHotLight"/>
    {$endregion}
    procedure InvalidateSelection; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Selects a specified date as if clicked with the mouse. Conditional upon
    /// the value of <see cref="SelectionStyle"/> property, one date or multiple
    /// dates can be selected simultaneously.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value to select.
    /// </param>
    /// <param name="ShiftState">
    /// Simulates the effect of using the Control key or the Shift key or mouse buttons
    /// when selecting the date.
    /// </param>
    /// <seealso cref="FocusDate"/>
    /// <seealso cref="Select"/>
    /// <seealso cref="ClearSelection"/>
    /// <seealso cref="IsSelected"/>
    /// <seealso cref="SelectionStart"/>
    /// <seealso cref="SelectionEnd"/>
    /// <seealso cref="SelectionCount"/>
    /// <seealso cref="Selection"/>
    /// <seealso cref="SelectionStyle"/>
    {$endregion}
    procedure SelectDate(const ADate: TDate; ShiftState: TShiftState = []);
    {$region 'xmldoc'}
    /// <summary>
    /// Marks a specified date as focused. Only one date can be focused at a time.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value to mark as focused.
    /// </param>
    /// <seealso cref="SelectDate"/>
    /// <seealso cref="Date"/>
    {$endregion}
    procedure FocusDate(const ADate: TDate);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the handle to the Windows theme that is used for rendering the control.
    /// </summary>
    {$endregion}
    property Theme: HTHEME read GetTheme;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of columns in the calendar's grid.
    /// </summary>
    {$endregion}
    property ColCount: Integer read GetColCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of rows in the calendar's grid.
    /// </summary>
    {$endregion}
    property RowCount: Integer read GetRowCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of cells in the calendar's grid.
    /// </summary>
    {$endregion}
    property CellCount: Integer read GetCellCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the day of the month in the first cell of the calendar's grid.
    /// NOTE: The value of this property is valid only with a <see cref="CurrentView"/> of vmDay.
    /// </summary>
    {$endregion}
    property FirstCellDayNumber: Integer read fFirstCellDayNumber;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TDate"/> value represented by the first cell of the calendar's grid.
    /// </summary>
    {$endregion}
    property FirstCellDate: TDate read GetFirstCellDate;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TDate"/> value represented by the last cell of the calendar's grid.
    /// </summary>
    {$endregion}
    property LastCellDate: TDate read GetLastCellDate;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the year in the first cell of the calendar's grid.
    /// NOTE: The value of this property is valid only with a <see cref="CurrentView"/> of vmYear.
    /// </summary>
    {$endregion}
    property FirstCellYear: Integer read fFirstCellYear;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the year in the last cell of the calendar's grid.
    /// NOTE: The value of this property is valid only with a <see cref="CurrentView"/> of vmYear.
    /// </summary>
    {$endregion}
    property LastCellYear: Integer read GetLastCellYear;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the smallest year in the calendar's grid that belongs to the current page.
    /// NOTE: The value of this property is valid only with a <see cref="CurrentView"/> of vmYear.
    /// </summary>
    {$endregion}
    property FirstYearOnPage: Integer read GetFirstYearOnPage;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the largest year in the calendar's grid that belongs to the current page.
    /// NOTE: The value of this property is valid only with a <see cref="CurrentView"/> of vmYear.
    /// </summary>
    {$endregion}
    property LastYearOnPage: Integer read GetlastYearOnPage;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets index of the cell in the calendar's grid, which represents the start of the month.
    /// NOTE: The value of this property is valid only with a <see cref="CurrentView"/> of vmDay.
    /// </summary>
    {$endregion}
    property MonthStartCell: Integer read fMonthStartCell;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets index of the cell in the calendar's grid that has the focus.
    /// </summary>
    {$endregion}
    property FocusedCell: Integer read GetFocusedCell;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets index of the cell in the calendar's grid that has the hot state.
    /// </summary>
    {$endregion}
    property HotCell: Integer read fHotCell;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the last mouse hit test information.
    /// </summary>
    {$endregion}
    property HotInfo: TIntlCalHitTest read fHotInfo;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets index of the cell in the calendar's grid that is being dragged.
    /// </summary>
    {$endregion}
    property DraggingCell: Integer read fDraggingCell;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the current cell dragging changes the current page.
    /// </summary>
    {$endregion}
    property DraggingOffPage: Boolean read fDraggingOffPage;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the identifier of the current automatic navigation timer.
    /// </summary>
    {$endregion}
    property NavigationTimer: Integer read fNavigationTimer;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the current focused date.
    /// </summary>
    /// <exception cref="EDateTimeError">
    /// Occurs when the assigned date is not in the supported range of the calendar.
    /// </exception>
    /// <seealso cref="IsDateInRange"/>
    {$endregion}
    property Date: TDate read fDate write SetDate stored IsDateStored;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the smallest date that is allowed for the calendar.
    /// </summary>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="DateMax"/>
    {$endregion}
    property DateMin: TDate read fDateMin write SetDateMin stored IsDateMinStored;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the largest date that is allowed for the calendar.
    /// </summary>
    /// <seealso cref="ActualDateMax"/>
    /// <seealso cref="DateMin"/>
    {$endregion}
    property DateMax: TDate read fDateMax write SetDateMax stored IsDateMaxStored;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCalendar"/> class that is used for manipulating dates.
    /// </summary>
    /// <seealso cref="Culture"/>
    {$endregion}
    property CalendarType: TCalendarClass read fCalendarType write SetCalendarType;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCultureInfo"/> object that provides the locale specific
    /// settings of the calendar.
    ///
    /// NOTE: If the <see cref="CalendarType"/> property is <see langword="nil"/>, depends
    /// on the values of <see cref="TLocalizer"/> and <see cref="CultureCalendar"/> properties,
    /// the Culture property may also determine the calendar system of the control.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="CultureCalendar"/>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="CultureDigits"/>
    /// <seealso cref="CalendarType"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property Culture: TCultureInfo read fCulture write SetCulture;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar system used by the control should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.
    /// NOTE: Value of this property has no effect when the <see cref="CalendarType"/>
    /// property is not <see langword="nil"/>.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureCalendar: TLocalizerSwicth read fCultureCalendar write SetCultureCalendar default lsDefault;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the way the control renders digits should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.</summary>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureDigits: TLocalizerSwicth read fCultureDigits write SetCultureDigits default lsDefault;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the ways thet a user can select dates of the calendar.
    /// </summary>
    {$endregion}
    property SelectionStyle: TIntlCalSelectionStyle read fSelectionStyle write SetSelectionStyle default ssSingle;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether week numbers are shown on the calendar.
    /// </summary>
    {$endregion}
    property WeekNumbers: Boolean read fWeekNumbers write SetWeekNumbers default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar's header is shown.
    /// </summary>
    {$endregion}
    property ShowHeader: Boolean read fShowHeader write SetShowHeader default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar's footer is shown.
    /// </summary>
    {$endregion}
    property ShowFooter: Boolean read fShowFooter write SetShowFooter default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing text on the calendar's header.
    /// </summary>
    {$endregion}
    property FontHeader: TFont read fFontHeader write SetFontHeader stored fSaveFontHeader;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing text on the calendar's footer.
    /// </summary>
    {$endregion}
    property FontFooter: TFont read fFontFooter write SetFontFooter stored fSaveFontFooter;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing name of days on the calendar.
    /// </summary>
    {$endregion}
    property FontDayOfWeek: TFont read fFontDayOfWeek write SetFontDayOfWeek stored fSaveFontDayOfWeek;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing week numbers on the calendar.
    /// </summary>
    {$endregion}
    property FontWeekNumbers: TFont read fFontWeekNumbers write SetFontWeekNumbers stored fSaveFontWeekNumbers;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which date's component can be selected by the user right now.
    /// </summary>
    {$endregion}
    property CurrentView: TIntlCalViewMode read fCurrentView write SetCurrentView;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the control.
    /// </summary>
    /// <seealso cref="Culture"/>
    {$endregion}
    property Localizer: TLocalizer read fLocalizer write SetLocalizer;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the color of divider line between calendar's grid and the other parts.
    /// </summary>
    {$endregion}
    property ColorDivider: TColor read fColorDivider write SetColorDivider default clBtnFace;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the color of marker that distinguishes the today's date.
    /// </summary>
    {$endregion}
    property ColorToday: TColor read fColorToday write SetColorToday default clRed;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control changes the pages with an animation.
    /// </summary>
    {$endregion}
    property Animate: Boolean read fAnimate write fAnimate default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the <see cref="Date"/> property changes.
    /// </summary>
    {$endregion}
    property OnDateChange: TNotifyEvent read fOnDateChange write fOnDateChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the control's page changes.
    /// </summary>
    {$endregion}
    property OnPageChange: TNotifyEvent read fOnPageChange write fOnPageChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the <see cref="Calendar"/> property changes.
    /// </summary>
    {$endregion}
    property OnCalendarChange: TNotifyEvent read fOnCalendarChange write fOnCalendarChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the <see cref="Selection"/> property changes.
    /// </summary>
    {$endregion}
    property OnSelectionChange: TNotifyEvent read fOnSelectionChange write fOnSelectionChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse pauses over a day of a month.
    /// </summary>
    {$endregion}
    property OnDateHint: TDateHintEvent read fOnDateHint write fOnDateHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a day of a month is about to be selected.
    /// NOTE: This event occurs only with a <see cref="SelectionStyle"/> of ssFree.
    /// </summary>
    {$endregion}
    property OnDateSelectable: TDateSelectableEvent read fOnDateSelectable write fOnDateSelectable;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when name of a day of the week needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawDayOfWeek: TCustomDrawDayOfWeekEvent read fOnCustomDrawDayOfWeek write fOnCustomDrawDayOfWeek;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a week number needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawWeekNumber: TCustomDrawWeekNumberEvent read fOnCustomDrawWeekNumber write fOnCustomDrawWeekNumber;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a day of a month needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawDay: TCustomDrawDayEvent read fOnCustomDrawDay write fOnCustomDrawDay;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when name of a month needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawMonth: TCustomDrawMonthEvent read fOnCustomDrawMonth write fOnCustomDrawMonth;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a year needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawYear: TCustomDrawYearEvent read fOnCustomDrawYear write fOnCustomDrawYear;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when text of calendar's header needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawHeaderText: TCustomDrawTextEvent read fOnCustomDrawHeaderText write fOnCustomDrawHeaderText;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when text of calendar's footer needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawFooterText: TCustomDrawTextEvent read fOnCustomDrawFooterText write fOnCustomDrawFooterText;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the control.
    /// </summary>
    /// <param name="AOwner">
    /// The owner component.
    /// </param>
    {$endregion}
    constructor Create(AOwner: TComponent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the control and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Reevaluates the calendar and repaints it.
    /// </summary>
    {$endregion}
    procedure UpdateCalendar; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Deselects all selected dates in the control, if possible.
    /// </summary>
    /// <seealso cref="Select"/>
    /// <seealso cref="IsSelected"/>
    /// <seealso cref="SelectionStart"/>
    /// <seealso cref="SelectionEnd"/>
    /// <seealso cref="SelectionCount"/>
    /// <seealso cref="Selection"/>
    /// <seealso cref="SelectionStyle"/>
    {$endregion}
    procedure ClearSelection; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Selects a specified date as if clicked with the mouse. Conditional upon
    /// the value of <see cref="SelectionStyle"/> property, one date or multiple
    /// dates can be selected simultaneously.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value to select.
    /// </param>
    /// <param name="ShiftState">
    /// Simulates the effect of using the Control key or the Shift key when selecting
    /// the date.
    /// </param>
    /// <seealso cref="ClearSelection"/>
    /// <seealso cref="IsSelected"/>
    /// <seealso cref="SelectionStart"/>
    /// <seealso cref="SelectionEnd"/>
    /// <seealso cref="SelectionCount"/>
    /// <seealso cref="Selection"/>
    /// <seealso cref="SelectionStyle"/>
    {$endregion}
    procedure Select(const ADate: TDate; ShiftState: TShiftState = []); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a soecified date is selected.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the <see cref="TDate"/> value is selected,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Select"/>
    /// <seealso cref="ClearSelection"/>
    /// <seealso cref="SelectionStart"/>
    /// <seealso cref="SelectionEnd"/>
    /// <seealso cref="SelectionCount"/>
    /// <seealso cref="Selection"/>
    /// <seealso cref="SelectionStyle"/>
    {$endregion}
    function IsSelected(const ADate: TDate): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the value of <see cref="Culture"/> property is controlled
    /// by a <see cref="TLocalizer"/> component.
    /// </summary>
    /// <returns>
    /// Rteurns <see langword="true"/> if <see cref="Localizer"/> property is not
    /// <see langword="nil"/>, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="Localizer"/>
    {$endregion}
    function IsManagedByLocalizer: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified year of a specified era is in the supported
    /// and allowed range of the control.
    /// </summary>
    /// <param name="AEra">
    /// The era.
    /// </param>
    /// <param name="AYear">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// Rteurns <see langword="true"/> if at least one day within the specified
    /// year can be represented by the cotnrol, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsMonthInRange"/>
    /// <seealso cref="IsDateInRange"/>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function IsYearInRange(AEra, AYear: Integer): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified year is in the supported and allowed range of
    /// the control.
    /// </summary>
    /// <param name="AYear">
    /// The year to examine.
    /// </param>
    /// <returns>
    /// Rteurns <see langword="true"/> if at least one day within the specified
    /// year can be represented by the cotnrol, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsMonthInRange"/>
    /// <seealso cref="IsDateInRange"/>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function IsYearInRange(AYear: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified month of a specified year in a specified era
    /// is in the supported and allowed range of the control.
    /// </summary>
    /// <param name="AEra">
    /// The era.
    /// </param>
    /// <param name="AYear">
    /// The year of the era.
    /// </param>
    /// <param name="AMonth">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// Rteurns <see langword="true"/> if at least one day within the specified
    /// month can be represented by the cotnrol, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsYearInRange"/>
    /// <seealso cref="IsDateInRange"/>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function IsMonthInRange(AEra, AYear, AMonth: Integer): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified month of a specified year is in the supported
    /// and allowed range of the control.
    /// </summary>
    /// <param name="AYear">
    /// The year of the era.
    /// </param>
    /// <param name="AMonth">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// Rteurns <see langword="true"/> if at least one day within the specified
    /// month can be represented by the cotnrol, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsYearInRange"/>
    /// <seealso cref="IsDateInRange"/>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function IsMonthInRange(AYear, AMonth: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified date is in the supported and allowed range
    /// of the control.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value to examine.
    /// </param>
    /// <returns>
    /// Rteurns <see langword="true"/> if the date can be represented by the cotnrol,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsYearInRange"/>
    /// <seealso cref="IsMonthInRange"/>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function IsDateInRange(const ADate: TDate): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Confines a specified date to be in the supported and allowed range
    /// of the control.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value to confine.
    /// </param>
    /// <returns>
    /// If the date is less than <see cref="ActualDateMin"/>, returns <see cref="ActualDateMin"/>.
    /// If the date is greater than <see cref="ActualDateMax"/>, returns <see cref="ActualDateMax"/>.
    /// If the date is between <see cref="ActualDateMax"/> and <see cref="ActualDateMax"/>, returns
    /// the date as is.
    /// </returns>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function EnsureDateInRange(const ADate: TDate): TDate; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Formats a <see cref="TDateTime"/> value.
    /// </summary>
    /// <remarks>
    /// The Format method formats the <see cref="TDateTime"/> value given by
    /// <paramref name="Date"/> using the format string given by <paramref name="FmtStr"/>.
    ///
    /// The format string controls the formatting of date and time, and is composed
    /// from specifiers that represent values to insert into the formatted string.
    /// See <see cref="TCalendar.Format"/> for details.
    /// </remarks>
    /// <param name="FmtStr">
    /// The format string. See <see cref="TCalendar.Format"/> method for details.
    /// </param>
    /// <param name="DateTime">
    /// The <see cref="TDateTime"/> value.
    /// </param>
    /// <returns>
    /// The formatted <see cref="TDateTime"/> value as a string.
    /// </returns>
    /// <seealso cref="TCalendar.Format"/>
    {$endregion}
    function Format(const FmtStr: String; const ADate: TDateTime): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TCalendar"/> object that the control is using to
    /// manipulate and display dates.
    /// </summary>
    {$endregion}
    property Calendar: TCalendar read fCalendar;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the era represented by the <see cref="Date"/> property.
    /// </summary>
    {$endregion}
    property Era: Integer read fEra;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the year represented by the <see cref="Date"/> property.
    /// </summary>
    {$endregion}
    property Year: Integer read fYear;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the month represented by the <see cref="Date"/> property.
    /// </summary>
    {$endregion}
    property Month: Integer read fMonth;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the day of the month represented by the <see cref="Date"/> property.
    /// </summary>
    {$endregion}
    property Day: Integer read fDay;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of days in the month represented by the <see cref="Date"/>
    /// property.
    /// </summary>
    {$endregion}
    property DaysInMonth: Integer read fDaysInMonth;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TDate"/> value for the first day of the the month
    /// represented by the <see cref="Date"/> property.
    /// </summary>
    {$endregion}
    property MonthFirstDate: TDate read fMonthFirstDate;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TDate"/> value for the last day of the the month
    /// represented by the <see cref="Date"/> property.
    /// </summary>
    {$endregion}
    property MonthLastDate: TDate read GetMonthLastDate;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the first day of the week for the the month represented by the
    /// <see cref="Date"/> property.
    /// </summary>
    {$endregion}
    property MonthFirstDayOfWeek: TDayOfWeek read fMonthFirstDayOfWeek;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of months in the year represented by the <see cref="Date"/>
    /// property.
    /// </summary>
    {$endregion}
    property MonthsInYear: Integer read fMonthsInYear;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of years in the year represented by the <see cref="Date"/>
    /// property.
    /// </summary>
    {$endregion}
    property DaysInYear: Integer read fDaysInYear;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the smallest date that is supported by the <see cref="Calendar"/> and
    /// is allowed by the control.
    /// </summary>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="TCalendar.MinSupportedDateTime"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    property ActualDateMin: TDate read fActualDateMin;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the largest date that is supported by the <see cref="Calendar"/> and
    /// is allowed by the control.
    /// </summary>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="TCalendar.MaxSupportedDateTime"/>
    /// <seealso cref="ActualDateMin"/>
    {$endregion}
    property ActualDateMax: TDate read fActualDateMax;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the smallest selected date.
    /// </summary>
    /// <seealso cref="SelectionEnd"/>
    /// <seealso cref="SelectionCount"/>
    /// <seealso cref="Selection"/>
    /// <seealso cref="SelectionStyle"/>
    {$endregion}
    property SelectionStart: TDate read GetSelectionStart;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the largest selected date.
    /// </summary>
    /// <seealso cref="SelectionStart"/>
    /// <seealso cref="SelectionCount"/>
    /// <seealso cref="Selection"/>
    /// <seealso cref="SelectionStyle"/>
    {$endregion}
    property SelectionEnd: TDate read GetSelectionEnd;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of selected dates.
    /// </summary>
    /// <seealso cref="SelectionStart"/>
    /// <seealso cref="SelectionEnd"/>
    /// <seealso cref="Selection"/>
    /// <seealso cref="SelectionStyle"/>
    {$endregion}
    property SelectionCount: Integer read GetSelectionCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the selected dates.
    /// </summary>
    /// <seealso cref="SelectionCount"/>
    /// <seealso cref="SelectionStart"/>
    /// <seealso cref="SelectionEnd"/>
    /// <seealso cref="SelectionStyle"/>
    {$endregion}
    property Selection[Index: Integer]: TDate read GetSelection;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the calendar system used by the control is determined by the
    /// <see cref="Culture"/> property.
    /// </summary>
    /// <seealso cref="CultureCalendar"/>
    /// <seealso cref="Culture"/>
    {$endregion}
    property UsingCultureCalendar: Boolean read GetUsingCultureCalendar;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the way that the control renders digits is determined by the
    /// <see cref="Culture"/> property.
    /// </summary>
    /// <seealso cref="CultureDigits"/>
    /// <seealso cref="Culture"/>
    {$endregion}
    property UsingCultureDigits: Boolean read GetUsingCultureDigits;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TIntlMonthCalendar is a month calendar control that displays a fully localizable
  /// calendar on a form.
  /// </summary>
  /// <remarks>
  /// Use TIntlMonthCalendar to enable users to select dates from a localized calendar.
  ///
  /// TIntlMonthCalendar can use different calendar systems for manipulating and displaying
  /// the dates. The <see cref="CalendarType"/> property is the main property that specifies
  /// which calendar system should be used by the cotnrol. If the <see cref="CalendarType"/>
  /// property is not set, the <see cref="Culture"/> property can do this task. In addition,
  /// the <see cref="Culture"/> property provides the locale specific settings of the control.
  /// If none of these two properties are set, the <see cref="DefaultCalendar"/> global
  /// variable determines the calendar system used by the control.
  ///
  /// TIntlMonthCalendar can be linked to a <see cref="TLocalizer"/> component. In this case,
  /// the linked <see cref="TLocalizer"/> component can provide both the locale specific
  /// settings and the calendar system of the calendar control.
  ///
  /// TIntlMonthCalendar publishes many of the properties, events, and methods of
  /// <see cref="TCustomIntlMonthCalendar"/>, but does not introduce any new behavior.
  /// </remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TIntlMonthCalendar = class(TCustomIntlMonthCalendar)
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which date's component can be selected by the user right now.
    /// </summary>
    {$endregion}
    property CurrentView;
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control aligns within its container (parent control).
    /// </summary>
    {$endregion}
    property Align;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether a control should be constrained by <see cref="Margins"/>.
    /// </summary>
    {$endregion}
    property AlignWithMargins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control is anchored to its parent.
    /// </summary>
    {$endregion}
    property Anchors;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control changes the pages with an animation.
    /// </summary>
    {$endregion}
    property Animate;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control sizes itself automatically to accommodate
    /// its contents.
    /// </summary>
    {$endregion}
    property AutoSize;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which edges of the control are beveled.
    /// </summary>
    {$endregion}
    property BevelEdges;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the cut of the inner bevel.
    /// </summary>
    {$endregion}
    property BevelInner;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the control's bevel style.
    /// </summary>
    {$endregion}
    property BevelKind default bkTile;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the cut of the outer bevel.
    /// </summary>
    {$endregion}
    property BevelOuter;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the width of the control's border.
    /// </summary>
    {$endregion}
    property BorderWidth default 2;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCalendar"/> class that is used for manipulating dates.
    /// </summary>
    /// <seealso cref="Culture"/>
    {$endregion}
    property CalendarType;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the background color of the control.
    /// </summary>
    {$endregion}
    property Color default clWindow;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the color of divider line between calendar's grid and the other parts.
    /// </summary>
    {$endregion}
    property ColorDivider;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the color of marker that distinguishes the today's date.
    /// </summary>
    {$endregion}
    property ColorToday;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the size constraints for the control.
    /// </summary>
    {$endregion}
    property Constraints;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control has a three-dimensional look.
    /// </summary>
    {$endregion}
    property Ctl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCultureInfo"/> object that provides the locale specific
    /// settings of the calendar.
    ///
    /// NOTE: If the <see cref="CalendarType"/> property is <see langword="nil"/>, depends
    /// on the values of <see cref="TLocalizer"/> and <see cref="CultureCalendar"/> properties,
    /// the Culture property may also determine the calendar system of the control.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="CultureCalendar"/>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="CultureDigits"/>
    /// <seealso cref="CalendarType"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property Culture;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar system used by the control should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.
    /// NOTE: Value of this property has no effect when the <see cref="CalendarType"/>
    /// property is not <see langword="nil"/>.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureCalendar;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the way the control renders digits should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.</summary>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureDigits;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the current focused date.
    /// </summary>
    /// <exception cref="EDateTimeError">
    /// Occurs when the assigned date is not in the supported range of the calendar.
    /// </exception>
    /// <seealso cref="IsDateInRange"/>
    {$endregion}
    property Date;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the smallest date that is allowed for the calendar.
    /// </summary>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="DateMax"/>
    {$endregion}
    property DateMin;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the largest date that is allowed for the calendar.
    /// </summary>
    /// <seealso cref="ActualDateMax"/>
    /// <seealso cref="DateMin"/>
    {$endregion}
    property DateMax;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control's image is rendered directly to the window or
    /// painted to an in-memory bitmap first.
    /// </summary>
    {$endregion}
    property DoubleBuffered;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the image used to represent the mouse pointer when it passes
    /// into the region covered by the control.
    /// </summary>
    {$endregion}
    property DragCursor;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control is being dragged normally or for docking.
    /// </summary>
    {$endregion}
    property DragKind;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control initiates drag-and-drop or drag-and-dock operations.
    /// </summary>
    {$endregion}
    property DragMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control responds to keyboard, mouse, and timer events.
    /// </summary>
    {$endregion}
    property Enabled;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the attributes of text written on the control.
    /// </summary>
    {$endregion}
    property Font;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing text on the calendar's header.
    /// </summary>
    {$endregion}
    property FontHeader;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing text on the calendar's footer.
    /// </summary>
    {$endregion}
    property FontFooter;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing name of days on the calendar.
    /// </summary>
    {$endregion}
    property FontDayOfWeek;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing week numbers on the calendar.
    /// </summary>
    {$endregion}
    property FontWeekNumbers;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the text string that can appear when the user moves the mouse over
    /// the control.
    /// </summary>
    {$endregion}
    property Hint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the behavior of the input method editor (IME).
    /// </summary>
    {$endregion}
    property ImeMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the input method editor (IME) to use for converting keyboard input.
    /// </summary>
    {$endregion}
    property ImeName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the
    /// control.
    /// </summary>
    {$endregion}
    property Localizer;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the margins of the control.
    /// </summary>
    {$endregion}
    property Margins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Color"/>.
    /// </summary>
    {$endregion}
    property ParentColor default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Ctl3D"/>.
    /// </summary>
    {$endregion}
    property ParentCtl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="DoubleBuffered"/>.
    /// </summary>
    {$endregion}
    property ParentDoubleBuffered;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Font"/>.
    /// </summary>
    {$endregion}
    property ParentFont;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="ShowHint"/>.
    /// </summary>
    {$endregion}
    property ParentShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the pop-up menu associated with the control.
    /// </summary>
    {$endregion}
    property PopupMenu;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the ways thet a user can select dates of the calendar.
    /// </summary>
    {$endregion}
    property SelectionStyle;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar's header is shown.
    /// </summary>
    {$endregion}
    property ShowHeader;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar's footer is shown.
    /// </summary>
    {$endregion}
    property ShowFooter;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.
    /// </summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the position of the control in its parent's tab order.
    /// </summary>
    {$endregion}
    property TabOrder;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can tab to a control.
    /// </summary>
    {$endregion}
    property TabStop default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the touch manager component associated with the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property Touch;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control appears onscreen.
    /// </summary>
    {$endregion}
    property Visible;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether week numbers are shown on the calendar.
    /// </summary>
    {$endregion}
    property WeekNumbers;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the <see cref="Calendar"/> property changes.
    /// </summary>
    {$endregion}
    property OnCalendarChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user clicks the control.
    /// </summary>
    {$endregion}
    property OnClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user right-clicks the control or otherwise invokes the pop-up
    /// menu (such as using the keyboard).
    /// </summary>
    {$endregion}
    property OnContextPopup;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when text of calendar's header needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawHeaderText;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a day of a month needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawDay;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when name of a month needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawMonth;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a year needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawYear;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when name of a day of the week needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawDayOfWeek;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a week number needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawWeekNumber;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when text of calendar's footer needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawFooterText;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the <see cref="Date"/> property changes.
    /// </summary>
    {$endregion}
    property OnDateChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a day of a month is about to be selected.
    /// NOTE: This event occurs only with a <see cref="SelectionStyle"/> of ssFree.
    /// </summary>
    {$endregion}
    property OnDateSelectable;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse pauses over a day of a month.
    /// </summary>
    {$endregion}
    property OnDateHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user double-clicks the left mouse button when the mouse pointer
    /// is over the control.
    /// </summary>
    {$endregion}
    property OnDblClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user drops an object being dragged on the control.
    /// </summary>
    {$endregion}
    property OnDragDrop;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user drags an object over the control.
    /// </summary>
    {$endregion}
    property OnDragOver;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dragging of an object ends, either by docking the object or
    /// by canceling the dragging.
    /// </summary>
    {$endregion}
    property OnEndDock;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dragging of an object ends, either by dropping the object or
    /// by canceling the dragging.
    /// </summary>
    {$endregion}
    property OnEndDrag;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a control receives the input focus.
    /// </summary>
    {$endregion}
    property OnEnter;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the input focus shifts away from one control to another.
    /// </summary>
    {$endregion}
    property OnExit;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when user performs a gesture associated with the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnGesture;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a user presses any key while the control has focus.
    /// </summary>
    {$endregion}
    property OnKeyDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when key pressed.
    /// </summary>
    {$endregion}
    property OnKeyPress;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user releases a key that has been pressed.
    /// </summary>
    {$endregion}
    property OnKeyUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user presses a mouse button with the mouse pointer over the
    /// control and the parent form is not active.
    /// </summary>
    {$endregion}
    property OnMouseActivate;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user presses a mouse button with the mouse pointer over the
    /// control.
    /// </summary>
    {$endregion}
    property OnMouseDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse pointer while the mouse pointer is over
    /// the control.
    /// </summary>
    {$endregion}
    property OnMouseMove;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user releases a mouse button that was pressed with the mouse
    /// pointer over the control.
    /// </summary>
    {$endregion}
    property OnMouseUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse into the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnMouseEnter;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse outside of the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnMouseLeave;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated.
    /// </summary>
    {$endregion}
    property OnMouseWheel;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated downward.
    /// </summary>
    {$endregion}
    property OnMouseWheelDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated upward.
    /// </summary>
    {$endregion}
    property OnMouseWheelUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the control's page changes.
    /// </summary>
    {$endregion}
    property OnPageChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately after the control is resized.
    /// </summary>
    {$endregion}
    property OnResize;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the <see cref="Selection"/> property changes.
    /// </summary>
    {$endregion}
    property OnSelectionChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user begins to drag the control with a <see cref="DragKind"/>
    /// of <see cref="DragKind.dkDock"/>.
    /// </summary>
    {$endregion}
    property OnStartDock;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user begins to drag the control with a <see cref="DragKind"/>
    /// of <see cref="DragKind.dkDrag"/>.
    /// </summary>
    {$endregion}
    property OnStartDrag;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TPopupCloseUpEvent is the type for event handlers that respond when a popup
  /// window closes.
  /// </summary>
  /// <param name="Sender">
  /// The pop-up window that generated the event.
  /// </param>
  /// <param name="Accept">
  /// Indicates whether the user accepted the pop-up window or canceled it.
  /// </param>
  {$endregion}
  TPopupCloseUpEvent = procedure(Sender: TObject; Accept: Boolean) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TIntlMonthCalendarPopup is a pop-up window for controls that needs the user
  /// select a localized date.
  /// </summary>
  /// <remarks>
  /// Use TIntlMonthCalendarPopup in your own control to enable users to select a
  /// localized date from a pop-up window.
  ///
  /// TIntlMonthCalendarPopup can use different calendar systems for manipulating and
  /// displaying the dates. The <see cref="CalendarType"/> property is the main property
  /// that specifies which calendar system should be used by the cotnrol. If the
  /// <see cref="CalendarType"/> property is not set, the <see cref="Culture"/> property
  /// can do this task. In addition, the <see cref="Culture"/> property provides the
  /// locale specific settings of the control. If none of these two properties are set,
  /// the <see cref="DefaultCalendar"/> global variable determines the calendar system
  /// used by the control.
  /// </remarks>
  {$endregion}
  TIntlMonthCalendarPopup = class(TCustomIntlMonthCalendar)
  private
    fOnCloseUp: TPopupCloseUpEvent;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Initializes the window-creation parameter record.
    /// </summary>
    /// <param name="Params">
    /// The window-creation parameter record.
    /// </param>
    {$endregion}
    procedure CreateParams(var Params: TCreateParams); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user releases a previously pressed mouse button down while the
    /// cursor's hotspot is over the popup.
    /// </summary>
    /// <param name="Button">
    /// Determines which mouse button the user released.
    /// </param>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user released the mouse button.
    /// </param>
    /// <param name="X">
    /// The horizontal pixel coordinate of the mouse pointer within the client area of the popup.
    /// </param>
    /// <param name="Y">
    /// The vertical pixel coordinate of the mouse pointer within the client area of the popup.
    /// </param>
    {$endregion}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key.
    /// </summary>
    /// <param name="Key">
    /// The key that was pressed. If the method changes Key to 0, processing of the keystroke stops.
    /// </param>
    /// <param name="Shift">
    /// Indicates the concurrent state of the Ctrl, Shift, and Alt keys.
    /// </param>
    {$endregion}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnCloseUp"/> event.
    /// </summary>
    /// <param name="Accept">
    /// Indicates whether the currently selected date is acceptable.
    /// </param>
    {$endregion}
    procedure DoCloseUp(Accept: Boolean); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the pop-up hides.
    /// </summary>
    {$endregion}
    property OnCloseUp: TPopupCloseUpEvent read fOnCloseUp write fOnCloseUp;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the popup.
    /// </summary>
    /// <param name="AParent">
    /// The windowed control that owns the popup.
    /// </param>
    {$endregion}
    constructor Create(AOwner: TWinControl); reintroduce;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the pop-up has input focus.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the popup's owner has input focus, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Focused: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Displays the pop-up at a specified coordinates.
    /// </summary>
    /// <param name="X">
    /// The horizontal screen coordinate, in pixels.
    /// </param>
    /// <param name="Y">
    /// The vertical screen coordinate, in pixels.
    /// </param>
    {$endregion}
    procedure Show(X, Y: Integer);
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the horizontal coordinate of the left edge of the pop-up relative to screen.
    /// </summary>
    {$endregion}
    property Left stored False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the vertical coordinate of the top edge of the pop-up relative to screen.
    /// </summary>
    {$endregion}
    property Top stored False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the horizontal size of the pop-up in pixels.
    /// </summary>
    {$endregion}
    property Width stored False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the vertical size of the pop-up in pixels.
    /// </summary>
    {$endregion}
    property Height stored False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control changes the pages with an animation.
    /// </summary>
    {$endregion}
    property Animate;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the background color of the control.
    /// </summary>
    {$endregion}
    property Color default clWindow;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the color of divider line between calendar's grid and the other parts.
    /// </summary>
    {$endregion}
    property ColorDivider;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the color of marker that distinguishes the today's date.
    /// </summary>
    {$endregion}
    property ColorToday;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the attributes of text written on the control.
    /// </summary>
    {$endregion}
    property Font;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing text on the calendar's header.
    /// </summary>
    {$endregion}
    property FontHeader;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing text on the calendar's footer.
    /// </summary>
    {$endregion}
    property FontFooter;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing name of days on the calendar.
    /// </summary>
    {$endregion}
    property FontDayOfWeek;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the font to use when writing week numbers on the calendar.
    /// </summary>
    {$endregion}
    property FontWeekNumbers;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Font"/>.
    /// </summary>
    {$endregion}
    property ParentFont;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="ShowHint"/>.
    /// </summary>
    {$endregion}
    property ParentShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar's header is shown.
    /// </summary>
    {$endregion}
    property ShowHeader;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar's footer is shown.
    /// </summary>
    {$endregion}
    property ShowFooter default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.
    /// </summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether week numbers are shown on the calendar.
    /// </summary>
    {$endregion}
    property WeekNumbers;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when text of calendar's header needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawHeaderText;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a day of a month needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawDay;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when name of a month needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawMonth;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a year needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawYear;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when name of a day of the week needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawDayOfWeek;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a week number needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawWeekNumber;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when text of calendar's footer needs to be displayed.
    /// </summary>
    {$endregion}
    property OnCustomDrawFooterText;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse pauses over a day of a month.
    /// </summary>
    {$endregion}
    property OnDateHint;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the sections of a date picker control
  /// that a mouse pointer can be over it.
  /// </summary>
  {$endregion}
  TIntlPickerHitTest = (
    {$region 'xmldoc'}
    /// Nowhere
    {$endregion}
    phtNowhere,
    {$region 'xmldoc'}
    /// Drop-down button
    {$endregion}
    phtDropDown,
    {$region 'xmldoc'}
    /// Check box
    {$endregion}
    phtCheckBox,
    {$region 'xmldoc'}
    /// Displayed date
    {$endregion}
    phtDate,
    {$region 'xmldoc'}
    /// A non-specific part of the control
    {$endregion}
    phtControl
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomIntlDatePicker is the base class for controls that request a localized
  /// date from the user.
  /// </summary>
  /// <remarks>
  /// Use TCustomIntlDatePicker as a base class when defining objects that enable
  /// users to input a localized date.
  ///
  /// TCustomIntlDatePicker can use different calendar systems for getting the input
  /// date. The <see cref="CalendarType"/> property is the main property that specifies
  /// which calendar system should be used by the cotnrol. If the <see cref="CalendarType"/>
  /// property is not set, the <see cref="Culture"/> property can do this task. In addition,
  /// the <see cref="Culture"/> property provides the locale specific settings of the control.
  /// If none of these two properties are set, the <see cref="DefaultCalendar"/> global
  /// variable determines the calendar system used by the control.
  ///
  /// TCustomIntlDatePicker can be linked to a <see cref="TLocalizer"/> component.
  /// In this case, the linked <see cref="TLocalizer"/> component can provide both the
  /// locale specific settings and the calendar system of the control.
  /// </remarks>
  {$endregion}
  TCustomIntlDatePicker = class(TCustomControl)
  private
    fDate: TDate;
    fDateFormat: String;
    fChecked: Boolean;
    fShowCheckBox: Boolean;
    fBorderStyle: TBorderStyle;
    fCalendarPopup: TIntlMonthCalendarPopup;
    fCheckBoxRect: TRect;
    fDropDownRect: TRect;
    fDateRect: TRect;
    fHotInfo: TIntlPickerHitTest;
    fTheme: HTHEME;
    fCheckPressed: Boolean;
    fOnChange: TNotifyEvent;
    procedure SetDate(const Value: TDate);
    procedure SetDateFormat(const Value: String);
    procedure SetChecked(Value: Boolean);
    procedure SetShowCheckBox(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetDateMin: TDate; inline;
    procedure SetDateMin(const Value: TDate); inline;
    function GetDateMax: TDate; inline;
    procedure SetDateMax(const Value: TDate); inline;
    function GetCalendarType: TCalendarClass; inline;
    procedure SetCalendarType(Value: TCalendarClass); inline;
    function GetCulture: TCultureInfo; inline;
    procedure SetCulture(Value: TCultureInfo); inline;
    function GetCultureCalendar: TLocalizerSwicth; inline;
    procedure SetCultureCalendar(Value: TLocalizerSwicth); inline;
    function GetCultureDigits: TLocalizerSwicth; inline;
    procedure SetCultureDigits(Value: TLocalizerSwicth); inline;
    function GetLocalizer: TLocalizer; inline;
    procedure SetLocalizer(Value: TLocalizer); inline;
    function GetCalendar: TCalendar; inline;
    function GetActualDateMin: TDate; inline;
    function GetActualDateMax: TDate; inline;
    function GetUsingCultureCalendar: Boolean; inline;
    function GetUsingCultureDigits: Boolean; inline;
    function GetTheme: HTHEME;
    function IsDateStored: Boolean;
    function IsDateMinStored: Boolean;
    function IsDateMaxStored: Boolean;
    procedure ReadCulture(Reader: TReader);
    procedure WriteCulture(Writer: TWriter);
    procedure ReadCalendarType(Reader: TReader);
    procedure WriteCalendarType(Writer: TWriter);
    procedure CalendarChanged(Sender: TObject);
    procedure PopupCloseUp(Sender: TObject; Accept: Boolean);
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMEnableChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="Culture"/> and <see cref="CalendarType"/> properties
    /// as if they were published.
    /// </summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.
    /// </param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Initializes the window-creation parameter record.
    /// </summary>
    /// <param name="Params">
    /// The window-creation parameter record.
    /// </param>
    {$endregion}
    procedure CreateParams(var Params: TCreateParams); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Makes any required adjustments when the control changes size.
    /// </summary>
    {$endregion}
    procedure Resize; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Renders the control.
    /// </summary>
    {$endregion}
    procedure Paint; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the control can resize itself automatically to accommodate
    /// its contents.
    /// </summary>
    /// <param name="NewWidth">
    /// Indicates the proposed new width of the control.
    /// </param>
    /// <param name="NewHeight">
    /// Indicates the proposed new height of the control.
    /// </param>
    /// <returns>
    /// Always returns <see langword="true"/>.
    /// </returns>
    {$endregion}
    function CanAutoSize(var NewWidth: Integer; var NewHeight: Integer): Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a mouse button down while the cursor's hotspot is
    /// over the control.
    /// </summary>
    /// <param name="Button">
    /// Determines which mouse button the user pressed.
    /// </param>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user pressed the mouse button.
    /// </param>
    /// <param name="X">
    /// The horizontal pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    /// <param name="Y">
    /// The vertical pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    {$endregion}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user moves the mouse pointer while the cursor's hotspot is
    /// over the control.
    /// </summary>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user moved the mouse pointer.
    /// </param>
    /// <param name="X">
    /// The horizontal pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    /// <param name="Y">
    /// The vertical pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    {$endregion}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user releases a previously pressed mouse button down while the
    /// cursor's hotspot is over the control.
    /// </summary>
    /// <param name="Button">
    /// Determines which mouse button the user released.
    /// </param>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user released the mouse button.
    /// </param>
    /// <param name="X">
    /// The horizontal pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    /// <param name="Y">
    /// The vertical pixel coordinate of the mouse pointer within the client area of the control.
    /// </param>
    {$endregion}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user rotates the mouse wheel downward.
    /// </summary>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user rotated the mouse wheel.
    /// </param>
    /// <param name="MousePos">
    /// The pixel coordinates of the mouse pointer within the screen.
    /// </param>
    {$endregion}
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user rotates the mouse wheel upward.
    /// </summary>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user rotated the mouse wheel.
    /// </param>
    /// <param name="MousePos">
    /// The pixel coordinates of the mouse pointer within the screen.
    /// </param>
    {$endregion}
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key.
    /// </summary>
    /// <param name="Key">
    /// The key that was pressed. If the method changes Key to 0, processing of the keystroke stops.
    /// </param>
    /// <param name="Shift">
    /// Indicates the concurrent state of the Ctrl, Shift, and Alt keys.
    /// </param>
    {$endregion}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether the user can enter a different date in the control.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the user can enter a date, oterwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function CanModify: Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the displayed date on the control.
    /// </summary>
    {$endregion}
    procedure UpdateText; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Recalculates the bounding rectangle of the control's different sections.
    /// </summary>
    {$endregion}
    procedure UpdateRects; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the minimum vertical size of the control, in pixels.
    /// </summary>
    /// <returns>
    /// The minimum vertical size of the control, in pixels.
    /// </returns>
    {$endregion}
    function GetMinHeight: Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines that a point is in which section of the control.
    /// </summary>
    /// <param name="X">
    /// The point's horizontal coordinate in the control's coordinate space.
    /// </param>
    /// <param name="Y">
    /// The point's vertical coordinate in the control's coordinate space.
    /// </param>
    /// <returns>
    /// Returns a <see cref="TIntlPickerHitTest"/> value.
    /// </returns>
    {$endregion}
    function GetHitTestInfoAt(X, Y: Integer): TIntlPickerHitTest; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for the check box of the control.
    /// </summary>
    {$endregion}
    procedure InvalidateCheckBox; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for the drop-down button of the control.
    /// </summary>
    {$endregion}
    procedure InvalidateDropDown; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for the displayed date of the control.
    /// </summary>
    {$endregion}
    procedure InvalidateDate; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Schedules a repaint for the frame of the control.
    /// </summary>
    {$endregion}
    procedure InvalidateFrame; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Displays the pop-up calendar.
    /// </summary>
    /// <seealso cref="CalendarPopup"/>
    {$endregion}
    procedure PopupDropDown; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnChange"/> event.
    /// </summary>
    {$endregion}
    procedure Change; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the handle to the Windows theme that is used for rendering the control.
    /// </summary>
    {$endregion}
    property Theme: HTHEME read GetTheme;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the last mouse hit test information.
    /// </summary>
    {$endregion}
    property HotInfo: TIntlPickerHitTest read fHotInfo;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the user holding the check box pressed.
    /// </summary>
    {$endregion}
    property CheckPressed: Boolean read fCheckPressed;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides access to the pop-up calendar's settings.
    /// </summary>
    {$endregion}
    property CalendarPopup: TIntlMonthCalendarPopup read fCalendarPopup;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the current focused date.
    /// </summary>
    /// <exception cref="EDateTimeError">
    /// Occurs when the assigned date is not in the supported range of the calendar.
    /// </exception>
    /// <seealso cref="IsDateInRange"/>
    {$endregion}
    property Date: TDate read fDate write SetDate stored IsDateStored;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the format string that determines how the <see cref="Date"/> property
    /// should be displayed on the control. See <see cref="TCalendar.Format"/> for details.
    /// </summary>
    /// <seealso cref="TCalendar.Format"/>
    {$endregion}
    property DateFormat: String read fDateFormat write SetDateFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the smallest date that is allowed for the calendar.
    /// </summary>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="DateMax"/>
    {$endregion}
    property DateMin: TDate read GetDateMin write SetDateMin stored IsDateMinStored;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the largest date that is allowed for the calendar.
    /// </summary>
    /// <seealso cref="ActualDateMax"/>
    /// <seealso cref="DateMin"/>
    {$endregion}
    property DateMax: TDate read GetDateMax write SetDateMax stored IsDateMaxStored;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCalendar"/> class that is used for manipulating dates.
    /// </summary>
    /// <seealso cref="Culture"/>
    {$endregion}
    property CalendarType: TCalendarClass read GetCalendarType write SetCalendarType;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCultureInfo"/> object that provides the locale specific
    /// settings of the calendar.
    ///
    /// NOTE: If the <see cref="CalendarType"/> property is <see langword="nil"/>, conditional
    /// upon the values of <see cref="TLocalizer"/> and <see cref="CultureCalendar"/> properties,
    /// the Culture property may also determine the calendar system of the control.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="CultureCalendar"/>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="CultureDigits"/>
    /// <seealso cref="CalendarType"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property Culture: TCultureInfo read GetCulture write SetCulture;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar system used by the control should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.
    /// NOTE: Value of this property has no effect when the <see cref="CalendarType"/>
    /// property is not <see langword="nil"/>.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureCalendar: TLocalizerSwicth read GetCultureCalendar write SetCultureCalendar default lsDefault;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the way the control renders digits should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.</summary>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureDigits: TLocalizerSwicth read GetCultureDigits write SetCultureDigits default lsDefault;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the control.
    /// </summary>
    {$endregion}
    property Localizer: TLocalizer read GetLocalizer write SetLocalizer;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box next to the date is selected. When the
    /// check box is not checked, user cannot enter a date.
    /// </summary>
    {$endregion}
    property Checked: Boolean read fChecked write SetChecked default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether a check box next to the date should be displayed.
    /// </summary>
    {$endregion}
    property ShowCheckBox: Boolean read fShowCheckBox write SetShowCheckBox default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the border style for the control.
    /// </summary>
    {$endregion}
    property BorderStyle: TBorderStyle read fBorderStyle write SetBorderStyle default bsSingle;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when either of <see cref="Date"/> or <see cref="Checked"/> properties change.</summary>
    {$endregion}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the control.
    /// </summary>
    /// <param name="AOwner">
    /// The owner component.
    /// </param>
    {$endregion}
    constructor Create(AOwner: TComponent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the control and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the value of <see cref="Culture"/> property is controlled
    /// by a <see cref="TLocalizer"/> component.
    /// </summary>
    /// <returns>
    /// Rteurns <see langword="true"/> if <see cref="Localizer"/> property is not
    /// <see langword="nil"/>, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="Localizer"/>
    {$endregion}
    function IsManagedByLocalizer: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified year of a specified era is in the supported
    /// and allowed range of the control.
    /// </summary>
    /// <param name="AEra">
    /// The era.
    /// </param>
    /// <param name="AYear">
    /// The year of the era.
    /// </param>
    /// <returns>
    /// Rteurns <see langword="true"/> if at least one day within the specified
    /// year can be represented by the cotnrol, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsMonthInRange"/>
    /// <seealso cref="IsDateInRange"/>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function IsYearInRange(AEra, AYear: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified year is in the supported and allowed range of
    /// the control.
    /// </summary>
    /// <param name="AYear">
    /// The year to examine.
    /// </param>
    /// <returns>
    /// Rteurns <see langword="true"/> if at least one day within the specified
    /// year can be represented by the cotnrol, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsMonthInRange"/>
    /// <seealso cref="IsDateInRange"/>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function IsYearInRange(AYear: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified month of a specified year in a specified era
    /// is in the supported and allowed range of the control.
    /// </summary>
    /// <param name="AEra">
    /// The era.
    /// </param>
    /// <param name="AYear">
    /// The year of the era.
    /// </param>
    /// <param name="AMonth">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// Rteurns <see langword="true"/> if at least one day within the specified
    /// month can be represented by the cotnrol, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsYearInRange"/>
    /// <seealso cref="IsDateInRange"/>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function IsMonthInRange(AEra, AYear, AMonth: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified month of a specified year is in the supported
    /// and allowed range of the control.
    /// </summary>
    /// <param name="AYear">
    /// The year of the era.
    /// </param>
    /// <param name="AMonth">
    /// The month of the year.
    /// </param>
    /// <returns>
    /// Rteurns <see langword="true"/> if at least one day within the specified
    /// month can be represented by the cotnrol, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsYearInRange"/>
    /// <seealso cref="IsDateInRange"/>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function IsMonthInRange(AYear, AMonth: Integer): Boolean; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified date is in the supported and allowed range
    /// of the control.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value to examine.
    /// </param>
    /// <returns>
    /// Rteurns <see langword="true"/> if the date can be represented by the cotnrol,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="IsYearInRange"/>
    /// <seealso cref="IsMonthInRange"/>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function IsDateInRange(const ADate: TDate): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Confines a specified date to be in the supported and allowed range
    /// of the control.
    /// </summary>
    /// <param name="ADate">
    /// The <see cref="TDate"/> value to confine.
    /// </param>
    /// <returns>
    /// If the date is less than <see cref="ActualDateMin"/>, returns <see cref="ActualDateMin"/>.
    /// If the date is greater than <see cref="ActualDateMax"/>, returns <see cref="ActualDateMax"/>.
    /// If the date is between <see cref="ActualDateMax"/> and <see cref="ActualDateMax"/>, returns
    /// the date as is.
    /// </returns>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    function EnsureDateInRange(const ADate: TDate): TDate; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TCalendar"/> object that the control is using to
    /// get and display the input date.
    /// </summary>
    {$endregion}
    property Calendar: TCalendar read GetCalendar;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the smallest date that is supported by the <see cref="Calendar"/> and
    /// is allowed by the control.
    /// </summary>
    /// <seealso cref="DateMin"/>
    /// <seealso cref="TCalendar.MinSupportedDateTime"/>
    /// <seealso cref="ActualDateMax"/>
    {$endregion}
    property ActualDateMin: TDate read GetActualDateMin;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the largest date that is supported by the <see cref="Calendar"/> and
    /// is allowed by the control.
    /// </summary>
    /// <seealso cref="DateMax"/>
    /// <seealso cref="TCalendar.MaxSupportedDateTime"/>
    /// <seealso cref="ActualDateMin"/>
    {$endregion}
    property ActualDateMax: TDate read GetActualDateMax;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the calendar system used by the control is determined by the
    /// <see cref="Culture"/> property.
    /// </summary>
    /// <seealso cref="CultureCalendar"/>
    /// <seealso cref="Culture"/>
    {$endregion}
    property UsingCultureCalendar: Boolean read GetUsingCultureCalendar;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the way that the control renders digits is determined by the
    /// <see cref="Culture"/> property.
    /// </summary>
    /// <seealso cref="CultureDigits"/>
    /// <seealso cref="Culture"/>
    {$endregion}
    property UsingCultureDigits: Boolean read GetUsingCultureDigits;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TIntlDatePicker is a control designed specifically for entering localized dates.
  /// </summary>
  /// <remarks>
  /// Use TIntlDatePicker to enables users to eneter a date in a custom calendar system
  /// and in a locale specific format.
  ///
  /// TIntlDatePicker can use different calendar systems for getting the input  date.
  /// The <see cref="CalendarType"/> property is the main property that specifies which
  /// calendar system should be used by the cotnrol. If the <see cref="CalendarType"/>
  /// property is not set, the <see cref="Culture"/> property can do this task. In addition,
  /// the <see cref="Culture"/> property provides the locale specific settings of the control.
  /// If none of these two properties are set, the <see cref="DefaultCalendar"/> global
  /// variable determines the calendar system used by the control.
  ///
  /// TIntlDatePicker can be linked to a <see cref="TLocalizer"/> component. In this case,
  /// the linked <see cref="TLocalizer"/> component can provide both the locale specific
  /// settings and the calendar system of the control.
  ///
  /// TIntlDatePicker publishes many of the properties, events, and methods of
  /// <see cref="TCustomIntlDatePicker"/>, but does not introduce any new behavior.
  /// </remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TIntlDatePicker = class(TCustomIntlDatePicker)
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control aligns within its container (parent control).
    /// </summary>
    {$endregion}
    property Align;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether a control should be constrained by <see cref="Margins"/>.
    /// </summary>
    {$endregion}
    property AlignWithMargins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control is anchored to its parent.
    /// </summary>
    {$endregion}
    property Anchors;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control sizes itself automatically to accommodate
    /// its contents.
    /// </summary>
    {$endregion}
    property AutoSize default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which edges of the control are beveled.
    /// </summary>
    {$endregion}
    property BevelEdges;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the cut of the inner bevel.
    /// </summary>
    {$endregion}
    property BevelInner;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the control's bevel style.
    /// </summary>
    {$endregion}
    property BevelKind default bkNone;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the cut of the outer bevel.
    /// </summary>
    {$endregion}
    property BevelOuter;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the bidirectional mode for the control.
    /// </summary>
    {$endregion}
    property BiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the border style for the control.
    /// </summary>
    {$endregion}
    property BorderStyle;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides access to the pop-up calendar's settings.
    /// </summary>
    {$endregion}
    property CalendarPopup;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCalendar"/> class that is used for manipulating dates.
    /// </summary>
    /// <seealso cref="Culture"/>
    {$endregion}
    property CalendarType;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box next to the date is selected. When the
    /// check box is not checked, user cannot enter a date.
    /// </summary>
    {$endregion}
    property Checked;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the background color of the control.
    /// </summary>
    {$endregion}
    property Color default clWindow;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the size constraints for the control.
    /// </summary>
    {$endregion}
    property Constraints;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control has a three-dimensional look.
    /// </summary>
    {$endregion}
    property Ctl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCultureInfo"/> object that provides the locale specific
    /// settings of the calendar.
    ///
    /// NOTE: If the <see cref="CalendarType"/> property is <see langword="nil"/>, depends
    /// on the values of <see cref="TLocalizer"/> and <see cref="CultureCalendar"/> properties,
    /// the Culture property may also determine the calendar system of the control.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="CultureCalendar"/>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="CultureDigits"/>
    /// <seealso cref="CalendarType"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property Culture;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the calendar system used by the control should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.
    /// NOTE: Value of this property has no effect when the <see cref="CalendarType"/>
    /// property is not <see langword="nil"/>.
    /// </summary>
    /// <seealso cref="UsingCultureCalendar"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureCalendar;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the way the control renders digits should be determined
    /// by the <see cref="Culture"/> or <see cref="Localizer"/> properties.</summary>
    /// <seealso cref="UsingCultureDigits"/>
    /// <seealso cref="Culture"/>
    /// <seealso cref="Localizer"/>
    {$endregion}
    property CultureDigits;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the current focused date.
    /// </summary>
    /// <exception cref="EDateTimeError">
    /// Occurs when the assigned date is not in the supported range of the calendar.
    /// </exception>
    /// <seealso cref="IsDateInRange"/>
    {$endregion}
    property Date;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the format string that determines how the <see cref="Date"/> property
    /// should be displayed on the control. See <see cref="TCalendar.Format"/> for details.
    /// </summary>
    /// <seealso cref="TCalendar.Format"/>
    {$endregion}
    property DateFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the smallest date that is allowed for the calendar.
    /// </summary>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="DateMax"/>
    {$endregion}
    property DateMin;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the largest date that is allowed for the calendar.
    /// </summary>
    /// <seealso cref="ActualDateMax"/>
    /// <seealso cref="DateMin"/>
    {$endregion}
    property DateMax;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control's image is rendered directly to the window or
    /// painted to an in-memory bitmap first.
    /// </summary>
    {$endregion}
    property DoubleBuffered;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the image used to represent the mouse pointer when it passes
    /// into the region covered by the control.
    /// </summary>
    {$endregion}
    property DragCursor;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control is being dragged normally or for docking.
    /// </summary>
    {$endregion}
    property DragKind;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control initiates drag-and-drop or drag-and-dock operations.
    /// </summary>
    {$endregion}
    property DragMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control responds to keyboard, mouse, and timer events.
    /// </summary>
    {$endregion}
    property Enabled;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the attributes of text written on the control.
    /// </summary>
    {$endregion}
    property Font;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the text string that can appear when the user moves the mouse over
    /// the control.
    /// </summary>
    {$endregion}
    property Hint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the behavior of the input method editor (IME).
    /// </summary>
    {$endregion}
    property ImeMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the input method editor (IME) to use for converting keyboard input.
    /// </summary>
    {$endregion}
    property ImeName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the
    /// control.
    /// </summary>
    {$endregion}
    property Localizer;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the margins of the control.
    /// </summary>
    {$endregion}
    property Margins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="BiDiMode"/>.
    /// </summary>
    {$endregion}
    property ParentBiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Color"/>.
    /// </summary>
    {$endregion}
    property ParentColor default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Ctl3D"/>.
    /// </summary>
    {$endregion}
    property ParentCtl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="DoubleBuffered"/>.
    /// </summary>
    {$endregion}
    property ParentDoubleBuffered;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Font"/>.
    /// </summary>
    {$endregion}
    property ParentFont;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="ShowHint"/>.
    /// </summary>
    {$endregion}
    property ParentShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the pop-up menu associated with the control.
    /// </summary>
    {$endregion}
    property PopupMenu;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether a check box next to the date should be displayed.
    /// </summary>
    {$endregion}
    property ShowCheckBox;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.
    /// </summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the position of the control in its parent's tab order.
    /// </summary>
    {$endregion}
    property TabOrder;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can tab to a control.
    /// </summary>
    {$endregion}
    property TabStop default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the touch manager component associated with the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property Touch;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control appears onscreen.
    /// </summary>
    {$endregion}
    property Visible;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when either of <see cref="Date"/> or <see cref="Checked"/> properties change.</summary>
    {$endregion}
    property OnChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user clicks the control.
    /// </summary>
    {$endregion}
    property OnClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user right-clicks the control or otherwise invokes the pop-up
    /// menu (such as using the keyboard).
    /// </summary>
    {$endregion}
    property OnContextPopup;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user double-clicks the left mouse button when the mouse pointer
    /// is over the control.
    /// </summary>
    {$endregion}
    property OnDblClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user drops an object being dragged on the control.
    /// </summary>
    {$endregion}
    property OnDragDrop;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user drags an object over the control.
    /// </summary>
    {$endregion}
    property OnDragOver;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dragging of an object ends, either by docking the object or
    /// by canceling the dragging.
    /// </summary>
    {$endregion}
    property OnEndDock;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dragging of an object ends, either by dropping the object or
    /// by canceling the dragging.
    /// </summary>
    {$endregion}
    property OnEndDrag;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a control receives the input focus.
    /// </summary>
    {$endregion}
    property OnEnter;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the input focus shifts away from one control to another.
    /// </summary>
    {$endregion}
    property OnExit;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when user performs a gesture associated with the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnGesture;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a user presses any key while the control has focus.
    /// </summary>
    {$endregion}
    property OnKeyDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when key pressed.
    /// </summary>
    {$endregion}
    property OnKeyPress;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user releases a key that has been pressed.
    /// </summary>
    {$endregion}
    property OnKeyUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user presses a mouse button with the mouse pointer over the
    /// control and the parent form is not active.
    /// </summary>
    {$endregion}
    property OnMouseActivate;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user presses a mouse button with the mouse pointer over the
    /// control.
    /// </summary>
    {$endregion}
    property OnMouseDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse pointer while the mouse pointer is over
    /// the control.
    /// </summary>
    {$endregion}
    property OnMouseMove;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user releases a mouse button that was pressed with the mouse
    /// pointer over the control.
    /// </summary>
    {$endregion}
    property OnMouseUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse into the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnMouseEnter;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse outside of the control.
    /// </summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnMouseLeave;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated.
    /// </summary>
    {$endregion}
    property OnMouseWheel;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated downward.
    /// </summary>
    {$endregion}
    property OnMouseWheelDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated upward.
    /// </summary>
    {$endregion}
    property OnMouseWheelUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately after the control is resized.
    /// </summary>
    {$endregion}
    property OnResize;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user begins to drag the control with a <see cref="DragKind"/>
    /// of <see cref="DragKind.dkDock"/>.
    /// </summary>
    {$endregion}
    property OnStartDock;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user begins to drag the control with a <see cref="DragKind"/>
    /// of <see cref="DragKind.dkDrag"/>.
    /// </summary>
    {$endregion}
    property OnStartDrag;
  end;

implementation

uses
  RTLConsts, Math, Types, Variants, Themes, ComCtrls, i18nUnicode;

resourcestring
  SDateOutOfRangeError = 'The date must be between %s and %s';

type
  TControlHack = class(TControl);

{ Helper Functions }

procedure SwapScreenByScroll(DC: HDC; const Rect: TRect; OfsX, OfsY: Integer;
  OldBmp, NewBmp: TBitmap; RightToLeft: Boolean; Duration: Cardinal);
var
  Step, LastStep, Distance, X: Integer;
  StartTime: Cardinal;
begin
  StartTime := GetTickCount;
  Distance := Rect.Right - Rect.Left;
  Step := 0;
  repeat
    LastStep := Step;
    repeat
      Sleep(1);
      Step := MulDiv(Distance, GetTickCount - StartTime, Duration);
    until Step <> LastStep;
    X := Min(Rect.Left + Step, Rect.Right);
    if RightToLeft then
    begin
      BitBlt(DC, Rect.Left, Rect.Top, X - Rect.Left, Rect.Bottom - Rect.Top,
        NewBmp.Canvas.Handle, Rect.Right - X - OfsX, Rect.Top - OfsY, SRCCOPY);
      BitBlt(DC, X, Rect.Top, Rect.Right - X, Rect.Bottom - Rect.Top,
        OldBmp.Canvas.Handle, Rect.Left - OfsX, Rect.Top - OfsY, SRCCOPY);
    end
    else
    begin
      BitBlt(DC, Rect.Left, Rect.Top, Rect.Right - X, Rect.Bottom - Rect.Top,
        OldBmp.Canvas.Handle, X - OfsX, Rect.Top - OfsY, SRCCOPY);
      BitBlt(DC, Rect.Right - X, Rect.Top, X - Rect.Left, Rect.Bottom - Rect.Top,
        NewBmp.Canvas.Handle, Rect.Left - OfsX, Rect.Top - OfsY, SRCCOPY);
    end;
  until X = Rect.Right;
end;

procedure SwapScreenByZoom(DC: HDC; const Rect: TRect; OfsX, OfsY: Integer;
  OldBmp, NewBmp: TBitmap; const OldRect, NewRect: TRect; Duration: Cardinal);
var
  Progress, LastProgress: Integer;
  StartTime: Cardinal;
  R: TRect;
begin
  StartTime := GetTickCount;
  Progress := 0;
  repeat
    LastProgress := Progress;
    repeat
      Sleep(1);
      Progress := MulDiv(200, GetTickCount - StartTime, Duration);
    until Progress <> LastProgress;
    if Progress > 100 then
      Progress := 100;
    R.Left := OldRect.Left + MulDiv(Rect.Left - OldRect.Left, 100 - Progress, 100);
    R.Top := OldRect.Top + MulDiv(Rect.Top - OldRect.Top, 100 - Progress, 100);
    R.Right := OldRect.Right + MulDiv(Rect.Right - OldRect.Right, 100 - Progress, 100);
    R.Bottom := OldRect.Bottom + MulDiv(Rect.Bottom - OldRect.Bottom, 100 - Progress, 100);
    StretchBlt(DC, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
      OldBmp.Canvas.Handle, R.Left - OfsX, R.Top - OfsY, R.Right - R.Left, R.Bottom - R.Top, SRCCOPY);
  until Progress = 100;
  StartTime := GetTickCount;
  Progress := 0;
  repeat
    LastProgress := Progress;
    repeat
      Sleep(1);
      Progress := MulDiv(200, GetTickCount - StartTime, Duration);
    until Progress <> LastProgress;
    if Progress > 100 then
      Progress := 100;
    R.Left := NewRect.Left + MulDiv(Rect.Left - NewRect.Left, Progress, 100);
    R.Top := NewRect.Top + MulDiv(Rect.Top - NewRect.Top, Progress, 100);
    R.Right := NewRect.Right + MulDiv(Rect.Right - NewRect.Right, Progress, 100);
    R.Bottom := NewRect.Bottom + MulDiv(Rect.Bottom - NewRect.Bottom, Progress, 100);
    StretchBlt(DC, Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
      NewBmp.Canvas.Handle, R.Left - OfsX, R.Top - OfsY, R.Right - R.Left, R.Bottom - R.Top, SRCCOPY);
  until Progress = 100;
end;

{ TDateTimeList }

destructor TDateTimeList.Destroy;
begin
  Capacity := 0;
  inherited Destroy;
end;

function TDateTimeList.GetCapacity: Integer;
begin
  Result := Length(fList);
end;

procedure TDateTimeList.SetCapacity(Value: Integer);
begin
  if Capacity <> Value then
  begin
    SetLength(fList, Value);
    if Capacity < Count then
    begin
      Changing;
      fCount := Capacity;
      Change;
    end;
  end;
end;

function TDateTimeList.GetItems(Index: Integer): TDate;
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := fList[Index];
end;

procedure TDateTimeList.Changing;
begin
  if (fUpdateCount = 0) and Assigned(OnChanging) then
    OnChanging(Self);
end;

procedure TDateTimeList.Change;
begin
  if (fUpdateCount = 0) and Assigned(OnChange) then
    OnChange(Self);
end;

procedure TDateTimeList.Assign(Source: TPersistent);
begin
  if Source is TDateTimeList then
  begin
    Changing;
    fList := Copy(TDateTimeList(Source).fList, 0, TDateTimeList(Source).fCount);
    fCount := TDateTimeList(Source).fCount;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TDateTimeList.BeginUpdate;
begin
  if fUpdateCount = 0 then
    Changing;
  Inc(fUpdateCount);
end;

procedure TDateTimeList.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then
    Change;
end;

procedure TDateTimeList.Clear;
begin
  if Count <> 0 then
  begin
    Changing;
    fCount := 0;
    Change;
  end;
end;

function TDateTimeList.Add(const ADateTime: TDateTime): Integer;
var
  Index: Integer;
begin
  if not Find(ADateTime, Index) then
  begin
    Changing;
    if Capacity = Count then
      Capacity := Count + 4;
    if Index <> Count then
      Move(fList[Index], fList[Index + 1], (Count - Index) * SizeOf(TDateTime));
    fList[Index] := ADateTime;
    Inc(fCount);
    Change;
  end;
  Result := Index;
end;

function TDateTimeList.Remove(const ADateTime: TDateTime): Integer;
var
  Index: Integer;
begin
  Result := -1;
  if Find(ADateTime, Index) then
    Delete(Index);
end;

procedure TDateTimeList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Changing;
  Dec(fCount);
  if Index <> Count then
    Move(fList[Index + 1], fList[Index], (Count - Index) * SizeOf(TDateTime));
  Change;
end;

function TDateTimeList.Exists(const ADateTime: TDateTime): Boolean;
var
  Index: Integer;
begin
  Result := Find(ADateTime, Index);
end;

function TDateTimeList.IndexOf(const ADateTime: TDateTime): Integer;
begin
  if not Find(ADateTime, Result) then
    Result := -1;
end;

function TDateTimeList.Find(const ADateTime: TDateTime; out Index: Integer): Boolean;
var
  L, H, M: Integer;
  C: Integer;
begin
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    M := (L + H) shr 1;
    C := TCalendar.Compare(fList[M], ADateTime);
    if  C < 0 then
      L := M + 1
    else
    begin
      H := M - 1;
      if C = 0 then
      begin
        Result := True;
        Index := M;
        Exit;
      end;
    end;
  end;
  Index := L;
  Result := False;
end;

function TDateTimeList.First: TDateTime;
begin
  if Count <> 0 then
    Result := Items[0]
  else
    Result := TCalendar.NoDate;
end;

function TDateTimeList.Last: TDateTime;
begin
  if Count <> 0 then
    Result := Items[Count - 1]
  else
    Result := TCalendar.NoDate;
end;

{ TCustomIntlDateTimeLabel }

constructor TCustomIntlDateTimeLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  fCalendar := DefaultCalendar.Clone;
  fCalendar.OnChange := CalendarChanged;
  fDateTime := TCalendar.Today;
  ShowAccelChar := False;
end;

destructor TCustomIntlDateTimeLabel.Destroy;
begin
  Localizer := nil;
  fCalendar.Free;
  inherited Destroy;
end;

procedure TCustomIntlDateTimeLabel.SetDateTime(const Value: TDateTime);
begin
  if not TCalendar.IsSame(DateTime, Value) then
  begin
    fDateTime := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomIntlDateTimeLabel.SetDateTimeFormat(const Value: String);
begin
  if DateTimeFormat <> Value then
  begin
    fDateTimeFormat := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomIntlDateTimeLabel.SetCalendarType(Value: TCalendarClass);
begin
  if CalendarType <> Value then
  begin
    fCalendarType := Value;
    ValidateCalendar;
  end;
end;

procedure TCustomIntlDateTimeLabel.SetCulture(Value: TCultureInfo);
begin
  if Culture <> Value then
  begin
    fCulture := Value;
    ValidateCalendar;
  end;
end;

procedure TCustomIntlDateTimeLabel.SetCultureCalendar(Value: TLocalizerSwicth);
begin
  if CultureCalendar <> Value then
  begin
    fCultureCalendar := Value;
    if Assigned(Culture) then
      ValidateCalendar;
  end;
end;

procedure TCustomIntlDateTimeLabel.SetCultureDigits(Value: TLocalizerSwicth);
begin
  if CultureDigits <> Value then
  begin
    fCultureDigits := Value;
    if Assigned(Culture) then
    begin
      AdjustBounds;
      Invalidate;
    end;
  end;
end;

procedure TCustomIntlDateTimeLabel.SetLocalizer(Value: TLocalizer);
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
    if not (csDestroying in ComponentState) then
      LocalizerNotify(Localizer, lnCultureChanged);
  end;
end;

function TCustomIntlDateTimeLabel.GetUsingCultureCalendar: Boolean;
begin
  Result := (CultureCalendar <> lsNever) and Assigned(Culture) and not Assigned(CalendarType);
  if Result and Assigned(Localizer) and (CultureCalendar = lsDefault) then
    Result := (loUseNativeCalendar in Localizer.Options)
end;

function TCustomIntlDateTimeLabel.GetUsingCultureDigits: Boolean;
begin
  Result := (CultureDigits <> lsNever) and Assigned(Culture) and not Culture.IsUsingNominalDigits;
  if Result and Assigned(Localizer) and (CultureDigits = lsDefault) then
    Result := (loUseNativeDigits in Localizer.Options)
end;

function TCustomIntlDateTimeLabel.IsDateTimeStored: Boolean;
begin
  Result := not TCalendar.IsToday(DateTime);
end;

procedure TCustomIntlDateTimeLabel.CalendarChanged(Sender: TObject);
begin
  AdjustBounds;
  Invalidate;
end;

procedure TCustomIntlDateTimeLabel.ReadCulture(Reader: TReader);
begin
  Culture := CultureOf(Reader.ReadString);
end;

procedure TCustomIntlDateTimeLabel.WriteCulture(Writer: TWriter);
begin
  Writer.WriteString(Culture.Locale);
end;

procedure TCustomIntlDateTimeLabel.ReadCalendarType(Reader: TReader);
begin
  CalendarType := CalendarTypes.ByName(Reader.ReadString);
end;

procedure TCustomIntlDateTimeLabel.WriteCalendarType(Writer: TWriter);
begin
  Writer.WriteString(CalendarType.CalendarName);
end;

procedure TCustomIntlDateTimeLabel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('CultureInfo', ReadCulture, WriteCulture,
    not Assigned(Localizer) and Assigned(Culture));
  Filer.DefineProperty('CalendarClass', ReadCalendarType, WriteCalendarType,
    Assigned(CalendarType));
end;

procedure TCustomIntlDateTimeLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Localizer) then
    Localizer := nil;
end;

procedure TCustomIntlDateTimeLabel.LocalizerNotify(Sender: TLocalizer;
  Reason: TLocalizerNotification);
begin
  case Reason of
    lnCultureChanged:
      if Assigned(Localizer) then
        Culture := Localizer.Culture
      else
        Culture := nil;
    lnOptionsChanged:
      ValidateCalendar;
  end;
end;

function TCustomIntlDateTimeLabel.GetLabelText: string;
begin
  Result := Calendar.Format(DateTimeFormat, DateTime);
  if Assigned(Culture) then
    Result := Culture.FreezeDigits(Result, UsingCultureDigits);
end;

function TCustomIntlDateTimeLabel.IsManagedByLocalizer: Boolean;
begin
  Result := Assigned(Localizer);
end;

procedure TCustomIntlDateTimeLabel.ValidateCalendar;

  procedure InitNewCalendar;
  begin
    fCalendar.OnChange := CalendarChanged;
    CalendarChanged(nil);
  end;

var
  Reference: TCalendar;
begin
  if not Assigned(CalendarType) then
  begin
    if UsingCultureCalendar then
      Reference := Culture.NativeCalendar
    else
      Reference := DefaultCalendar;
    if Reference.ClassType <> Calendar.ClassType then
    begin
      FreeAndNil(fCalendar);
      fCalendar := Reference.Clone;
      InitNewCalendar;
    end
    else
      fCalendar.Assign(Reference);
  end
  else if CalendarType <> Calendar.ClassType then
  begin
    FreeAndNil(fCalendar);
    if Assigned(Culture) then
      fCalendar := CalendarType.Create(Culture.Locale)
    else
      fCalendar := CalendarType.Create(LOCALE_USER_DEFAULT);
    InitNewCalendar;
  end
  else if Assigned(Culture) then
    fCalendar.Settings.Prepare(Culture.Locale)
  else
    fCalendar.Settings.Prepare(LOCALE_USER_DEFAULT);
end;

{ TCustomIntlMonthCalendar }

constructor TCustomIntlMonthCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque] - [csSetCaption];
  fCalendar := DefaultCalendar.Clone;
  fCalendar.OnChange := CalendarChanged;
  fSelection := TDateTimeList.Create;
  fFontHeader := TFont.Create;
  fFontHeader.OnChange := FontHeaderChanged;
  fFontFooter := TFont.Create;
  fFontFooter.OnChange := FontFooterChanged;
  fFontDayOfWeek := TFont.Create;
  fFontDayOfWeek.OnChange := FontDayOfWeekChanged;
  fFontWeekNumbers := TFont.Create;
  fFontWeekNumbers.OnChange := FontWeekNumbersChanged;
  fDate := TCalendar.Today;
  fDateMin := TCalendar.NoDate;
  fDateMax := TCalendar.NoDate;
  fColorDivider := clBtnFace;
  fColorToday := clRed;
  fShowHeader := True;
  fShowFooter := True;
  fAnimate := True;
  fHintDate := Calendar.NoDate;
  fDraggingCell := -1;
  fHotCell := -1;
  ParentColor := False;
  Color := clWindow;
  BevelKind := bkTile;
  BorderWidth := 2;
  TabStop := True;
  UpdateCalendar;
  Width := 148;
  Height := 133;
end;

destructor TCustomIntlMonthCalendar.Destroy;
begin
  Localizer := nil;
  fSelection.Free;
  fFontHeader.Free;
  fFontFooter.Free;
  fFontDayOfWeek.Free;
  fFontWeekNumbers.Free;
  fCalendar.Free;
  if Assigned(fSavedScreen) then
    fSavedScreen.Free;
  if fTheme <> 0 then
    CloseThemeData(fTheme);
  inherited Destroy;
end;

procedure TCustomIntlMonthCalendar.SetDate(const Value: TDate);
begin
  if (csDesigning in ComponentState) and Calendar.IsNoDate(Value) then
  begin
    Select(Calendar.Today);
    Exit;
  end;
  if not IsDateInRange(Value) then
  begin
    raise EDateTimeError.CreateResFmt(@SDateOutOfRangeError,
      [Calendar.Format('ddddd', ActualDateMin),
       Calendar.Format('ddddd', ActualDateMax)]);
  end;
  if csLoading in ComponentState then
  begin
    fDate := Trunc(Value);
    fSelectionAnchor := fDate;
  end
  else
    Select(Value);
end;

procedure TCustomIntlMonthCalendar.SetDateMin(const Value: TDate);
begin
  if Calendar.IsNoDate(Value) then
    fDateMin := Calendar.NoDate
  else
  begin
    fDateMin := Trunc(Value);
    if not Calendar.IsNoDate(DateMax) and
      (Calendar.CompareDate(DateMin, DateMax) > 0)
    then
      fDateMin := fDateMax;
  end;
  UpdateMinMaxRanges;
end;

procedure TCustomIntlMonthCalendar.SetDateMax(const Value: TDate);
begin
  if Calendar.IsNoDate(Value) then
    fDateMax := Calendar.NoDate
  else
  begin
    fDateMax := Trunc(Value);
    if not Calendar.IsNoDate(DateMin) and
      (Calendar.CompareDate(DateMin, DateMax) > 0)
    then
      fDateMax := fDateMin;
  end;
  UpdateMinMaxRanges;
end;

procedure TCustomIntlMonthCalendar.SetCalendarType(Value: TCalendarClass);
begin
  if CalendarType <> Value then
  begin
    fCalendarType := Value;
    ValidateCalendar;
  end;
end;

procedure TCustomIntlMonthCalendar.SetCulture(Value: TCultureInfo);
begin
  if Culture <> Value then
  begin
    fCulture := Value;
    ValidateCalendar;
  end;
end;

procedure TCustomIntlMonthCalendar.SetCultureCalendar(Value: TLocalizerSwicth);
begin
  if CultureCalendar <> Value then
  begin
    fCultureCalendar := Value;
    if Assigned(Culture) then
      ValidateCalendar;
  end;
end;

procedure TCustomIntlMonthCalendar.SetCultureDigits(Value: TLocalizerSwicth);
begin
  if CultureDigits <> Value then
  begin
    fCultureDigits := Value;
    if Assigned(Culture) then
      UpdateMetrics;
  end;
end;

procedure TCustomIntlMonthCalendar.SetWeekNumbers(Value: Boolean);
begin
  if WeekNumbers <> Value then
  begin
    fWeekNumbers := Value;
    UpdateMetrics;
  end;
end;

procedure TCustomIntlMonthCalendar.SetSelectionStyle(Value: TIntlCalSelectionStyle);
var
  OldStyle: TIntlCalSelectionStyle;
  TheDate: TDate;
  I: Integer;
begin
  if SelectionStyle <> Value then
  begin
    OldStyle := SelectionStyle;
    fSelectionStyle := Value;
    if not (csLoading in ComponentState) then
    begin
      case OldStyle of
        ssSingle:
        begin
          if SelectionStyle = ssFree then
            fSelection.Add(Date);
          fSelectionAnchor := Date;
        end;
        ssRange:
        begin
          if SelectionStyle = ssFree then
          begin
            TheDate := Min(Date, fSelectionAnchor);
            for I := 0 to Calendar.DaysBetween(Date, fSelectionAnchor) do
              fSelection.Add(Calendar.NextDay(TheDate, I));
          end
          else
            InvalidateDateRange(fSelectionAnchor, Date);
          fSelectionAnchor := Date;
        end;
        ssFree:
        begin
          if (SelectionStyle <> ssRange) or (fSelection.Count = 0) then
            fSelectionAnchor := Date
          else if fSelection.First = Date then
            fSelectionAnchor := fSelection.Last
          else if fSelection.Last = Date then
            fSelectionAnchor := fSelection.First
          else
          begin
            fSelectionAnchor := fSelection.First;
            FocusDate(fSelection.Last);
          end;
          fSelection.Clear;
          InvalidateDateRange(fSelectionAnchor, Date);
        end;
      end;
    end
  end;
end;

procedure TCustomIntlMonthCalendar.SetShowHeader(Value: Boolean);
begin
  if ShowHeader <> Value then
  begin
    fShowHeader := Value;
    UpdateMetrics;
  end;
end;

procedure TCustomIntlMonthCalendar.SetShowFooter(Value: Boolean);
begin
  if ShowFooter <> Value then
  begin
    fShowFooter := Value;
    UpdateMetrics;
  end;
end;

procedure TCustomIntlMonthCalendar.SetColorDivider(Value: TColor);
begin
  if ColorDivider <> Value then
  begin
    fColorDivider := Value;
    InvalidateSections([csDaysOfWeek, csWeekNumbers]);
  end;
end;

procedure TCustomIntlMonthCalendar.SetColorToday(Value: TColor);
begin
  if ColorToday <> Value then
  begin
    fColorToday := Value;
    InvalidateDate(Calendar.Today);
    InvalidateSections([csFooter]);
  end;
end;

procedure TCustomIntlMonthCalendar.SetFontHeader(Value: TFont);
begin
  fFontHeader.Assign(Value);
end;

procedure TCustomIntlMonthCalendar.SetFontFooter(Value: TFont);
begin
  fFontFooter.Assign(Value);
end;

procedure TCustomIntlMonthCalendar.SetFontDayOfWeek(Value: TFont);
begin
  fFontDayOfWeek.Assign(Value);
end;

procedure TCustomIntlMonthCalendar.SetFontWeekNumbers(Value: TFont);
begin
  fFontWeekNumbers.Assign(Value);
end;

procedure TCustomIntlMonthCalendar.SetCurrentView(Value: TIntlCalViewMode);
begin
  if CurrentView <> Value then
  begin
    if not (csLoading in ComponentState) then
    begin
      fHintDate := Calendar.NoDate;
      fDraggingCell := -1;
      fHotCell := -1;
      fHotInfo := chtNowhere;
      AnimateBegin;
      try
        fCurrentView := Value;
        CalcMetrics(CurrentView, fMetrics);
        CalcMetricsSections(fMetrics);
        AnimatePlay;
      finally
        AnimateEnd;
      end;
      DoPageChange;
      Invalidate;
    end
    else
      fCurrentView := Value;
  end;
end;

procedure TCustomIntlMonthCalendar.SetLocalizer(Value: TLocalizer);
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
    if not (csDestroying in ComponentState) then
      LocalizerNotify(Localizer, lnCultureChanged);
  end;
end;

function TCustomIntlMonthCalendar.GetUsingCultureCalendar: Boolean;
begin
  Result := (CultureCalendar <> lsNever) and Assigned(Culture) and not Assigned(CalendarType);
  if Result and Assigned(Localizer) and (CultureCalendar = lsDefault) then
    Result := (loUseNativeCalendar in Localizer.Options)
end;

function TCustomIntlMonthCalendar.GetUsingCultureDigits: Boolean;
begin
  Result := (CultureDigits <> lsNever) and Assigned(Culture) and not Culture.IsUsingNominalDigits;
  if Result and Assigned(Localizer) and (CultureDigits = lsDefault) then
    Result := (loUseNativeDigits in Localizer.Options)
end;

procedure TCustomIntlMonthCalendar.NormalizeDigits(var Str: String);
begin
  if Assigned(Culture) then
    Str := Culture.FreezeDigits(Str, UsingCultureDigits);
end;

function TCustomIntlMonthCalendar.GetDayName(DayOfWeek: TDayOfWeek;
  NameKind: TIntlCalNameKind): String;
begin
  case NameKind of
    nkLong:
      Result := Calendar.Settings.DayNames[DayOfWeek];
    nkShort:
      Result := Calendar.Settings.ShortDayNames[DayOfWeek];
    nkShortest:
      Result := Calendar.Settings.ShortestDayNames[DayOfWeek];
  else
    Result := '';
  end;
  NormalizeDigits(Result);
end;

function TCustomIntlMonthCalendar.GetMonthName(Month: Integer;
  NameKind: TIntlCalNameKind): String;
begin
  if NameKind = nkLong then
    Result := Calendar.Settings.MonthNames[Month]
  else
    Result := Calendar.Settings.ShortMonthNames[Month];
  NormalizeDigits(Result);
end;

function TCustomIntlMonthCalendar.GetYearText(AYear: Integer): String;
var
  YearStartDate: TDate;
begin
  YearStartDate := Calendar.StartOfYear(Era, AYear);
  Result := Format(Calendar.Settings.YearFormat, YearStartDate);
end;

function TCustomIntlMonthCalendar.GetDayText(ADay: Integer): String;
begin
  Result := IntToStr(ADay);
  NormalizeDigits(Result);
end;

function TCustomIntlMonthCalendar.GetWeekText(AWeek: Integer): String;
begin
  Result := IntToStr(AWeek);
  NormalizeDigits(Result);
end;

function TCustomIntlMonthCalendar.GetHeaderText(ViewMode: TIntlCalViewMode): String;
begin
  if ViewMode = vmYear then
  begin
    if IsRightToLeft then
      Result := GetYearText(LastYearOnPage) + ' - ' + GetYearText(FirstYearOnPage)
    else
      Result := GetYearText(FirstYearOnPage) + ' - ' + GetYearText(LastYearOnPage);
  end
  else
  begin
    if ViewMode = vmMonth then
      Result := Format(Calendar.Settings.YearFormat, Date)
    else
      Result := Format(Calendar.Settings.YearMonthFormat, Date);
  end;
end;

function TCustomIntlMonthCalendar.GetFooterText(LongFormat: Boolean): String;
begin
  if LongFormat then
    Result := Format(Calendar.Settings.LongDateFormat, Calendar.Today)
  else
    Result := Format(Calendar.Settings.ShortDateFormat, Calendar.Today);
end;

function TCustomIntlMonthCalendar.GetMonthLastDate: TDate;
begin
  Result := MonthFirstDate + DaysInMonth - 1;
end;

function TCustomIntlMonthCalendar.GetFirstCellDate: TDate;
begin
  Result := MonthFirstDate - MonthStartCell;
end;

function TCustomIntlMonthCalendar.GetLastCellDate: TDate;
begin
  Result := FirstCellDate + CellCount - 1;
end;

function TCustomIntlMonthCalendar.GetLastCellYear: Integer;
begin
  Result := FirstCellYear + CellCount - 1;
end;

function TCustomIntlMonthCalendar.GetFirstYearOnPage: Integer;
begin
  Result := FirstCellYear;
  while (Result < Year) and not (IsYearOnPage(Result) and IsYearInRange(Era, Result)) do
    Result := Calendar.OffsetYear(Era, Result, +1);
end;

function TCustomIntlMonthCalendar.GetLastYearOnPage: Integer;
begin
  Result := LastCellYear;
  while (Result > Year) and not (IsYearOnPage(Result) and IsYearInRange(Era, Result)) do
    Result := Calendar.OffsetYear(Era, Result, -1);
end;

function TCustomIntlMonthCalendar.GetColCount: Integer;
begin
  Result := fMetrics.ColCount;
end;

function TCustomIntlMonthCalendar.GetRowCount: Integer;
begin
  Result := fMetrics.RowCount;
end;

function TCustomIntlMonthCalendar.GetCellCount: Integer;
begin
  Result := RowCount * ColCount;
end;

function TCustomIntlMonthCalendar.GetFocusedCell: Integer;
begin
  case CurrentView of
    vmDay: Result := MonthStartCell + Day - 1;
    vmMonth: Result := Month - 1;
    vmYear: Result := Year - FirstCellYear;
  else
    Result := -1;
  end;
end;

function TCustomIntlMonthCalendar.GetSelectionStart: TDate;
begin
  case SelectionStyle of
    ssSingle:
      Result := Date;
    ssRange:
      Result := Min(Date, fSelectionAnchor);
  else
    Result := fSelection.First;
  end;
end;

function TCustomIntlMonthCalendar.GetSelectionEnd: TDate;
begin
  case SelectionStyle of
    ssSingle:
      Result := Date;
    ssRange:
      Result := Max(Date, fSelectionAnchor);
  else
    Result := fSelection.Last;
  end;
end;

function TCustomIntlMonthCalendar.GetSelectionCount: Integer;
begin
  case SelectionStyle of
    ssSingle:
      Result := 1;
    ssRange:
      Result := Calendar.DaysBetween(fSelectionAnchor, Date) + 1;
  else
    Result := fSelection.Count;
  end;
end;

function TCustomIntlMonthCalendar.GetSelection(Index: Integer): TDate;
begin
  case SelectionStyle of
    ssSingle:
    begin
      if Index <> 0 then
        raise EListError.CreateFmt(SListIndexError, [Index]);
      Result := Date;
    end;
    ssRange:
    begin
      if (Index < 0) or (Index > Calendar.DaysBetween(fSelectionAnchor, Date)) then
        raise EListError.CreateFmt(SListIndexError, [Index]);
      Result := Min(Date, fSelectionAnchor) + Index;
    end
  else
    Result := fSelection[Index];
  end;
end;

function TCustomIntlMonthCalendar.GetAdjacentPageDate(Backward: Boolean): TDate;
var
  Dir: Integer;
begin
  if Backward then
    Dir := -1
  else
    Dir := +1;
  Result := Date;
  case CurrentView of
    vmDay:
      Calendar.IncMonth(Result, Dir);
    vmMonth:
      Calendar.IncYear(Result, Dir);
    vmYear:
      Calendar.IncYear(Result, Dir * 10);
  end;
end;

function TCustomIntlMonthCalendar.GetCalendarClientRect: TRect;
var
  TL, BR: Integer;
begin
  if fUpdateMetricsCount = 0 then
  begin
    Result := fMetrics.Bounds;
    Exit;
  end;
  Result := ClientRect;
  if BevelKind in [bkTile, bkSoft] then
  begin
    TL := 0; BR := 0;
    if (BevelInner = bvLowered) or ((BevelInner = bvRaised) and (BevelKind = bkSoft)) then
      Inc(TL);
    if BevelOuter in [bvLowered, bvRaised] then
      Inc(BR);
    if beLeft in BevelEdges then
      Inc(Result.Left, TL);
    if beTop in BevelEdges then
      Inc(Result.Top, TL);
    if beRight in BevelEdges then
      Dec(Result.Right, BR);
    if beBottom in BevelEdges then
      Dec(Result.Bottom, BR);
  end;
end;

function TCustomIntlMonthCalendar.GetSectionRect(Section: TIntlCalSection): TRect;
begin
  Result := fMetrics.Sections[Section];
end;

function TCustomIntlMonthCalendar.GetSectionWidth(Section: TIntlCalSection): Integer;
begin
  with fMetrics.Sections[Section] do Result := Right - Left;
end;

function TCustomIntlMonthCalendar.GetSectionHeight(Section: TIntlCalSection): Integer;
begin
  with fMetrics.Sections[Section] do Result := Bottom - Top;
end;

function TCustomIntlMonthCalendar.GetCellRect(Row, Col: Integer): TRect;
begin
  if (Row < 0) and (Col < 0) then
    Result := fMetrics.Sections[csWeekSpace]
  else if Row < 0 then
  begin
    Result := fMetrics.Sections[csDaysOfWeek];
    Dec(Result.Bottom, 2);
  end
  else if Col < 0 then
  begin
    Result := fMetrics.Sections[csWeekNumbers];
    if IsRightToLeft then
      Inc(Result.Left, 2)
    else
      Dec(Result.Right, 2);
  end
  else
    Result := fMetrics.Sections[csYMD];
  if Row >= 0 then
  begin
    Inc(Result.Top, Row * fMetrics.RowHeight + Min(Row, fMetrics.StretchedRows));
    Result.Bottom := Result.Top + fMetrics.RowHeight;
    if Row < fMetrics.StretchedRows then
      Inc(Result.Bottom);
  end;
  if Col >= 0 then
  begin
    if IsRightToLeft and (CurrentView in [vmDay, vmMonth]) then
    begin
      Dec(Result.Right, Col * fMetrics.ColWidth + Min(Col, fMetrics.StretchedCols));
      Result.Left := Result.Right - fMetrics.ColWidth;
      if Col < fMetrics.StretchedCols then
        Dec(Result.Left);
    end
    else
    begin
      Inc(Result.Left, Col * fMetrics.ColWidth + Min(Col, fMetrics.StretchedCols));
      Result.Right := Result.Left + fMetrics.ColWidth;
      if Col < fMetrics.StretchedCols then
        Inc(Result.Right);
    end;
  end;
end;

function TCustomIntlMonthCalendar.GetCellRect(Index: Integer): TRect;
begin
  Result := GetCellRect(Index div ColCount, Index mod ColCount);
end;

function TCustomIntlMonthCalendar.GetRowAt(Y: Integer): Integer;
var
  D: Integer;
begin
  Dec(Y, fMetrics.Sections[csYMD].Top);
  D := (fMetrics.RowHeight + 1) * fMetrics.StretchedRows;
  if Y < D then
    Result := Y div (fMetrics.RowHeight + 1)
  else
    Result := fMetrics.StretchedRows + (Y - D) div fMetrics.RowHeight;
end;

function TCustomIntlMonthCalendar.GetColAt(X: Integer): Integer;
var
  D: Integer;
begin
  Dec(X, fMetrics.Sections[csYMD].Left);
  if IsRightToLeft and (CurrentView in [vmDay, vmMonth]) then
    X := (fMetrics.Sections[csYMD].Right - fMetrics.Sections[csYMD].Left) - X - 1;
  D := (fMetrics.ColWidth + 1) * fMetrics.StretchedCols;
  if X < D then
    Result := X div (fMetrics.ColWidth + 1)
  else
    Result := fMetrics.StretchedCols + (X - D) div fMetrics.ColWidth;
end;

function TCustomIntlMonthCalendar.GetPreferredClientSize(out AWidth,
  AHeight: Integer): Boolean;
var
  Metrics: TIntlCalMetrics;
  Mode: TIntlCalViewMode;
begin
  if not fPrefferedSizeReady then
  begin
    fPrefferedSizeReady := True;
    fPrefferedSize.cx := fMetrics.PrefferedWidth;
    fPrefferedSize.cy := fMetrics.PrefferedHeight;
    fPrefferedSizeSmallest := (fMetrics.NameKind = nkShortest);
    for Mode := Low(TIntlCalViewMode) to High(TIntlCalViewMode) do
    begin
      CalcMetrics(Mode, Metrics);
      if fPrefferedSize.cx < Metrics.PrefferedWidth then
        fPrefferedSize.cx := Metrics.PrefferedWidth;
      if fPrefferedSize.cy < Metrics.PrefferedHeight then
        fPrefferedSize.cy := Metrics.PrefferedHeight;
      if Mode = vmDay then
        fPrefferedSizeSmallest := (Metrics.NameKind = nkShortest);
    end;
  end;
  AWidth := fPrefferedSize.cx;
  AHeight := fPrefferedSize.cy;
  Result := fPrefferedSizeSmallest;
end;

function TCustomIntlMonthCalendar.GetPreferredControlSize(out AWidth,
  AHeight: Integer): Boolean;
var
  EdgeSize: Integer;
begin
  Result := GetPreferredClientSize(AWidth, AHeight);
  Inc(AWidth, 2 * BorderWidth);
  Inc(AHeight, 2 * BorderWidth);
  if BevelKind <> bkNone then
  begin
    EdgeSize := 0;
    if BevelInner <> bvNone then
      Inc(EdgeSize, BevelWidth);
    if BevelOuter <> bvNone then
      Inc(EdgeSize, BevelWidth);
    if beLeft in BevelEdges then
      Inc(AWidth, EdgeSize);
    if beRight in BevelEdges then
      Inc(AWidth, EdgeSize);
    if beTop in BevelEdges then
      Inc(AHeight, EdgeSize);
    if beBottom in BevelEdges then
      Inc(AHeight, EdgeSize);
  end;
end;

function TCustomIntlMonthCalendar.GetHitTestInfoAt(X, Y: Integer;
  out Cell: Integer): TIntlCalHitTest;
var
  Pt: TPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  Cell := -1;
  Result := chtNowhere;
  if PtInRect(fMetrics.Bounds, Pt) then
  begin
    if PtInRect(fMetrics.Sections[csHeader], Pt) then
    begin
      if PtInRect(fMetrics.Sections[csHeaderText], Pt) then
        Result := chtHeaderText
      else if PtInRect(fMetrics.Sections[csArrowLeft], Pt) then
        Result := chtArrowLeft
      else if PtInRect(fMetrics.Sections[csArrowRight], Pt) then
        Result := chtArrowRight;
    end
    else if PtInRect(fMetrics.Sections[csFooter], Pt) then
      Result := chtFooter
    else if (CurrentView = vmDay) and PtInRect(fMetrics.Sections[csDaysOfWeek], Pt) then
      Result := chtDayOfWeek
    else if (CurrentView = vmDay) and WeekNumbers and PtInRect(fMetrics.Sections[csWeekNumbers], Pt) then
      Result := chtWeekNumber
    else if PtInRect(fMetrics.Sections[csYMD], Pt) then
    begin
      Cell := GetRowAt(Y) * fMetrics.ColCount + GetColAt(X);
      case CurrentView of
        vmDay: Result := chtDay;
        vmMonth: Result := chtMonth;
        vmYear: Result := chtYear;
      end;
    end;
  end
  else
end;

function TCustomIntlMonthCalendar.GetTheme: HTHEME;
var
  ListView: TListView;
begin
  if fTheme = 0 then
  begin
    ListView := TListView.Create(nil);
    try
      ListView.Visible := False;
      ListView.ParentWindow := Handle;
      fTheme := OpenThemeData(ListView.Handle, 'listview');
    finally
      ListView.Free;
    end;
  end;
  Result := fTheme;
end;

function TCustomIntlMonthCalendar.IsDateStored: Boolean;
begin
  Result := not Calendar.IsToday(Date);
end;

function TCustomIntlMonthCalendar.IsDateMinStored: Boolean;
begin
  Result := not Calendar.IsNoDate(DateMin);
end;

function TCustomIntlMonthCalendar.IsDateMaxStored: Boolean;
begin
  Result := not Calendar.IsNoDate(DateMax);
end;

function TCustomIntlMonthCalendar.IsManagedByLocalizer: Boolean;
begin
  Result := Assigned(Localizer);
end;

procedure TCustomIntlMonthCalendar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomIntlMonthCalendar.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TCustomIntlMonthCalendar.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  if CanFocus then
    SetFocus;
end;

procedure TCustomIntlMonthCalendar.WMTimer(var Message: TWMTimer);
var
  TheDate: TDate;
begin
  if NavigationTimer = AutoNavTimerForward then
    TheDate := GetAdjacentPageDate(False)
  else if NavigationTimer = AutoNavTimerBackward then
    TheDate := GetAdjacentPageDate(True)
  else
  begin
    inherited;
    Exit;
  end;
  if IsDateInRange(TheDate) then
    SelectDate(TheDate);
end;

procedure TCustomIntlMonthCalendar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  InvalidateHotLight;
  fHotCell := -1;
  fHotInfo := chtNowhere;
end;

procedure TCustomIntlMonthCalendar.CMHintShow(var Message: TCMHintShow);
var
  TheDate: TDate;
  TheCell: Integer;
begin
  TheCell := -1;
  with Message.HintInfo^ do
  begin
    if (GetHitTestInfoAt(CursorPos.X, CursorPos.Y, TheCell) = chtDay) and GetDateAtCell(TheCell, TheDate) then
    begin
      if TheDate <> fHintDate then
      begin
        fHintDate := TheDate;
        DoDateHint(TheDate, HintStr);
      end;
    end
    else
    begin
      fHintDate := Calendar.NoDate;
      inherited;
    end;
    CursorRect := GetCellRect(TheCell);
  end;
end;

procedure TCustomIntlMonthCalendar.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Inc(fUpdateMetricsCount);
  try
    if not fSaveFontHeader then
      FontHeader.Assign(Font);
    if not fSaveFontFooter then
      FontFooter.Assign(Font);
    if not fSaveFontDayOfWeek then
      FontDayOfWeek.Assign(Font);
    if not fSaveFontWeekNumbers then
      FontWeekNumbers.Assign(Font);
  finally
    Dec(fUpdateMetricsCount);
  end;
  UpdateMetrics;
end;

procedure TCustomIntlMonthCalendar.DoDateChange;
begin
  if Assigned(OnDateChange) then
    OnDateChange(Self);
end;

procedure TCustomIntlMonthCalendar.DoPageChange;
begin
  if Assigned(OnPageChange) then
    OnPageChange(Self);
end;

procedure TCustomIntlMonthCalendar.DoCalendarChange;
begin
  if Assigned(OnCalendarChange) then
    OnCalendarChange(Self);
end;

procedure TCustomIntlMonthCalendar.DoSelectionChange;
begin
  if Assigned(OnSelectionChange) then
    OnSelectionChange(Self);
end;

procedure TCustomIntlMonthCalendar.DoDateHint(const ADate: TDate;
  var HintStr: String);
begin
  HintStr := Hint;
  if Assigned(OnDateHint) then
    OnDateHint(Self, ADate, HintStr);
end;

function TCustomIntlMonthCalendar.DoDateSelectable(const ADate: TDate): Boolean;
begin
  Result := True;
  if Assigned(OnDateSelectable) then
    OnDateSelectable(Self, ADate, Result);
end;

function TCustomIntlMonthCalendar.DoCustomDrawDayOfWeek(Canvas: TCanvas;
  const ARect: TRect; State: TIntlCalDrawStates; ADayOfWeek: TDayOfWeek): Boolean;
begin
  Result := True;
  if Assigned(OnCustomDrawDayOfWeek) then
    OnCustomDrawDayOfWeek(Self, Canvas, ARect, State, ADayOfWeek, Result);
end;

function TCustomIntlMonthCalendar.DoCustomDrawWeekNumber(Canvas: TCanvas;
  const ARect: TRect; State: TIntlCalDrawStates; AWeek: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnCustomDrawWeekNumber) then
    OnCustomDrawWeekNumber(Self, Canvas, ARect, State, AWeek, Result);
end;

function TCustomIntlMonthCalendar.DoCustomDrawDay(Canvas: TCanvas;
  const ARect: TRect; State: TIntlCalDrawStates; const ADate: TDate;
  ADay: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnCustomDrawDay) then
    OnCustomDrawDay(Self, Canvas, ARect, State, ADate, ADay, Result);
end;

function TCustomIntlMonthCalendar.DoCustomDrawMonth(Canvas: TCanvas;
  const ARect: TRect; State: TIntlCalDrawStates; AMonth: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnCustomDrawMonth) then
    OnCustomDrawMonth(Self, Canvas, ARect, State, AMonth, Result);
end;

function TCustomIntlMonthCalendar.DoCustomDrawYear(Canvas: TCanvas;
  const ARect: TRect; State: TIntlCalDrawStates; AYear: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnCustomDrawYear) then
    OnCustomDrawYear(Self, Canvas, ARect, State, AYear, Result);
end;

function TCustomIntlMonthCalendar.DoCustomDrawHeaderText(Canvas: TCanvas;
  const ARect: TRect; State: TIntlCalDrawStates; const AText: String): Boolean;
begin
  Result := True;
  if Assigned(OnCustomDrawHeaderText) then
    OnCustomDrawHeaderText(Self, Canvas, ARect, State, AText, Result);
end;

function TCustomIntlMonthCalendar.DoCustomDrawFooterText(Canvas: TCanvas;
  const ARect: TRect; State: TIntlCalDrawStates; const AText: String): Boolean;
begin
  Result := True;
  if Assigned(OnCustomDrawFooterText) then
    OnCustomDrawFooterText(Self, Canvas, ARect, State, AText, Result);
end;

procedure TCustomIntlMonthCalendar.LocalizerNotify(Sender: TLocalizer;
  Reason: TLocalizerNotification);
begin
  case Reason of
    lnCultureChanged:
      if Assigned(Localizer) then
        Culture := Localizer.Culture
      else
        Culture := nil;
    lnOptionsChanged:
      ValidateCalendar;
  end;
end;

procedure TCustomIntlMonthCalendar.FontHeaderChanged(Sender: TObject);
begin
  fSaveFontHeader := (FontHeader.Handle <> Font.Handle);
  UpdateMetrics;
end;

procedure TCustomIntlMonthCalendar.FontFooterChanged(Sender: TObject);
begin
  fSaveFontFooter := (FontFooter.Handle <> Font.Handle);
  UpdateMetrics;
end;

procedure TCustomIntlMonthCalendar.FontDayOfWeekChanged(Sender: TObject);
begin
  fSaveFontDayOfWeek := (FontDayOfWeek.Handle <> Font.Handle);
  UpdateMetrics;
end;

procedure TCustomIntlMonthCalendar.FontWeekNumbersChanged(Sender: TObject);
begin
  fSaveFontWeekNumbers := (FontWeekNumbers.Handle <> Font.Handle);
  if WeekNumbers then
    UpdateMetrics;
end;

procedure TCustomIntlMonthCalendar.CalendarChanged(Sender: TObject);
begin
  UpdateCalendar;
  DoCalendarChange;
end;

procedure TCustomIntlMonthCalendar.ReadCulture(Reader: TReader);
begin
  Culture := CultureOf(Reader.ReadString);
end;

procedure TCustomIntlMonthCalendar.WriteCulture(Writer: TWriter);
begin
  Writer.WriteString(Culture.Locale);
end;

procedure TCustomIntlMonthCalendar.ReadCalendarType(Reader: TReader);
begin
  CalendarType := CalendarTypes.ByName(Reader.ReadString);
end;

procedure TCustomIntlMonthCalendar.WriteCalendarType(Writer: TWriter);
begin
  Writer.WriteString(CalendarType.CalendarName);
end;

procedure TCustomIntlMonthCalendar.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('CultureInfo', ReadCulture, WriteCulture,
    not Assigned(Localizer) and Assigned(Culture));
  Filer.DefineProperty('CalendarClass', ReadCalendarType, WriteCalendarType,
    Assigned(CalendarType));
end;

procedure TCustomIntlMonthCalendar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Localizer) then
    Localizer := nil;
end;

procedure TCustomIntlMonthCalendar.Resize;
begin
  inherited Resize;
  UpdateMetrics;
end;

procedure TCustomIntlMonthCalendar.Paint;
var
  Rect: TRect;
begin
  DrawCalendar(Canvas);
  Rect := ClientRect;
  if not EqualRect(fMetrics.Bounds, Rect) then
  begin
    with fMetrics.Bounds do
      ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);
  end;
end;

procedure TCustomIntlMonthCalendar.Loaded;
begin
  inherited Loaded;
  if (Assigned(CalendarType) or Assigned(Culture)) or
     (Assigned(Localizer) and Assigned(Localizer.Culture))
  then
    ValidateCalendar
  else
  begin
    UpdateCalendar;
    DoCalendarChange;
  end;
  DoPageChange;
  DoDateChange;
  DoSelectionChange;
end;

function TCustomIntlMonthCalendar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if AutoSize and not (csLoading in ComponentState) then
    GetPreferredControlSize(NewWidth, NewHeight);
end;

procedure TCustomIntlMonthCalendar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TheDate: TDate;
  TheYear, TheMonth, TheDay: Integer;
  TheCell: Integer;
begin
  fDraggingCell := -1;
  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    case GetHitTestInfoAt(X, Y, TheCell) of
      chtArrowLeft:
      begin
        TheDate := GetAdjacentPageDate(True);
        if IsDateInRange(TheDate) then
        begin
          fNavigationTimer := SetTimer(Handle, AutoNavTimerBackward, AutoNavInterval, nil);
          SelectDate(TheDate);
        end;
      end;
      chtArrowRight:
      begin
        TheDate := GetAdjacentPageDate(False);
        if IsDateInRange(TheDate) then
        begin
          fNavigationTimer := SetTimer(Handle, AutoNavTimerForward, AutoNavInterval, nil);
          SelectDate(TheDate);
        end;
      end;
      chtHeaderText:
        case CurrentView of
          vmDay:
            CurrentView := vmMonth;
          vmMonth:
            CurrentView := vmYear;
        end;
      chtFooter:
      begin
        CurrentView := vmDay;
        Select(Calendar.Today);
        InvalidateSections([csFooter]);
      end;
      chtDay:
        if GetDateAtCell(TheCell, TheDate) then
        begin
          SelectDate(TheDate, Shift);
          fDraggingCell := TheCell;
        end;
      chtMonth:
        if GetMonthAtCell(TheCell, TheMonth) then
        begin
          TheDay := Min(Day, Calendar.DaysInMonth(Era, Year, TheMonth));
          Select(Calendar.EncodeDate(Era, Year, TheMonth, TheDay));
          fDraggingCell := TheCell;
        end;
      chtYear:
        if GetYearAtCell(TheCell, TheYear) then
        begin
          fDraggingOffPage := not IsYearOnPage(TheYear);
          TheMonth := Min(Month, Calendar.MonthsInYear(Era, TheYear));
          TheDay := Min(Day, Calendar.DaysInMonth(Era, TheYear, TheMonth));
          Select(Calendar.EncodeDate(Era, TheYear, TheMonth, TheDay));
          fDraggingCell := TheCell;
        end;
    end;
    fDraggingShiftState := Shift + [ssShift];
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomIntlMonthCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  TheDate: TDate;
  TheYear, TheMonth, TheDay: Integer;
  TheCell: Integer;
  TheHitTest: TIntlCalHitTest;
begin
  if NavigationTimer <> 0 then
    Exit;
  TheHitTest := GetHitTestInfoAt(X, Y, TheCell);
  if (TheHitTest <> HotInfo) or (TheCell <> HotCell) then
  begin
    InvalidateHotLight;
    fHotInfo := TheHitTest;
    fHotCell := TheCell;
    InvalidateHotLight;
  end;
  if (DraggingCell >= 0) and (DraggingCell <> TheCell) and (TheCell >= 0) then
  begin
    case HotInfo of
      chtDay:
        if GetDateAtCell(TheCell, TheDate) then
        begin
          fDraggingCell := TheCell;
          SelectDate(TheDate, fDraggingShiftState);
        end;
      chtMonth:
        if GetMonthAtCell(TheCell, TheMonth) then
        begin
          fDraggingCell := TheCell;
          TheDay := Min(Day, Calendar.DaysInMonth(Era, Year, TheMonth));
          Select(Calendar.EncodeDate(Era, Year, TheMonth, TheDay));
        end;
      chtYear:
        if GetYearAtCell(TheCell, TheYear) then
        begin
          fDraggingCell := TheCell;
          fDraggingOffPage := not IsYearOnPage(TheYear);
          TheMonth := Min(Month, Calendar.MonthsInYear(Era, TheYear));
          TheDay := Min(Day, Calendar.DaysInMonth(Era, TheYear, TheMonth));
          Select(Calendar.EncodeDate(Era, TheYear, TheMonth, TheDay));
        end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomIntlMonthCalendar.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TheMonth, TheYear: Integer;
begin
  if NavigationTimer <> 0 then
  begin
    KillTimer(Handle, NavigationTimer);
    fNavigationTimer := 0;
    InvalidateHotLight;
  end;
  if DraggingCell >= 0 then
  begin
    fDraggingCell := -1;
    if (Button = mbLeft) and (HotCell >= 0) then
      case HotInfo of
        chtMonth:
          if GetMonthAtCell(HotCell, TheMonth) then
            CurrentView := vmDay;
        chtYear:
          if not DraggingOffPage and GetYearAtCell(HotCell, TheYear) then
            CurrentView := vmMonth;
      end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TCustomIntlMonthCalendar.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    case CurrentView of
      vmDay:
        Select(Calendar.NextDay(Date), Shift);
      vmMonth:
        Select(Calendar.NextMonth(Date));
      vmYear:
        Select(Calendar.NextYear(Date));
    end;
    Result := True;
  end;
end;

function TCustomIntlMonthCalendar.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
  begin
    case CurrentView of
      vmDay:
        Select(Calendar.PrevDay(Date), Shift);
      vmMonth:
        Select(Calendar.PrevMonth(Date));
      vmYear:
        Select(Calendar.prevYear(Date));
    end;
    Result := True;
  end;
end;

procedure TCustomIntlMonthCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (SelectionStyle <> ssFree) or (ssShift in Shift) or not (ssCtrl in Shift) then
    Include(Shift, ssLeft);
  case Key of
    VK_RIGHT:
    begin
      case CurrentView of
        vmDay:
          if IsRightToLeft then
            SelectDate(Calendar.PrevDay(Date), Shift)
          else
            SelectDate(Calendar.NextDay(Date), Shift);
        vmMonth:
          if IsRightToLeft then
            Select(Calendar.PrevMonth(Date))
          else
            Select(Calendar.NextMonth(Date));
        vmYear:
          Select(Calendar.NextYear(Date));
      end;
      Key := 0;
    end;
    VK_LEFT:
    begin
      case CurrentView of
        vmDay:
          if IsRightToLeft then
            SelectDate(Calendar.NextDay(Date), Shift)
          else
            SelectDate(Calendar.PrevDay(Date), Shift);
        vmMonth:
          if IsRightToLeft then
            Select(Calendar.NextMonth(Date))
          else
            Select(Calendar.PrevMonth(Date));
        vmYear:
          Select(Calendar.PrevYear(Date));
      end;
      Key := 0;
    end;
    VK_DOWN:
    begin
      case CurrentView of
        vmDay:
          SelectDate(Calendar.NextDay(Date, ColCount), Shift);
        vmMonth:
          Select(Calendar.NextMonth(Date, ColCount));
        vmYear:
          Select(Calendar.NextYear(Date, ColCount));
      end;
      Key := 0;
    end;
    VK_UP:
    begin
      case CurrentView of
        vmDay:
          SelectDate(Calendar.PrevDay(Date, ColCount), Shift);
        vmMonth:
          Select(Calendar.PrevMonth(Date, ColCount));
        vmYear:
          Select(Calendar.PrevYear(Date, ColCount));
      end;
      Key := 0;
    end;
    VK_NEXT:
    begin
      case CurrentView of
        vmDay:
          SelectDate(Calendar.NextMonth(Date), Shift);
        vmMonth:
          Select(Calendar.NextYear(Date));
        vmYear:
          Select(Calendar.NextYear(Date, 10));
      end;
      Key := 0;
    end;
    VK_PRIOR:
    begin
      case CurrentView of
        vmDay:
          SelectDate(Calendar.PrevMonth(Date), Shift);
        vmMonth:
          Select(Calendar.PrevYear(Date));
        vmYear:
          Select(Calendar.PrevYear(Date, 10));
      end;
      Key := 0;
    end;
    VK_HOME:
    begin
      case CurrentView of
        vmDay:
          SelectDate(MonthFirstDate, Shift);
        vmMonth:
          Select(Calendar.StartOfYear(Date));
        vmYear:
          Select(Calendar.StartOfYear(Era, FirstYearOnPage));
      end;
      Key := 0;
    end;
    VK_END:
    begin
      case CurrentView of
        vmDay:
          SelectDate(MonthLastDate, Shift);
        vmMonth:
          Select(Calendar.EndOfYear(Date));
        vmYear:
          Select(Calendar.EndOfYear(Era, LastYearOnPage));
      end;
      Key := 0;
    end;
    VK_SPACE:
    begin
      if CurrentView = vmDay then
        SelectDate(Date, [ssLeft, ssCtrl])
      else if CurrentView = vmYear then
        CurrentView := vmMonth
      else
        CurrentView := vmDay;
      Key := 0;
    end;
    VK_BACK:
    begin
      if CurrentView = vmDay then
        CurrentView := vmMonth
      else if CurrentView = vmMonth then
        CurrentView := vmYear;
      Key := 0;
    end;
  end;
end;

procedure TCustomIntlMonthCalendar.DoEnter;
begin
  inherited DoEnter;
  InvalidateSelection;
end;

procedure TCustomIntlMonthCalendar.DoExit;
begin
  inherited DoExit;
  InvalidateSelection;
end;

function TCustomIntlMonthCalendar.IsThemed: Boolean;
begin
  Result := (Win32MajorVersion > 5) and ThemeControl(Self) and (Theme <> 0);
end;

function TCustomIntlMonthCalendar.IsYearOnPage(AYear: Integer): Boolean;
begin
  Result := (AYear = Year)
         or ((Calendar.EraOf(Calendar.StartOfYear(Era, AYear)) = Era)
        and (AYear div 10 = Year div 10));
end;

procedure TCustomIntlMonthCalendar.AnimateBegin;
begin
  if Assigned(fSavedScreen) then
    FreeAndNil(fSavedScreen);
  if Animate and Showing then
  begin
    fSavedScreen := TakeSnapshot;
    fSavedView := CurrentView;
    fSavedDate := Date;
    fSavedCellRect := GetCellRect(FocusedCell);
  end;
end;

procedure TCustomIntlMonthCalendar.AnimateEnd;
begin
  if Assigned(fSavedScreen) then
    FreeAndNil(fSavedScreen);
end;

procedure TCustomIntlMonthCalendar.AnimatePlay;
var
  NewScreen: TBitmap;
  Rect: TRect;
  DC: HDC;
begin
  if Animate and Showing and Assigned(fSavedScreen) then
  begin
    NewScreen := TakeSnapshot;
    try
      DC := GetDC(Handle);
      try
        Rect := fMetrics.Bounds;
        if ShowHeader then
        begin
          BitBlt(DC, Rect.Left, Rect.Top, Rect.Right - Rect.Left,
            fMetrics.HeaderHeight, NewScreen.Canvas.Handle, 0, 0, SRCCOPY);
          Inc(Rect.Top, fMetrics.HeaderHeight);
        end;
        if ShowFooter then
          Dec(Rect.Bottom, fMetrics.FooterHeight);
        if CurrentView = fSavedView then
        begin
          SwapScreenByScroll(DC, Rect, fMetrics.Bounds.Left, fMetrics.Bounds.Top,
            fSavedScreen, NewScreen, Date < fSavedDate, AnimateDuration);
        end
        else
        begin
         SwapScreenByZoom(DC, Rect, fMetrics.Bounds.Left, fMetrics.Bounds.Top,
           fSavedScreen, NewScreen, fSavedCellRect, GetCellRect(FocusedCell),
           AnimateDuration);
        end;
      finally
        ReleaseDC(Handle, DC);
      end;
    finally
      NewScreen.Free;
    end;
  end;
end;

function TCustomIntlMonthCalendar.TakeSnapshot: TBitmap;
begin
  Result := TBitmap.Create;
  with fMetrics.Bounds do
  begin
    Result.Width := Right - Left;
    Result.Height := Bottom - Top;
    SetWindowOrgEx(Result.Canvas.Handle, -Left, -Top, nil);
  end;
  DrawCalendar(Result.Canvas);
  SetWindowOrgEx(Result.Canvas.Handle, 0, 0, nil);
end;

procedure TCustomIntlMonthCalendar.RenderBackground(Canvas: TCanvas;
  var Rect: TRect; State: TIntlCalDrawStates);
const
  CP_CALITEM = LVP_LISTITEM;
var
  StateId: Integer;
  ForeColor: Cardinal;
begin
  if ((cdsSelected in State) or (cdsHotLight in State)) and IsThemed then
  begin
    if cdsGrayed in State then
      StateId := LIS_DISABLED
    else if cdsSelected in State then
    begin
      if Focused then
        if cdsHotLight in State then
          StateId := LISS_HOTSELECTED
        else
          StateId := LIS_SELECTED
      else
        StateId := LIS_SELECTEDNOTFOCUS
    end
    else if cdsHotLight in State then
      StateId := LIS_HOT
    else
      StateId := LIS_NORMAL;
    if cdsGrayed in State then
      ForeColor := ColorToRGB(clGrayText)
    else if cdsHeader in State then
      ForeColor := ColorToRGB(FontHeader.Color)
    else if cdsFixedRow in State then
      ForeColor := ColorToRGB(FontDayOfWeek.Color)
    else if cdsFixedCol in State then
      ForeColor := ColorToRGB(FontWeekNumbers.Color)
    else
      ForeColor := ColorToRGB(Font.Color);
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);
    DrawThemeBackground(Theme, Canvas.Handle, CP_CALITEM, StateId, Rect, @Rect);
    GetThemeColor(Theme, CP_CALITEM, StateId, TMT_TEXTCOLOR, ForeColor);
    Canvas.Font.Color := ForeColor;
  end
  else
  begin
    if cdsGrayed in State then
      Canvas.Font.Color := clGrayText
    else if cdsSelected in State then
      if cdsFocused in State then
        Canvas.Font.Color := clHighlightText
      else
        Canvas.Font.Color := clBtnText
    else if cdsHotLight in State then
      Canvas.Font.Color := clHotLight
    else if cdsHeader in State then
      Canvas.Font.Color := FontHeader.Color
    else if cdsFooter in State then
      Canvas.Font.Color := FontFooter.Color
    else if cdsFixedRow in State then
      Canvas.Font.Color := FontDayOfWeek.Color
    else if cdsFixedCol in State then
      Canvas.Font.Color := FontWeekNumbers.Color
    else
      Canvas.Font.Color := Font.Color;
    if cdsSelected in State then
      if cdsFocused in State then
        Canvas.Brush.Color := clHighlight
      else
        Canvas.Brush.Color := clBtnFace
    else
      Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);
  end;
  InflateRect(Rect, -1, -1);
  SetBkMode(Canvas.Handle, TRANSPARENT);
end;

procedure TCustomIntlMonthCalendar.RenderText(Canvas: TCanvas;
  var Rect: TRect; State: TIntlCalDrawStates; const Text: String;
  Alignment: TAlignment);
const
  TextAlignments: array[TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  DrawFlags: Cardinal;
begin
  DrawFlags := DrawTextBiDiModeFlags(TextAlignments[Alignment] or DT_VCENTER
    or DT_NOPREFIX or DT_SINGLELINE);
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, DrawFlags);
end;

procedure TCustomIntlMonthCalendar.RenderArrow(Canvas: TCanvas;
  var Rect: TRect; State: TIntlCalDrawStates; Backward: Boolean);
var
  Points: array[0..2] of TPoint;
  ArrowWidth: Integer;
  W, H: Integer;
begin
  W := Rect.Right - Rect.Left;
  H := Rect.Bottom - Rect.Top;
  if W < H then
    ArrowWidth := W div 6
  else
    ArrowWidth := H div 6;
  if Backward then
  begin
    Points[0].X := Rect.Left + (W div 2 - ArrowWidth + 1);
    Points[1].X := Points[0].X + ArrowWidth;
  end
  else
  begin
    Points[0].X := Rect.Right - (W div 2 - ArrowWidth + 1);
    Points[1].X := Points[0].X - ArrowWidth;
  end;
  Points[2].X := Points[1].X;
  Points[0].Y := (Rect.Top + Rect.Bottom) div 2;
  Points[1].Y := Points[0].Y - ArrowWidth;
  Points[2].Y := Points[0].Y + ArrowWidth;
  Canvas.Brush.Color := Canvas.Font.Color;
  Canvas.Pen.Color := Canvas.Font.Color;
  Canvas.Polygon(Points);
end;

procedure TCustomIntlMonthCalendar.RenderTodayMarker(Canvas: TCanvas;
  var Rect: TRect);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Width := 0;
  Canvas.Pen.Color := ColorToday;
  Canvas.RoundRect(Rect.Left + 1, Rect.Top + 1, Rect.Right - 1, Rect.Bottom - 1, 4, 4);
  Canvas.Brush.Style := bsSolid;
end;

procedure TCustomIntlMonthCalendar.DrawCalendar(Canvas: TCanvas);
begin
  if RectVisible(Canvas.Handle, fMetrics.Bounds) then
  begin
    if ShowHeader then
      DrawHeader(Canvas);
    case CurrentView of
      vmDay:
      begin
        if WeekNumbers then
        begin
          DrawWeekSpace(Canvas);
          DrawWeekNumbers(Canvas);
        end;
        DrawDaysOfWeek(Canvas);
        DrawDays(Canvas);
      end;
      vmMonth:
        DrawMonths(Canvas);
      vmYear:
        DrawYears(Canvas);
    end;
    if ShowFooter then
      DrawFooter(Canvas);
  end;
end;

procedure TCustomIntlMonthCalendar.DrawHeader(Canvas: TCanvas);
var
  Rect: TRect;
  State: TIntlCalDrawStates;
  HeaderText: String;
begin
  Rect := GetSectionRect(csHeader);
  if RectVisible(Canvas.Handle, Rect) then
  begin
    Canvas.Font := FontHeader;
    // Left Arrow
    Rect := GetSectionRect(csArrowLeft);
    if RectVisible(Canvas.Handle, Rect) then
    begin
      State := [cdsHeader];
      if not IsDateInRange(GetAdjacentPageDate(True)) then
        Include(State, cdsGrayed)
      else if HotInfo = chtArrowLeft then
      begin
        Include(State, cdsHotLight);
        if NavigationTimer <> 0 then
          Include(State, cdsSelected);
      end;
      RenderBackground(Canvas, Rect, State);
      RenderArrow(Canvas, Rect, State, True);
    end;
    // Right Arrow
    Rect := GetSectionRect(csArrowRight);
    if RectVisible(Canvas.Handle, Rect) then
    begin
      State := [cdsHeader];
      if not IsDateInRange(GetAdjacentPageDate(False)) then
        Include(State, cdsGrayed)
      else if HotInfo = chtArrowRight then
      begin
        Include(State, cdsHotLight);
        if NavigationTimer <> 0 then
          Include(State, cdsSelected);
      end;
      RenderBackground(Canvas, Rect, State);
      RenderArrow(Canvas, Rect, State, False);
    end;
    // Header text
    Rect := GetSectionRect(csHeaderText);
    if RectVisible(Canvas.Handle, Rect) then
    begin
      State := [cdsHeader];
      if (HotInfo = chtHeaderText) and (CurrentView <> vmYear) then
        Include(State, cdsHotLight);
      RenderBackground(Canvas, Rect, State);
      HeaderText := GetHeaderText(CurrentView);
      if DoCustomDrawHeaderText(Canvas, Rect, State, HeaderText) then
        RenderText(Canvas, Rect, State, HeaderText, taCenter);
    end;
  end;
end;

procedure TCustomIntlMonthCalendar.DrawFooter(Canvas: TCanvas);
const
  Spacing = 2;
var
  Rect, R: TRect;
  State: TIntlCalDrawStates;
  FooterText: String;
begin
  Rect := GetSectionRect(csFooter);
  if RectVisible(Canvas.Handle, Rect) then
  begin
    Canvas.Pen.Width := 0;
    Canvas.Pen.Color := Color;
    Canvas.MoveTo(Rect.Left, Rect.Top);
    Canvas.LineTo(Rect.Right, Rect.Top);
    Inc(Rect.Top);
    Canvas.MoveTo(Rect.Left, Rect.Top);
    Canvas.LineTo(Rect.Right, Rect.Top);
    Inc(Rect.Top);
    Canvas.Font := FontFooter;
    State := [cdsFooter];
    if (HotInfo = chtFooter) and not Calendar.IsToday(Date) then
      Include(State, cdsHotLight);
    RenderBackground(Canvas, Rect, State);
    FooterText := GetFooterText(fMetrics.LongFormat);
    if DoCustomDrawFooterText(Canvas, Rect, State, FooterText) then
    begin
      InflateRect(Rect, -Spacing, -Spacing);
      R.Top := Rect.Top;
      R.Bottom := Rect.Bottom;
      if IsRightToLeft then
      begin
        R.Right := Rect.Right;
        R.Left := R.Right - fMetrics.FooterHeight;
        Dec(Rect.Right, fMetrics.FooterHeight + Spacing);
      end
      else
      begin
        R.Left := Rect.Left;
        R.Right := R.Left + fMetrics.FooterHeight;
        Inc(Rect.Left, fMetrics.FooterHeight + Spacing);
      end;
      RenderText(Canvas, Rect, State, FooterText, taLeftJustify);
      RenderTodayMarker(Canvas, R);
    end;
  end;
end;

procedure TCustomIntlMonthCalendar.DrawWeekSpace(Canvas: TCanvas);
var
  Rect: TRect;
begin
  Rect := GetSectionRect(csWeekSpace);
  if RectVisible(Canvas.Handle, Rect) then
    RenderBackground(Canvas, Rect, [cdsFixedRow, cdsFixedCol, cdsBlank]);
end;

procedure TCustomIntlMonthCalendar.DrawDaysOfWeek(Canvas: TCanvas);
var
  Rect: TRect;
  Col: Integer;
  DayOfWeek: TDayOfWeek;
  State: TIntlCalDrawStates;
begin
  Rect := GetSectionRect(csDaysOfWeek);
  if RectVisible(Canvas.Handle, Rect) then
  begin
    Canvas.Pen.Width := 0;
    Dec(Rect.Bottom);
    Canvas.Pen.Color := Color;
    Canvas.MoveTo(Rect.Left, Rect.Bottom);
    Canvas.LineTo(Rect.Right, Rect.Bottom);
    Dec(Rect.Bottom);
    Canvas.Pen.Color := ColorDivider;
    Canvas.MoveTo(Rect.Left, Rect.Bottom);
    Canvas.LineTo(Rect.Right, Rect.Bottom);
    Canvas.Font := FontDayOfWeek;
    for Col := 0 to ColCount - 1 do
    begin
      DayOfWeek := TDayOfWeek(((Ord(Calendar.Settings.FirstDayOfWeek) + Col - 1) mod Calendar.DaysPerWeek) + 1);
      Rect := GetCellRect(-1, Col);
      if RectVisible(Canvas.Handle, Rect) then
      begin
        State := [cdsFixedRow];
        if Calendar.IsToday(DayOfWeek) and Calendar.IsThisMonth(Era, Year, Month) then
          Include(State, cdsToday);
        RenderBackground(Canvas, Rect, State);
        if DoCustomDrawDayOfWeek(Canvas, Rect, State, DayOfWeek) then
          RenderText(Canvas, Rect, State, GetDayName(DayOfWeek, fMetrics.NameKind), taCenter);
      end;
    end;
  end;
end;

procedure TCustomIntlMonthCalendar.DrawWeekNumbers(Canvas: TCanvas);
var
  Rect: TRect;
  Row, TheWeek: Integer;
  StartRange, EndRange: Integer;
  WeekStartDate: TDateTime;
  State: TIntlCalDrawStates;
begin
  Rect := GetSectionRect(csWeekNumbers);
  if RectVisible(Canvas.Handle, Rect) then
  begin
    Canvas.Pen.Width := 0;
    if IsRightToLeft then
    begin
      Canvas.Pen.Color := Color;
      Canvas.MoveTo(Rect.Left, Rect.Top);
      Canvas.LineTo(Rect.Left, Rect.Bottom);
      Inc(Rect.Left);
      Canvas.Pen.Color := ColorDivider;
      Canvas.MoveTo(Rect.Left, Rect.Top);
      Canvas.LineTo(Rect.Left, Rect.Bottom);
      Inc(Rect.Left);
    end
    else
    begin
      Dec(Rect.Right);
      Canvas.Pen.Color := Color;
      Canvas.MoveTo(Rect.Right, Rect.Top);
      Canvas.LineTo(Rect.Right, Rect.Bottom);
      Dec(Rect.Right);
      Canvas.Pen.Color := ColorDivider;
      Canvas.MoveTo(Rect.Right, Rect.Top);
      Canvas.LineTo(Rect.Right, Rect.Bottom);
    end;
    StartRange := MonthStartCell div ColCount;
    EndRange := (MonthStartCell + DaysInMonth - 1) div ColCount;
    Canvas.Font := FontWeekNumbers;
    for Row := 0 to RowCount - 1 do
    begin
      Rect := GetCellRect(Row, -1);
      if RectVisible(Canvas.Handle, Rect) then
      begin
        WeekStartDate := Calendar.NextWeek(FirstCellDate, Row);
        State := [cdsFixedCol];
        if (Row = (MonthStartCell + Day - 1) div ColCount) or
           IsDateInRange(WeekStartDate) or
           IsDateInRange(Calendar.EndOfWeek(WeekStartDate)) then
        begin
          if (Row < StartRange) or (Row > EndRange) then
            Include(State, cdsGrayed);
          if Calendar.IsThisWeek(WeekStartDate) then
            Include(State, cdsToday);
          if Row = StartRange then
            TheWeek := Calendar.WeekOfYear(MonthFirstDate)
          else
            TheWeek := Calendar.WeekOfYear(WeekStartDate);
        end
        else
        begin
          Include(State, cdsBlank);
          TheWeek := 0;
        end;
        RenderBackground(Canvas, Rect, State);
        if not (cdsBlank in State) and DoCustomDrawWeekNumber(Canvas, Rect, State, TheWeek) then
          RenderText(Canvas, Rect, State, GetWeekText(TheWeek), taCenter);
      end;
    end;
  end;
end;

procedure TCustomIntlMonthCalendar.DrawDays(Canvas: TCanvas);
var
  Rect: TRect;
  Cell: Integer;
  TheDate: TDate;
  TheDay: Integer;
  State: TIntlCalDrawStates;
begin
  Rect := GetSectionRect(csYMD);
  if RectVisible(Canvas.Handle, Rect) then
  begin
    Canvas.Font := Font;
    for Cell := 0 to CellCount - 1 do
    begin
      Rect := GetCellRect(Cell);
      if RectVisible(Canvas.Handle, Rect) then
      begin
        TheDate := FirstCellDate + Cell;
        State := [];
        if Cell < MonthStartCell then
        begin
          TheDay := FirstCellDayNumber + Cell;
          Include(State, cdsGrayed);
        end
        else if Cell >= MonthStartCell + DaysInMonth then
        begin
          TheDay := Cell - MonthStartCell - DaysInMonth + 1;
          Include(State, cdsGrayed);
        end
        else
          TheDay := Cell - MonthStartCell + 1;
        if IsDateInRange(TheDate) then
        begin
          if IsSelected(TheDate) then
            Include(State, cdsSelected);
          if (Cell = FocusedCell) and Focused then
            Include(State, cdsFocused);
          if Cell = HotCell then
            Include(State, cdsHotLight);
          if Calendar.IsToday(TheDate) then
            Include(State, cdsToday);
        end
        else
          Include(State, cdsBlank);
        RenderBackground(Canvas, Rect, State);
        if not (cdsBlank in State) and DoCustomDrawDay(Canvas, Rect, State, TheDate, TheDay) then
        begin
          RenderText(Canvas, Rect, State, GetDayText(TheDay), taCenter);
          if cdsToday in State then
            RenderTodayMarker(Canvas, Rect);
          if cdsFocused in State then
            Canvas.DrawFocusRect(Rect);
        end;
      end;
    end;
  end;
end;

procedure TCustomIntlMonthCalendar.DrawMonths(Canvas: TCanvas);
var
  Rect: TRect;
  Cell: Integer;
  TheMonth: Integer;
  State: TIntlCalDrawStates;
begin
  Rect := GetSectionRect(csYMD);
  if RectVisible(Canvas.Handle, Rect) then
  begin
    Canvas.Font := Font;
    for Cell := 0 to CellCount - 1 do
    begin
      Rect := GetCellRect(Cell);
      if RectVisible(Canvas.Handle, Rect) then
      begin
        TheMonth := Cell + 1;
        State := [];
        if IsMonthInRange(Era, Year, TheMonth) then
        begin
          if Cell = FocusedCell then
          begin
            Include(State, cdsSelected);
            if Focused then
              Include(State, cdsFocused);
          end;
          if Cell = HotCell then
            Include(State, cdsHotLight);
          if Calendar.IsThisMonth(Era, Year, TheMonth) then
            Include(State, cdsToday);
        end
        else
          Include(State, cdsBlank);
        RenderBackground(Canvas, Rect, State);
        if not (cdsBlank in State) and DoCustomDrawMonth(Canvas, Rect, State, TheMonth) then
        begin
          RenderText(Canvas, Rect, State, GetMonthName(TheMonth, fMetrics.NameKind), taCenter);
          if cdsFocused in State then
            Canvas.DrawFocusRect(Rect);
        end;
      end;
    end;
  end;
end;

procedure TCustomIntlMonthCalendar.DrawYears(Canvas: TCanvas);
var
  Rect: TRect;
  Cell: Integer;
  TheYear: Integer;
  State: TIntlCalDrawStates;
begin
  Rect := GetSectionRect(csYMD);
  if RectVisible(Canvas.Handle, Rect) then
  begin
    Canvas.Font := Font;
    for Cell := 0 to CellCount - 1 do
    begin
      Rect := GetCellRect(Cell);
      if RectVisible(Canvas.Handle, Rect) then
      begin
        TheYear := Calendar.OffsetYear(Era, FirstCellYear, Cell);
        State := [];
        if IsYearInRange(Era, TheYear) then
        begin
          if Cell = FocusedCell then
          begin
            Include(State, cdsSelected);
            if Focused then
              Include(State, cdsFocused);
          end
          else if not IsYearOnPage(TheYear) then
            Include(State, cdsGrayed);
          if Cell = HotCell then
            Include(State, cdsHotLight);
          if Calendar.IsThisYear(Era, TheYear) then
            Include(State, cdsToday);
        end
        else
          Include(State, cdsBlank);
        RenderBackground(Canvas, Rect, State);
        if not (cdsBlank in State) and DoCustomDrawYear(Canvas, Rect, State, TheYear) then
        begin
          RenderText(Canvas, Rect, State, GetYearText(TheYear), taCenter);
          if cdsFocused in State then
            Canvas.DrawFocusRect(Rect);
        end;
      end;
    end;
  end;
end;

procedure TCustomIntlMonthCalendar.InvalidateSections(Sections: TIntlCalSections);
var
  Section: TIntlCalSection;
begin
  if WindowHandle <> 0 then
  begin
    if (CurrentView <> vmDay) or not WeekNumbers then
    begin
      Exclude(Sections, csWeekSpace);
      Exclude(Sections, csWeekNumbers);
      if CurrentView <> vmDay then
        Exclude(Sections, csDaysOfWeek);
    end;
    if not ShowHeader then
    begin
      Exclude(Sections, csHeader);
      Exclude(Sections, csHeaderText);
      Exclude(Sections, csArrowLeft);
      Exclude(Sections, csArrowRight);
    end;
    if not ShowFooter then
      Exclude(Sections, csFooter);
    for Section := Low(TIntlCalSection) to High(TIntlCalSection) do
      if Section in Sections then
        InvalidateRect(WindowHandle, GetSectionRect(Section), False);
  end;
end;

procedure TCustomIntlMonthCalendar.InvalidateDateDependentSections;
begin
  InvalidateSections([csHeader, csWeekNumbers, csYMD, csFooter]);
end;

procedure TCustomIntlMonthCalendar.InvalidateCell(Index: Integer);
begin
  if (WindowHandle <> 0) and (Index >= 0) and (Index < CellCount) then
    InvalidateRect(WindowHandle, GetCellRect(Index), False);
end;

procedure TCustomIntlMonthCalendar.InvalidateDate(const ADate: TDate);
var
  TheYear: Integer;
begin
  if WindowHandle <> 0 then
    case CurrentView of
      vmDay:
        InvalidateCell(Trunc(ADate) - Trunc(FirstCellDate));
      vmMonth:
        InvalidateCell(Calendar.MonthOfYear(ADate) - 1);
      vmYear:
      begin
        TheYear := Calendar.ConvertYear(Calendar.YearOf(ADate), Calendar.DefaultEra, Era);
        InvalidateCell(TheYear - FirstCellYear);
      end;
    end;
end;

procedure TCustomIntlMonthCalendar.InvalidateDates(ADates: TDateTimeList);
var
  I, Index: Integer;
begin
  if (WindowHandle <> 0) and (CurrentView = vmDay) and (ADates.Count <> 0) then
  begin
    ADates.Find(FirstCellDate, I);
    for I := I to fSelection.Count - 1 do
    begin
      Index := Trunc(ADates[I]) - Trunc(FirstCellDate);
      if Index >= CellCount then
        Exit;
      InvalidateCell(Index);
    end;
  end;
end;

procedure TCustomIntlMonthCalendar.InvalidateDateRange(const ADate1, ADate2: TDate);
var
  Index: Integer;
  iStart, iEnd: Integer;
begin
  if (WindowHandle <> 0) and (CurrentView = vmDay) then
  begin
    iStart := Trunc(ADate1) - Trunc(FirstCellDate);
    iEnd := Trunc(ADate2) - Trunc(FirstCellDate);
    if iStart > iEnd then
    begin
      Index := iStart;
      iStart := iEnd;
      iEnd := Index;
    end;
    if iStart < 0 then
      iStart := 0;
    if iEnd >= CellCount then
      iEnd := CellCount - 1;
    for Index := iStart to iEnd do
      InvalidateCell(Index);
  end;
end;

procedure TCustomIntlMonthCalendar.InvalidateSelection;
begin
  case CurrentView of
    vmDay:
    begin
      case SelectionStyle of
        ssSingle:
          InvalidateCell(MonthStartCell + Day - 1);
        ssRange:
          InvalidateDateRange(Date, fSelectionAnchor);
        ssFree:
        begin
          InvalidateDates(fSelection);
          if not fSelection.Exists(Date) then
            InvalidateCell(MonthStartCell + Day - 1);
        end;
      end;
    end;
    vmMonth: InvalidateCell(Month - 1);
    vmYear: InvalidateCell(Year - FirstCellYear);
  end;
end;

procedure TCustomIntlMonthCalendar.InvalidateHotLight;
begin
  case HotInfo of
    chtYear, chtMonth, chtDay:
      InvalidateCell(HotCell);
    chtHeaderText:
      InvalidateSections([csHeaderText]);
    chtArrowLeft:
      InvalidateSections([csArrowLeft]);
    chtArrowRight:
      InvalidateSections([csArrowRight]);
    chtFooter:
      InvalidateSections([csFooter]);
  end;
end;

procedure TCustomIntlMonthCalendar.CalcMetrics(ViewMode: TIntlCalViewMode;
  out Metrics: TIntlCalMetrics);
const
  HSpacing = 4;
  VSpacing = 2;
var
  DC: HDC;
  OldFont: HFONT;
  NameKind: TIntlCalNameKind;
  DoW: TDayOfWeek;
  Extends: array[TIntlCalNameKind] of TSize;
  Extend: TSize;
  W, H, M: Integer;
  S: String;
begin
  if (csLoading in ComponentState) or (WindowHandle = 0) or (fUpdateMetricsCount <> 0) then
    Exit;
  Inc(fUpdateMetricsCount);
  DC := CreateCompatibleDC(0);
  try
    OldFont := SelectObject(DC, Font.Handle);
    try
      Metrics.Bounds := GetCalendarClientRect;
      Metrics.HeaderHeight := 0;
      Metrics.FooterHeight := 0;
      Metrics.FixedRowHeight := 0;
      Metrics.FixedColWidth := 0;
      W := Metrics.Bounds.Right - Metrics.Bounds.Left;
      H := Metrics.Bounds.Bottom - Metrics.Bounds.Top;
      if ShowHeader then
      begin
        S := GetHeaderText(ViewMode);
        SelectObject(DC, FontHeader.Handle);
        GetTextExtentPoint32(DC, PChar(S), Length(S), Extend);
        SelectObject(DC, Font.Handle);
        Metrics.HeaderHeight := Max(3 * Extend.cy div 2, 13) + VSpacing;
        if not Odd(Metrics.HeaderHeight) then
          Inc(Metrics.HeaderHeight);
        Dec(H, Metrics.HeaderHeight);
      end;
      if ShowFooter then
      begin
        Metrics.LongFormat := True;
        SelectObject(DC, FontFooter.Handle);
        S := GetFooterText(Metrics.LongFormat);
        GetTextExtentPoint32(DC, PChar(S), Length(S), Extend);
        if Extend.cx > 75 * W div 100 then
        begin
          Metrics.LongFormat := False;
          S := GetFooterText(Metrics.LongFormat);
          GetTextExtentPoint32(DC, PChar(S), Length(S), Extend);
        end;
        SelectObject(DC, Font.Handle);
        Metrics.FooterHeight := Max(3 * Extend.cy div 2, 13) + VSpacing;
        if not Odd(Metrics.FooterHeight) then
          Inc(Metrics.FooterHeight);
        Dec(H, Metrics.FooterHeight);
      end;
      case ViewMode of
        vmDay:
        begin
          Metrics.ColCount := Calendar.DaysPerWeek;
          Metrics.RowCount := Ceil(Ceil(DaysInYear / MonthsInYear) / Metrics.ColCount) + 1;
          if WeekNumbers then
          begin
            S := GetWeekText(Calendar.WeeksInYear(Year));
            SelectObject(DC, FontWeekNumbers.Handle);
            GetTextExtentPoint32(DC, PChar(S), Length(S), Extend);
            SelectObject(DC, Font.Handle);
            Metrics.FixedColWidth := Extend.cx + 3 * HSpacing div 2 + 2;
            Dec(W, Metrics.FixedColWidth);
          end;
          S := GetDayText(Ceil(DaysInYear / MonthsInYear));
          GetTextExtentPoint32(DC, PChar(S), Length(S), Extend);
          for NameKind := Low(TIntlCalNameKind) to High(TIntlCalNameKind) do
            Extends[NameKind] := Extend;
          SelectObject(DC, FontDayOfWeek.Handle);
          for DoW := Low(TDayOfWeek) to High(TDayOfWeek) do
          begin
            for NameKind := nkLong to nkShortest do
            begin
              S := GetDayName(DoW, NameKind);
              GetTextExtentPoint32(DC, PChar(S), Length(S), Extend);
              if Extend.cx > Extends[NameKind].cx then
                Extends[NameKind].cx := Extend.cx;
              if Extend.cy > Extends[NameKind].cy then
                Extends[NameKind].cy := Extend.cy;
            end;
          end;
          SelectObject(DC, Font.Handle);
          Metrics.NameKind := nkShortest;
          for NameKind := nkLong to nkShortest do
          begin
            if (Extends[NameKind].cx + HSpacing) * Metrics.ColCount <= W then
            begin
              Metrics.NameKind := NameKind;
              Break;
            end;
          end;
          Metrics.FixedRowHeight := Extends[Metrics.NameKind].cy + VSpacing + 2;
          Dec(H, Metrics.FixedRowHeight);
          Metrics.ColWidth := W div Metrics.ColCount;
          Metrics.RowHeight := H div Metrics.RowCount;
          Metrics.StretchedCols := W - Metrics.ColWidth * Metrics.ColCount;
          Metrics.StretchedRows := H - Metrics.RowHeight * Metrics.RowCount;
          Metrics.PrefferedWidth := Metrics.FixedColWidth + (Extends[Metrics.NameKind].cx + HSpacing) * Metrics.ColCount;
          Metrics.PrefferedHeight := Metrics.FixedRowHeight + (Extends[Metrics.NameKind].cy + 2 * VSpacing) * Metrics.RowCount;
        end;
        vmMonth:
        begin
          Metrics.ColCount := 3;
          Metrics.RowCount := Ceil(MonthsInYear / Metrics.ColCount);
          Metrics.ColWidth := W div Metrics.ColCount;
          Metrics.RowHeight := H div Metrics.RowCount;
          Metrics.StretchedRows := H - (Metrics.RowHeight * Metrics.RowCount);
          Metrics.StretchedCols := W - (Metrics.ColWidth * Metrics.ColCount);
          FillChar(Extends[nkLong], SizeOf(Extend) * 2, 0);
          for M := 1 to Calendar.MaxMonthsPerYear do
            for NameKind := nkLong to nkShort do
            begin
              S := GetMonthName(M, NameKind);
              GetTextExtentPoint32(DC, PChar(S), Length(S), Extend);
              if Extend.cx > Extends[NameKind].cx then
                Extends[NameKind].cx := Extend.cx;
              if Extend.cy > Extends[NameKind].cy then
                Extends[NameKind].cy := Extend.cy;
            end;
          Metrics.NameKind := nkShort;
          for NameKind := nkLong to nkShort do
          begin
            if (Extends[NameKind].cx + HSpacing) * Metrics.ColCount <= W then
            begin
              Metrics.NameKind := NameKind;
              Break;
            end;
          end;
          Metrics.PrefferedWidth := (Extends[Metrics.NameKind].cx + HSpacing) * Metrics.ColCount;
          Metrics.PrefferedHeight := (Extends[Metrics.NameKind].cy + VSpacing) * Metrics.RowCount;
        end;
        vmYear:
        begin
          Metrics.ColCount := 4;
          Metrics.RowCount := 3;
          Metrics.ColWidth := W div Metrics.ColCount;
          Metrics.RowHeight := H div Metrics.RowCount;
          Metrics.StretchedRows := H - (Metrics.RowHeight * Metrics.RowCount);
          Metrics.StretchedCols := W - (Metrics.ColWidth * Metrics.ColCount);
          S := GetYearText(Calendar.YearOf(Calendar.MaxSupportedDateTime));
          GetTextExtentPoint32(DC, PChar(S), Length(S), Extend);
          Metrics.PrefferedWidth := (Extend.cx + HSpacing) * Metrics.ColCount;
          Metrics.PrefferedHeight := (Extend.cy + VSpacing) * Metrics.RowCount;
        end;
      end;
      Inc(Metrics.PrefferedHeight, Metrics.HeaderHeight);
      Inc(Metrics.PrefferedHeight, Metrics.FooterHeight);
    finally
      SelectObject(DC, OldFont);
    end;
  finally
    DeleteObject(DC);
    Dec(fUpdateMetricsCount);
  end;
end;

procedure TCustomIntlMonthCalendar.CalcMetricsSections(var Metrics: TIntlCalMetrics);
var
  Sec: TIntlCalSection;
begin
  if (csLoading in ComponentState) or (WindowHandle = 0) or (fUpdateMetricsCount <> 0) then
    Exit;
  for Sec := Low(TIntlCalSection) to High(TIntlCalSection) do
    with fMetrics, Sections[Sec] do
    begin
      Sections[Sec] := Bounds;
      case Sec of
        csHeader:
        begin
          Bottom := Top + HeaderHeight;
        end;
        csHeaderText:
        begin
          Inc(Left, HeaderHeight);
          Dec(Right, HeaderHeight);
          Bottom := Top + HeaderHeight;
        end;
        csArrowLeft:
        begin
          Right := Left + HeaderHeight;
          Bottom := Top + HeaderHeight;
        end;
        csArrowRight:
        begin
          Left := Right - HeaderHeight;
          Bottom := Top + HeaderHeight;
        end;
        csFooter:
        begin
          Top := Bottom - FooterHeight;
        end;
        csWeekSpace:
        begin
          Inc(Top, HeaderHeight);
          if IsRightToLeft then
            Left := Right - FixedColWidth
          else
            Right := Left + FixedColWidth;
          Bottom := Top + FixedRowHeight;
        end;
        csDaysOfWeek:
        begin
          Inc(Top, HeaderHeight);
          if IsRightToLeft then
            Dec(Right, FixedColWidth)
          else
            Inc(Left, FixedColWidth);
          Bottom := Top + FixedRowHeight;
        end;
        csWeekNumbers:
        begin
          Inc(Top, HeaderHeight);
          Inc(Top, FixedRowHeight);
          if IsRightToLeft then
            Left := Right - FixedColWidth
          else
            Right := Left + FixedColWidth;
          Dec(Bottom, FooterHeight);
        end;
        csYMD:
        begin
          Inc(Top, HeaderHeight);
          Inc(Top, FixedRowHeight);
          if IsRightToLeft then
            Dec(Right, FixedColWidth)
          else
            Inc(Left, FixedColWidth);
          Dec(Bottom, FooterHeight);
        end;
      end;
    end;
end;

procedure TCustomIntlMonthCalendar.UpdateMetrics;
begin
  if fUpdateMetricsCount = 0 then
  begin
    fPrefferedSizeReady := False;
    CalcMetrics(CurrentView, fMetrics);
    CalcMetricsSections(fMetrics);
    if not (csLoading in ComponentState) then
      AdjustSize;
    Invalidate;
  end;
end;

procedure TCustomIntlMonthCalendar.UpdateMinMaxRanges;
var
  I: Integer;
begin
  if Calendar.IsNoDate(DateMin) or
    (Calendar.CompareDate(DateMin, Calendar.MinSupportedDateTime) < 0)
  then
    fActualDateMin := Trunc(Calendar.MinSupportedDateTime)
  else
    fActualDateMin := DateMin;
  if Calendar.IsNoDate(DateMax) or
    (Calendar.CompareDate(DateMax, Calendar.MaxSupportedDateTime) > 0)
  then
    fActualDateMax := Trunc(Calendar.MaxSupportedDateTime)
  else
    fActualDateMax := DateMax;
  if not (csLoading in ComponentState) then
  begin
    case SelectionStyle of
      ssRange:
        fSelectionAnchor := EnsureDateInRange(fSelectionAnchor);
      ssFree:
      begin
        fSelection.Find(ActualDateMin, I);
        if I = fSelection.Count then
          fSelection.Clear
        else
        begin
          for I := I - 1 downto 0 do
            fSelection.Delete(I);
          if fSelection.Find(ActualDateMax, I) then
            Inc(I);
          if I = 0 then
            fSelection.Clear
          else
          begin
            for I := fSelection.Count - 1 downto I do
              fSelection.Delete(I);
          end;
        end;
      end;
    end;
    FocusDate(EnsureDateInRange(Date));
    InvalidateDateDependentSections;
  end;
end;

procedure TCustomIntlMonthCalendar.UpdateCalendar;
begin
  Inc(fUpdateMetricsCount);
  try
    if Assigned(Culture) then
      BiDiMode := Culture.BiDiMode
    else
      ParentBiDiMode := True;
    UpdateMinMaxRanges;
    ExtractDateInfo(EnsureDateInRange(Date));
  finally
    Dec(fUpdateMetricsCount);
  end;
  UpdateMetrics;
end;

procedure TCustomIntlMonthCalendar.ValidateCalendar;

  procedure InitNewCalendar;
  begin
    fCalendar.OnChange := CalendarChanged;
    UpdateCalendar;
    DoCalendarChange;
  end;

var
  Reference: TCalendar;
begin
  if csLoading in ComponentState then
    Exit;
  if not Assigned(CalendarType) then
  begin
    if UsingCultureCalendar then
      Reference := Culture.NativeCalendar
    else
      Reference := DefaultCalendar;
    if Reference.ClassType <> Calendar.ClassType then
    begin
      FreeAndNil(fCalendar);
      fCalendar := Reference.Clone;
      InitNewCalendar;
    end
    else
      fCalendar.Assign(Reference);
  end
  else if CalendarType <> Calendar.ClassType then
  begin
    FreeAndNil(fCalendar);
    if Assigned(Culture) then
      fCalendar := CalendarType.Create(Culture.Locale)
    else
      fCalendar := CalendarType.Create(LOCALE_USER_DEFAULT);
    InitNewCalendar;
  end
  else if Assigned(Culture) then
    fCalendar.Settings.Prepare(Culture.Locale)
  else
    fCalendar.Settings.Prepare(LOCALE_USER_DEFAULT);
end;

procedure TCustomIntlMonthCalendar.ExtractDateInfo(const ADate: TDate);
begin
  if csLoading in ComponentState then
    Exit;
  fDate := ADate;
  Calendar.DecodeDate(Date, fEra, fYear, fMonth, fDay);
  fMonthsInYear := Calendar.MonthsInYear(Era, Year);
  fDaysInYear := Calendar.DaysInYear(Era, Year);
  fDaysInMonth := Calendar.DaysInMonth(Era, Year, Month);
  fMonthFirstDate := Date - Day + 1;
  fMonthFirstDayOfWeek := Calendar.DayOfWeek(MonthFirstDate);
  fMonthStartCell := Calendar.DaysOfWeekBetween(MonthFirstDayOfWeek, Calendar.Settings.FirstDayOfWeek);
  if fMonthStartCell = 0 then Inc(fMonthStartCell, Calendar.DaysPerWeek);
  fFirstCellDayNumber := Calendar.DayOfMonth(FirstCellDate);
  fFirstCellYear := Calendar.OffsetYear(Era, Year, -(Year mod 10) - 1);
end;

procedure TCustomIntlMonthCalendar.FocusDate(const ADate: TDate);
var
  OldEra, OldYear, OldMonth, OldDay: Integer;
  OldMonthsInYear: Integer;
begin
  if (csLoading in ComponentState) or (ADate = Date) then
    Exit;
  OldDay := Day;
  fDay := Trunc(ADate) - Trunc(MonthFirstDate) + 1;
  if (Day >= 1) and (Day <= DaysInMonth) then
  begin
    fDate := ADate;
    if CurrentView = vmDay then
    begin
      InvalidateCell(MonthStartCell + OldDay - 1);
      InvalidateCell(MonthStartCell + Day - 1);
    end;
  end
  else
  begin
    OldEra := Era; OldYear := Year; OldMonth := Month; fDay := OldDay;
    OldMonthsInYear := MonthsInYear;
    AnimateBegin;
    try
      ExtractDateInfo(ADate);
      case CurrentView of
        vmDay:
        begin
          AnimatePlay;
          DoPageChange;
          InvalidateDateDependentSections;
        end;
        vmMonth:
        begin
          if (Era <> OldEra) or (Year <> OldYear) then
          begin
            if OldMonthsInYear <> MonthsInYear then
              CalcMetrics(CurrentView, fMetrics);
            AnimatePlay;
            DoPageChange;
            InvalidateDateDependentSections;
          end
          else
          begin
            InvalidateCell(OldMonth - 1);
            InvalidateCell(Month - 1);
          end;
        end;
        vmYear:
        begin
          if (Era <> OldEra) or ((OldYear div 10) <> (Year div 10)) then
          begin
            AnimatePlay;
            DoPageChange;
            InvalidateDateDependentSections;
          end
          else
          begin
            InvalidateCell(OldYear - FirstCellYear);
            InvalidateCell(Year - FirstCellYear);
          end;
        end;
      end;
    finally
      AnimateEnd;
    end;
  end;
  DoDateChange;
end;

function TCustomIntlMonthCalendar.GetDateAt(X, Y: Integer;
  out ADate: TDate): Boolean;
var
  Cell: Integer;
begin
  Result := (GetHitTestInfoAt(X, Y, Cell) = chtDay) and GetDateAtCell(Cell, ADate);
end;

function TCustomIntlMonthCalendar.GetDateAtCell(Index: Integer;
  out ADate: TDate): Boolean;
begin
  Result := False;
  if (CurrentView = vmDay) and (Index >= 0) and (Index < CellCount) then
  begin
    ADate := FirstCellDate + Index;
    Result := IsDateInRange(ADate);
  end;
end;

function TCustomIntlMonthCalendar.GetMonthAt(X, Y: Integer;
  out AMonth: Integer): Boolean;
var
  Cell: Integer;
begin
  Result := (GetHitTestInfoAt(X, Y, Cell) = chtMonth) and GetMonthAtCell(Cell, AMonth);
end;

function TCustomIntlMonthCalendar.GetMonthAtCell(Index: Integer;
  out AMonth: Integer): Boolean;
begin
  Result := False;
  if (CurrentView = vmMonth) and (Index >= 0) and (Index < CellCount) then
  begin
    AMonth := Index + 1;
    Result := IsMonthInRange(Era, Year, AMonth);
  end;
end;

function TCustomIntlMonthCalendar.GetYearAt(X, Y: Integer;
  out AYear: Integer): Boolean;
var
  Cell: Integer;
begin
  Result := (GetHitTestInfoAt(X, Y, Cell) = chtYear) and GetYearAtCell(Cell, AYear);
end;

function TCustomIntlMonthCalendar.GetYearAtCell(Index: Integer;
  out AYear: Integer): Boolean;
begin
  Result := False;
  if (CurrentView = vmYear) and (Index >= 0) and (Index < CellCount) then
  begin
    AYear := Calendar.OffsetYear(FirstCellYear, Index);
    Result := IsYearInRange(Era, AYear);
  end;
end;

procedure TCustomIntlMonthCalendar.SelectDate(const ADate: TDate;
  ShiftState: TShiftState);
var
  I, Dir: Integer;
  AnchorDate, TheDate: TDate;
  Changed: Boolean;
begin
  Changed := False;
  AnchorDate := EnsureDateInRange(ADate);
  case SelectionStyle of
    ssSingle:
      Changed := (fDate <> AnchorDate);
    ssRange:
      if (ssLeft in ShiftState) and (fSelectionAnchor <> AnchorDate) then
      begin
        if not (ssShift in ShiftState) then
          fSelectionAnchor := AnchorDate;
        InvalidateDateRange(fSelectionAnchor, AnchorDate);
        Changed := True;
      end;
   else // ssFree
    if not (ssCtrl in ShiftState) and (fSelection.Count <> 0) then
    begin
      InvalidateDates(fSelection);
      fSelection.Clear;
      Changed := True;
    end;
    if ssLeft in ShiftState then
    begin
      if ssShift in ShiftState then
      begin
        if AnchorDate < fSelectionAnchor then
          Dir := +1
        else
          Dir := -1;
        TheDate := AnchorDate;
        for I := 0 to Calendar.DaysBetween(fSelectionAnchor, AnchorDate) do
        begin
          if not fSelection.Exists(TheDate) and DoDateSelectable(TheDate) then
          begin
            fSelection.Add(TheDate);
            InvalidateDate(TheDate);
            Changed := True;
          end;
          Calendar.IncDay(TheDate, Dir);
        end;
      end
      else if ssCtrl in ShiftState then
      begin
        if fSelection.Remove(AnchorDate) >= 0 then
        begin
          InvalidateDate(AnchorDate);
          Changed := True;
        end
        else if DoDateSelectable(AnchorDate) then
        begin
          fSelection.Add(AnchorDate);
          InvalidateDate(AnchorDate);
          Changed := True;
        end;
        fSelectionAnchor := AnchorDate;
      end
      else
      begin
        if not fSelection.Exists(AnchorDate) and DoDateSelectable(AnchorDate) then
        begin
          InvalidateDate(AnchorDate);
          fSelection.Add(AnchorDate);
          Changed := True;
        end;
        fSelectionAnchor := AnchorDate;
      end;
    end;
  end; // case
  FocusDate(AnchorDate);
  if Changed then
    DoSelectionChange;
end;

procedure TCustomIntlMonthCalendar.Select(const ADate: TDate;
  ShiftState: TShiftState);
begin
  SelectDate(ADate, ShiftState + [ssLeft]);
end;

procedure TCustomIntlMonthCalendar.ClearSelection;
begin
  case SelectionStyle of
    ssRange:
    begin
      if fSelectionAnchor <> Date then
      begin
        InvalidateDateRange(fSelectionAnchor, Date);
        fSelectionAnchor := Date;
        DoSelectionChange;
      end;
    end;
    ssFree:
    begin
      if fSelection.Count <> 0 then
      begin
        InvalidateDates(fSelection);
        fSelection.Clear;
        DoSelectionChange;
      end;
    end;
  end;
end;

function TCustomIntlMonthCalendar.IsSelected(const ADate: TDate): Boolean;
begin
  case SelectionStyle of
    ssSingle:
      Result := Calendar.IsSameDay(ADate, Date);
    ssRange:
    begin
      case Calendar.CompareDate(ADate, Date) of
        -1: Result := (Calendar.CompareDate(ADate, fSelectionAnchor) >= 0);
        +1: Result := (Calendar.CompareDate(ADate, fSelectionAnchor) <= 0);
      else
        Result := True;
      end;
    end
  else
    Result := fSelection.Exists(Trunc(ADate));
  end;
end;

function TCustomIntlMonthCalendar.IsYearInRange(AEra, AYear: Integer): Boolean;
begin
  Result := Calendar.IsValidYear(AEra, AYear) and
    (IsDateInRange(Calendar.StartOfYear(AEra, AYear)) or
     IsDateInRange(Calendar.EndOfYear(AEra, AYear)));
end;

function TCustomIntlMonthCalendar.IsYearInRange(AYear: Integer): Boolean;
begin
  Result := IsYearInRange(Era, AYear);
end;

function TCustomIntlMonthCalendar.IsMonthInRange(AEra, AYear, AMonth: Integer): Boolean;
begin
  Result := Calendar.IsValidMonth(AEra, AYear, AMonth) and
    (IsDateInRange(Calendar.StartOfMonth(AEra, AYear, AMonth)) or
     IsDateInRange(Calendar.EndOfMonth(AEra, AYear, AMonth)));
end;

function TCustomIntlMonthCalendar.IsMonthInRange(AYear, AMonth: Integer): Boolean;
begin
  Result := IsMonthInRange(Era, AYear, AMonth);
end;

function TCustomIntlMonthCalendar.IsDateInRange(const ADate: TDate): Boolean;
begin
  Result := Calendar.IsInRange(Trunc(ADate), ActualDateMin, ActualDateMax);
end;

function TCustomIntlMonthCalendar.EnsureDateInRange(const ADate: TDate): TDate;
begin
  if IsDateInRange(ADate) then
    Result := Trunc(ADate)
  else if Calendar.CompareDate(ADate, ActualDateMax) > 0 then
    Result := ActualDateMax
  else
    Result := ActualDateMin;
end;

function TCustomIntlMonthCalendar.Format(const FmtStr: String;
  const ADate: TDateTime): String;
begin
  Result := Calendar.Format(FmtStr, ADate);
  NormalizeDigits(Result);
end;

{ TIntlMonthCalendarPopup }

constructor TIntlMonthCalendarPopup.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  TabStop := False;
  ShowFooter := True;
  ParentColor := True;
  BevelKind := bkNone;
  Visible := False;
  Parent := AOwner;
end;

procedure TIntlMonthCalendarPopup.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TIntlMonthCalendarPopup.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  CurrentView := vmDay;
  if not Visible and Assigned(Parent) then
    Parent.Invalidate;
end;

procedure TIntlMonthCalendarPopup.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Offset: Integer;
begin
  if (Button = mbLeft) and (GetHitTestInfoAt(X, Y, Offset) in [chtDay, chtFooter]) then
    DoCloseUp(True);
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TIntlMonthCalendarPopup.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  if Key in [VK_RETURN, VK_ESCAPE] then
  begin
    DoCloseUp(Key = VK_RETURN);
    Key := 0;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TIntlMonthCalendarPopup.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS or CS_DROPSHADOW;
  end;
end;

function TIntlMonthCalendarPopup.Focused: Boolean;
begin
  Result := Parent.Focused;
end;

procedure TIntlMonthCalendarPopup.Show(X, Y: Integer);
begin
  SetWindowPos(Handle, HWND_TOP, X, Y, 0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  Visible := True;
end;

procedure TIntlMonthCalendarPopup.DoCloseUp(Accept: Boolean);
begin
  if Assigned(OnCloseUp) then
    OnCloseUp(Self, Accept);
end;

{ TCustomIntlDatePicker }

constructor TCustomIntlDatePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque] - [csSetCaption];
  fCalendarPopup := TIntlMonthCalendarPopup.Create(Self);
  fCalendarPopup.SetSubComponent(True);
  fCalendarPopup.Name := 'CalendarPopup';
  fCalendarPopup.OnCloseUp := PopupCloseUp;
  fCalendarPopup.OnCalendarChange := CalendarChanged;
  fDate := Calendar.Today;
  fBorderStyle := bsSingle;
  fShowCheckBox := False;
  fChecked := True;
  ParentColor := False;
  Color := clWindow;
  TabStop := True;
  AutoSize := True;
  UpdateText;
end;

destructor TCustomIntlDatePicker.Destroy;
begin
  if fTheme <> 0 then
    CloseThemeData(fTheme);
  inherited Destroy;
end;

procedure TCustomIntlDatePicker.CalendarChanged(Sender: TObject);
begin
  UpdateText;
end;

procedure TCustomIntlDatePicker.PopupCloseUp(Sender: TObject; Accept: Boolean);
begin
  if Accept then
    Date := CalendarPopup.Date;
  CalendarPopup.Visible := False;
end;

procedure TCustomIntlDatePicker.SetDate(const Value: TDate);
begin
  if (csDesigning in ComponentState) and Calendar.IsNoDate(Value) then
  begin
    Date := Calendar.Today;
    Exit;
  end;
  if not IsDateInRange(Value) then
  begin
    raise EDateTimeError.CreateResFmt(@SDateOutOfRangeError,
      [Calendar.Format('ddddd', ActualDateMin),
       Calendar.Format('ddddd', ActualDateMax)]);
  end;
  if not Calendar.IsSameDay(Date, Value) and CanModify then
  begin
    fDate := Calendar.DateOf(Value);
    UpdateText;
    Change;
  end;
end;

procedure TCustomIntlDatePicker.SetDateFormat(const Value: String);
begin
  if DateFormat <> Value then
  begin
    fDateFormat := Value;
    UpdateText;
  end;
end;

procedure TCustomIntlDatePicker.SetChecked(Value: Boolean);
begin
  if (Checked <> Value) and CanModify then
  begin
    fChecked := Value;
    if not Checked and CalendarPopup.Visible then
      PopupCloseUp(CalendarPopup, False);
    InvalidateCheckBox;
    InvalidateDate;
    Change;
  end;
end;

procedure TCustomIntlDatePicker.SetShowCheckBox(Value: Boolean);
begin
  if ShowCheckBox <> Value then
  begin
    fShowCheckBox := Value;
    UpdateRects;
  end;
end;

procedure TCustomIntlDatePicker.SetBorderStyle(Value: TBorderStyle);
begin
  if BorderStyle <> Value then
  begin
    fBorderStyle := Value;
    RecreateWnd;
  end;
end;

function TCustomIntlDatePicker.GetDateMin: TDate;
begin
  Result := CalendarPopup.DateMin;
end;

procedure TCustomIntlDatePicker.SetDateMin(const Value: TDate);
begin
  CalendarPopup.DateMin := Value;
end;

function TCustomIntlDatePicker.GetDateMax: TDate;
begin
  Result := CalendarPopup.DateMax;
end;

procedure TCustomIntlDatePicker.SetDateMax(const Value: TDate);
begin
  CalendarPopup.DateMax := Value;
end;

function TCustomIntlDatePicker.GetCalendarType: TCalendarClass;
begin
  Result := CalendarPopup.CalendarType;
end;

procedure TCustomIntlDatePicker.SetCalendarType(Value: TCalendarClass);
begin
  CalendarPopup.CalendarType := Value;
end;

function TCustomIntlDatePicker.GetCulture: TCultureInfo;
begin
  Result := CalendarPopup.Culture;
end;

procedure TCustomIntlDatePicker.SetCulture(Value: TCultureInfo);
begin
  CalendarPopup.Culture := Value;
end;

function TCustomIntlDatePicker.GetCultureCalendar: TLocalizerSwicth;
begin
  Result := CalendarPopup.CultureCalendar;
end;

procedure TCustomIntlDatePicker.SetCultureCalendar(Value: TLocalizerSwicth);
begin
  CalendarPopup.CultureCalendar := Value;
end;

function TCustomIntlDatePicker.GetCultureDigits: TLocalizerSwicth;
begin
  Result := CalendarPopup.CultureDigits;
end;

procedure TCustomIntlDatePicker.SetCultureDigits(Value: TLocalizerSwicth);
begin
  CalendarPopup.CultureDigits := Value;
  UpdateText;
end;

function TCustomIntlDatePicker.GetLocalizer: TLocalizer;
begin
  Result := CalendarPopup.Localizer;
end;

procedure TCustomIntlDatePicker.SetLocalizer(Value: TLocalizer);
begin
  CalendarPopup.Localizer := Value;
end;

function TCustomIntlDatePicker.GetCalendar: TCalendar;
begin
  Result := CalendarPopup.Calendar;
end;

function TCustomIntlDatePicker.GetActualDateMin: TDate;
begin
  Result := CalendarPopup.ActualDateMin;
end;

function TCustomIntlDatePicker.GetActualDateMax: TDate;
begin
  Result := CalendarPopup.ActualDateMax;
end;

function TCustomIntlDatePicker.GetUsingCultureCalendar: Boolean;
begin
  Result := CalendarPopup.UsingCultureCalendar;
end;

function TCustomIntlDatePicker.GetUsingCultureDigits: Boolean;
begin
  Result := CalendarPopup.UsingCultureDigits;
end;

function TCustomIntlDatePicker.GetTheme: HTHEME;
begin
  if fTheme = 0 then
    fTheme := OpenThemeData(WindowHandle, 'datepicker');
  Result := fTheme;
end;

function TCustomIntlDatePicker.IsDateStored: Boolean;
begin
  Result := not Calendar.IsToday(Date);
end;

function TCustomIntlDatePicker.IsDateMinStored: Boolean;
begin
  Result := not Calendar.IsNoDate(DateMin);
end;

function TCustomIntlDatePicker.IsDateMaxStored: Boolean;
begin
  Result := not Calendar.IsNoDate(DateMax);
end;

procedure TCustomIntlDatePicker.ReadCulture(Reader: TReader);
begin
  Culture := CultureOf(Reader.ReadString);
end;

procedure TCustomIntlDatePicker.WriteCulture(Writer: TWriter);
begin
  Writer.WriteString(Culture.Locale);
end;

procedure TCustomIntlDatePicker.ReadCalendarType(Reader: TReader);
begin
  CalendarType := CalendarTypes.ByName(Reader.ReadString);
end;

procedure TCustomIntlDatePicker.WriteCalendarType(Writer: TWriter);
begin
  Writer.WriteString(CalendarType.CalendarName);
end;

procedure TCustomIntlDatePicker.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('CultureInfo', ReadCulture, WriteCulture,
    not Assigned(Localizer) and Assigned(Culture));
  Filer.DefineProperty('CalendarClass', ReadCalendarType, WriteCalendarType,
    Assigned(CalendarType));
end;

procedure TCustomIntlDatePicker.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if BorderStyle = bsSingle then
  begin
    if Ctl3D then
      Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE
    else
      Params.Style := Params.Style or WS_BORDER;
  end;
end;

procedure TCustomIntlDatePicker.Resize;
begin
  inherited Resize;
  UpdateRects;
end;

function TCustomIntlDatePicker.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if AutoSize then
    NewHeight := GetMinHeight;
  Result := True;
end;

procedure TCustomIntlDatePicker.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    case HotInfo of
      phtCheckBox:
      begin
        fCheckPressed := True;
        InvalidateCheckBox;
      end;
      phtDropDown, phtDate:
      begin
        if CalendarPopup.Visible then
          PopupCloseUp(CalendarPopup, False)
        else
          PopupDropDown;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomIntlDatePicker.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  TheHitTest: TIntlPickerHitTest;
begin
  TheHitTest := GetHitTestInfoAt(X, Y);
  if TheHitTest <> HotInfo then
  begin
    case HotInfo of
      phtCheckBox:
        InvalidateCheckBox;
      phtDropDown:
        InvalidateDropDown;
    end;
    fHotInfo := TheHitTest;
    case HotInfo of
      phtCheckBox:
        InvalidateCheckBox;
      phtDropDown:
        InvalidateDropDown;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomIntlDatePicker.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if CheckPressed then
    begin
      if HotInfo = phtCheckBox then
        Checked := not Checked
      else
        InvalidateCheckBox;
    end;
    fCheckPressed := False;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TCustomIntlDatePicker.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    Date := EnsureDateInRange(Calendar.NextDay(Date));
    Result := True;
  end;
end;

function TCustomIntlDatePicker.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
  begin
    Date := EnsureDateInRange(Calendar.PrevDay(Date));
    Result := True;
  end;
end;

procedure TCustomIntlDatePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if CalendarPopup.Visible then
  begin
    CalendarPopup.KeyDown(Key, Shift);
    Exit;
  end;
  case Key of
    VK_DOWN:
    begin
      if ssAlt in Shift then
        PopupDropDown
      else
        Date := EnsureDateInRange(Calendar.NextDay(Date));
      Key := 0;
    end;
    VK_UP:
    begin
      Date := EnsureDateInRange(Calendar.PrevDay(Date));
      Key := 0;
    end;
    VK_NEXT:
    begin
      Date := EnsureDateInRange(Calendar.NextMonth(Date));
      Key := 0;
    end;
    VK_PRIOR:
    begin
      Date := EnsureDateInRange(Calendar.PrevMonth(Date));
      Key := 0;
    end;
    VK_SPACE:
    begin
      if ShowCheckBox then
        Checked := not Checked;
      Key := 0;
    end;
  end;
end;

function TCustomIntlDatePicker.CanModify: Boolean;
begin
  Result := True;
end;

procedure TCustomIntlDatePicker.Paint;
const
  CheckStates: array[Boolean] of TCheckBoxState = (cbUnchecked, cbChecked);
var
  DrawState: TPartDrawState;
  DrawFlags: Cardinal;
  TextRect, FocusRect: TRect;
  StateID: Integer;
begin
  // check box
  if ShowCheckBox and RectVisible(Canvas.Handle, fCheckBoxRect) then
  begin
    if Enabled then
      if HotInfo = phtCheckBox then
        if CheckPressed then
          DrawState := pdsPressed
        else
          DrawState := pdsHot
      else
        DrawState := pdsNormal
    else
      DrawState := pdsDisabled;
    fCheckBoxRect := DrawCheckBox(Canvas.Handle, fCheckBoxRect,
      CheckStates[Checked], DrawState, False, ThemeControl(Self));
  end;
  // drop down
  if RectVisible(Canvas.Handle, fDropDownRect) then
  begin
    if Enabled then
      if CalendarPopup.Visible then
        DrawState := pdsPressed
      else if HotInfo = phtDropDown then
        DrawState := pdsHot
      else
        DrawState := pdsNormal
    else
      DrawState := pdsDisabled;
    DrawDropDown(Canvas.Handle, fDropDownRect, DrawState, ThemeControl(Self));
  end;
  // content
  if RectVisible(Canvas.Handle, fDateRect) then
  begin
    Canvas.Font := Font;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(fDateRect);
    TextRect := fDateRect;
    InflateRect(TextRect, -2, 0);
    DrawFlags := DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or DT_NOPREFIX or DT_SINGLELINE);
    if CalendarPopup.IsThemed then
    begin
      if Checked and Enabled then
        StateId := DPDT_NORMAL
      else
        StateID := DPDT_DISABLED;
      DrawThemeText(Theme, Canvas.Handle, DP_DATETEXT, StateId, PChar(Caption), Length(Caption), DrawFlags, 0, TextRect);
    end
    else
    begin
      if not (Checked and Enabled) then
        Canvas.Font.Color := clGrayText;
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextRect, DrawFlags);
    end;
    if Focused and not CalendarPopup.Visible then
    begin
      FillChar(FocusRect, SizeOf(FocusRect), 0);
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), FocusRect, DrawFlags or DT_CALCRECT);
      OffsetRect(FocusRect, 0, (TextRect.Top + TextRect.Bottom - FocusRect.Bottom) div 2);
      if UseRightToLeftAlignment then
        OffsetRect(FocusRect, TextRect.Right - FocusRect.Right, 0)
      else
        OffsetRect(FocusRect, TextRect.Left, 0);
      InflateRect(FocusRect, 2, 4);
      SetTextColor(Canvas.Handle, ColorToRGB(Font.Color));
      Canvas.DrawFocusRect(FocusRect);
    end;
  end;
end;

procedure TCustomIntlDatePicker.UpdateText;
begin
  Caption := CalendarPopup.Format(DateFormat, Date);
  InvalidateDate;
end;

procedure TCustomIntlDatePicker.UpdateRects;
var
  Rect: TRect;
begin
  if not HandleAllocated then
    Exit;
  Rect := ClientRect;
  fDateRect := Rect;
  fDropDownRect := Rect;
  fCheckBoxRect := Rect;
  if UseRightToLeftAlignment then
  begin
    fDropDownRect.Right := fDropDownRect.Left + GetDropDownSize.cx;
    fDateRect.Left := fDropDownRect.Right + 2;
    if fShowCheckBox then
    begin
      fCheckBoxRect.Left := fCheckBoxRect.Right - GetCheckBoxSize.cx - 4;
      fDateRect.Right := fCheckBoxRect.Left - 2;
    end;
  end
  else
  begin
    fDropDownRect.Left := fDropDownRect.Right - GetDropDownSize.cx;
    fDateRect.Right := fDropDownRect.Left - 2;
    if fShowCheckBox then
    begin
      fCheckBoxRect.Right := fCheckBoxRect.Left + GetCheckBoxSize.cx + 4;
      fDateRect.Left := fCheckBoxRect.Right + 2;
    end;
  end;
  Invalidate;
end;

function TCustomIntlDatePicker.GetMinHeight: Integer;
var
  DC: HDC;
  OldFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(WindowHandle);
  try
    OldFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, OldFont);
  finally
    ReleaseDC(WindowHandle, DC);
  end;
  Result := Metrics.tmHeight + (Height - ClientHeight) + 4;
  if Result < GetDropDownSize.cy then
    Result := GetDropDownSize.cy;
  if ShowCheckBox and (Result < GetCheckBoxSize.cy) then
    Result := GetCheckBoxSize.cy;
end;

function TCustomIntlDatePicker.GetHitTestInfoAt(X, Y: Integer): TIntlPickerHitTest;
var
  Pt: TPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  if ShowCheckBox and PtInRect(fCheckBoxRect, Pt) then
    Result := phtCheckBox
  else if PtInRect(fDropDownRect, Pt) then
    Result := phtDropDown
  else if PtInRect(fDateRect, Pt) then
    Result := phtDate
  else if PtInRect(ClientRect, Pt) then
    Result := phtControl
  else
    Result := phtNowhere;
end;

procedure TCustomIntlDatePicker.InvalidateCheckBox;
begin
  if ShowCheckBox and HandleAllocated then
    InvalidateRect(WindowHandle, fCheckBoxRect, False);
end;

procedure TCustomIntlDatePicker.InvalidateDropDown;
begin
  if HandleAllocated then
    InvalidateRect(WindowHandle, fDropDownRect, False);
end;

procedure TCustomIntlDatePicker.InvalidateDate;
begin
  if HandleAllocated then
    InvalidateRect(WindowHandle, fDateRect, False);
end;

procedure TCustomIntlDatePicker.InvalidateFrame;
begin
  if ThemeControl(Self) and HandleAllocated then
    RedrawWindow(WindowHandle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
end;

procedure TCustomIntlDatePicker.PopupDropDown;
var
  TopLeft: TPoint;
  X, Y, W, H: Integer;
  Monitor: TMonitor;
begin
  if Enabled and CanModify and not CalendarPopup.Visible then
  begin
    Monitor := Screen.MonitorFromWindow(Handle);
    CalendarPopup.HandleNeeded;
    TopLeft := Parent.ClientToScreen(Point(Left, Top));
    CalendarPopup.GetPreferredControlSize(W, H);
    if W < Width then
      W := Width;
    if UseRightToLeftAlignment then
      X := TopLeft.X + Width - W
    else
      X := TopLeft.X;
    if X < Monitor.Left then
      X := Monitor.Left
    else if X + W > Monitor.Left + Monitor.Width then
      X := Monitor.Left + Monitor.Width - W;
    Y := TopLeft.Y + Height;
    if Y + H > Monitor.Top + Monitor.Height then
      Y := TopLeft.Y - H;
    CalendarPopup.Date := Date;
    Checked := True;
    CalendarPopup.SetBounds(X, Y, W, H);
    CalendarPopup.Show(X, Y);
    InvalidateDropDown;
    InvalidateDate;
  end;
end;

procedure TCustomIntlDatePicker.Change;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCustomIntlDatePicker.WMNCPaint(var Message: TWMNCPaint);
var
  DC: HDC;
  StateId: Integer;
  Rect: TRect;
begin
  inherited;
  if (BorderStyle = bsSingle) and ThemeControl(Self) then
  begin
    Rect := BoundsRect;
    OffsetRect(Rect, -Rect.Left, -Rect.Top);
    if Enabled then
      if Focused then
        StateId := DPDB_FOCUSED
      else if HotInfo <> phtNowhere then
        StateId := DPDB_HOT
      else
        StateId := DPDB_NORMAL
    else
      StateId := DPDB_DISABLED;
    DC := GetWindowDC(WindowHandle);
    try
      ExcludeClipRect(DC, Rect.Left + 2, Rect.Top + 2, Rect.Right - 2, Rect.Bottom - 2);
      DrawThemeBackground(Theme, DC, DP_DATEBORDER, StateId, Rect, nil);
    finally
      ReleaseDC(WindowHandle, DC);
    end;
  end;
end;

procedure TCustomIntlDatePicker.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  if CanFocus then
    SetFocus;
end;

procedure TCustomIntlDatePicker.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  InvalidateDate;
  InvalidateFrame;
end;

procedure TCustomIntlDatePicker.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if CalendarPopup.Visible then
    PopupCloseUp(CalendarPopup, False);
  InvalidateDate;
  InvalidateFrame;
end;

procedure TCustomIntlDatePicker.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TCustomIntlDatePicker.CMEnableChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomIntlDatePicker.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  UpdateRects;
end;

procedure TCustomIntlDatePicker.CMBorderChanged(var Message: TMessage);
begin
  inherited;
  AdjustSize;
  InvalidateFrame;
end;

procedure TCustomIntlDatePicker.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  AdjustSize;
  InvalidateFrame;
end;

procedure TCustomIntlDatePicker.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> CalendarPopup) and
     CalendarPopup.Visible and not CalendarPopup.ContainsControl(Message.Sender)
  then
    PopupCloseUp(CalendarPopup, False);
end;

procedure TCustomIntlDatePicker.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  fHotInfo := phtControl;
  InvalidateFrame;
end;

procedure TCustomIntlDatePicker.CMMouseLeave(var Message: TMessage);
begin
  case HotInfo of
    phtCheckBox:
      InvalidateCheckBox;
    phtDropDown:
      InvalidateDropDown;
  end;
  fHotInfo := phtNowhere;
  InvalidateFrame;
  inherited;
end;

function TCustomIntlDatePicker.IsManagedByLocalizer: Boolean;
begin
  Result := Assigned(Localizer);
end;

function TCustomIntlDatePicker.IsYearInRange(AEra, AYear: Integer): Boolean;
begin
  Result := CalendarPopup.IsYearInRange(AEra, AYear);
end;

function TCustomIntlDatePicker.IsYearInRange(AYear: Integer): Boolean;
begin
  Result := CalendarPopup.IsYearInRange(AYear);
end;

function TCustomIntlDatePicker.IsMonthInRange(AEra, AYear, AMonth: Integer): Boolean;
begin
  Result := CalendarPopup.IsMonthInRange(AEra, AYear, AMonth);
end;

function TCustomIntlDatePicker.IsMonthInRange(AYear, AMonth: Integer): Boolean;
begin
  Result := CalendarPopup.IsMonthInRange(AYear, AMonth);
end;

function TCustomIntlDatePicker.IsDateInRange(const ADate: TDate): Boolean;
begin
  Result := CalendarPopup.IsDateInRange(ADate);
end;

function TCustomIntlDatePicker.EnsureDateInRange(const ADate: TDate): TDate;
begin
  Result := CalendarPopup.EnsureDateInRange(ADate);
end;

end.
