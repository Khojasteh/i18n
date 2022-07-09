{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements data-aware controls for displaying and editing data
/// fields that represent date and time values.
unit i18nDBDateCtrls;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, Types, Windows, Messages, Classes, Graphics, Controls, DB,
  DBCtrls, i18nDateCtrls, i18nCalendar;

type
  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDBIntlDateTimeLabel is the base class for data-aware label controls
  /// that display value of a date-time field.</summary>
  /// <remarks>
  /// Use TCustomDBIntlDateTimeLabel as a base class when defining objects that
  /// display value of a date-time database field on a form.
  ///
  /// TCustomDBIntlDateTimeLabel can use different calendar systems for displaying the
  /// dates. The <see cref="CalendarType"/> property is the main property that specifies
  /// which calendar system should be used by the cotnrol. If the <see cref="CalendarType"/>
  /// property is not set, the <see cref="Culture"/> property can do this task. In addition,
  /// the <see cref="Culture"/> property provides the locale specific settings of the control.
  /// If none of these two properties are set the <see cref="DefaultCalendar"/> global
  /// variable determines the calendar system used by the control.
  ///
  /// TCustomDBIntlDateTimeLabel can be linked to a <see cref="TLocalizer"/>
  /// component. In this case, the <see cref="TLocalizer"/> component determines
  /// the localized strings and the calendar system that should be used for displaying
  /// the date-time value.</remarks>
  {$endregion}
  TCustomDBIntlDateTimeLabel = class(TCustomIntlDateTimeLabel)
  private
    fDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetField: TField;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the control when it is first loaded into memory.</summary>
    {$endregion}
    procedure Loaded; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines the string that appears as the text of the label.</summary>
    /// <returns>
    /// The text that appears on the label.</returns>
    {$endregion}
    function GetLabelText: String; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets the <see cref="AutoSize"/> property.</summary>
    /// <param name="Value">
    /// Indicates whether the auto size should be on or off.</param>
    {$endregion}
    procedure SetAutoSize(Value: Boolean); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Helps the data-aware control manage its link to the data source and respond
    /// to data events.</summary>
    {$endregion}
    property DataLink: TFieldDataLink read fDataLink;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of field whose value is displayed by the control.</summary>
    {$endregion}
    property DataField: WideString read GetDataField write SetDataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the control.</summary>
    /// <param name="AOwner">
    /// The owner component.</param>
    {$endregion}
    constructor Create(AOwner: TComponent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the control and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies whether the control's alignment is in a right-to-left mode.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the control's alignment is in a right-to-left
    /// mode, otherwise returns <see langword="false"/>.</returns>
    {$endregion}
    function UseRightToLeftAlignment: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TField"/> object for the database field the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TDBIntlDateTimeLabel is a data-aware label control to display value of a
  /// date-time field.</summary>
  /// <remarks>
  /// Use TDBIntlDateTimeLabel to display localized value of a date-time database
  /// field on a form.
  ///
  /// TDBIntlDateTimeLabel can use different calendar systems for displaying the dates.
  /// The <see cref="CalendarType"/> property is the main property that specifies which
  /// calendar system should be used by the cotnrol. If the <see cref="CalendarType"/>
  /// property is not set, the <see cref="Culture"/> property can do this task. In addition,
  /// the <see cref="Culture"/> property provides the locale specific settings of the control.
  /// If none of these two properties are set the <see cref="DefaultCalendar"/> global
  /// variable determines the calendar system used by the control.
  ///
  /// TDBIntlDateTimeLabel can be linked to a <see cref="TLocalizer"/> component.
  /// In this case, the <see cref="TLocalizer"/> component determines the localized
  /// strings and the calendar system that should be used for displaying the date-time
  /// value.
  ///
  /// TDBIntlDateTimeLabel publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBIntlDateTimeLabel"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBIntlDateTimeLabel = class(TCustomDBIntlDateTimeLabel)
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control aligns within its container (parent control).</summary>
    {$endregion}
    property Align;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the horizontal placement of the text within the label.</summary>
    {$endregion}
    property Alignment;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether a control should be constrained by <see cref="Margins"/>.</summary>
    {$endregion}
    property AlignWithMargins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control is anchored to its parent.</summary>
    {$endregion}
    property Anchors;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control sizes itself automatically to accommodate
    /// its contents.</summary>
    {$endregion}
    property AutoSize default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the bidirectional mode for the control.</summary>
    {$endregion}
    property BiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the color of the control's border.</summary>
    {$endregion}
    property BorderColor;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the width of the control's border.</summary>
    {$endregion}
    property BorderWidth;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCalendar"/> class that is used for manipulating dates.</summary>
    /// <seealso cref="Culture"/>
    {$endregion}
    property CalendarType;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the background color of the control.</summary>
    {$endregion}
    property Color nodefault;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the size constraints for the control.</summary>
    {$endregion}
    property Constraints;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCultureInfo"/> object that provides the locale specific
    /// settings of the calendar.
    ///
    /// NOTE: If the <see cref="CalendarType"/> property is <see langword="nil"/>, depends
    /// on the values of <see cref="TLocalizer"/> and <see cref="CultureCalendar"/> properties,
    /// the Culture property may also determine the calendar system of the control.</summary>
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
    /// property is not <see langword="nil"/>.</summary>
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
    /// Gets or sets the name of field whose value is displayed by the control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the format string that determines how the date-time value
    /// should be displayed on the control. See <see cref="TCalendar.Format"/> for details.</summary>
    {$endregion}
    property DateTimeFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the image used to represent the mouse pointer when it passes
    /// into the region covered by the control.</summary>
    {$endregion}
    property DragCursor;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control is being dragged normally or for docking.</summary>
    {$endregion}
    property DragKind;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control initiates drag-and-drop or drag-and-dock operations.</summary>
    {$endregion}
    property DragMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control responds to keyboard, mouse, and timer events.</summary>
    {$endregion}
    property Enabled;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets a windowed control associated with the label.</summary>
    {$endregion}
    property FocusControl;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the attributes of text written on the control.</summary>
    {$endregion}
    property Font;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the radius of the glow around the label.
    /// NOTE: This feature is only available in Windows Vista and later.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property GlowSize;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the text string that can appear when the user moves the mouse over
    /// the control.</summary>
    {$endregion}
    property Hint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which image is displayed as the icon on the label.</summary>
    {$endregion}
    property ImageIndex;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the images that can appear on the control.</summary>
    {$endregion}
    property Images;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the control.</summary>
    {$endregion}
    property Localizer;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the margins of the control.</summary>
    {$endregion}
    property Margins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the distance between the image and the text from the control's border.</summary>
    {$endregion}
    property Padding;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="BiDiMode"/>.</summary>
    {$endregion}
    property ParentBiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Color"/>.</summary>
    {$endregion}
    property ParentColor;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Font"/>.</summary>
    {$endregion}
    property ParentFont;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="ShowHint"/>.</summary>
    {$endregion}
    property ParentShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the pop-up menu associated with the control.</summary>
    {$endregion}
    property PopupMenu;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how an ampersand in the label text is displayed.</summary>
    {$endregion}
    property ShowAccelChar default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.</summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of pixels between the image and the text of the control.</summary>
    {$endregion}
    property Spacing;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the touch manager component associated with the control.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property Touch;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether controls that sit below the label on a form can be seen
    /// through the label.</summary>
    {$endregion}
    property Transparent;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the vertical placement of the text within the label.</summary>
    {$endregion}
    property Layout;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control appears onscreen.</summary>
    {$endregion}
    property Visible;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets Specifies whether the label text wraps when it is too long for the
    /// width of the label.</summary>
    {$endregion}
    property WordWrap;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user clicks the control.</summary>
    {$endregion}
    property OnClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user right-clicks the control or otherwise invokes the pop-up
    /// menu (such as using the keyboard).</summary>
    {$endregion}
    property OnContextPopup;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user double-clicks the left mouse button when the mouse pointer
    /// is over the control.</summary>
    {$endregion}
    property OnDblClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user drops an object being dragged on the control.</summary>
    {$endregion}
    property OnDragDrop;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user drags an object over the control.</summary>
    {$endregion}
    property OnDragOver;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dragging of an object ends, either by docking the object or
    /// by canceling the dragging.</summary>
    {$endregion}
    property OnEndDock;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dragging of an object ends, either by dropping the object or
    /// by canceling the dragging.</summary>
    {$endregion}
    property OnEndDrag;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when user performs a gesture associated with the control.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnGesture;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user presses a mouse button with the mouse pointer over the
    /// control and the parent form is not active.</summary>
    {$endregion}
    property OnMouseActivate;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user presses a mouse button with the mouse pointer over the
    /// control.</summary>
    {$endregion}
    property OnMouseDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse pointer while the mouse pointer is over
    /// the control.</summary>
    {$endregion}
    property OnMouseMove;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user releases a mouse button that was pressed with the mouse
    /// pointer over the control.</summary>
    {$endregion}
    property OnMouseUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse into the control.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnMouseEnter;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse outside of the control.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnMouseLeave;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated.</summary>
    {$endregion}
    property OnMouseWheel;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated downward.</summary>
    {$endregion}
    property OnMouseWheelDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated upward.</summary>
    {$endregion}
    property OnMouseWheelUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately after the control is resized.</summary>
    {$endregion}
    property OnResize;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user begins to drag the control with a <see cref="DragKind"/>
    /// of dkDock.</summary>
    {$endregion}
    property OnStartDock;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user begins to drag the control with a <see cref="DragKind"/>
    /// of dkDrag.</summary>
    {$endregion}
    property OnStartDrag;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDBIntlDatePicker is the base class for data-aware controls that request
  /// a localized date from the user and update a date field.</summary>
  /// <remarks>
  /// Use TCustomDBIntlDatePicker as a base class when defining objects that enable
  /// users to enter a localized date for a database field.
  ///
  /// TCustomDBIntlDatePicker can use different calendar systems for getting the input
  /// date. The <see cref="CalendarType"/> property is the main property that specifies
  /// which calendar system should be used by the cotnrol. If the <see cref="CalendarType"/>
  /// property is not set, the <see cref="Culture"/> property can do this task. In addition,
  /// the <see cref="Culture"/> property provides the locale specific settings of the control.
  /// If none of these two properties are set, the <see cref="DefaultCalendar"/> global
  /// variable determines the calendar system used by the control.
  ///
  /// TCustomDBIntlDatePicker can be linked to a <see cref="TLocalizer"/> component.
  /// In this case, the linked <see cref="TLocalizer"/> component can provide both the
  /// locale specific settings and the calendar system of the control.</remarks>
  {$endregion}
  TCustomDBIntlDatePicker = class(TCustomIntlDatePicker)
  private
    fDataLink: TFieldDataLink;
    fChanging: Boolean;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject); inline;
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetReadOnly: Boolean; inline;
    procedure SetReadOnly(Value: Boolean); inline;
    function GetField: TField; inline;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when either of <see cref="Date"/> or <see cref="Checked"/> properties
    /// changes.</summary>
    {$endregion}
    procedure Change; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether the user can enter a different date in the control.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the user can enter a date, oterwise
    /// returns <see langword="false"/>.</returns>
    {$endregion}
    function CanModify: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key while the control has focus.</summary>
    /// <param name="Key">
    /// The key code of the key that was pressed.</param>
    {$endregion}
    procedure KeyPress(var Key: Char); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Helps the data-aware control manage its link to the data source and respond
    /// to data events.</summary>
    {$endregion}
    property DataLink: TFieldDataLink read fDataLink;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the date field that is represented by the control.</summary>
    {$endregion}
    property DataField: WideString read GetDataField write SetDataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether the user can change the field value using the control.</summary>
    {$endregion}
    property Readonly: Boolean read GetReadOnly write SetReadOnly default False;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the control.</summary>
    /// <param name="AOwner">
    /// The owner component.</param>
    {$endregion}
    constructor Create(AOwner: TComponent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the control and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Invokes an action with the control as its target.</summary>
    /// <param name="Action">
    /// Specifies the action that was invoked.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the action was successfully dispatched,
    /// and <see langword="false"/> if the control could not handle the action.</returns>
    /// <seealso cref="UpdateAction"/>
    {$endregion}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates an action to reflect the current state of the control.</summary>
    /// <param name="Action">
    /// Specifies the action that should be updated.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the action component now reflects the
    /// state of the control, and <see langword="false"/> if it did not know how
    /// to update the action.</returns>
    /// <seealso cref="ExecuteAction"/>
    {$endregion}
    function UpdateAction(Action: TBasicAction): Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Specifies whether the control's alignment is in a right-to-left mode.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the control's alignment is in a right-to-left
    /// mode, otherwise returns <see langword="false"/>.</returns>
    {$endregion}
    function UseRightToLeftAlignment: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TField"/> object for the database field the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TDBIntlDatePicker is data-aware control that enables the user to enter a localized
  /// date for a date field.</summary>
  /// <remarks>
  /// Use TDBIntlDatePicker to enable the user to enter a localized date in a database field.
  ///
  /// TDBIntlDatePicker can use different calendar systems for getting the input date. The
  /// <see cref="CalendarType"/> property is the main property that specifies which calendar
  /// system should be used by the cotnrol. If the <see cref="CalendarType"/> property is not
  /// set, the <see cref="Culture"/> property can do this task. In addition, the <see cref="Culture"/>
  /// property provides the locale specific settings of the control. If none of these two
  /// properties are set, the <see cref="DefaultCalendar"/> global variable determines the calendar
  /// system used by the control.
  ///
  /// TDBIntlDatePicker can be linked to a <see cref="TLocalizer"/> component. In this
  /// case, the linked <see cref="TLocalizer"/> component can provide both the locale
  /// specific settings and the calendar system of the control.
  ///
  /// TDBIntlDatePicker publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBIntlDatePicker"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBIntlDatePicker = class(TCustomDBIntlDatePicker)
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control aligns within its container (parent control).</summary>
    {$endregion}
    property Align;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether a control should be constrained by <see cref="Margins"/>.</summary>
    {$endregion}
    property AlignWithMargins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control is anchored to its parent.</summary>
    {$endregion}
    property Anchors;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control sizes itself automatically to accommodate
    /// its contents.</summary>
    {$endregion}
    property AutoSize default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which edges of the control are beveled.</summary>
    {$endregion}
    property BevelEdges;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the cut of the inner bevel.</summary>
    {$endregion}
    property BevelInner;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the control's bevel style.</summary>
    {$endregion}
    property BevelKind default bkNone;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the cut of the outer bevel.</summary>
    {$endregion}
    property BevelOuter;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the bidirectional mode for the control.</summary>
    {$endregion}
    property BiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the border style for the control.</summary>
    {$endregion}
    property BorderStyle;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides access to the pop-up calendar's settings.</summary>
    {$endregion}
    property CalendarPopup;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCalendar"/> class that is used for manipulating dates.</summary>
    /// <seealso cref="Culture"/>
    {$endregion}
    property CalendarType;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the background color of the control.</summary>
    {$endregion}
    property Color default clWindow;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the size constraints for the control.</summary>
    {$endregion}
    property Constraints;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control has a three-dimensional look.</summary>
    {$endregion}
    property Ctl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCultureInfo"/> object that provides the locale specific
    /// settings of the calendar.
    ///
    /// NOTE: If the <see cref="CalendarType"/> property is <see langword="nil"/>, depends
    /// on the values of <see cref="TLocalizer"/> and <see cref="CultureCalendar"/> properties,
    /// the Culture property may also determine the calendar system of the control.</summary>
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
    /// property is not <see langword="nil"/>.</summary>
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
    /// Gets or sets the name of the date field that is represented by the control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the format string that determines how the <see cref="Date"/> property
    /// should be displayed on the control. See <see cref="TCalendar.Format"/> for details.</summary>
    /// <seealso cref="TCalendar.Format"/>
    {$endregion}
    property DateFormat;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the smallest date that is allowed for the calendar.</summary>
    /// <seealso cref="ActualDateMin"/>
    /// <seealso cref="DateMax"/>
    {$endregion}
    property DateMin;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the largest date that is allowed for the calendar.</summary>
    /// <seealso cref="ActualDateMax"/>
    /// <seealso cref="DateMin"/>
    {$endregion}
    property DateMax;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control's image is rendered directly to the window or
    /// painted to an in-memory bitmap first.</summary>
    {$endregion}
    property DoubleBuffered;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the image used to represent the mouse pointer when it passes
    /// into the region covered by the control.</summary>
    {$endregion}
    property DragCursor;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control is being dragged normally or for docking.</summary>
    {$endregion}
    property DragKind;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets how the control initiates drag-and-drop or drag-and-dock operations.</summary>
    {$endregion}
    property DragMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control responds to keyboard, mouse, and timer events.</summary>
    {$endregion}
    property Enabled;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the attributes of text written on the control.</summary>
    {$endregion}
    property Font;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the text string that can appear when the user moves the mouse over
    /// the control.</summary>
    {$endregion}
    property Hint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the behavior of the input method editor (IME).</summary>
    {$endregion}
    property ImeMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the input method editor (IME) to use for converting keyboard input.</summary>
    {$endregion}
    property ImeName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the
    /// control.</summary>
    {$endregion}
    property Localizer;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the margins of the control.</summary>
    {$endregion}
    property Margins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="BiDiMode"/>.</summary>
    {$endregion}
    property ParentBiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Color"/>.</summary>
    {$endregion}
    property ParentColor default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Ctl3D"/>.</summary>
    {$endregion}
    property ParentCtl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="DoubleBuffered"/>.</summary>
    {$endregion}
    property ParentDoubleBuffered;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="Font"/>.</summary>
    {$endregion}
    property ParentFont;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control uses its parent's <see cref="ShowHint"/>.</summary>
    {$endregion}
    property ParentShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the pop-up menu associated with the control.</summary>
    {$endregion}
    property PopupMenu;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether the user can change the field value using the control.</summary>
    {$endregion}
    property Readonly;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether a check box next to the date should be displayed.</summary>
    {$endregion}
    property ShowCheckBox;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.</summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the position of the control in its parent's tab order.</summary>
    {$endregion}
    property TabOrder;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can tab to a control.</summary>
    {$endregion}
    property TabStop default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the touch manager component associated with the control.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property Touch;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control appears onscreen.</summary>
    {$endregion}
    property Visible;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when either of <see cref="Date"/> or <see cref="Checked"/> properties change.</summary>
    {$endregion}
    property OnChange;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user clicks the control.</summary>
    {$endregion}
    property OnClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user right-clicks the control or otherwise invokes the pop-up
    /// menu (such as using the keyboard).</summary>
    {$endregion}
    property OnContextPopup;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user double-clicks the left mouse button when the mouse pointer
    /// is over the control.</summary>
    {$endregion}
    property OnDblClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user drops an object being dragged on the control.</summary>
    {$endregion}
    property OnDragDrop;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user drags an object over the control.</summary>
    {$endregion}
    property OnDragOver;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dragging of an object ends, either by docking the object or
    /// by canceling the dragging.</summary>
    {$endregion}
    property OnEndDock;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dragging of an object ends, either by dropping the object or
    /// by canceling the dragging.</summary>
    {$endregion}
    property OnEndDrag;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a control receives the input focus.</summary>
    {$endregion}
    property OnEnter;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the input focus shifts away from one control to another.</summary>
    {$endregion}
    property OnExit;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when user performs a gesture associated with the control.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnGesture;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when a user presses any key while the control has focus.</summary>
    {$endregion}
    property OnKeyDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when key pressed.</summary>
    {$endregion}
    property OnKeyPress;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user releases a key that has been pressed.</summary>
    {$endregion}
    property OnKeyUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user presses a mouse button with the mouse pointer over the
    /// control and the parent form is not active.</summary>
    {$endregion}
    property OnMouseActivate;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user presses a mouse button with the mouse pointer over the
    /// control.</summary>
    {$endregion}
    property OnMouseDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse pointer while the mouse pointer is over
    /// the control.</summary>
    {$endregion}
    property OnMouseMove;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user releases a mouse button that was pressed with the mouse
    /// pointer over the control.</summary>
    {$endregion}
    property OnMouseUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse into the control.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnMouseEnter;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user moves the mouse outside of the control.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property OnMouseLeave;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated.</summary>
    {$endregion}
    property OnMouseWheel;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated downward.</summary>
    {$endregion}
    property OnMouseWheelDown;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the mouse wheel is rotated upward.</summary>
    {$endregion}
    property OnMouseWheelUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs immediately after the control is resized.</summary>
    {$endregion}
    property OnResize;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user begins to drag the control with a <see cref="DragKind"/>
    /// of dkDock.</summary>
    {$endregion}
    property OnStartDock;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user begins to drag the control with a <see cref="DragKind"/>
    /// of dkDrag.</summary>
    {$endregion}
    property OnStartDrag;
  end;

implementation

uses
  VDBConsts;

{ TCustomDBIntlDateTimeLabel }

constructor TCustomDBIntlDateTimeLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  AutoSize := False;
end;

destructor TCustomDBIntlDateTimeLabel.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBIntlDateTimeLabel.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

function TCustomDBIntlDateTimeLabel.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBIntlDateTimeLabel.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBIntlDateTimeLabel.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBIntlDateTimeLabel.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBIntlDateTimeLabel.GetField: TField;
begin
  if Assigned(DataLink) then
    Result := DataLink.Field
  else
    Result := nil;
end;

function TCustomDBIntlDateTimeLabel.GetLabelText: String;
begin
  if Assigned(Field) then
    Result := inherited GetLabelText
  else if csDesigning in ComponentState then
    Result := Name
  else
    Result := '';
end;

procedure TCustomDBIntlDateTimeLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    if Value and FDataLink.DataSourceFixed then
      DatabaseError(SDataSourceFixed);
    inherited SetAutoSize(Value);
  end;
end;

procedure TCustomDBIntlDateTimeLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBIntlDateTimeLabel.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    DateTime := Field.AsDateTime
  else
    DateTime := TCalendar.NoDate;
end;

function TCustomDBIntlDateTimeLabel.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{ TCustomDBIntlDatePicker }

constructor TCustomDBIntlDatePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  fDataLink.OnUpdateData := UpdateData;
  Checked := False;
end;

destructor TCustomDBIntlDatePicker.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBIntlDatePicker.Change;
begin
  if not fChanging and DataLink.Edit then
  begin
    inherited Change;
    DataLink.Modified;
  end;
end;

function TCustomDBIntlDatePicker.CanModify: Boolean;
begin
  Result := fChanging or DataLink.CanModify;
end;

procedure TCustomDBIntlDatePicker.KeyPress(var Key: Char);
begin
  if not DataLink.CanModify then
    Key := #0
  else if Key = #27 then
    DataLink.Reset;
  inherited KeyPress(Key);
end;

function TCustomDBIntlDatePicker.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBIntlDatePicker.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBIntlDatePicker.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBIntlDatePicker.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBIntlDatePicker.GetField: TField;
begin
  Result := DataLink.Field;
end;

function TCustomDBIntlDatePicker.GetReadOnly: Boolean;
begin
  Result := DataLink.ReadOnly;
end;

procedure TCustomDBIntlDatePicker.SetReadOnly(Value: Boolean);
begin
  DataLink.ReadOnly := Value;
end;

procedure TCustomDBIntlDatePicker.CMExit(var Message: TCMExit);
begin
  try
    DataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBIntlDatePicker.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBIntlDatePicker.DataChange(Sender: TObject);
begin
  fChanging := True;
  try
    if Assigned(Field) then
    begin
      if Field.IsNull then
        Checked := False
      else
      begin
        Checked := True;
        Date := Field.AsDateTime;
      end;
    end
    else
    begin
      Date := Calendar.Today;
      Checked := False;
    end;
  finally
    fChanging := False;
  end;
end;

procedure TCustomDBIntlDatePicker.UpdateData(Sender: TObject);
begin
  if Assigned(Field) then
  begin
    if Checked then
      Field.AsDateTime := Date
    else
      Field.Clear;
  end;
end;

function TCustomDBIntlDatePicker.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action)
         or Assigned(DataLink) and DataLink.ExecuteAction(Action);
end;

function TCustomDBIntlDatePicker.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action)
         or Assigned(DataLink) and DataLink.UpdateAction(Action);
end;

function TCustomDBIntlDatePicker.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

end.

