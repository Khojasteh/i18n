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
/// fields that represent locales, countries, and currencies.
unit i18nDBCtrls;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, DB, DBCtrls, ImgList,
  {$IFDEF COMPILER_XE3_UP} UITypes, {$ENDIF}
  SysUtils, i18nCore, i18nCtrls;

type
  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDBImageLabel is the base class for data-aware label controls that display
  /// an image icon plus value of a field.</summary>
  /// <remarks>
  /// Use TCustomImageDBLabel as a base class when defining objects that display an
  /// icon followed by value of a database field on a form. TCustomDBImageLabel
  /// implements properties to specify the icon from an image list component, along
  /// with its appearance and placement on the label.</remarks>
  {$endregion}
  TCustomDBImageLabel = class(TCustomImageLabel)
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
    /// Gets the <see cref="TField"/> object for the database field that the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TDBImageLabel is a data-aware label control that displays an image icon plus value of
  /// a field.</summary>
  /// <remarks>
  /// Use TDBImageLabel to display an icon followed by value of a database field on a form.
  /// The <see cref="ImageIndex"/> property determines which image of the image list identified
  /// by the <see cref="Images"/> property will be displayed.
  ///
  /// TDBImageLabel control can have a border. The <see cref="BorderColor"/> and
  /// <see cref="BorderWidth"/> properties determine color and thickness of the
  /// border.
  ///
  /// TDBImageLabel publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBImageLabel"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBImageLabel = class(TCustomDBImageLabel)
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
    property ShowAccelChar;
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
  /// TCustomDBCultureLabel is the base class for data-aware label controls that display
  /// value of a locale field.</summary>
  /// <remarks>
  /// Use TCustomDBCultureLabel as a base class when defining objects that display
  /// value of a database field, which the field represents either a locale name or
  /// a locale identifier.</remarks>
  {$endregion}
  TCustomDBCultureLabel = class(TCustomCultureLabel)
  private
    fDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetCulture: TCultureInfo; inline;
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetField: TField;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents <see cref="Culture"/> property to be saved and loaded.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
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
    /// Determines the country flag icon that appears on the control.</summary>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon should be displayed.</returns>
    {$endregion}
    function GetImageIndex: TImageIndex; override;
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
    /// Gets or sets the name of the locale field that is represented by the control.</summary>
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
    /// Gets the <see cref="TField"/> object for the database field that the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TCultureInfo"/> object that the value of the locale field
    /// represents.</summary>
    {$endregion}
    property Culture: TCultureInfo read GetCulture;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TDBCultureLabel is a data-aware label control that displays value of a locale
  /// field.</summary>
  /// <remarks>
  /// Use TDBCultureLabel to display value of a database field on a form, when the field
  /// represents either a locale name or a locale identifier.
  ///
  /// TDBCultureLabel publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBCultureLabel"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBCultureLabel = class(TCustomDBCultureLabel)
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
    /// Gets or sets the text of the label when the database field has no value or its
    /// value is not a valid locale name or locale identifer.</summary>
    {$endregion}
    property Caption;
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
    /// Gets or sets the name of the locale field that is represented by the control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the text label on the control.</summary>
    {$endregion}
    property DisplayName;
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
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags;
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
    property ShowAccelChar;
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
  /// TCustomCultureBox is the base class for data-aware combo box controls that enable
  /// users to choose value of a locale field from a list of <see cref="TCultureInfo"/>
  /// objects.</summary>
  /// <remarks>
  /// Use TCustomCultureBox as a base class when defining data-aware combo box controls
  /// that provide a list of <see cref="TCultureInfo"/> objects to update value of a
  /// database field.
  ///
  /// The type of the database field must be either string or numeric to store respectively
  /// the locale name or the locale identifier of the selected <see cref="TCultureInfo"/>
  /// object.</remarks>
  {$endregion}
  TCustomDBCultureBox = class(TCustomCultureBox)
  private
    fDataLink: TFieldDataLink;
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetField: TField; inline;
    function GetReadOnly: Boolean; inline;
    procedure SetReadOnly(Value: Boolean); inline;
    procedure UpdateData(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents <see cref="ItemSelected"/> property to be saved and loaded.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the control when it is first loaded into memory.</summary>
    {$endregion}
    procedure Loaded; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to the user's item selection.</summary>
    {$endregion}
    procedure Select; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key.</summary>
    /// <param name="Key">
    /// The key that was pressed. If the method changes Key to 0, processing of the keystroke stops.</param>
    /// <param name="Shift">
    /// Indicates the concurrent state of the Ctrl, Shift, and Alt keys.</param>
    {$endregion}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key while the control has focus.</summary>
    /// <param name="Key">
    /// The key code of the key that was pressed.</param>
    {$endregion}
    procedure KeyPress(var Key: Char); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides specific message responses for the control.</summary>
    /// <param name="Message">
    /// The message's details.</param>
    {$endregion}
    procedure WndProc(var Message: TMessage); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to changes in the <see cref="Items"/> property</summary>
    {$endregion}
    procedure DoItemsChange; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Helps the data-aware control manage its link to the data source and respond
    /// to data events.</summary>
    {$endregion}
    property DataLink: TFieldDataLink read fDataLink;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the locale field that is represented by the control.</summary>
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
    /// Gets the <see cref="TField"/> object for the database field that the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomCultureBox is a data-aware combo box control that enable users to choose
  /// value of a locale field from a list of <see cref="TCultureInfo"/> objects.</summary>
  /// <remarks>
  /// Use TDBCultureBox to enable users to choose value of a database field, which represents
  /// a locale name or a locale identifier, from a list of <see cref="TCultureInfo"/> objects.
  ///
  /// The type of the database field must be either string or numeric to store respectively
  /// the locale name or the locale identifier of the selected <see cref="TCultureInfo"/>
  /// object.
  ///
  /// TDBCultureBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBCultureBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBCultureBox = class(TCustomDBCultureBox)
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
    /// Gets or sets whether the user can give focus to items by typing in the list.</summary>
    {$endregion}
    property AutoComplete;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the delay between a key press and an attempt to <see cref="AutoComplete"/>.</summary>
    {$endregion}
    property AutoCompleteDelay;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the drop-down closes up automatically when the user selects an item.</summary>
    {$endregion}
    property AutoCloseUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the drop-down list drops down automatically in response to user keystrokes.</summary>
    {$endregion}
    property AutoDropDown;
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
    property BevelKind;
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
    /// Gets or sets whether the control has a three-dimensional look.</summary>
    {$endregion}
    property Ctl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the locale field that is represented by the control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName;
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
    /// Gets or sets the maximum number of items displayed in the drop-down list.</summary>
    {$endregion}
    property DropDownCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control responds to keyboard, mouse, and timer events.</summary>
    {$endregion}
    property Enabled;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the placement of the flag icon in regard of the text label.</summary>
    {$endregion}
    property FlagPosition;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags;
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
    /// Lists the <see cref="TCultureInfo"/> objects in the control.</summary>
    {$endregion}
    property Items;
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
    property ParentColor;
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
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.</summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the items in the list are arranged alphabetically.</summary>
    {$endregion}
    property Sorted;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the position of the control in its parent's tab order.</summary>
    {$endregion}
    property TabOrder;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can tab to a control.</summary>
    {$endregion}
    property TabStop;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets thet text that is displayed as a text watermark in the control.
    /// NOTE: This feature is only available in Windows Vista and later.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property TextHint;
    {$ENDIF}
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
    /// Occurs when the user clicks the control.</summary>
    {$endregion}
    property OnClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the drop-down list closes up due to some user action.</summary>
    {$endregion}
    property OnCloseUp;
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
    /// Occurs when text label of an item needs to be displayed.</summary>
    {$endregion}
    property OnDrawItemLabel;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user opens the drop-down list.</summary>
    {$endregion}
    property OnDropDown;
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
    /// Occurs when <see cref="Items"/> property changed.</summary>
    {$endregion}
    property OnItemsChange;
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
    /// Occurs when the user selects an item in the drop-down list</summary>
    {$endregion}
    property OnSelect;
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
  /// TCustomDBCultureListBox is the base class for data-aware list box controls that
  /// enable users to choose value of a locale field from a list of <see cref="TCultureInfo"/>
  /// objects.</summary>
  /// <remarks>
  /// Use TCustomDBCultureListBox as a base class when defining data-aware list box controls
  /// that provide a list of <see cref="TCultureInfo"/> objects to update value of a database
  /// field.
  ///
  /// The type of the database field must be either string or numeric to store respectively
  /// the locale name or the locale identifier of the selected <see cref="TCultureInfo"/>
  /// object.</remarks>
  {$endregion}
  TCustomDBCultureListBox = class(TCustomCultureListBox)
  private
    fDataLink: TFieldDataLink;
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetField: TField; inline;
    function GetReadOnly: Boolean; inline;
    procedure SetReadOnly(Value: Boolean); inline;
    procedure UpdateData(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents <see cref="ItemSelected"/> property to be saved and loaded.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the control when it is first loaded into memory.</summary>
    {$endregion}
    procedure Loaded; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to mouse clicks.</summary>
    {$endregion}
    procedure Click; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key.</summary>
    /// <param name="Key">
    /// The key that was pressed. If the method changes Key to 0, processing of the keystroke stops.</param>
    /// <param name="Shift">
    /// Indicates the concurrent state of the Ctrl, Shift, and Alt keys.</param>
    {$endregion}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key while the control has focus.</summary>
    /// <param name="Key">
    /// The key code of the key that was pressed.</param>
    {$endregion}
    procedure KeyPress(var Key: Char); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides specific message responses for the control.</summary>
    /// <param name="Message">
    /// The message's details.</param>
    {$endregion}
    procedure WndProc(var Message: TMessage); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to changes in the <see cref="Items"/> property</summary>
    {$endregion}
    procedure DoItemsChange; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Helps the data-aware control manage its link to the data source and respond
    /// to data events.</summary>
    {$endregion}
    property DataLink: TFieldDataLink read fDataLink;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the locale field that is represented by the control.</summary>
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
    /// Gets the <see cref="TField"/> object for the database field that the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomCultureBox is a data-aware list box control that enable users to choose
  /// value of a locale field from a list of <see cref="TCultureInfo"/> objects.</summary>
  /// <remarks>
  /// Use TDBCultureBox to enable users to choose value of a database field, which represents
  /// a locale name or a locale identifier, from a list of <see cref="TCultureInfo"/> objects.
  ///
  /// The type of the database field must be either string or numeric to store respectively
  /// the locale name or the locale identifier of the selected <see cref="TCultureInfo"/>
  /// object.
  ///
  /// TDBCultureListBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBCultureListBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBCultureListBox = class(TCustomDBCultureListBox)
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
    /// Gets or sets whether the user can give focus to items by typing in the list.</summary>
    {$endregion}
    property AutoComplete;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the delay between a key press and an attempt to <see cref="AutoComplete"/>.</summary>
    {$endregion}
    property AutoCompleteDelay;
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
    property BevelKind;
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
    /// Gets or sets whether the control has a border.</summary>
    {$endregion}
    property BorderStyle;
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
    /// Gets or sets whether the control has a three-dimensional look.</summary>
    {$endregion}
    property Ctl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of columns, in a multi-column list box, that are visible
    /// without having to scroll.</summary>
    {$endregion}
    property Columns;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the locale field that is represented by the control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName;
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
    /// Gets or sets the placement of the flag icon in regard of the text label.</summary>
    {$endregion}
    property FlagPosition;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags;
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
    /// Gets or sets whether the list box displays the partial items.</summary>
    {$endregion}
    property IntegralHeight;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCultureInfo"/> objects in the control.</summary>
    {$endregion}
    property Items;
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
    property ParentColor;
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
    /// Gets or sets the width, in pixels, by which the list box can scroll horizontally.</summary>
    {$endregion}
    property ScrollWidth;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.</summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the items in the list are arranged alphabetically.</summary>
    {$endregion}
    property Sorted;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the position of the control in its parent's tab order.</summary>
    {$endregion}
    property TabOrder;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can tab to a control.</summary>
    {$endregion}
    property TabStop;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the size of the tabs in the list box.</summary>
    {$endregion}
    property TabWidth;
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
    /// Occurs when text label of an item needs to be displayed.</summary>
    {$endregion}
    property OnDrawItemLabel;
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
    /// Occurs when <see cref="Items"/> property changed.</summary>
    {$endregion}
    property OnItemsChange;
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

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDBTerritoryLabel is the base class for data-aware label controls that
  /// display value of a country field.</summary>
  /// <remarks>
  /// Use TCustomDBTerritoryLabel as a base class when defining objects that display
  /// value of a database field, which the field represents either an international
  /// country code or a geographical identifier.</remarks>
  {$endregion}
  TCustomDBTerritoryLabel = class(TCustomTerritoryLabel)
  private
    fDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetTerritory: TTerritoryInfo; inline;
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetField: TField;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents <see cref="Territory"/> property to be saved and loaded.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
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
    /// Determines the country flag icon that appears on the control.</summary>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon should be displayed.</returns>
    {$endregion}
    function GetImageIndex: TImageIndex; override;
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
    /// Gets or sets the name of the country field that is represented by the control.</summary>
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
    /// Gets the <see cref="TField"/> object for the database field that the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TTerritoryInfo"/> object that the value of the country field
    /// represents.</summary>
    {$endregion}
    property Territory: TTerritoryInfo read GetTerritory;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TDBTerritoryLabel is a data-aware label control that displays value of a country
  /// field.</summary>
  /// <remarks>
  /// Use TDBCultureLabel to display value of a database field on a form, when the field
  /// represents either an international country code or a geographical identifier.
  ///
  /// TDBTerritoryLabel publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBTerritoryLabel"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBTerritoryLabel = class(TCustomDBTerritoryLabel)
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
    /// Gets or sets the text of the label when the database field has no value or its
    /// value is not a valid international country code or geographical identifer.</summary>
    {$endregion}
    property Caption;
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
    /// Gets or sets the name of the country field that is represented by the control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TTerritoryInfo.DisplayNames"/> should be
    /// displayed as the text label on the control.</summary>
    {$endregion}
    property DisplayName;
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
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags;
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
    property ShowAccelChar;
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
  /// TCustomDBTerritoryBox is the base class for data-aware combo box controls that enable
  /// users to choose value of a country field from a list of <see cref="TTerritoryInfo"/>
  /// objects.</summary>
  /// <remarks>
  /// Use TCustomDBTerritoryBox as a base class when defining data-aware combo box controls
  /// that provide a list of <see cref="TTerritoryInfo"/> objects to update value of a database
  /// field.
  ///
  /// The type of the database field must be either string (size of 2 or 3 characters) or numeric
  /// to store respectively the internatioal country code or the geographical identifier of the
  /// selected <see cref="TTerritoryInfo"/> object.</remarks>
  {$endregion}
  TCustomDBTerritoryBox = class(TCustomTerritoryBox)
  private
    fDataLink: TFieldDataLink;
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetField: TField; inline;
    function GetReadOnly: Boolean; inline;
    procedure SetReadOnly(Value: Boolean); inline;
    procedure UpdateData(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents <see cref="ItemSelected"/> property to be saved and loaded.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the control when it is first loaded into memory.</summary>
    {$endregion}
    procedure Loaded; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to the user's item selection.</summary>
    {$endregion}
    procedure Select; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key.</summary>
    /// <param name="Key">
    /// The key that was pressed. If the method changes Key to 0, processing of the keystroke stops.</param>
    /// <param name="Shift">
    /// Indicates the concurrent state of the Ctrl, Shift, and Alt keys.</param>
    {$endregion}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key while the control has focus.</summary>
    /// <param name="Key">
    /// The key code of the key that was pressed.</param>
    {$endregion}
    procedure KeyPress(var Key: Char); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides specific message responses for the control.</summary>
    /// <param name="Message">
    /// The message's details.</param>
    {$endregion}
    procedure WndProc(var Message: TMessage); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to changes in the <see cref="Items"/> property</summary>
    {$endregion}
    procedure DoItemsChange; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Helps the data-aware control manage its link to the data source and respond
    /// to data events.</summary>
    {$endregion}
    property DataLink: TFieldDataLink read fDataLink;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the country field that is represented by the control.</summary>
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
    /// Gets the <see cref="TField"/> object for the database field that the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TDBTerritoryBox is a data-aware combo box control that enable users to choose
  /// value of a country field from a list of <see cref="TTerritoryInfo"/> objects.</summary>
  /// <remarks>
  /// Use TDBTerritoryBox to enable users to choose value of a database field, which represents
  /// an international country code or a geographical identifier, from a list of
  /// <see cref="TTerritoryInfo"/> objects.
  ///
  /// The type of the database field must be either string (size of 2 or 3 characters) or numeric
  /// to store respectively the internatioal country code or the geographical identifier of the
  /// selected <see cref="TTerritoryInfo"/> object.
  ///
  /// TDBTerritoryBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBTerritoryBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBTerritoryBox = class(TCustomDBTerritoryBox)
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
    /// Gets or sets whether the user can give focus to items by typing in the list.</summary>
    {$endregion}
    property AutoComplete;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the delay between a key press and an attempt to <see cref="AutoComplete"/>.</summary>
    {$endregion}
    property AutoCompleteDelay;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the drop-down closes up automatically when the user selects an item.</summary>
    {$endregion}
    property AutoCloseUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the drop-down list drops down automatically in response to user keystrokes.</summary>
    {$endregion}
    property AutoDropDown;
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
    property BevelKind;
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
    /// Gets or sets whether the control has a three-dimensional look.</summary>
    {$endregion}
    property Ctl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the country field that is represented by the control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName;
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
    /// Gets or sets the maximum number of items displayed in the drop-down list.</summary>
    {$endregion}
    property DropDownCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control responds to keyboard, mouse, and timer events.</summary>
    {$endregion}
    property Enabled;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the placement of the flag icon in regard of the text label.</summary>
    {$endregion}
    property FlagPosition;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags;
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
    /// Lists the <see cref="TCultureInfo"/> objects in the control.</summary>
    {$endregion}
    property Items;
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
    property ParentColor;
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
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.</summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the items in the list are arranged alphabetically.</summary>
    {$endregion}
    property Sorted;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the position of the control in its parent's tab order.</summary>
    {$endregion}
    property TabOrder;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can tab to a control.</summary>
    {$endregion}
    property TabStop;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets thet text that is displayed as a text watermark in the control.
    /// NOTE: This feature is only available in Windows Vista and later.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property TextHint;
    {$ENDIF}
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
    /// Occurs when the user clicks the control.</summary>
    {$endregion}
    property OnClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the drop-down list closes up due to some user action.</summary>
    {$endregion}
    property OnCloseUp;
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
    /// Occurs when text label of an item needs to be displayed.</summary>
    {$endregion}
    property OnDrawItemLabel;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user opens the drop-down list.</summary>
    {$endregion}
    property OnDropDown;
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
    /// Occurs when <see cref="Items"/> property changed.</summary>
    {$endregion}
    property OnItemsChange;
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
    /// Occurs when the user selects an item in the drop-down list</summary>
    {$endregion}
    property OnSelect;
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
  /// TCustomDBTerritoryListBox is the base class for data-aware list box controls that
  /// enable users to choose value of a country field from a list of <see cref="TTerritoryInfo"/>
  /// objects.</summary>
  /// <remarks>
  /// Use TCustomDBTerritoryListBox as a base class when defining data-aware list box controls
  /// that provide a list of <see cref="TTerritoryInfo"/> objects to update value of a database
  /// field.
  ///
  /// The type of the database field must be either string (size of 2 or 3 characters) or numeric
  /// to store respectively the internatioal country code or the geographical identifier of the
  /// selected <see cref="TTerritoryInfo"/> object.</remarks>
  {$endregion}
  TCustomDBTerritoryListBox = class(TCustomTerritoryListBox)
  private
    fDataLink: TFieldDataLink;
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetField: TField; inline;
    function GetReadOnly: Boolean; inline;
    procedure SetReadOnly(Value: Boolean); inline;
    procedure UpdateData(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents <see cref="ItemSelected"/> property to be saved and loaded.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the control when it is first loaded into memory.</summary>
    {$endregion}
    procedure Loaded; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to mouse clicks.</summary>
    {$endregion}
    procedure Click; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key.</summary>
    /// <param name="Key">
    /// The key that was pressed. If the method changes Key to 0, processing of the keystroke stops.</param>
    /// <param name="Shift">
    /// Indicates the concurrent state of the Ctrl, Shift, and Alt keys.</param>
    {$endregion}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key while the control has focus.</summary>
    /// <param name="Key">
    /// The key code of the key that was pressed.</param>
    {$endregion}
    procedure KeyPress(var Key: Char); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides specific message responses for the control.</summary>
    /// <param name="Message">
    /// The message's details.</param>
    {$endregion}
    procedure WndProc(var Message: TMessage); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to changes in the <see cref="Items"/> property</summary>
    {$endregion}
    procedure DoItemsChange; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Helps the data-aware control manage its link to the data source and respond
    /// to data events.</summary>
    {$endregion}
    property DataLink: TFieldDataLink read fDataLink;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the country field that is represented by the control.</summary>
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
    /// Gets the <see cref="TField"/> object for the database field that the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TDBTerritoryListBox is a data-aware list box control that enable users to choose
  /// value of a country field from a list of <see cref="TTerritoryInfo"/> objects.</summary>
  /// <remarks>
  /// Use TDBTerritoryListBox to enable users to choose value of a database field, which
  /// represents an international country code or a geographical identifier, from a list of
  /// <see cref="TTerritoryInfo"/> objects.
  ///
  /// The type of the database field must be either string (size of 2 or 3 characters) or numeric
  /// to store respectively the internatioal country code or the geographical identifier of the
  /// selected <see cref="TTerritoryInfo"/> object.
  ///
  /// TDBTerritoryListBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBTerritoryListBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBTerritoryListBox = class(TCustomDBTerritoryListBox)
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
    /// Gets or sets whether the user can give focus to items by typing in the list.</summary>
    {$endregion}
    property AutoComplete;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the delay between a key press and an attempt to <see cref="AutoComplete"/>.</summary>
    {$endregion}
    property AutoCompleteDelay;
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
    property BevelKind;
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
    /// Gets or sets whether the control has a border.</summary>
    {$endregion}
    property BorderStyle;
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
    /// Gets or sets whether the control has a three-dimensional look.</summary>
    {$endregion}
    property Ctl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of columns, in a multi-column list box, that are visible
    /// without having to scroll.</summary>
    {$endregion}
    property Columns;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the country field that is represented by the control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName;
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
    /// Gets or sets the placement of the flag icon in regard of the text label.</summary>
    {$endregion}
    property FlagPosition;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags;
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
    /// Gets or sets whether the list box displays the partial items.</summary>
    {$endregion}
    property IntegralHeight;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCultureInfo"/> objects in the control.</summary>
    {$endregion}
    property Items;
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
    property ParentColor;
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
    /// Gets or sets the width, in pixels, by which the list box can scroll horizontally.</summary>
    {$endregion}
    property ScrollWidth;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.</summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the items in the list are arranged alphabetically.</summary>
    {$endregion}
    property Sorted;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the position of the control in its parent's tab order.</summary>
    {$endregion}
    property TabOrder;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can tab to a control.</summary>
    {$endregion}
    property TabStop;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the size of the tabs in the list box.</summary>
    {$endregion}
    property TabWidth;
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
    /// Occurs when text label of an item needs to be displayed.</summary>
    {$endregion}
    property OnDrawItemLabel;
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
    /// Occurs when <see cref="Items"/> property changed.</summary>
    {$endregion}
    property OnItemsChange;
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

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDBCurrencyLabel is the base class for data-aware label controls that
  /// display value of a currency symbol field.</summary>
  /// <remarks>
  /// Use TCustomDBCurrencyLabel as a base class when defining objects that display
  /// value of a database field, which the field represents an international monetary
  /// symbol.</remarks>
  {$endregion}
  TCustomDBCurrencyLabel = class(TCustomCurrencyLabel)
  private
    fDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetCurrency: TCurrencyInfo; inline;
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetField: TField;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents <see cref="Currency"/> property to be saved and loaded.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
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
    /// Determines the country flag icon that appears on the control.</summary>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon should be displayed.</returns>
    {$endregion}
    function GetImageIndex: TImageIndex; override;
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
    /// Gets or sets the name of the currency symbol field that is represented by the
    /// control.</summary>
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
    /// Gets the <see cref="TField"/> object for the database field that the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TCurrencyInfo"/> object that the value of the currency symbol
    /// field represents.</summary>
    {$endregion}
    property Currency: TCurrencyInfo read GetCurrency;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TDBCurrencyLabel is a data-aware label control that displays value of a currency
  /// symbol field.</summary>
  /// <remarks>
  /// Use TDBCurrencyLabel to display value of a database field on a form, when the field
  /// represents an international monetary symbol.
  ///
  /// TDBCurrencyLabel publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBCurrencyLabel"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBCurrencyLabel = class(TCustomDBCurrencyLabel)
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
    /// Gets or sets the text of the label when the database field has no value or its
    /// value is not a valid international monetary symbol.</summary>
    {$endregion}
    property Caption;
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
    /// Gets or sets the name of the currency symbol field that is represented by the
    /// control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCurrencyInfo.DisplayNames"/> should be
    /// displayed as the text label on the control.</summary>
    {$endregion}
    property DisplayName;
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
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags;
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
    property ShowAccelChar;
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
  /// TCustomDBCurrencyBox is the base class for data-aware combo box controls that enable
  /// users to choose value of a currency symbol field from a list of <see cref="TCurrencyInfo"/>
  /// objects.</summary>
  /// <remarks>
  /// Use TCustomDBCurrencyBox as a base class when defining data-aware combo box controls
  /// that provide a list of <see cref="TCurrencyInfo"/> objects to update value of a database
  /// field.
  ///
  /// The type of the database field must be string (size of 3 characters) to store the
  /// internatioal monetary symbol of the selected <see cref="TCurrencyInfo"/> object.</remarks>
  {$endregion}
  TCustomDBCurrencyBox = class(TCustomCurrencyBox)
  private
    fDataLink: TFieldDataLink;
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetField: TField; inline;
    function GetReadOnly: Boolean; inline;
    procedure SetReadOnly(Value: Boolean); inline;
    procedure UpdateData(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents <see cref="ItemSelected"/> property to be saved and loaded.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the control when it is first loaded into memory.</summary>
    {$endregion}
    procedure Loaded; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to the user's item selection.</summary>
    {$endregion}
    procedure Select; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key.</summary>
    /// <param name="Key">
    /// The key that was pressed. If the method changes Key to 0, processing of the keystroke stops.</param>
    /// <param name="Shift">
    /// Indicates the concurrent state of the Ctrl, Shift, and Alt keys.</param>
    {$endregion}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key while the control has focus.</summary>
    /// <param name="Key">
    /// The key code of the key that was pressed.</param>
    {$endregion}
    procedure KeyPress(var Key: Char); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides specific message responses for the control.</summary>
    /// <param name="Message">
    /// The message's details.</param>
    {$endregion}
    procedure WndProc(var Message: TMessage); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to changes in the <see cref="Items"/> property</summary>
    {$endregion}
    procedure DoItemsChange; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Helps the data-aware control manage its link to the data source and respond
    /// to data events.</summary>
    {$endregion}
    property DataLink: TFieldDataLink read fDataLink;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the currency symbol field that is represented by
    /// the control.</summary>
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
    /// Gets the <see cref="TField"/> object for the database field that the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TDBCurrencyBox is a data-aware combo box control that enable users to choose value
  /// of a currency symbol field from a list of <see cref="TCurrencyInfo"/> objects.</summary>
  /// <remarks>
  /// Use TDBCurrencyBox to enable users to choose value of a database field, which
  /// represents an international monetary symbol, from a list of <see cref="TCurrencyInfo"/>
  /// objects.
  ///
  /// The type of the database field must be string (size of 3 characters) to store the
  /// internatioal monetary symbol of the selected <see cref="TCurrencyInfo"/> object.
  ///
  /// TDBCurrencyListBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBCurrencyBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBCurrencyBox = class(TCustomDBCurrencyBox)
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
    /// Gets or sets whether the user can give focus to items by typing in the list.</summary>
    {$endregion}
    property AutoComplete;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the delay between a key press and an attempt to <see cref="AutoComplete"/>.</summary>
    {$endregion}
    property AutoCompleteDelay;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the drop-down closes up automatically when the user selects an item.</summary>
    {$endregion}
    property AutoCloseUp;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the drop-down list drops down automatically in response to user keystrokes.</summary>
    {$endregion}
    property AutoDropDown;
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
    property BevelKind;
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
    /// Gets or sets whether the control has a three-dimensional look.</summary>
    {$endregion}
    property Ctl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the currency symbol field that is represented by the
    /// control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName;
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
    /// Gets or sets the maximum number of items displayed in the drop-down list.</summary>
    {$endregion}
    property DropDownCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control responds to keyboard, mouse, and timer events.</summary>
    {$endregion}
    property Enabled;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the placement of the flag icon in regard of the text label.</summary>
    {$endregion}
    property FlagPosition;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags;
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
    /// Lists the <see cref="TCultureInfo"/> objects in the control.</summary>
    {$endregion}
    property Items;
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
    property ParentColor;
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
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.</summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the items in the list are arranged alphabetically.</summary>
    {$endregion}
    property Sorted;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the position of the control in its parent's tab order.</summary>
    {$endregion}
    property TabOrder;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can tab to a control.</summary>
    {$endregion}
    property TabStop;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets thet text that is displayed as a text watermark in the control.
    /// NOTE: This feature is only available in Windows Vista and later.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property TextHint;
    {$ENDIF}
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
    /// Occurs when the user clicks the control.</summary>
    {$endregion}
    property OnClick;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the drop-down list closes up due to some user action.</summary>
    {$endregion}
    property OnCloseUp;
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
    /// Occurs when text label of an item needs to be displayed.</summary>
    {$endregion}
    property OnDrawItemLabel;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user opens the drop-down list.</summary>
    {$endregion}
    property OnDropDown;
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
    /// Occurs when <see cref="Items"/> property changed.</summary>
    {$endregion}
    property OnItemsChange;
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
    /// Occurs when the user selects an item in the drop-down list</summary>
    {$endregion}
    property OnSelect;
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
  /// TCustomDBCurrencyListBox is the base class for data-aware list box controls that enable
  /// users to choose value of a currency symbol field from a list of <see cref="TCurrencyInfo"/>
  /// objects.</summary>
  /// <remarks>
  /// Use TCustomDBCurrencyListBox as a base class when defining data-aware list box controls
  /// that provide a list of <see cref="TCurrencyInfo"/> objects to update value of a database
  /// field.
  ///
  /// The type of the database field must be string (size of 3 characters) to store the
  /// internatioal monetary symbol of the selected <see cref="TCurrencyInfo"/> object.</remarks>
  {$endregion}
  TCustomDBCurrencyListBox = class(TCustomCurrencyListBox)
  private
    fDataLink: TFieldDataLink;
    function GetDataField: WideString; inline;
    procedure SetDataField(const Value: WideString); inline;
    function GetDataSource: TDataSource; inline;
    procedure SetDataSource(Value: TDataSource); inline;
    function GetField: TField; inline;
    function GetReadOnly: Boolean; inline;
    procedure SetReadOnly(Value: Boolean); inline;
    procedure UpdateData(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents <see cref="ItemSelected"/> property to be saved and loaded.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the control when it is first loaded into memory.</summary>
    {$endregion}
    procedure Loaded; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to mouse clicks.</summary>
    {$endregion}
    procedure Click; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key.</summary>
    /// <param name="Key">
    /// The key that was pressed. If the method changes Key to 0, processing of the keystroke stops.</param>
    /// <param name="Shift">
    /// Indicates the concurrent state of the Ctrl, Shift, and Alt keys.</param>
    {$endregion}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key while the control has focus.</summary>
    /// <param name="Key">
    /// The key code of the key that was pressed.</param>
    {$endregion}
    procedure KeyPress(var Key: Char); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides specific message responses for the control.</summary>
    /// <param name="Message">
    /// The message's details.</param>
    {$endregion}
    procedure WndProc(var Message: TMessage); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds to changes in the <see cref="Items"/> property</summary>
    {$endregion}
    procedure DoItemsChange; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Helps the data-aware control manage its link to the data source and respond
    /// to data events.</summary>
    {$endregion}
    property DataLink: TFieldDataLink read fDataLink;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the currency symbol field that is represented by
    /// the control.</summary>
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
    /// Gets the <see cref="TField"/> object for the database field that the control
    /// represents.</summary>
    {$endregion}
    property Field: TField read GetField;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TDBCurrencyListBox is a data-aware list box control that enable users to choose value
  /// of a currency symbol field from a list of <see cref="TCurrencyInfo"/> objects.</summary>
  /// <remarks>
  /// Use TDBCurrencyListBox to enable users to choose value of a database field, which
  /// represents an international monetary symbol, from a list of <see cref="TCurrencyInfo"/>
  /// objects.
  ///
  /// The type of the database field must be string (size of 3 characters) to store the
  /// internatioal monetary symbol of the selected <see cref="TCurrencyInfo"/> object.
  ///
  /// TDBCurrencyListBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomDBCurrencyListBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBCurrencyListBox = class(TCustomDBCurrencyListBox)
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
    /// Gets or sets whether the user can give focus to items by typing in the list.</summary>
    {$endregion}
    property AutoComplete;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the delay between a key press and an attempt to <see cref="AutoComplete"/>.</summary>
    {$endregion}
    property AutoCompleteDelay;
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
    property BevelKind;
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
    /// Gets or sets whether the control has a border.</summary>
    {$endregion}
    property BorderStyle;
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
    /// Gets or sets whether the control has a three-dimensional look.</summary>
    {$endregion}
    property Ctl3D;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of columns, in a multi-column list box, that are visible
    /// without having to scroll.</summary>
    {$endregion}
    property Columns;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of the currency symbol field that is represented by the
    /// control.</summary>
    {$endregion}
    property DataField;
    {$region 'xmldoc'}
    /// <summary>
    /// Links the control to the dataset that contains the field it represents.</summary>
    {$endregion}
    property DataSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName;
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
    /// Gets or sets the placement of the flag icon in regard of the text label.</summary>
    {$endregion}
    property FlagPosition;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags;
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
    /// Gets or sets whether the list box displays the partial items.</summary>
    {$endregion}
    property IntegralHeight;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCultureInfo"/> objects in the control.</summary>
    {$endregion}
    property Items;
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
    property ParentColor;
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
    /// Gets or sets the width, in pixels, by which the list box can scroll horizontally.</summary>
    {$endregion}
    property ScrollWidth;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the control displays a Help <see cref="Hint"/> when the mouse
    /// pointer rests momentarily on the control.</summary>
    {$endregion}
    property ShowHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the items in the list are arranged alphabetically.</summary>
    {$endregion}
    property Sorted;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the position of the control in its parent's tab order.</summary>
    {$endregion}
    property TabOrder;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can tab to a control.</summary>
    {$endregion}
    property TabStop;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the size of the tabs in the list box.</summary>
    {$endregion}
    property TabWidth;
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
    /// Occurs when text label of an item needs to be displayed.</summary>
    {$endregion}
    property OnDrawItemLabel;
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
    /// Occurs when <see cref="Items"/> property changed.</summary>
    {$endregion}
    property OnItemsChange;
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
  Types, VDBConsts, Forms, StdCtrls;

{ TCustomDBImageLabel }

constructor TCustomDBImageLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  AutoSize := False;
end;

destructor TCustomDBImageLabel.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBImageLabel.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

function TCustomDBImageLabel.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBImageLabel.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBImageLabel.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBImageLabel.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBImageLabel.GetField: TField;
begin
  if Assigned(DataLink) then
    Result := DataLink.Field
  else
    Result := nil;
end;

function TCustomDBImageLabel.GetLabelText: String;
begin
  if Assigned(Field) then
    Result := inherited GetLabelText
  else if csDesigning in ComponentState then
    Result := Name
  else
    Result := '';
end;

procedure TCustomDBImageLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    if Value and FDataLink.DataSourceFixed then
      DatabaseError(SDataSourceFixed);
    inherited SetAutoSize(Value);
  end;
end;

procedure TCustomDBImageLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBImageLabel.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    Caption := Field.DisplayText
  else
    Caption := '';
end;

function TCustomDBImageLabel.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field)
end;

{ TCustomDBCultureLabel }

constructor TCustomDBCultureLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  AutoSize := False;
end;

destructor TCustomDBCultureLabel.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBCultureLabel.DefineProperties(Filer: TFiler);
begin
  inherited Culture := nil;
  inherited DefineProperties(Filer);
  DataChange(Self);
end;

procedure TCustomDBCultureLabel.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

function TCustomDBCultureLabel.GetCulture: TCultureInfo;
begin
  Result := inherited Culture;
end;

function TCustomDBCultureLabel.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBCultureLabel.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBCultureLabel.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBCultureLabel.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBCultureLabel.GetField: TField;
begin
  if Assigned(DataLink) then
    Result := DataLink.Field
  else
    Result := nil;
end;

function TCustomDBCultureLabel.GetLabelText: String;
begin
  if Assigned(Field) then
    Result := inherited GetLabelText
  else if csDesigning in ComponentState then
    Result := Name
  else
    Result := '';
end;

function TCustomDBCultureLabel.GetImageIndex: TImageIndex;
begin
  if Assigned(Field) then
    Result := inherited GetImageIndex
  else
    Result := -1;
end;

procedure TCustomDBCultureLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    if Value and FDataLink.DataSourceFixed then
      DatabaseError(SDataSourceFixed);
    inherited SetAutoSize(Value);
  end;
end;

procedure TCustomDBCultureLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBCultureLabel.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    if Field is TNumericField then
      inherited Culture := CultureOf(Field.AsInteger)  // Locale ID
    else
      inherited Culture := CultureOf(Field.AsString)   // Locale Name
  else
    inherited Culture := nil;
end;

function TCustomDBCultureLabel.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{ TCustomDBCultureBox }

constructor TCustomDBCultureBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  fDataLink.OnUpdateData := UpdateData;
end;

destructor TCustomDBCultureBox.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBCultureBox.DefineProperties(Filer: TFiler);
begin
  ItemIndex := -1;
  inherited DefineProperties(Filer);
  DataChange(Self);
end;

procedure TCustomDBCultureBox.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TCustomDBCultureBox.Select;
begin
  if DataLink.Edit then
  begin
    inherited Select;
    DataLink.Modified;
  end;
end;

procedure TCustomDBCultureBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key <> VK_TAB) and not DataLink.CanModify then
    Key := 0;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomDBCultureBox.KeyPress(var Key: Char);
begin
  if not DataLink.CanModify then
    Key := #0
  else if Key = #27 then
    DataLink.Reset;
  inherited KeyPress(Key);
end;

procedure TCustomDBCultureBox.CMExit(var Message: TCMExit);
begin
  try
    DataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBCultureBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBCultureBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if not DataLink.Edit then
        begin
          SetFocus;
          with TWMLButtonDown(Message) do
            MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
          Exit;
        end;
      WM_COMMAND:
        if (TWMCommand(Message).NotifyCode = CBN_SELCHANGE) and not DataLink.Edit then
        begin
          PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
          Exit;
        end;
      CB_SHOWDROPDOWN:
        if Message.WParam <> 0 then
          DataLink.Edit
        else if not DataLink.Editing then
          DataChange(Self); // Restore last value
    end;
  inherited WndProc(Message);
end;

procedure TCustomDBCultureBox.UpdateData(Sender: TObject);
begin
  if Assigned(Field) then
  begin
    if Assigned(ItemSelected) then
      if Field is TNumericField then
        Field.AsInteger := ItemSelected.LocaleID
      else
        Field.AsString := ItemSelected.Locale
    else
      Field.Clear;
  end;
end;

procedure TCustomDBCultureBox.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    if Field is TNumericField then
      ItemSelected := CultureOf(Field.AsInteger)
    else
      ItemSelected := CultureOf(Field.AsString)
  else
    ItemSelected := nil;
end;

function TCustomDBCultureBox.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBCultureBox.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBCultureBox.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBCultureBox.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBCultureBox.GetReadOnly: Boolean;
begin
  Result := DataLink.ReadOnly;
end;

procedure TCustomDBCultureBox.SetReadOnly(Value: Boolean);
begin
  DataLink.ReadOnly := Value;
end;

function TCustomDBCultureBox.GetField: TField;
begin
  Result := DataLink.Field;
end;

procedure TCustomDBCultureBox.DoItemsChange;
begin
  DataChange(Self);
  inherited DoItemsChange;
end;

function TCustomDBCultureBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or Assigned(DataLink) and
    DataLink.UpdateAction(Action);
end;

function TCustomDBCultureBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or Assigned(DataLink) and
    DataLink.ExecuteAction(Action);
end;

function TCustomDBCultureBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{ TCustomDBCultureListBox }

constructor TCustomDBCultureListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  fDataLink.OnUpdateData := UpdateData;
end;

destructor TCustomDBCultureListBox.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBCultureListBox.DefineProperties(Filer: TFiler);
begin
  ItemIndex := -1;
  inherited DefineProperties(Filer);
  DataChange(Self);
end;

procedure TCustomDBCultureListBox.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TCustomDBCultureListBox.Click;
begin
  if DataLink.Edit then
  begin
    inherited Click;
    DataLink.Modified;
  end;
end;

procedure TCustomDBCultureListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key <> VK_TAB) and not DataLink.Edit then
    Key := 0;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomDBCultureListBox.KeyPress(var Key: Char);
begin
  if Key = #27 then
    DataLink.Reset
  else if not DataLink.Edit then
    Key := #0;
  inherited KeyPress(Key);
end;

procedure TCustomDBCultureListBox.CMExit(var Message: TCMExit);
begin
  try
    DataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBCultureListBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBCultureListBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if not DataLink.Edit then
        begin
          SetFocus;
          with TWMLButtonDown(Message) do
            MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
          Exit;
        end;
      WM_COMMAND:
        if (TWMCommand(Message).NotifyCode = LBN_SELCHANGE) and not DataLink.Edit then
          Exit;
    end;
  inherited WndProc(Message);
end;

procedure TCustomDBCultureListBox.UpdateData(Sender: TObject);
begin
  if Assigned(Field) then
  begin
    if Assigned(ItemSelected) then
      if Field is TNumericField then
        Field.AsInteger := ItemSelected.LocaleID
      else
        Field.AsString := ItemSelected.Locale
    else
      Field.Clear;
  end;
end;

procedure TCustomDBCultureListBox.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    if Field is TNumericField then
      ItemSelected := CultureOf(Field.AsInteger)
    else
      ItemSelected := CultureOf(Field.AsString)
  else
    ItemSelected := nil;
end;

function TCustomDBCultureListBox.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBCultureListBox.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBCultureListBox.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBCultureListBox.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBCultureListBox.GetReadOnly: Boolean;
begin
  Result := DataLink.ReadOnly;
end;

procedure TCustomDBCultureListBox.SetReadOnly(Value: Boolean);
begin
  DataLink.ReadOnly := Value;
end;

function TCustomDBCultureListBox.GetField: TField;
begin
  Result := DataLink.Field;
end;

procedure TCustomDBCultureListBox.DoItemsChange;
begin
  DataChange(Self);
  inherited DoItemsChange;
end;

function TCustomDBCultureListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or Assigned(DataLink) and
    DataLink.UpdateAction(Action);
end;

function TCustomDBCultureListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or Assigned(DataLink) and
    DataLink.ExecuteAction(Action);
end;

function TCustomDBCultureListBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{ TCustomDBTerritoryLabel }

constructor TCustomDBTerritoryLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  AutoSize := False;
end;

procedure TCustomDBTerritoryLabel.DefineProperties(Filer: TFiler);
begin
  inherited Territory := nil;
  inherited DefineProperties(Filer);
  DataChange(Self);
end;

destructor TCustomDBTerritoryLabel.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBTerritoryLabel.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

function TCustomDBTerritoryLabel.GetTerritory: TTerritoryInfo;
begin
  Result := inherited Territory;
end;

function TCustomDBTerritoryLabel.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBTerritoryLabel.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBTerritoryLabel.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBTerritoryLabel.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBTerritoryLabel.GetField: TField;
begin
  if Assigned(DataLink) then
    Result := DataLink.Field
  else
    Result := nil;
end;

function TCustomDBTerritoryLabel.GetLabelText: String;
begin
  if Assigned(Field) then
    Result := inherited GetLabelText
  else if csDesigning in ComponentState then
    Result := Name
  else
    Result := '';
end;

function TCustomDBTerritoryLabel.GetImageIndex: TImageIndex;
begin
  if Assigned(Field) then
    Result := inherited GetImageIndex
  else
    Result := -1;
end;

procedure TCustomDBTerritoryLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    if Value and FDataLink.DataSourceFixed then
      DatabaseError(SDataSourceFixed);
    inherited SetAutoSize(Value);
  end;
end;

procedure TCustomDBTerritoryLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBTerritoryLabel.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    if Field is TNumericField then
      inherited Territory := TerritoryOf(Field.AsInteger)
    else
      inherited Territory := TerritoryOf(Field.AsString)
  else
    inherited Territory := nil;
end;

function TCustomDBTerritoryLabel.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{ TCustomDBTerritoryBox }

constructor TCustomDBTerritoryBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  fDataLink.OnUpdateData := UpdateData;
end;

destructor TCustomDBTerritoryBox.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBTerritoryBox.DefineProperties(Filer: TFiler);
begin
  ItemIndex := -1;
  inherited DefineProperties(Filer);
  DataChange(Self);
end;

procedure TCustomDBTerritoryBox.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TCustomDBTerritoryBox.Select;
begin
  if DataLink.Edit then
  begin
    inherited Select;
    DataLink.Modified;
  end;
end;

procedure TCustomDBTerritoryBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key <> VK_TAB) and not DataLink.CanModify then
    Key := 0;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomDBTerritoryBox.KeyPress(var Key: Char);
begin
  if not DataLink.CanModify then
    Key := #0
  else if Key = #27 then
    DataLink.Reset;
  inherited KeyPress(Key);
end;

procedure TCustomDBTerritoryBox.CMExit(var Message: TCMExit);
begin
  try
    DataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBTerritoryBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBTerritoryBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if not DataLink.Edit then
        begin
          SetFocus;
          with TWMLButtonDown(Message) do
            MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
          Exit;
        end;
      WM_COMMAND:
        if (TWMCommand(Message).NotifyCode = CBN_SELCHANGE) and not DataLink.Edit then
        begin
          PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
          Exit;
        end;
      CB_SHOWDROPDOWN:
        if Message.WParam <> 0 then
          DataLink.Edit
        else if not DataLink.Editing then
          DataChange(Self); // Restore last value
    end;
  inherited WndProc(Message);
end;

procedure TCustomDBTerritoryBox.UpdateData(Sender: TObject);
begin
  if Assigned(Field) then
  begin
    if Assigned(ItemSelected) then
      if Field is TNumericField then
        Field.AsInteger := ItemSelected.GeoID
      else if Field.Size = 3 then
        Field.AsString := ItemSelected.Code3
      else
        Field.AsString := ItemSelected.Code2
    else
      Field.Clear;
  end;
end;

procedure TCustomDBTerritoryBox.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    if Field is TNumericField then
      ItemSelected := TerritoryOf(Field.AsInteger)
    else
      ItemSelected := TerritoryOf(Field.AsString)
  else
    ItemSelected := nil;
end;

function TCustomDBTerritoryBox.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBTerritoryBox.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBTerritoryBox.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBTerritoryBox.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBTerritoryBox.GetReadOnly: Boolean;
begin
  Result := DataLink.ReadOnly;
end;

procedure TCustomDBTerritoryBox.SetReadOnly(Value: Boolean);
begin
  DataLink.ReadOnly := Value;
end;

function TCustomDBTerritoryBox.GetField: TField;
begin
  Result := DataLink.Field;
end;

procedure TCustomDBTerritoryBox.DoItemsChange;
begin
  DataChange(Self);
  inherited DoItemsChange;
end;

function TCustomDBTerritoryBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or Assigned(DataLink) and
    DataLink.UpdateAction(Action);
end;

function TCustomDBTerritoryBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or Assigned(DataLink) and
    DataLink.ExecuteAction(Action);
end;

function TCustomDBTerritoryBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{ TCustomDBTerritoryListBox }

constructor TCustomDBTerritoryListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  fDataLink.OnUpdateData := UpdateData;
end;

destructor TCustomDBTerritoryListBox.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBTerritoryListBox.DefineProperties(Filer: TFiler);
begin
  ItemIndex := -1;
  inherited DefineProperties(Filer);
  DataChange(Self);
end;

procedure TCustomDBTerritoryListBox.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TCustomDBTerritoryListBox.Click;
begin
  if DataLink.Edit then
  begin
    inherited Click;
    DataLink.Modified;
  end;
end;

procedure TCustomDBTerritoryListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key <> VK_TAB) and not DataLink.Edit then
    Key := 0;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomDBTerritoryListBox.KeyPress(var Key: Char);
begin
  if Key = #27 then
    DataLink.Reset
  else if not DataLink.Edit then
    Key := #0;
  inherited KeyPress(Key);
end;

procedure TCustomDBTerritoryListBox.CMExit(var Message: TCMExit);
begin
  try
    DataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBTerritoryListBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBTerritoryListBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if not DataLink.Edit then
        begin
          SetFocus;
          with TWMLButtonDown(Message) do
            MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
          Exit;
        end;
      WM_COMMAND:
        if (TWMCommand(Message).NotifyCode = LBN_SELCHANGE) and not DataLink.Edit then
          Exit;
    end;
  inherited WndProc(Message);
end;

procedure TCustomDBTerritoryListBox.UpdateData(Sender: TObject);
begin
  if Assigned(Field) then
  begin
    if Assigned(ItemSelected) then
      if Field is TNumericField then
        Field.AsInteger := ItemSelected.GeoID
      else if Field.Size = 3 then
        Field.AsString := ItemSelected.Code3
      else
        Field.AsString := ItemSelected.Code2
    else
      Field.Clear;
  end;
end;

procedure TCustomDBTerritoryListBox.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    if Field is TNumericField then
      ItemSelected := TerritoryOf(Field.AsInteger)
    else
      ItemSelected := TerritoryOf(Field.AsString)
  else
    ItemSelected := nil;
end;

function TCustomDBTerritoryListBox.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBTerritoryListBox.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBTerritoryListBox.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBTerritoryListBox.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBTerritoryListBox.GetReadOnly: Boolean;
begin
  Result := DataLink.ReadOnly;
end;

procedure TCustomDBTerritoryListBox.SetReadOnly(Value: Boolean);
begin
  DataLink.ReadOnly := Value;
end;

function TCustomDBTerritoryListBox.GetField: TField;
begin
  Result := DataLink.Field;
end;

procedure TCustomDBTerritoryListBox.DoItemsChange;
begin
  DataChange(Self);
  inherited DoItemsChange;
end;

function TCustomDBTerritoryListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or Assigned(DataLink) and
    DataLink.UpdateAction(Action);
end;

function TCustomDBTerritoryListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or Assigned(DataLink) and
    DataLink.ExecuteAction(Action);
end;

function TCustomDBTerritoryListBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{ TCustomDBCurrencyLabel }

constructor TCustomDBCurrencyLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  AutoSize := False;
end;

procedure TCustomDBCurrencyLabel.DefineProperties(Filer: TFiler);
begin
  inherited Currency := nil;
  inherited DefineProperties(Filer);
  DataChange(Self);
end;

destructor TCustomDBCurrencyLabel.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBCurrencyLabel.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

function TCustomDBCurrencyLabel.GetCurrency: TCurrencyInfo;
begin
  Result := inherited Currency;
end;

function TCustomDBCurrencyLabel.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBCurrencyLabel.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBCurrencyLabel.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBCurrencyLabel.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBCurrencyLabel.GetField: TField;
begin
  if Assigned(DataLink) then
    Result := DataLink.Field
  else
    Result := nil;
end;

function TCustomDBCurrencyLabel.GetLabelText: String;
begin
  if Assigned(Field) then
    Result := inherited GetLabelText
  else if csDesigning in ComponentState then
    Result := Name
  else
    Result := '';
end;

function TCustomDBCurrencyLabel.GetImageIndex: TImageIndex;
begin
  if Assigned(Field) then
    Result := inherited GetImageIndex
  else
    Result := -1;
end;

procedure TCustomDBCurrencyLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    if Value and FDataLink.DataSourceFixed then
      DatabaseError(SDataSourceFixed);
    inherited SetAutoSize(Value);
  end;
end;

procedure TCustomDBCurrencyLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBCurrencyLabel.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    inherited Currency := CurrencyOf(Field.AsString)
  else
    inherited Currency := nil;
end;

function TCustomDBCurrencyLabel.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{ TCustomDBCurrencyBox }

constructor TCustomDBCurrencyBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  fDataLink.OnUpdateData := UpdateData;
end;

destructor TCustomDBCurrencyBox.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBCurrencyBox.DefineProperties(Filer: TFiler);
begin
  ItemIndex := -1;
  inherited DefineProperties(Filer);
  DataChange(Self);
end;

procedure TCustomDBCurrencyBox.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TCustomDBCurrencyBox.Select;
begin
  if DataLink.Edit then
  begin
    inherited Select;
    DataLink.Modified;
  end;
end;

procedure TCustomDBCurrencyBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key <> VK_TAB) and not DataLink.CanModify then
    Key := 0;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomDBCurrencyBox.KeyPress(var Key: Char);
begin
  if not DataLink.CanModify then
    Key := #0
  else if Key = #27 then
    DataLink.Reset;
  inherited KeyPress(Key);
end;

procedure TCustomDBCurrencyBox.CMExit(var Message: TCMExit);
begin
  try
    DataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBCurrencyBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBCurrencyBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if not DataLink.Edit then
        begin
          SetFocus;
          with TWMLButtonDown(Message) do
            MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
          Exit;
        end;
      WM_COMMAND:
        if (TWMCommand(Message).NotifyCode = CBN_SELCHANGE) and not DataLink.Edit then
        begin
          PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
          Exit;
        end;
      CB_SHOWDROPDOWN:
        if Message.WParam <> 0 then
          DataLink.Edit
        else if not DataLink.Editing then
          DataChange(Self); // Restore last value
    end;
  inherited WndProc(Message);
end;

procedure TCustomDBCurrencyBox.UpdateData(Sender: TObject);
begin
  if Assigned(Field) then
  begin
    if Assigned(ItemSelected) then
      Field.AsString := ItemSelected.IntlSymbol
    else
      Field.Clear;
  end;
end;

procedure TCustomDBCurrencyBox.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    ItemSelected := CurrencyOf(Field.AsString)
  else
    ItemSelected := nil;
end;

function TCustomDBCurrencyBox.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBCurrencyBox.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBCurrencyBox.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBCurrencyBox.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBCurrencyBox.GetReadOnly: Boolean;
begin
  Result := DataLink.ReadOnly;
end;

procedure TCustomDBCurrencyBox.SetReadOnly(Value: Boolean);
begin
  DataLink.ReadOnly := Value;
end;

function TCustomDBCurrencyBox.GetField: TField;
begin
  Result := DataLink.Field;
end;

procedure TCustomDBCurrencyBox.DoItemsChange;
begin
  DataChange(Self);
  inherited DoItemsChange;
end;

function TCustomDBCurrencyBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or Assigned(DataLink) and
    DataLink.UpdateAction(Action);
end;

function TCustomDBCurrencyBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or Assigned(DataLink) and
    DataLink.ExecuteAction(Action);
end;

function TCustomDBCurrencyBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{ TCustomDBCurrencyListBox }

constructor TCustomDBCurrencyListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  fDataLink := TFieldDataLink.Create;
  fDataLink.Control := Self;
  fDataLink.OnDataChange := DataChange;
  fDataLink.OnUpdateData := UpdateData;
end;

destructor TCustomDBCurrencyListBox.Destroy;
begin
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

procedure TCustomDBCurrencyListBox.DefineProperties(Filer: TFiler);
begin
  ItemIndex := -1;
  inherited DefineProperties(Filer);
  DataChange(Self);
end;

procedure TCustomDBCurrencyListBox.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TCustomDBCurrencyListBox.Click;
begin
  if DataLink.Edit then
  begin
    inherited Click;
    DataLink.Modified;
  end;
end;

procedure TCustomDBCurrencyListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key <> VK_TAB) and not DataLink.Edit then
    Key := 0;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomDBCurrencyListBox.KeyPress(var Key: Char);
begin
  if Key = #27 then
    DataLink.Reset
  else if not DataLink.Edit then
    Key := #0;
  inherited KeyPress(Key);
end;

procedure TCustomDBCurrencyListBox.CMExit(var Message: TCMExit);
begin
  try
    DataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBCurrencyListBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(DataLink);
end;

procedure TCustomDBCurrencyListBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if not DataLink.Edit then
        begin
          SetFocus;
          with TWMLButtonDown(Message) do
            MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
          Exit;
        end;
      WM_COMMAND:
        if (TWMCommand(Message).NotifyCode = LBN_SELCHANGE) and not DataLink.Edit then
          Exit;
    end;
  inherited WndProc(Message);
end;

procedure TCustomDBCurrencyListBox.UpdateData(Sender: TObject);
begin
  if Assigned(Field) then
  begin
    if Assigned(ItemSelected) then
      Field.AsString := ItemSelected.IntlSymbol
    else
      Field.Clear;
  end;
end;

procedure TCustomDBCurrencyListBox.DataChange(Sender: TObject);
begin
  if Assigned(Field) and not Field.IsNull then
    ItemSelected := CurrencyOf(Field.AsString)
  else
    ItemSelected := nil;
end;

function TCustomDBCurrencyListBox.GetDataField: WideString;
begin
  Result := DataLink.FieldName;
end;

procedure TCustomDBCurrencyListBox.SetDataField(const Value: WideString);
begin
  DataLink.FieldName := Value;
end;

function TCustomDBCurrencyListBox.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TCustomDBCurrencyListBox.SetDataSource(Value: TDataSource);
begin
  DataLink.DataSource := Value;
end;

function TCustomDBCurrencyListBox.GetReadOnly: Boolean;
begin
  Result := DataLink.ReadOnly;
end;

procedure TCustomDBCurrencyListBox.SetReadOnly(Value: Boolean);
begin
  DataLink.ReadOnly := Value;
end;

function TCustomDBCurrencyListBox.GetField: TField;
begin
  Result := DataLink.Field;
end;

procedure TCustomDBCurrencyListBox.DoItemsChange;
begin
  DataChange(Self);
  inherited DoItemsChange;
end;

function TCustomDBCurrencyListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or Assigned(DataLink) and
    DataLink.UpdateAction(Action);
end;

function TCustomDBCurrencyListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or Assigned(DataLink) and
    DataLink.ExecuteAction(Action);
end;

function TCustomDBCurrencyListBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

end.
