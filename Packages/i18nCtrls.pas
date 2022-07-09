{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements controls that represent locales, countries, and
/// currencies.
unit i18nCtrls;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Messages, Classes, Contnrs, Graphics, Controls, StdCtrls, ImgList,
  {$IFDEF COMPILER_XE3_UP} UITypes, {$ENDIF}
  i18nCore, i18nLocalizer, i18nHashList;

type


  {$IFNDEF COMPILER_XE2_UP}
  TListBoxItemData = TCustomLongData;
  {$ENDIF}

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the ways that the display parts of a control
  /// can be rendered.</summary>
  {$endregion}
  TPartDrawState = (
    {$region 'xmldoc'}
    /// Render as normal.
    {$endregion}
    pdsNormal,
    {$region 'xmldoc'}
    /// The mouse pointer is over the control's part; render in hot state.
    {$endregion}
    pdsHot,
    {$region 'xmldoc'}
    /// The user has pressed the control's part; render in pressed state.
    {$endregion}
    pdsPressed,
    {$region 'xmldoc'}
    /// The control's part is disabled; render as disabled.
    {$endregion}
    pdsDisabled
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the possible locations of a pixel that
  /// specifies the transparent color of a bitmap</summary>
  {$endregion}
  TTransparentPixel = (
    {$region 'xmldoc'}
    /// There is no transparent color.
    {$endregion}
    tpNone,
    {$region 'xmldoc'}
    /// The color of pixel at top-left corner of the bitmap image is the transparent color.
    {$endregion}
    tpTopLeft,
    {$region 'xmldoc'}
    /// The color of pixel at top-right corner of the bitmap image is the transparent color.
    {$endregion}
    tpTopRight,
    {$region 'xmldoc'}
    /// The color of pixel at bottom-right corner of the bitmap image is the transparent color.
    {$endregion}
    tpBottomRight,
    {$region 'xmldoc'}
    /// The color of pixel at bottom-left corner of the bitmap image is the transparent color.
    {$endregion}
    tpBottomLeft
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomFlagImageList is the base class for collections of same-sized country flag
  /// images, each of which can be referred to by its index or international country code.</summary>
  /// <remarks>
  /// The flag image list is a special image list that stores a collection of icons or
  /// birmaps, which are country flags. The flag image list has methods to facilitate
  /// retrieving, and drawing of the stored flag images.
  ///
  /// Use TCustomFlagImageList as a base class for image lists that stores image of
  /// country flags.</remarks>
  {$endregion}
  TCustomFlagImageList = class abstract(TCustomImageList)
  private
    Regions: TKeyLookup<String,Integer>;
    HelperDC: HDC;
    {$IFDEF COMPILER2010_UP}
    function GetColorDepth: TColorDepth;
    procedure SetColorDepth(Value: TColorDepth);
    {$ENDIF}
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents bitmap data to be saved and loaded.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the flag images.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Resizes a bitmap specified by its handle to be the same size of the flag
    /// image list entries. This method also makes the color specified by its pixel
    /// location transparent.</summary>
    /// <param name="hImage">
    /// Handle of the bitmap image.</param>
    /// <param name="TranparentPixel">
    /// The pixel location of the transparent color.</param>
    /// <see cref="NormalizeFlag"/>
    {$endregion}
    procedure NormalizeImage(var hImage: HBITMAP; TranparentPixel: TTransparentPixel = tpNone);
    {$region 'xmldoc'}
    /// <summary>
    /// Checks size of the flag image specified by its handle. If the image's size
    /// is not as same size as the image list entries, it resizes the image. This
    /// method also makes non-rectangular flags (for example, Nepal's flag) transparent.
    ///
    /// NormalizeFlag calls <see cref="NormalizeImage"/> to resize the flag image and
    /// make it transparent.</summary>
    /// <param name="hImage">
    /// Handle of the flag's bitmap image.</param>
    /// <param name="ISO_3166_2">
    /// Two-character international country code of the flag.</param>
    /// <see cref="NormalizeFlag"/>
    {$endregion}
    procedure NormalizeFlag(var hImage: HBITMAP; const ISO_3166_2: String); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Addes the flag image of a country/region specified by its two characters ISO 3166/2
    /// international country code.
    ///
    /// AddFlags calls <see cref="NormalizeFlag"/> to resize the image to the size of
    /// image list entries, and make it transparent as well.</summary>
    /// <param name="ISO_3166_2">
    /// Two-character international country code of the flag.</param>
    /// <param name="hImage">
    /// Handle of the flag's bitmap image.</param>
    /// <returns>
    /// Index of the newly added flag in the image list.</returns>
    {$endregion}
    function AddFlag(const ISO_3166_2: String; hImage: HBITMAP): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets size of image list entries and adds flag images to it.</summary>
    {$endregion}
    procedure PrepareFlags; virtual; abstract;
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
    /// Finds index of a flag image represented by its two characters ISO 3166/2
    /// international country code.</summary>
    /// <param name="ISO_3166_2">
    /// Two-character international country code of the flag.</param>
    /// <returns>
    /// Index of the flag image or -1 if flag is not found.</returns>
    {$endregion}
    function ImageIndexOf(const ISO_3166_2: String): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a flag image represented by its <see cref="TTerritoryInfo"/> object.</summary>
    /// <param name="Territory">
    /// The <see cref="TTerritoryInfo"/> object</param>
    /// <returns>
    /// Index of the flag image or -1 if flag is not found.</returns>
    {$endregion}
    function ImageIndexOf(Territory: TTerritoryInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a flag image represented by its <see cref="TCultureInfo"/> object.</summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object</param>
    /// <returns>
    /// Index of the flag image or -1 if flag is not found.</returns>
    {$endregion}
    function ImageIndexOf(Culture: TCultureInfo): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds index of a flag image represented by its <see cref="TCurrencyInfo"/> object.</summary>
    /// <param name="Currency">
    /// The <see cref="TCurrencyInfo"/> object</param>
    /// <returns>
    /// Index of the flag image or -1 if flag is not found.</returns>
    {$endregion}
    function ImageIndexOf(Currency: TCurrencyInfo): Integer; overload;
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of bits per pixel used for colors of the flag images.</summary>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    property ColorDepth: TColorDepth read GetColorDepth write SetColorDepth default cdDeviceDependent;
    {$ENDIF}
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomResFlagImageList is the base class for those flag image lists components,
  /// which load the country flag images from an application's resource.</summary>
  /// <remarks>
  /// The flag image list is a special image list that stores a collection of icons or
  /// birmaps, which are country flags. The flag image list has methods to facilitate
  /// retrieving, and drawing of the stored flag images.
  ///
  /// Use TCustomResFlagImageList as a base class when defining flag image lists that
  /// loads the flag images from the application's resource. TCustomResFlagImageList
  /// expects each image to be a bitmap resource named as <c>PREFIXcc</c>, where
  /// <c>PREFIX</c> is a custom prefix shared with all flag images, and <c>cc</c> is the
  /// two characters ISO 3166/2 international country code of the flag image.</remarks>
  {$endregion}
  TCustomResFlagImageList = class(TCustomFlagImageList)
  private
    function AddFlagFromResource(const ISO_3166_2: String;
      const ResName: String; hModule: HMODULE = 0): Integer;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns size of each individual flag image in pixels.</summary>
    /// <returns>
    /// width and height of the image list entries.</returns>
    {$endregion}
    function GetFlagSize: TSize; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the prefix used in name of each flag's bitmap recource.</summary>
    /// <returns>
    /// The prefix of bitmap recource names.</returns>
    {$endregion}
    function GetResourcePrefix: String; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets size of image list entries and loads flag images from the application
    /// resource.</summary>
    {$endregion}
    procedure PrepareFlags; override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TFlagImageList is a special image list component that provides country flag
  /// icons.</summary>
  /// <remarks>
  /// The flag image list is a special image list that stores a collection of icons or
  /// birmaps, which are country flags. The flag image list has methods to facilitate
  /// retrieving, and drawing of the stored flag images.
  ///
  /// Size of flag icons provided by TFlagImageList is 16 by 11 pixels.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TFlagImageList = class(TCustomResFlagImageList)
  protected
    function GetFlagSize: TSize; override;
    function GetResourcePrefix: String; override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomImageLabel is the base class for label controls that can include an image
  /// on their face.</summary>
  /// <remarks>
  /// Use TCustomImageLabel as a base class when defining objects that display an icon
  /// followed by a text on a form. TCustomImageLabel implements properties to specify
  /// the icon from an image list component, along with its appearance and placement on
  /// the label.</remarks>
  {$endregion}
  TCustomImageLabel = class abstract(TCustomLabel)
  private
    fSpacing: Integer;
    fImageIndex: TImageIndex;
    fPadding: TPadding;
    fBorderWidth: TBorderWidth;
    fBorderColor: TColor;
    fImages: TCustomImageList;
    fImagesLink: TChangeLink;
    procedure SetSpacing(Value: Integer);
    procedure SetPadding(Value: TPadding);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetBorderColor(Value: TColor);
    procedure SetImages(Value: TCustomImageList);
    procedure ImagesChanged(Sender: TObject);
    procedure PaddingChanged(Sender: TObject);
    procedure CNVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
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
    /// Draws the label's caption along with its image on a specified rectangle.</summary>
    /// <param name="Rect">
    /// Specifies the boundaries of the text and the image within the client area.</param>
    /// <param name="DrawFlags">
    /// Flags for Windows API function, DrawText.</param>
    {$endregion}
    procedure DoDrawText(var Rect: TRect; DrawFlags: Longint); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the index of the label's image in the <see cref="Images"/> image list.</summary>
    /// <returns>
    /// The index of the label's image in the image list or -1 if there is no image.</returns>
    /// <seealso cref="SetImageIndex"/>
    {$endregion}
    function GetImageIndex: TImageIndex; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the label's image by specifying its index in the <see cref="Images"/> image list.</summary>
    /// <param name="Value">
    /// The index of the label's image in the image list or -1 if there is no image.</param>
    /// <seealso cref="GetImageIndex"/>
    {$endregion}
    procedure SetImageIndex(Value: TImageIndex); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Resizes the label when the text or image changes.</summary>
    {$endregion}
    procedure AdjustBounds; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Draws the text and image of the label's client area.</summary>
    {$endregion}
    procedure Paint; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the color of the control's border.</summary>
    {$endregion}
    property BorderColor: TColor read fBorderColor write SetBorderColor default clNone;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the width of the control's border.</summary>
    {$endregion}
    property BorderWidth: TBorderWidth read fBorderWidth write SetBorderWidth default 0;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which image is displayed as the icon on the label.</summary>
    {$endregion}
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the images that can appear on the control.</summary>
    {$endregion}
    property Images: TCustomImageList read fImages write SetImages;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the number of pixels between the image and the text of the control.</summary>
    {$endregion}
    property Spacing: Integer read fSpacing write SetSpacing default 4;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the distance between the image and the text from the control's border.</summary>
    {$endregion}
    property Padding: TPadding read fPadding write SetPadding;
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
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TImageLabel is a label control that can include an image on its face.</summary>
  /// <remarks>
  /// Use TImageLabel to display an icon followed by a text on a form. The <see cref="ImageIndex"/>
  /// property determines which image of the image list identified by the <see cref="Images"/>
  /// property will be displayed.
  ///
  /// TImageLabel control can have a border. The <see cref="BorderColor"/> and
  /// <see cref="BorderWidth"/> properties determine color and thickness of the
  /// border.
  ///
  /// TImageLabel publishes many of the properties, events, and methods of
  /// <see cref="TCustomImageLabel"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TImageLabel = class(TCustomImageLabel)
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
    property AutoSize;
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
    /// Gets or sets the text of the label.</summary>
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
  /// TCustomFlagLabel is the base class for label controls that display a country
  /// flag icon on their face.</summary>
  /// <remarks>
  /// Use TCustomFlagLabel as a base class when defining objects that display an country
  /// flag icon followed by a text on a form. TCustomFlagLabel implements properties that
  /// specify the flag icon from a flag image list component, along with its appearance
  /// and placement on the label.</remarks>
  {$endregion}
  TCustomFlagLabel = class abstract(TCustomImageLabel)
  private
    procedure SetFlags(Value: TCustomFlagImageList);
    function GetFlags: TCustomFlagImageList;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags: TCustomFlagImageList read GetFlags write SetFlags;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the control.</summary>
    /// <param name="AOwner">
    /// The owner component.</param>
    {$endregion}
    constructor Create(AOwner: TComponent); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the poosible locations of a country flag
  /// icon in regard of a text label.</summary>
  {$endregion}
  TFlagPosition = (
    {$region 'xmldoc'}
    /// The country flag places before the text label.
    {$endregion}
    fpBeforeLabel,
    {$region 'xmldoc'}
    /// The country flag places after the text label.
    {$endregion}
    fpAfterLabel
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDrawLabelEvent is the type for event handlers that respond when label of
  /// a list item is being rendered.</summary>
  /// <param name="Sender">
  /// The object that generated the event.</param>
  /// <param name="Index">
  /// The index of the item, which its label is being rendered.</param>
  /// <param name="Rect">
  /// The bounding rectangle of the item's label on the countrl's canvas.</param>
  /// <param name="State">
  /// The state information that can affect the way the item is drawn.</param>
  /// <param name="TheLabel">
  /// The text of the item's label.</param>
  /// <param name="DefaultDraw">Indicates whether the default rendering should proceed after
  /// the event handler exits.</param>
  {$endregion}
  TCustomDrawItemLabelEvent = procedure(Sender: TObject; Index: Integer;
    var Rect: TRect; State: TOwnerDrawState;
    var TheLabel: String; var DefaultDraw: Boolean) of object;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomFlagBox is the base class for combo box controls that represent a list
  /// of country/region related items.</summary>
  /// <remarks>
  /// Use TCustomFlagBox as a base class when defining combo box controls that represent a
  /// list of country/region related items. TCustomFlagBox implements properties to specify
  /// a country flag icon, along with its appearance and placement for each individual item.</remarks>
  {$endregion}
  TCustomFlagBox = class abstract(TCustomComboBox)
  private
    fSorted: Boolean;
    fFlags: TCustomFlagImageList;
    fFlagPosition: TFlagPosition;
    fStandardItemHeight: Integer;
    fItemIndex: Integer;
    fItemsReady: Boolean;
    fOnDrawItemLabel: TCustomDrawItemLabelEvent;
    fOnItemsChange: TNotifyEvent;
    fFlagsLink: TChangeLink;
    procedure SetSorted(Value: Boolean);
    procedure SetFlagPosition(Value: TFlagPosition);
    procedure SetFlags(Value: TCustomFlagImageList);
    procedure FlagsChanged(Sender: TObject);
    procedure ResetItemHeight;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnter(var Message: TMessage); message CM_ENTER;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Renders an individual item in the control.</summary>
    /// <param name="Index">
    /// The index of the item to draw.</param>
    /// <param name="Rect">
    /// The boundaries of the item on the control's canvas, in client coordinates.</param>
    /// <param name="State">
    /// The state information that can affect the way the item is drawn.</param>
    {$endregion}
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
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
    /// Initializes the window-creation parameter record.</summary>
    /// <param name="Params">
    /// The window-creation parameter record.</param>
    {$endregion}
    procedure CreateParams(var Params: TCreateParams); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates the underlying Windows control that implements the control.</summary>
    {$endregion}
    procedure CreateWnd; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Regenerates the internal list of items.</summary>
    {$endregion}
    procedure RebuildInternalItems; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnDrawItemLabel"/> event.</summary>
    /// <param name="Index">
    /// The index of the item, which its label is being rendered.</param>
    /// <param name="Rect">
    /// The bounding rectangle of the item's label on the countrl's canvas.</param>
    /// <param name="State">
    /// The drawing state of the item.</param>
    /// <param name="TheLabel">
    /// The text of the item's label.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the default rendering should proceed after
    /// the method exits, otherwise returns <see langword="false"/>.</returns>
    {$endregion}
    function DoDrawItemLabel(Index: Integer; var Rect: TRect;
      State: TOwnerDrawState; var TheLabel: String): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnItemsChange"/> event.</summary>
    {$endregion}
    procedure DoItemsChange; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the standard height of each item, in pixels.</summary>
    /// <returns>
    /// The height of each item, in pixels.</returns>
    {$endregion}
    function GetStandardItemHeight: Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the index of the selected item.</summary>
    /// <returns>
    /// The index of the selected item.</returns>
    /// <seealso cref="SetItemIndex"/>
    {$endregion}
    function GetItemIndex: Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Selects an item specified by its index.</summary>
    /// <param name="Value">
    /// The index of item to select.</param>
    /// <seealso cref="GetItemIndex"/>
    {$endregion}
    procedure SetItemIndex(const Value: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns an image to display for a particular item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of an image.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a text label to display for a particular item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The text label.</returns>
    {$endregion}
    function GetLabelText(Index: Integer): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the standard height of each item, in pixels.</summary>
    {$endregion}
    property StandardItemHeight: Integer read fStandardItemHeight;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the internal list of items is generated.</summary>
    {$endregion}
    property ItemsReady: Boolean read fItemsReady;
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
    /// Fills the list with all the possible items.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the method succeeded, otherwise returns
    /// <see langword="false"/>.</returns>
    {$endregion}
    function CollectAll: Boolean; virtual; abstract;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the placement of the country flag icon in regard of the text label.</summary>
    {$endregion}
    property FlagPosition: TFlagPosition read fFlagPosition write SetFlagPosition default fpBeforeLabel;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags: TCustomFlagImageList read fFlags write SetFlags;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the items in the list are arranged alphabetically.</summary>
    {$endregion}
    property Sorted: Boolean read fSorted write SetSorted default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when text label of an item needs to be displayed.</summary>
    {$endregion}
    property OnDrawItemLabel: TCustomDrawItemLabelEvent read fOnDrawItemLabel write fOnDrawItemLabel;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the list is changed.</summary>
    {$endregion}
    property OnItemsChange: TNotifyEvent read fOnItemsChange write fOnItemsChange;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomFlagListBox is the base class for list box controls that represent a list
  /// of country/region related items.</summary>
  /// <remarks>
  /// Use TCustomFlagListBox as a base class when defining list box controls that represent a
  /// list of country/region related items. TCustomFlagListBox implements properties to specify
  /// a country flag icon, along with its appearance and placement for each individual item.</remarks>
  {$endregion}
  TCustomFlagListBox = class abstract(TCustomListBox)
  private
    fSorted: Boolean;
    fFlags: TCustomFlagImageList;
    fFlagPosition: TFlagPosition;
    fStandardItemHeight: Integer;
    fItemIndex: Integer;
    fItemsReady: Boolean;
    fOnDrawItemLabel: TCustomDrawItemLabelEvent;
    fOnItemsChange: TNotifyEvent;
    fFlagsLink: TChangeLink;
    procedure SetSorted(Value: Boolean);
    procedure SetFlagPosition(Value: TFlagPosition);
    procedure SetFlags(Value: TCustomFlagImageList);
    procedure FlagsChanged(Sender: TObject);
    procedure ResetItemHeight;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Renders an individual item in the control.</summary>
    /// <param name="Index">
    /// The index of the item to draw.</param>
    /// <param name="Rect">
    /// The boundaries of the item on the control's canvas, in client coordinates.</param>
    /// <param name="State">
    /// The state information that can affect the way the item is drawn.</param>
    {$endregion}
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
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
    /// Creates the underlying Windows control that implements the control.</summary>
    {$endregion}
    procedure CreateWnd; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Regenerates the internal list of items.</summary>
    {$endregion}
    procedure RebuildInternalItems; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnDrawItemLabel"/> event.</summary>
    /// <param name="Index">
    /// The index of the item, which its label is being rendered.</param>
    /// <param name="Rect">
    /// The bounding rectangle of the item's label on the countrl's canvas.</param>
    /// <param name="State">
    /// The drawing state of the item.</param>
    /// <param name="TheLabel">
    /// The text of the item's label.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the default rendering should proceed after
    /// the method exits, otherwise returns <see langword="false"/>.</returns>
    {$endregion}
    function DoDrawItemLabel(Index: Integer; var Rect: TRect;
      State: TOwnerDrawState; var TheLabel: String): Boolean; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnItemsChange"/> event.</summary>
    {$endregion}
    procedure DoItemsChange; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the standard height of each item, in pixels.</summary>
    /// <returns>
    /// The height of each item, in pixels.</returns>
    {$endregion}
    function GetStandardItemHeight: Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the index of the selected item.</summary>
    /// <returns>
    /// The index of the selected item.</returns>
    /// <seealso cref="SetItemIndex"/>
    {$endregion}
    function GetItemIndex: Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Selects an item specified by its index.</summary>
    /// <param name="Value">
    /// The index of item to select.</param>
    /// <seealso cref="GetItemIndex"/>
    {$endregion}
    procedure SetItemIndex(const Value: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns an image to display for a particular item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of an image.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a text label to display for a particular item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The text label.</returns>
    {$endregion}
    function GetLabelText(Index: Integer): String; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the standard height of each item, in pixels.</summary>
    {$endregion}
    property StandardItemHeight: Integer read fStandardItemHeight;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the internal list of items is generated.</summary>
    {$endregion}
    property ItemsReady: Boolean read fItemsReady;
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
    /// Fills the list with all the possible items.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the method succeeded, otherwise returns
    /// <see langword="false"/>.</returns>
    {$endregion}
    function CollectAll: Boolean; virtual; abstract;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the placement of the country flag icon in regard of the text label.</summary>
    {$endregion}
    property FlagPosition: TFlagPosition read fFlagPosition write SetFlagPosition default fpBeforeLabel;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the flag icons that can appear on the control.</summary>
    {$endregion}
    property Flags: TCustomFlagImageList read fFlags write SetFlags;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the items in the list are arranged alphabetically.</summary>
    {$endregion}
    property Sorted: Boolean read fSorted write SetSorted default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when text label of an item needs to be displayed.</summary>
    {$endregion}
    property OnDrawItemLabel: TCustomDrawItemLabelEvent read fOnDrawItemLabel write fOnDrawItemLabel;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the list is changed.</summary>
    {$endregion}
    property OnItemsChange: TNotifyEvent read fOnItemsChange write fOnItemsChange;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class stores state of items in a check list for later retrieval.</summary>
  /// <remarks>
  /// Use TCheckListRecall to save current state of items in a check list.
  ///
  /// To restore the state of items in a check list to its original state, all that
  /// is necessary is to free the TCheckListRecall object and the referenced check
  /// list is automatically restored to the saved sates.
  ///
  /// You can update the TCheckListRecall instance to reflect the current state of the
  /// referenced check list by calling the <see cref="Store"/> method. You can prevent
  /// the TCheckListRecall destructor from updating the referenced check list by calling
  /// the <see cref="Forget"/> method.</remarks>
  {$endregion}
  TCheckListRecall = class(TPersistent)
  private
    type
      TStateRec = packed record
        State: TCheckBoxState;
        Disabled: Boolean;
      end;
  private
    CheckList: TPersistent;
    StateInfo: TKeyLookup<String,TStateRec>;
    function StateRec(AState: TCheckBoxState; ADisabled: Boolean): TStateRec; inline;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.</summary>
    /// <param name="AReference">
    /// The check list object to save and restore its state.</param>
    {$endregion}
    constructor Create(AReference: TPersistent);
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the TCheckListRecall instance, assigning the stored state to the
    /// referenced object.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Saves the current state of the check list referenced by <see cref="Reference"/> property.</summary>
    /// <seealso cref="Forget"/>
    {$endregion}
    procedure Store;
    {$region 'xmldoc'}
    /// <summary>
    /// Discards the saved states and prevents the TCheckListRecall object from updating the
    /// check list referenced by <see cref="Reference"/> property.</summary>
    /// <seealso cref="Store"/>
    {$endregion}
    procedure Forget;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies state of items from another object.</summary>
    /// <param name="Source">
    /// The source object.</param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies state of items to another object.</summary>
    /// <param name="Dest">
    /// The destination object.</param>
    {$endregion}
    procedure AssignTo(Dest: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the reference to the check list object that is updated when you destroy
    /// the TCheckListRecall instance.</summary>
    {$endregion}
    property Reference: TPersistent read CheckList write CheckList;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomFlagCheckListBox is the base class for check list box controls that represent
  /// a list of country/region related items.</summary>
  /// <remarks>
  /// Use TCustomFlagCheckListBox as a base class when defining check list box controls that
  /// represent a list of country/region related items. TCustomFlagCheckListBox implements
  /// properties to specify a country flag icon, along with its appearance and placement for
  /// each individual item.</remarks>
  {$endregion}
  TCustomFlagCheckListBox = class abstract(TCustomFlagListBox)
  private
    fFlat: Boolean;
    fAllowGrayed: Boolean;
    fOnClickCheck: TNotifyEvent;
    fWrapperList: TObjectList;
    fCheckWidth: Integer;
    procedure SetFlat(Value: Boolean);
    procedure SetChecked(Index: Integer; Value: Boolean);
    function GetChecked(Index: Integer): Boolean;
    procedure SetState(Index: Integer; Value: TCheckBoxState);
    function GetState(Index: Integer): TCheckBoxState;
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; Value: Boolean);
    procedure ToggleClickCheck(Index: Integer);
    procedure InvalidateCheck(Index: Integer);
    function CreateWrapper(Index: Integer): TObject;
    function ExtractWrapper(Index: Integer): TObject;
    function GetWrapper(Index: Integer): TObject;
    function HaveWrapper(Index: Integer): Boolean;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Renders a check box on the control's canvas.</summary>
    /// <param name="Rect">
    /// The boundaries of the check box on the control's canvas, in client coordinates.</param>
    /// <param name="AState">
    /// The state of the check box.</param>
    /// <param name="AEnabled">
    /// Determines whether the check box is enabled.</param>
    {$endregion}
    procedure DrawCheck(const Rect: TRect; AState: TCheckBoxState; AEnabled: Boolean);
    {$region 'xmldoc'}
    /// <summary>
    /// Renders an individual item in the control.</summary>
    /// <param name="Index">
    /// The index of the item to draw.</param>
    /// <param name="Rect">
    /// The boundaries of the item on the control's canvas, in client coordinates.</param>
    /// <param name="State">
    /// The state information that can affect the way the item is drawn.</param>
    {$endregion}
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the data associated with an item in the check list box.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The data associated with the item.</returns>
    /// <seealso cref="InternalSetItemData"/>
    {$endregion}
    function InternalGetItemData(Index: Integer): TListBoxItemData; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Associates a data with a specified item in the check list box.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <param name="AData">
    /// The data to associate with the item.</param>
    /// <seealso cref="InternalGetItemData"/>
    {$endregion}
    procedure InternalSetItemData(Index: Integer; AData: TListBoxItemData); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the state data associated with an item in the check list box.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The data associated with the item.</returns>
    /// <seealso cref="SetItemData"/>
    {$endregion}
    function GetItemData(Index: Integer): TListBoxItemData; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the state data associated with a specified item in the check list box.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <param name="AData">
    /// The data to associate with the item.</param>
    /// <seealso cref="GetItemData"/>
    {$endregion}
    procedure SetItemData(Index: Integer; AData: TListBoxItemData); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Loads the internal list of items from a <see cref="TStrings"/> object.</summary>
    /// <param name="RecreateItems">
    /// The source <see cref="TStrings"/> object.</param>
    /// <seealso cref="SaveRecreateItems"/>
    {$endregion}
    procedure LoadRecreateItems(RecreateItems: TStrings); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Saves the internal list of items into a <see cref="TStrings"/> object.</summary>
    /// <param name="RecreateItems">
    /// The destination <see cref="TStrings"/> object.</param>
    /// <seealso cref="LoadRecreateItems"/>
    {$endregion}
    procedure SaveRecreateItems(RecreateItems: TStrings); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a mouse button down while the cursor's hotspot is
    /// over the control.</summary>
    /// <param name="Button">
    /// Determines which mouse button the user pressed.</param>
    /// <param name="Shift">
    /// Indicates which shift keys (Shift, Ctrl, or Alt) were down when the user pressed the mouse button.</param>
    /// <param name="X">
    /// The horizontal pixel coordinate of the mouse pointer within the client area of the control.</param>
    /// <param name="Y">
    /// The vertical pixel coordinate of the mouse pointer within the client area of the control.</param>
    {$endregion}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when the user presses a key while the control has focus.</summary>
    /// <param name="Key">
    /// The key code of the key that was pressed.</param>
    {$endregion}
    procedure KeyPress(var Key: Char); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes an item from the internal list of items, and releases its associated data.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    {$endregion}
    procedure DeleteString(Index: Integer); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes all of the items from the internal list of items.</summary>
    {$endregion}
    procedure ResetContent; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnClickCheck"/> event.</summary>
    {$endregion}
    procedure ClickCheck; dynamic;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the standard height of each item, in pixels.</summary>
    /// <returns>
    /// The height of each item, in pixels.</returns>
    {$endregion}
    function GetStandardItemHeight: Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the width of check boxes, in pixels.</summary>
    {$endregion}
    property CheckWidth: Integer read fCheckWidth;
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
    /// Changes the state of all items to a specified state.</summary>
    /// <param name="AState">
    /// Specifies the new state of the items.</param>
    /// <param name="AllowGrayed">
    /// Determines whether the items in "grayed" state can be altered.</param>
    /// <param name="AllowDisabled">
    /// Determines whether the disabled items can be altered.</param>
    /// <seealso cref="Checked"/>
    /// <seealso cref="State"/>
    /// <seealso cref="ItemEnabled"/>
    /// <seealso cref="AllowGrayed"/>
    {$endregion}
    procedure RestateAll(AState: TCheckBoxState;
      AllowGrayed: Boolean = True; AllowDisabled: Boolean = True);
    {$region 'xmldoc'}
    /// <summary>
    /// Lists which items are checked.</summary>
    {$endregion}
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the state of individual items in the list.</summary>
    {$endregion}
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists enabled or disabled state of individual items in the list.</summary>
    {$endregion}
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check boxes can be in a "grayed" state.</summary>
    {$endregion}
    property AllowGrayed: Boolean read fAllowGrayed write fAllowGrayed default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check boxes appear three-dimensional.</summary>
    {$endregion}
    property Flat: Boolean read fFlat write SetFlat default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the user selects or deselects an item's check box.</summary>
    {$endregion}
    property OnClickCheck: TNotifyEvent read fOnClickCheck write fOnClickCheck;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomCultureLabel is the base class for label controls that display a
  /// <see cref="TCultureInfo"/> object on a form.</summary>
  /// <remarks>
  /// Use TCustomCultureLabel as a base class when defining objects that display a
  /// <see cref="TCultureInfo"/> object. TCustomCultureLabel implements properties to
  /// specify the <see cref="TCultureInfo"/> object, along with its interested piece
  /// of information that should be displayed.
  ///
  /// TCustomCultureLabel can be linked to a <see cref="TLocalizer"/> component. In
  /// this case, the <see cref="TLocalizer"/> component determines the culture being
  /// displayed by the control.</remarks>
  {$endregion}
  TCustomCultureLabel = class(TCustomFlagLabel, ILocalizerLink)
  private
    fCulture: TCultureInfo;
    fDisplayName: TCultureDisplayName;
    fLocalizer: TLocalizer;
    procedure SetCulture(Value: TCultureInfo);
    procedure SetDisplayName(Value: TCultureDisplayName);
    procedure SetLocalizer(Value: TLocalizer);
    procedure ReadCulture(Reader: TReader);
    procedure WriteCulture(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Notifies the control about changes in the linked <see cref="TLocalizer"/>
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
    /// Reads and writes the <see cref="Culture"/> property as if it was published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon to display on the control.</summary>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon should be displayed.</returns>
    {$endregion}
    function GetImageIndex: TImageIndex; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a text label to display for on the control.</summary>
    /// <returns>
    /// The text label.</returns>
    {$endregion}
    function GetLabelText: String; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the control.</summary>
    {$endregion}
    property Localizer: TLocalizer read fLocalizer write SetLocalizer;
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
    /// Destroys the component and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the value of <see cref="Culture"/> property is controlled
    /// by a <see cref="TLocalizer"/> component.</summary>
    /// <returns>
    /// Rteurns <see langword="true"/> if <see cref="Localizer"/> property is not
    /// <see langword="nil"/>, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="Localizer"/>
    {$endregion}
    function IsManagedByLocalizer: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the text label on the control.</summary>
    {$endregion}
    property DisplayName: TCultureDisplayName read fDisplayName write SetDisplayName default cnEnglishDisplayName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCultureInfo"/> object that the control is displaying.</summary>
    {$endregion}
    property Culture: TCultureInfo read fCulture write SetCulture;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCultureLabel is a label control that displays a <see cref="TCultureInfo"/> object
  /// on a form.</summary>
  /// <remarks>
  /// Use TCultureLabel to display a <see cref="TCultureInfo"/> object on a form. The
  /// <see cref="Culture"/> and <see cref="DisplayName"/> properties determine the content
  /// that will be displayed.
  ///
  /// TCultureLabel can be linked to a <see cref="TLocalizer"/> component. In this case,
  /// the <see cref="TLocalizer"/> component determines the <see cref="TCultureInfo"/>
  /// being displayed by the control.
  ///
  /// TCultureLabel publishes many of the properties, events, and methods of
  /// <see cref="TCustomCultureLabel"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCultureLabel = class(TCustomCultureLabel)
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
    property AutoSize;
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
    /// Gets or sets the text of the label when <see cref="Culture"/> property has no value.</summary>
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
    /// Gets or sets the <see cref="TCultureInfo"/> object that the control is displaying.</summary>
    {$endregion}
    property Culture;
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
  /// TCustomCultureBox is the base class for combo box controls that represent a list
  /// of <see cref="TCultureInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCustomCultureBox as a base class when defining combo box controls that represent
  /// a list of <see cref="TCultureInfo"/> objects. TCustomCultureBox has properties and
  /// methods to facilitate displaying, retrieving, sorting, and selecting of the stored
  /// <see cref="TCultureInfo"/> objects.
  ///
  /// TCustomCultureBox also implements properties to specify a country flag icon, along
  /// with its appearance and placement for each individual item.
  ///
  /// TCustomCultureBox can be linked to a <see cref="TLocalizer"/> component. In this case,
  /// the <see cref="TLocalizer"/> component provides the <see cref="TCultureInfo"/> objects
  /// for the <see cref="Items"/> property. Also the selected item of the control
  /// will be synchronized with the current culture of the <see cref="TLocalizer"/> component.</remarks>
  {$endregion}
  TCustomCultureBox = class(TCustomFlagBox, ILocalizerLink)
  private
    fCultures: TCultureList;
    fDisplayName: TCultureDisplayName;
    fLocalizer: TLocalizer;
    procedure SetCultures(Value: TCultureList);
    procedure SetItemSelected(Value: TCultureInfo);
    function GetItemSelected: TCultureInfo;
    procedure SetLocalizer(Value: TLocalizer);
    procedure SetDisplayName(Value: TCultureDisplayName);
    procedure CulturesChanged(Sender: TObject);
    procedure ReadItemSelected(Reader: TReader);
    procedure WriteItemSelected(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Notifies the control about changes in the linked <see cref="TLocalizer"/>
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
    /// Reads and writes the <see cref="ItemSelected"/> property as if it was published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gererates an <see cref="OnSelect"/> event.</summary>
    {$endregion}
    procedure Select; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Fills the internal list of items by display name of the <see cref="TCultureInfo"/>
    /// objects in the <see cref="Items"/> property.</summary>
    {$endregion}
    procedure RebuildInternalItems; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon for the specified item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon is available.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the control.</summary>
    {$endregion}
    property Localizer: TLocalizer read fLocalizer write SetLocalizer;
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
    /// Fills the <see cref="Items"/> property with all the known <see cref="TCultureInfo"/>
    /// objects.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the control is not linked to a <see cref="TLocalizer"/>
    /// component, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="18nCore.TWorld.Cultures"/>
    /// <seealso cref="IsManagedByLocalizer"/>
    {$endregion}
    function CollectAll: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the <see cref="Items"/> property is filled by a <see cref="TLocalizer"/>
    /// component.</summary>
    /// <returns>
    /// Rteurns <see langword="true"/> if <see cref="Localizer"/> property is not
    /// <see langword="nil"/>, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="Localizer"/>
    {$endregion}
    function IsManagedByLocalizer: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName: TCultureDisplayName read fDisplayName write SetDisplayName default cnEnglishDisplayName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TCultureInfo"/> object.</summary>
    {$endregion}
    property ItemSelected: TCultureInfo read GetItemSelected write SetItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCultureInfo"/> objects in the control.</summary>
    {$endregion}
    property Items: TCultureList read fCultures write SetCultures;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCultureBox is a combo box control that represents a list of <see cref="TCultureInfo"/>
  /// objects.</summary>
  /// <remarks>
  /// Use TCultureBox to enable users to select a <see cref="TCultureInfo"/> object from a list.
  ///
  /// TCultureBox also implements properties to specify a country flag icon, along with its
  /// appearance and placement for each individual item.
  ///
  /// TCultureBox can be linked to a <see cref="TLocalizer"/> component. In this case,
  /// the <see cref="TLocalizer"/> component provides the list of <see cref="TCultureInfo"/>
  /// objects being displayed by the control.
  ///
  /// TCultureBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomCultureBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCultureBox = class(TCustomCultureBox)
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
    /// Gets or sets the currently selected <see cref="TCultureInfo"/> object.</summary>
    {$endregion}
    property ItemSelected;
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
  /// TCustomCultureListBox is the base class for list box controls that represent a list
  /// of <see cref="TCultureInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCustomCultureListBox as a base class when defining list box controls that represent
  /// a list of <see cref="TCultureInfo"/> objects. TCustomCultureListBox has properties and
  /// methods to facilitate displaying, retrieving, sorting, and selecting of the stored
  /// <see cref="TCultureInfo"/> objects.
  ///
  /// TCustomCultureListBox also implements properties to specify a country flag icon, along
  /// with its appearance and placement for each individual item.
  ///
  /// TCustomCultureListBox can be linked to a <see cref="TLocalizer"/> component. In this
  /// case, the <see cref="TLocalizer"/> component provides the <see cref="TCultureInfo"/>
  /// objects for the <see cref="Items"/> property. Also the selected item of the control
  /// will be synchronized with the current culture of the <see cref="TLocalizer"/> component.</remarks>
  {$endregion}
  TCustomCultureListBox = class(TCustomFlagListBox, ILocalizerLink)
  private
    fCultures: TCultureList;
    fDisplayName: TCultureDisplayName;
    fLocalizer: TLocalizer;
    procedure SetCultures(Value: TCultureList);
    procedure SetItemSelected(Value: TCultureInfo);
    function GetItemSelected: TCultureInfo;
    procedure SetLocalizer(Value: TLocalizer);
    procedure SetDisplayName(Value: TCultureDisplayName);
    procedure CulturesChanged(Sender: TObject);
    procedure ReadItemSelected(Reader: TReader);
    procedure WriteItemSelected(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Notifies the control about changes in the linked <see cref="TLocalizer"/>
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
    /// Reads and writes the <see cref="ItemSelected"/> property as if it was published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnClick"/> event.</summary>
    {$endregion}
    procedure Click; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets the <see cref="MultiSelect"/> property.</summary>
    /// <param name="Value">
    /// Determines whether more than one item can be selected.</param>
    {$endregion}
    procedure SetMultiSelect(Value: Boolean); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Fills the internal list of items by display name of the <see cref="TCultureInfo"/>
    /// objects in the <see cref="Items"/> property.</summary>
    {$endregion}
    procedure RebuildInternalItems; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon for the specified item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon is available.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the control.</summary>
    {$endregion}
    property Localizer: TLocalizer read fLocalizer write SetLocalizer;
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
    /// Fills the <see cref="Items"/> property with all the known <see cref="TCultureInfo"/>
    /// objects.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the control is not linked to a <see cref="TLocalizer"/>
    /// component, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="18nCore.TWorld.Cultures"/>
    /// <seealso cref="IsManagedByLocalizer"/>
    {$endregion}
    function CollectAll: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the list of currently selected <see cref="TCultureInfo"/> objects.</summary>
    /// <param name="Dest">
    /// The list that receives the selected <see cref="TCultureInfo"/> objects.</param>
    /// <seealso cref="SetSelection"/>
    {$endregion}
    procedure GetSelection(Dest: TCultureList);
    {$region 'xmldoc'}
    /// <summary>
    /// If <see cref="MultiSelect"/> is <see langword="true"/>, selects the items that are
    /// identified by a specified list of <see cref="TCultureInfo"/> objects.</summary>
    /// <param name="Source">
    /// The list of <see cref="TCultureInfo"/> objects to select.</param>
    /// <seealso cref="GetSelection"/>
    /// <seealso cref="MultiSelect"/>
    {$endregion}
    procedure SetSelection(Source: TReadonlyCultureList);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the <see cref="Items"/> property is filled by a <see cref="TLocalizer"/>
    /// component.</summary>
    /// <returns>
    /// Rteurns <see langword="true"/> if <see cref="Localizer"/> property is not
    /// <see langword="nil"/>, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="Localizer"/>
    {$endregion}
    function IsManagedByLocalizer: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName: TCultureDisplayName read fDisplayName write SetDisplayName default cnEnglishDisplayName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TCultureInfo"/> object.</summary>
    {$endregion}
    property ItemSelected: TCultureInfo read GetItemSelected write SetItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCultureInfo"/> objects in the control.</summary>
    {$endregion}
    property Items: TCultureList read fCultures write SetCultures;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCultureListBox is a list box control that represents a list of <see cref="TCultureInfo"/>
  /// objects.</summary>
  /// <remarks>
  /// Use TCultureListBox to enable users to select a <see cref="TCultureInfo"/> object from a list.
  ///
  /// TCultureListBox also implements properties to specify a country flag icon, along with its
  /// appearance and placement for each individual item.
  ///
  /// TCultureListBox can be linked to a <see cref="TLocalizer"/> component. In this case,
  /// the <see cref="TLocalizer"/> component provides the list of <see cref="TCultureInfo"/>
  /// objects being displayed by the control.
  ///
  /// TCultureListBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomCultureListBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCultureListBox = class(TCustomCultureListBox)
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
    /// Gets or sets whether the user can select a sequential range of items in the list box.</summary>
    {$endregion}
    property ExtendedSelect;
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
    /// Gets or sets the currently selected <see cref="TCultureInfo"/> object.</summary>
    {$endregion}
    property ItemSelected;
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
    /// Gets or sets whether the user can select more than one item at a time.</summary>
    {$endregion}
    property MultiSelect;
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
  /// TCustomCultureCheckListBox is the base class for check list box controls that
  /// represent a list of <see cref="TCultureInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCustomCultureCheckListBox as a base class when defining controls that represent
  /// a check list of <see cref="TCultureInfo"/> objects. TCustomCultureCheckListBox has
  /// properties and methods to facilitate displaying, retrieving, sorting, and selecting
  /// of the stored <see cref="TCultureInfo"/> objects.
  ///
  /// TCustomCultureCheckListBox also implements properties to specify a country flag
  /// icon, along with its appearance and placement for each individual item.
  ///
  /// TCustomCultureCheckListBox can be linked to a <see cref="TLocalizer"/> component.
  /// In this case, the <see cref="TLocalizer"/> component provides the <see cref="TCultureInfo"/>
  /// objects for the <see cref="Items"/> property.</remarks>
  {$endregion}
  TCustomCultureCheckListBox = class(TCustomFlagCheckListBox, ILocalizerLink)
  private
    fCultures: TCultureList;
    fDisplayName: TCultureDisplayName;
    fLocalizer: TLocalizer;
    procedure SetCultures(Value: TCultureList);
    procedure SetItemSelected(Value: TCultureInfo);
    function GetItemSelected: TCultureInfo;
    procedure SetLocalizer(Value: TLocalizer);
    procedure SetDisplayName(Value: TCultureDisplayName);
    procedure CulturesChanged(Sender: TObject);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Notifies the control about changes in the linked <see cref="TLocalizer"/>
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
    /// Fills the internal list of items by display name of the <see cref="TCultureInfo"/>
    /// objects in the <see cref="Items"/> property.</summary>
    {$endregion}
    procedure RebuildInternalItems; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon for the specified item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon is available.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TLocalizer"/> component that is linked to the control.</summary>
    {$endregion}
    property Localizer: TLocalizer read fLocalizer write SetLocalizer;
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
    /// Fills the <see cref="Items"/> property with all the known <see cref="TCultureInfo"/>
    /// objects.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the control is not linked to a <see cref="TLocalizer"/>
    /// component, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="18nCore.TWorld.Cultures"/>
    /// <seealso cref="IsManagedByLocalizer"/>
    {$endregion}
    function CollectAll: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the list of checked <see cref="TCultureInfo"/> objects.</summary>
    /// <param name="Dest">
    /// The list that receives the checked <see cref="TCultureInfo"/> objects.</param>
    /// <param name="AllowDisabled">
    /// Determines whether the disabled items can be retrieved.</param>
    /// <seealso cref="SetSelection"/>
    {$endregion}
    procedure GetSelection(Dest: TCultureList; AllowDisabled: Boolean = True); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes the items that are identified by a specified list of <see cref="TCultureInfo"/> objects.</summary>
    /// <param name="Source">
    /// The list of <see cref="TCultureInfo"/> objects to select.</param>
    /// <param name="AllowDisabled">
    /// Determines whether the disabled items can be altered.</param>
    /// <seealso cref="GetSelection"/>
    {$endregion}
    procedure SetSelection(Source: TReadonlyCultureList; AllowDisabled: Boolean = True); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the <see cref="Items"/> property is filled by a <see cref="TLocalizer"/>
    /// component.</summary>
    /// <returns>
    /// Rteurns <see langword="true"/> if <see cref="Localizer"/> property is not
    /// <see langword="nil"/>, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="Localizer"/>
    {$endregion}
    function IsManagedByLocalizer: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCultureInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName: TCultureDisplayName read fDisplayName write SetDisplayName default cnEnglishDisplayName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TCultureInfo"/> object.</summary>
    {$endregion}
    property ItemSelected: TCultureInfo read GetItemSelected write SetItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCultureInfo"/> objects in the control.</summary>
    {$endregion}
    property Items: TCultureList read fCultures write SetCultures;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCultureCheckListBox is a check list box control that represents a list of
  /// <see cref="TCultureInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCultureCheckListBox to enable users to select multiple <see cref="TCultureInfo"/>
  /// objects from a list.
  ///
  /// TCultureCheckListBox also implements properties to specify a country flag icon,
  /// along with its appearance and placement for each individual item.
  ///
  /// TCultureCheckListBox can be linked to a <see cref="TLocalizer"/> component.
  /// In this case, the <see cref="TLocalizer"/> component provides the list of
  /// <see cref="TCultureInfo"/> objects being displayed by the control.
  ///
  /// TCultureCheckListBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomCultureCheckListBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCultureCheckListBox = class(TCustomCultureCheckListBox)
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
    /// Gets or sets whether the check boxes can be in a "grayed" state.</summary>
    {$endregion}
    property AllowGrayed;
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
    /// Gets or sets whether the check boxes appear three-dimensional.</summary>
    {$endregion}
    property Flat;
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
    /// Occurs when the user selects or deselects an item's check box.</summary>
    {$endregion}
    property OnClickCheck;
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
  /// TCustomTerritoryLabel is the base class for label controls that display a
  /// <see cref="TTerritoryInfo"/> object on a form.</summary>
  /// <remarks>
  /// Use TCustomTerritoryLabel as a base class when defining objects that display a
  /// <see cref="TTerritoryInfo"/> object. TCustomTerritoryLabel implements properties
  /// to specify the <see cref="TTerritoryInfo"/> object, along with its interested piece
  /// of information that should be displayed.</remarks>
  {$endregion}
  TCustomTerritoryLabel = class(TCustomFlagLabel)
  private
    fTerritory: TTerritoryInfo;
    fDisplayName: TTerritoryDisplayName;
    procedure SetTerritory(Value: TTerritoryInfo);
    procedure SetDisplayName(Value: TTerritoryDisplayName);
    procedure ReadTerritory(Reader: TReader);
    procedure WriteTerritory(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="Territory"/> property as if it was published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon to display on the control.</summary>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon should be displayed.</returns>
    {$endregion}
    function GetImageIndex: TImageIndex; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a text label to display for on the control.</summary>
    /// <returns>
    /// The text label.</returns>
    {$endregion}
    function GetLabelText: String; override;
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
    /// Gets or sets which value of <see cref="TTerritoryInfo.DisplayNames"/> should be
    /// displayed as the text label on the control.</summary>
    {$endregion}
    property DisplayName: TTerritoryDisplayName read fDisplayName write SetDisplayName default tnFriendlyName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TTerritoryInfo"/> object that the control is displaying.</summary>
    {$endregion}
    property Territory: TTerritoryInfo read fTerritory write SetTerritory;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TTerritoryLabel is a label control that displays a <see cref="TTerritoryInfo"/> object
  /// on a form.</summary>
  /// <remarks>
  /// Use TTerritoryLabel to display a <see cref="TTerritoryInfo"/> object on a form. The
  /// <see cref="Territory"/> and <see cref="DisplayName"/> properties determine the content
  /// that will be displayed.
  ///
  /// TTerritoryLabel publishes many of the properties, events, and methods of
  /// <see cref="TCustomTerritoryLabel"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTerritoryLabel = class(TCustomTerritoryLabel)
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
    property AutoSize;
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
    /// Gets or sets the text of the label when <see cref="Territory"/> property has no value.</summary>
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
    /// Gets or sets the <see cref="TTerritoryInfo"/> object that the control is displaying.</summary>
    {$endregion}
    property Territory;
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
  /// TCustomTerritoryBox is the base class for combo box controls that represent a list
  /// of <see cref="TTerritoryInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCustomTerritoryBox as a base class when defining combo box controls that represent
  /// a list of <see cref="TTerritoryInfo"/> objects. TCustomTerritoryBox has properties and
  /// methods to facilitate displaying, retrieving, sorting, and selecting of the stored
  /// <see cref="TTerritoryInfo"/> objects.
  ///
  /// TCustomTerritoryBox also implements properties to specify a country flag icon, along
  /// with its appearance and placement for each individual item.</remarks>
  {$endregion}
  TCustomTerritoryBox = class(TCustomFlagBox)
  private
    fTerritories: TTerritoryList;
    fDisplayName: TTerritoryDisplayName;
    procedure SetTerritories(Value: TTerritoryList);
    procedure SetItemSelected(Value: TTerritoryInfo);
    function GetItemSelected: TTerritoryInfo;
    procedure SetDisplayName(Value: TTerritoryDisplayName);
    procedure TerritoriesChanged(Sender: TObject);
    procedure ReadItemSelected(Reader: TReader);
    procedure WriteItemSelected(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="ItemSelected"/> property as if it was published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Fills the internal list of items by display name of the <see cref="TTerritoryInfo"/>
    /// objects in the <see cref="Items"/> property.</summary>
    {$endregion}
    procedure RebuildInternalItems; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon for the specified item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon is available.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; override;
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
    /// Fills the <see cref="Items"/> property with all the known <see cref="TTerritoryInfo"/>
    /// objects.</summary>
    /// <returns>
    /// Returns <see langword="true"/>.</returns>
    /// <seealso cref="18nCore.TWorld.Territories"/>
    {$endregion}
    function CollectAll: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TTerritoryInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName: TTerritoryDisplayName read fDisplayName write SetDisplayName default tnFriendlyName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TTerritoryInfo"/> object.</summary>
    {$endregion}
    property ItemSelected: TTerritoryInfo read GetItemSelected write SetItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TTerritoryInfo"/> objects in the control.</summary>
    {$endregion}
    property Items: TTerritoryList read fTerritories write SetTerritories;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TTerritoryBox is a combo box control that represents a list of <see cref="TTerritoryInfo"/>
  /// objects.</summary>
  /// <remarks>
  /// Use TTerritoryBox to enable users to select a <see cref="TTerritoryInfo"/> object from a list.
  ///
  /// TTerritoryBox also implements properties to specify a country flag icon, along with its
  /// appearance and placement for each individual item.
  ///
  /// TTerritoryBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomTerritoryBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTerritoryBox = class(TCustomTerritoryBox)
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
    /// Gets or sets which value of <see cref="TTerritoryInfo.DisplayNames"/> should be
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
    /// Lists the <see cref="TTerritoryInfo"/> objects in the control.</summary>
    {$endregion}
    property Items;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TTerritoryInfo"/> object.</summary>
    {$endregion}
    property ItemSelected;
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
  /// TCustomTerritoryListBox is the base class for list box controls that represent a list
  /// of <see cref="TTerritoryInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCustomTerritoryListBox as a base class when defining list box controls that represent
  /// a list of <see cref="TTerritoryInfo"/> objects. TCustomTerritoryListBox has properties and
  /// methods to facilitate displaying, retrieving, sorting, and selecting of the stored
  /// <see cref="TTerritoryInfo"/> objects.
  ///
  /// TCustomTerritoryListBox also implements properties to specify a country flag icon, along
  /// with its appearance and placement for each individual item.</remarks>
  {$endregion}
  TCustomTerritoryListBox = class(TCustomFlagListBox)
  private
    fTerritories: TTerritoryList;
    fDisplayName: TTerritoryDisplayName;
    procedure SetTerritories(Value: TTerritoryList);
    procedure SetItemSelected(Value: TTerritoryInfo);
    function GetItemSelected: TTerritoryInfo;
    procedure SetDisplayName(Value: TTerritoryDisplayName);
    procedure TerritoriesChanged(Sender: TObject);
    procedure ReadItemSelected(Reader: TReader);
    procedure WriteItemSelected(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="ItemSelected"/> property as if it was published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Fills the internal list of items by display name of the <see cref="TTerritoryInfo"/>
    /// objects in the <see cref="Items"/> property.</summary>
    {$endregion}
    procedure RebuildInternalItems; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon for the specified item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon is available.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; override;
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
    /// Fills the <see cref="Items"/> property with all the known <see cref="TTerritoryInfo"/>
    /// objects.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the control is not linked to a <see cref="TLocalizer"/>
    /// component, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="18nCore.TWorld.Territorys"/>
    {$endregion}
    function CollectAll: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the list of currently selected <see cref="TTerritoryInfo"/> objects.</summary>
    /// <param name="Dest">
    /// The list that receives the selected <see cref="TTerritoryInfo"/> objects.</param>
    /// <seealso cref="SetSelection"/>
    {$endregion}
    procedure GetSelection(Dest: TTerritoryList);
    {$region 'xmldoc'}
    /// <summary>
    /// Selects the items that are identified by a specified list of <see cref="TTerritoryInfo"/> objects.</summary>
    /// <param name="Source">
    /// The list of <see cref="TTerritoryInfo"/> objects to select.</param>
    /// <seealso cref="GetSelection"/>
    /// <seealso cref="MultiSelect"/>
    {$endregion}
    procedure SetSelection(Source: TReadonlyTerritoryList);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TTerritoryInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName: TTerritoryDisplayName read fDisplayName write SetDisplayName default tnFriendlyName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TTerritoryInfo"/> object.</summary>
    {$endregion}
    property ItemSelected: TTerritoryInfo read GetItemSelected write SetItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TTerritoryInfo"/> objects in the control.</summary>
    {$endregion}
    property Items: TTerritoryList read fTerritories write SetTerritories;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TTerritoryListBox is a list box control that represents a list of
  /// <see cref="TTerritoryInfo"/> objects.</summary>
  /// <remarks>
  /// Use TTerritoryListBox to enable users to select a <see cref="TTerritoryInfo"/>
  /// object from a list.
  ///
  /// TTerritoryListBox also implements properties to specify a country flag icon,
  /// along with its appearance and placement for each individual item.
  ///
  /// TTerritoryListBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomTerritoryListBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTerritoryListBox = class(TCustomTerritoryListBox)
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
    /// Gets or sets which value of <see cref="TTerritoryInfo.DisplayNames"/> should be
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
    /// Gets or sets whether the user can select a sequential range of items in the list box.</summary>
    {$endregion}
    property ExtendedSelect;
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
    /// Lists the <see cref="TTerritoryInfo"/> objects in the control.</summary>
    {$endregion}
    property Items;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TTerritoryInfo"/> object.</summary>
    {$endregion}
    property ItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the margins of the control.</summary>
    {$endregion}
    property Margins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can select more than one item at a time.</summary>
    {$endregion}
    property MultiSelect;
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
  /// TCustomTerritoryCheckListBox is the base class for check list box controls that
  /// represent a list of <see cref="TTerritoryInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCustomTerritoryCheckListBox as a base class when defining controls that represent
  /// a check list of <see cref="TTerritoryInfo"/> objects. TCustomTerritoryCheckListBox has
  /// properties and methods to facilitate displaying, retrieving, sorting, and selecting
  /// of the stored <see cref="TTerritoryInfo"/> objects.
  ///
  /// TCustomTerritoryCheckListBox also implements properties to specify a country flag
  /// icon, along with its appearance and placement for each individual item.</remarks>
  {$endregion}
  TCustomTerritoryCheckListBox = class(TCustomFlagCheckListBox)
  private
    fTerritories: TTerritoryList;
    fDisplayName: TTerritoryDisplayName;
    procedure SetTerritories(Value: TTerritoryList);
    procedure SetItemSelected(Value: TTerritoryInfo);
    function GetItemSelected: TTerritoryInfo;
    procedure SetDisplayName(Value: TTerritoryDisplayName);
    procedure TerritoriesChanged(Sender: TObject);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Fills the internal list of items by display name of the <see cref="TTerritoryInfo"/>
    /// objects in the <see cref="Items"/> property.</summary>
    {$endregion}
    procedure RebuildInternalItems; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon for the specified item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon is available.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; override;
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
    /// Fills the <see cref="Items"/> property with all the known <see cref="TTerritoryInfo"/>
    /// objects.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the control is not linked to a <see cref="TLocalizer"/>
    /// component, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="18nCore.TWorld.Territorys"/>
    {$endregion}
    function CollectAll: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the list of checked <see cref="TTerritoryInfo"/> objects.</summary>
    /// <param name="Dest">
    /// The list that receives the checked <see cref="TTerritoryInfo"/> objects.</param>
    /// <param name="AllowDisabled">
    /// Determines whether the disabled items can be retrieved.</param>
    /// <seealso cref="SetSelection"/>
    {$endregion}
    procedure GetSelection(Dest: TTerritoryList; AllowDisabled: Boolean = True); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes the items that are identified by a specified list of <see cref="TTerritoryInfo"/> objects.</summary>
    /// <param name="Source">
    /// The list of <see cref="TTerritoryInfo"/> objects to select.</param>
    /// <param name="AllowDisabled">
    /// Determines whether the disabled items can be altered.</param>
    /// <seealso cref="GetSelection"/>
    {$endregion}
    procedure SetSelection(Source: TReadonlyTerritoryList; AllowDisabled: Boolean = True); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TTerritoryInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName: TTerritoryDisplayName read fDisplayName write SetDisplayName default tnFriendlyName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TTerritoryInfo"/> object.</summary>
    {$endregion}
    property ItemSelected: TTerritoryInfo read GetItemSelected write SetItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TTerritoryInfo"/> objects in the control.</summary>
    {$endregion}
    property Items: TTerritoryList read fTerritories write SetTerritories;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TTerritoryCheckListBox is a check list box control that represents a list of
  /// <see cref="TTerritoryInfo"/> objects.</summary>
  /// <remarks>
  /// Use TTerritoryCheckListBox to enable users to select multiple <see cref="TTerritoryInfo"/>
  /// objects from a list.
  ///
  /// TTerritoryCheckListBox also implements properties to specify a country flag icon,
  /// along with its appearance and placement for each individual item.
  ///
  /// TTerritoryCheckListBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomTerritoryCheckListBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTerritoryCheckListBox = class(TCustomTerritoryCheckListBox)
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
    /// Gets or sets whether the check boxes can be in a "grayed" state.</summary>
    {$endregion}
    property AllowGrayed;
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
    /// Gets or sets which value of <see cref="TTerritoryInfo.DisplayNames"/> should be
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
    /// Gets or sets whether the check boxes appear three-dimensional.</summary>
    {$endregion}
    property Flat;
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
    /// Lists the <see cref="TTerritoryInfo"/> objects in the control.</summary>
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
    /// Occurs when the user selects or deselects an item's check box.</summary>
    {$endregion}
    property OnClickCheck;
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
  /// TCustomCurrencyLabel is the base class for label controls that display a
  /// <see cref="TCurrencyInfo"/> object on a form.</summary>
  /// <remarks>
  /// Use TCustomCurrencyLabel as a base class when defining objects that display a
  /// <see cref="TCurrencyInfo"/> object. TCustomCurrencyLabel implements properties
  /// to specify the <see cref="TCurrencyInfo"/> object, along with its interested piece
  /// of information that should be displayed.</remarks>
  {$endregion}
  TCustomCurrencyLabel = class(TCustomFlagLabel)
  private
    fCurrency: TCurrencyInfo;
    fDisplayName: TCurrencyDisplayName;
    procedure SetCurrency(Value: TCurrencyInfo);
    procedure SetDisplayName(Value: TCurrencyDisplayName);
    procedure ReadCurrency(Reader: TReader);
    procedure WriteCurrency(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="Currency"/> property as if it was published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon to display on the control.</summary>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon should be displayed.</returns>
    {$endregion}
    function GetImageIndex: TImageIndex; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a text label to display for on the control.</summary>
    /// <returns>
    /// The text label.</returns>
    {$endregion}
    function GetLabelText: String; override;
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
    /// Gets or sets which value of <see cref="TCurrencyInfo.DisplayNames"/> should be
    /// displayed as the text label on the control.</summary>
    {$endregion}
    property DisplayName: TCurrencyDisplayName read fDisplayName write SetDisplayName default crnEnglishName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the <see cref="TCurrencyInfo"/> object that the control is displaying.</summary>
    {$endregion}
    property Currency: TCurrencyInfo read fCurrency write SetCurrency;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCurrencyLabel is a label control that displays a <see cref="TCurrencyInfo"/> object
  /// on a form.</summary>
  /// <remarks>
  /// Use TCurrencyLabel to display a <see cref="TCurrencyInfo"/> object on a form. The
  /// <see cref="Currency"/> and <see cref="DisplayName"/> properties determine the content
  /// that will be displayed.
  ///
  /// TCurrencyLabel publishes many of the properties, events, and methods of
  /// <see cref="TCustomCurrencyLabel"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCurrencyLabel = class(TCustomCurrencyLabel)
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
    property AutoSize;
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
    /// Gets or sets the text of the label when <see cref="Currency"/> property has no value.</summary>
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
    /// Gets or sets the <see cref="TCurrencyInfo"/> object that the control is displaying.</summary>
    {$endregion}
    property Currency;
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
  /// TCustomCurrencyBox is the base class for combo box controls that represent a list
  /// of <see cref="TCurrencyInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCustomCurrencyBox as a base class when defining combo box controls that represent
  /// a list of <see cref="TCurrencyInfo"/> objects. TCustomCurrencyBox has properties and
  /// methods to facilitate displaying, retrieving, sorting, and selecting of the stored
  /// <see cref="TCurrencyInfo"/> objects.
  ///
  /// TCustomCurrencyBox also implements properties to specify a country flag icon, along
  /// with its appearance and placement for each individual item.</remarks>
  {$endregion}
  TCustomCurrencyBox = class(TCustomFlagBox)
  private
    fCurrencies: TCurrencyList;
    fDisplayName: TCurrencyDisplayName;
    procedure SetCurrencies(Value: TCurrencyList);
    procedure SetItemSelected(Value: TCurrencyInfo);
    function GetItemSelected: TCurrencyInfo;
    procedure SetDisplayName(Value: TCurrencyDisplayName);
    procedure CurrenciesChanged(Sender: TObject);
    procedure ReadItemSelected(Reader: TReader);
    procedure WriteItemSelected(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="ItemSelected"/> property as if it was published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Fills the internal list of items by display name of the <see cref="TCurrencyInfo"/>
    /// objects in the <see cref="Items"/> property.</summary>
    {$endregion}
    procedure RebuildInternalItems; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon for the specified item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon is available.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; override;
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
    /// Fills the <see cref="Items"/> property with all the known <see cref="TCurrencyInfo"/>
    /// objects.</summary>
    /// <returns>
    /// Returns <see langword="true"/>.</returns>
    /// <seealso cref="18nCore.TWorld.Currencies"/>
    {$endregion}
    function CollectAll: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCurrencyInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName: TCurrencyDisplayName read fDisplayName write SetDisplayName default crnEnglishName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TCurrencyInfo"/> object.</summary>
    {$endregion}
    property ItemSelected: TCurrencyInfo read GetItemSelected write SetItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCurrencyInfo"/> objects in the control.</summary>
    {$endregion}
    property Items: TCurrencyList read fCurrencies write SetCurrencies;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCurrencyBox is a combo box control that represents a list of <see cref="TCurrencyInfo"/>
  /// objects.</summary>
  /// <remarks>
  /// Use TCurrencyBox to enable users to select a <see cref="TCurrencyInfo"/> object from a list.
  ///
  /// TCurrencyBox also implements properties to specify a country flag icon, along with its
  /// appearance and placement for each individual item.
  ///
  /// TCurrencyBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomCurrencyBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCurrencyBox = class(TCustomCurrencyBox)
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
    /// Gets or sets which value of <see cref="TCurrencyInfo.DisplayNames"/> should be
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
    /// Lists the <see cref="TCurrencyInfo"/> objects in the control.</summary>
    {$endregion}
    property Items;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TCurrencyInfo"/> object.</summary>
    {$endregion}
    property ItemSelected;
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
  /// TCustomCurrencyListBox is the base class for list box controls that represent a list
  /// of <see cref="TCurrencyInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCustomCurrencyListBox as a base class when defining list box controls that represent
  /// a list of <see cref="TCurrencyInfo"/> objects. TCustomCurrencyListBox has properties and
  /// methods to facilitate displaying, retrieving, sorting, and selecting of the stored
  /// <see cref="TCurrencyInfo"/> objects.
  ///
  /// TCustomCurrencyListBox also implements properties to specify a country flag icon, along
  /// with its appearance and placement for each individual item.</remarks>
  {$endregion}
  TCustomCurrencyListBox = class(TCustomFlagListBox)
  private
    fCurrencies: TCurrencyList;
    fDisplayName: TCurrencyDisplayName;
    procedure SetCurrencies(Value: TCurrencyList);
    procedure SetItemSelected(Value: TCurrencyInfo);
    function GetItemSelected: TCurrencyInfo;
    procedure SetDisplayName(Value: TCurrencyDisplayName);
    procedure CurrenciesChanged(Sender: TObject);
    procedure ReadItemSelected(Reader: TReader);
    procedure WriteItemSelected(Writer: TWriter);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads and writes the <see cref="ItemSelected"/> property as if it was published.</summary>
    /// <param name="Filer">
    /// The current <see cref="TReader"/> or <see cref="TWriter"/> object that is
    /// loading or saving the published properties.</param>
    {$endregion}
    procedure DefineProperties(Filer: TFiler); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Fills the internal list of items by display name of the <see cref="TCurrencyInfo"/>
    /// objects in the <see cref="Items"/> property.</summary>
    {$endregion}
    procedure RebuildInternalItems; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon for the specified item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon is available.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; override;
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
    /// Fills the <see cref="Items"/> property with all the known <see cref="TCurrencyInfo"/>
    /// objects.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the control is not linked to a <see cref="TLocalizer"/>
    /// component, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="18nCore.TWorld.Currencys"/>
    {$endregion}
    function CollectAll: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the list of currently selected <see cref="TCurrencyInfo"/> objects.</summary>
    /// <param name="Dest">
    /// The list that receives the selected <see cref="TCurrencyInfo"/> objects.</param>
    /// <seealso cref="SetSelection"/>
    {$endregion}
    procedure GetSelection(Dest: TCurrencyList);
    {$region 'xmldoc'}
    /// <summary>
    /// Selects the items that are identified by a specified list of <see cref="TCurrencyInfo"/> objects.</summary>
    /// <param name="Source">
    /// The list of <see cref="TCurrencyInfo"/> objects to select.</param>
    /// <seealso cref="GetSelection"/>
    /// <seealso cref="MultiSelect"/>
    {$endregion}
    procedure SetSelection(Source: TReadonlyCurrencyList);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCurrencyInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName: TCurrencyDisplayName read fDisplayName write SetDisplayName default crnEnglishName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TCurrencyInfo"/> object.</summary>
    {$endregion}
    property ItemSelected: TCurrencyInfo read GetItemSelected write SetItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCurrencyInfo"/> objects in the control.</summary>
    {$endregion}
    property Items: TCurrencyList read fCurrencies write SetCurrencies;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCurrencyListBox is a list box control that represents a list of
  /// <see cref="TCurrencyInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCurrencyListBox to enable users to select a <see cref="TCurrencyInfo"/>
  /// object from a list.
  ///
  /// TCurrencyListBox also implements properties to specify a country flag icon,
  /// along with its appearance and placement for each individual item.
  ///
  /// TCurrencyListBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomCurrencyListBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCurrencyListBox = class(TCustomCurrencyListBox)
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
    /// Gets or sets which value of <see cref="TCurrencyInfo.DisplayNames"/> should be
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
    /// Gets or sets whether the user can select a sequential range of items in the list box.</summary>
    {$endregion}
    property ExtendedSelect;
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
    /// Lists the <see cref="TCurrencyInfo"/> objects in the control.</summary>
    {$endregion}
    property Items;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TCurrencyInfo"/> object.</summary>
    {$endregion}
    property ItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the margins of the control.</summary>
    {$endregion}
    property Margins;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can select more than one item at a time.</summary>
    {$endregion}
    property MultiSelect;
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
  /// TCustomCurrencyCheckListBox is the base class for check list box controls that
  /// represent a list of <see cref="TCurrencyInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCustomCurrencyCheckListBox as a base class when defining controls that represent
  /// a check list of <see cref="TCurrencyInfo"/> objects. TCustomCurrencyCheckListBox has
  /// properties and methods to facilitate displaying, retrieving, sorting, and selecting
  /// of the stored <see cref="TCurrencyInfo"/> objects.
  ///
  /// TCustomCurrencyCheckListBox also implements properties to specify a country flag
  /// icon, along with its appearance and placement for each individual item.</remarks>
  {$endregion}
  TCustomCurrencyCheckListBox = class(TCustomFlagCheckListBox)
  private
    fCurrencies: TCurrencyList;
    fDisplayName: TCurrencyDisplayName;
    procedure SetCurrencies(Value: TCurrencyList);
    procedure SetItemSelected(Value: TCurrencyInfo);
    function GetItemSelected: TCurrencyInfo;
    procedure SetDisplayName(Value: TCurrencyDisplayName);
    procedure CurrenciesChanged(Sender: TObject);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Fills the internal list of items by display name of the <see cref="TCurrencyInfo"/>
    /// objects in the <see cref="Items"/> property.</summary>
    {$endregion}
    procedure RebuildInternalItems; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a country flag icon for the specified item.</summary>
    /// <param name="Index">
    /// The index of the item.</param>
    /// <returns>
    /// The index of a country flag icon or -1 if no icon is available.</returns>
    {$endregion}
    function GetImageIndex(Index: Integer): Integer; override;
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
    /// Fills the <see cref="Items"/> property with all the known <see cref="TCurrencyInfo"/>
    /// objects.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the control is not linked to a <see cref="TLocalizer"/>
    /// component, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="18nCore.TWorld.Currencys"/>
    {$endregion}
    function CollectAll: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the list of checked <see cref="TCurrencyInfo"/> objects.</summary>
    /// <param name="Dest">
    /// The list that receives the checked <see cref="TCurrencyInfo"/> objects.</param>
    /// <param name="AllowDisabled">
    /// Determines whether the disabled items can be retrieved.</param>
    /// <seealso cref="SetSelection"/>
    {$endregion}
    procedure GetSelection(Dest: TCurrencyList; AllowDisabled: Boolean = True); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Checkes the items that are identified by a specified list of <see cref="TCurrencyInfo"/> objects.</summary>
    /// <param name="Source">
    /// The list of <see cref="TCurrencyInfo"/> objects to select.</param>
    /// <param name="AllowDisabled">
    /// Determines whether the disabled items can be altered.</param>
    /// <seealso cref="GetSelection"/>
    {$endregion}
    procedure SetSelection(Source: TReadonlyCurrencyList; AllowDisabled: Boolean = True); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets which value of <see cref="TCurrencyInfo.DisplayNames"/> should be
    /// displayed as the label of items.</summary>
    {$endregion}
    property DisplayName: TCurrencyDisplayName read fDisplayName write SetDisplayName default crnEnglishName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the currently selected <see cref="TCurrencyInfo"/> object.</summary>
    {$endregion}
    property ItemSelected: TCurrencyInfo read GetItemSelected write SetItemSelected;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCurrencyInfo"/> objects in the control.</summary>
    {$endregion}
    property Items: TCurrencyList read fCurrencies write SetCurrencies;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCurrencyCheckListBox is a check list box control that represents a list of
  /// <see cref="TCurrencyInfo"/> objects.</summary>
  /// <remarks>
  /// Use TCurrencyCheckListBox to enable users to select multiple <see cref="TCurrencyInfo"/>
  /// objects from a list.
  ///
  /// TCurrencyCheckListBox also implements properties to specify a country flag icon,
  /// along with its appearance and placement for each individual item.
  ///
  /// TCurrencyCheckListBox publishes many of the properties, events, and methods of
  /// <see cref="TCustomCurrencyCheckListBox"/>, but does not introduce any new behavior.</remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCurrencyCheckListBox = class(TCustomCurrencyCheckListBox)
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
    /// Gets or sets whether the check boxes can be in a "grayed" state.</summary>
    {$endregion}
    property AllowGrayed;
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
    /// Gets or sets which value of <see cref="TCurrencyInfo.DisplayNames"/> should be
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
    /// Gets or sets whether the check boxes appear three-dimensional.</summary>
    {$endregion}
    property Flat;
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
    /// Lists the <see cref="TCurrencyInfo"/> objects in the control.</summary>
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
    /// Occurs when the user selects or deselects an item's check box.</summary>
    {$endregion}
    property OnClickCheck;
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
/// Returns size of the check box drawn by <see cref="DrawCheckBox"/> global function.</summary>
/// <returns>
/// The width and height of check box, in pixels.</returns>
/// <seealso cref="DrawCheckBox"/>
{$endregion}
function GetCheckBoxSize: TSize;

{$region 'xmldoc'}
/// <summary>
/// Draws a check box on a specified device context.</summary>
/// <param name="DC">
/// Handle to the device context.</param>
/// <param name="Rect">
/// Specifies the boundaries thet the check box will be centered in.</param>
/// <param name="State">
/// Specifies the state of the check box.</param>
/// <param name="DrawState">
/// Specifies how the check box should appear.</param>
/// <param name="Flat">
/// Indicates whether the check box appear three-dimensional.</param>
/// <param name="Themed">
/// Indicates whether the check box should be drawn using the Windows theme.</param>
/// <returns>
/// The bounding rectangle of the check box.</returns>
/// <seealso cref="GetCheckBoxSize"/>
{$endregion}
function DrawCheckBox(DC: HDC; const Rect: TRect; State: TCheckBoxState;
  DrawState: TPartDrawState; Flat, Themed: Boolean): TRect;

{$region 'xmldoc'}
/// <summary>
/// Returns size of the drop-down button drawn by <see cref="DrawDropDown"/> global function.</summary>
/// <returns>
/// The width and height of the drop-down button, in pixels.</returns>
/// <seealso cref="DrawDropDown"/>
{$endregion}
function GetDropDownSize: TSize;

{$region 'xmldoc'}
/// <summary>
/// Draws a drop-down button on a specified device context.</summary>
/// <param name="DC">
/// Handle to the device context.</param>
/// <param name="Rect">
/// Specifies the boundaries thet the drop-down button will be centered in.</param>
/// <param name="DrawState">
/// Specifies how the drop-down button should appear.</param>
/// <param name="Themed">
/// Indicates whether the check box should be drawn using the Windows theme.</param>
/// <returns>
/// The bounding rectangle of the drop-down button.</returns>
/// <seealso cref="GetDropDownSize"/>
{$endregion}
procedure DrawDropDown(DC: HDC; const Rect: TRect; DrawState: TPartDrawState; Themed: Boolean);

implementation

uses
  UxTheme, SysUtils, Types, CommCtrl, Themes, RTLConsts, CheckLst;

{$R i18nFlags_16x11.res}

const
  HorzSpacing = 2;
  VertSpacing = 1;

{$IFNDEF COMPILER_XE2_UP}
type
  TCustomStyleServices = class (TThemeServices)
  public
    function Enabled: boolean;
  end;

  function TCustomStyleServices.Enabled: boolean;
  begin
    Result := ThemesEnabled;
  end;

function StyleServices: TCustomStyleServices;
begin
  Result := TCustomStyleServices(ThemeServices);
end;
{$ENDIF}

{ Helper Functions }

var CheckBoxSize: TSize;

function GetCheckBoxSize: TSize;
begin
  Result := CheckBoxSize;
end;

function CalcCheckBoxSize: TSize;
begin
  with TBitmap.Create do
    try
      Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
      Result.cx := Width div 4;
      Result.cy := Height div 3;
    finally
      Free;
    end;
end;

function DrawCheckBox(DC: HDC; const Rect: TRect; State: TCheckBoxState;
  DrawState: TPartDrawState; Flat, Themed: Boolean): TRect;
var
  DrawRect: TRect absolute Result;
  DrawDetail: TThemedButton;
  DrawFlag: Integer;
  OldRgn, Rgn: HRgn;
  Brush: HBRUSH;
  ElementDetails: TThemedElementDetails;
begin
  OldRgn := 0;
  DrawRect.Left := (Rect.Left + Rect.Right - CheckBoxSize.cx) div 2;
  DrawRect.Top := (Rect.Top + Rect.Bottom - CheckBoxSize.cy) div 2;
  DrawRect.Right := DrawRect.Left + CheckBoxSize.cx;
  DrawRect.Bottom := DrawRect.Top + CheckBoxSize.cy;
  if not RectVisible(DC, DrawRect) then
    Exit;
  if Flat then
  begin
    OldRgn := CreateRectRgn(0, 0, 0, 0);
    GetClipRgn(DC, OldRgn);
    with DrawRect do
      Rgn := CreateRectRgn(Left + 2, Top + 2, Right - 2, Bottom - 2);
    SelectClipRgn(DC, Rgn);
    DeleteObject(Rgn);
  end;
  if Themed and StyleServices.Enabled then
  begin
    case State of
      cbChecked:
        case DrawState of
          pdsNormal:
            DrawDetail := tbCheckBoxCheckedNormal;
          pdsHot:
            DrawDetail := tbCheckBoxCheckedHot;
          pdsPressed:
            DrawDetail := tbCheckBoxCheckedPressed;
          else // pdsDisabled
            DrawDetail := tbCheckBoxCheckedDisabled;
        end;
      cbUnchecked:
        case DrawState of
          pdsNormal:
            DrawDetail := tbCheckBoxUncheckedNormal;
          pdsHot:
            DrawDetail := tbCheckBoxUncheckedHot;
          pdsPressed:
            DrawDetail := tbCheckBoxUncheckedPressed;
          else // pdsDisabled
            DrawDetail := tbCheckBoxUncheckedDisabled;
        end;
      else // cbGrayed
        case DrawState of
          pdsNormal:
            DrawDetail := tbCheckBoxMixedNormal;
          pdsHot:
            DrawDetail := tbCheckBoxMixedHot;
          pdsPressed:
            DrawDetail := tbCheckBoxMixedPressed;
          else // pdsDisabled
            DrawDetail := tbCheckBoxMixedDisabled;
        end;
    end;
    ElementDetails := StyleServices.GetElementDetails(DrawDetail);
    StyleServices.DrawElement(DC, ElementDetails, DrawRect);
  end
  else
  begin
    case State of
      cbChecked:
        DrawFlag := DFCS_BUTTONCHECK or DFCS_CHECKED;
      cbUnchecked:
        DrawFlag := DFCS_BUTTONCHECK;
      else // cbGrayed
        DrawFlag := DFCS_BUTTON3STATE or DFCS_CHECKED;
    end;
    case DrawState of
      pdsDisabled:
        DrawFlag := DrawFlag or DFCS_INACTIVE;
      pdsHot:
        DrawFlag := DrawFlag or DFCS_HOT;
      pdsPressed:
        DrawFlag := DrawFlag or DFCS_PUSHED;
    end;
    DrawFrameControl(DC, DrawRect, DFC_BUTTON, DrawFlag);
  end;
  if Flat then
  begin
    InflateRect(DrawRect, -1, -1);
    SelectClipRgn(DC, OldRgn);
    DeleteObject(OldRgn);
    Brush := CreateSolidBrush(ColorToRGB(clBtnShadow));
    FrameRect(DC, DrawRect, Brush);
    DeleteObject(Brush);
  end;
end;

var DropDownSize: TSize;

function GetDropDownSize: TSize;
begin
  Result := DropDownSize;
end;

function CalcDropDownSize: TSize;
begin
  Result.cx := GetSystemMetrics(SM_CXVSCROLL);
  Result.cy := GetSystemMetrics(SM_CYVSCROLL);
end;

procedure DrawDropDown(DC: HDC; const Rect: TRect; DrawState: TPartDrawState;
  Themed: Boolean);
var
  DrawDetail: TThemedComboBox;
  DrawFlag: Integer;
  ElementDetails: TThemedElementDetails;
begin
  if not RectVisible(DC, Rect) then
    Exit;
  if Themed and StyleServices.Enabled then
  begin
    case DrawState of
      pdsNormal:
        DrawDetail := tcDropDownButtonNormal;
      pdsHot:
        DrawDetail := tcDropDownButtonHot;
      pdsPressed:
        DrawDetail := tcDropDownButtonPressed;
      else // pdsDisabled
        DrawDetail := tcDropDownButtonDisabled;
    end;
    ElementDetails := StyleServices.GetElementDetails(DrawDetail);
    StyleServices.DrawElement(DC, ElementDetails, Rect);
  end
  else
  begin
    DrawFlag := DFCS_SCROLLCOMBOBOX;
    case DrawState of
      pdsDisabled:
        DrawFlag := DrawFlag or DFCS_INACTIVE;
      pdsHot:
        DrawFlag := DrawFlag or DFCS_HOT;
      pdsPressed:
        DrawFlag := DrawFlag or DFCS_PUSHED;
    end;
    DrawFrameControl(DC, Rect, DFC_SCROLL, DrawFlag);
  end;
end;

{$IFDEF COMPILER2010_UP}
procedure FillGlassRect(Canvas: TCanvas; Rect: TRect);
var
  MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
  try
    FillRect(MemDC, Rect, Canvas.Brush.Handle);
    BufferedPaintMakeOpaque(PaintBuffer, Rect);
  finally
    EndBufferedPaint(PaintBuffer, True);
  end;
end;
{$ENDIF}

function FindFlags(Root: TComponent): TCustomFlagImageList;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(Root) then
    for I := 0 to Root.ComponentCount - 1 do
      if Root.Components[I] is TCustomFlagImageList then
      begin
        Result := TCustomFlagImageList(Root.Components[I]);
        Exit;
      end;
end;

{ TCustomFlagImageList }

constructor TCustomFlagImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Regions := TKeyLookup<String,Integer>.Create;
  BlendColor := clFuchsia;
  HelperDC := CreateCompatibleDC(0);
  try
    PrepareFlags;
  finally
    DeleteDC(HelperDC);
  end;
end;

{$IFDEF COMPILER2010_UP}
function TCustomFlagImageList.GetColorDepth: TColorDepth;
begin
  Result := inherited ColorDepth;
end;
{$ENDIF}

{$IFDEF COMPILER2010_UP}
procedure TCustomFlagImageList.SetColorDepth(Value: TColorDepth);
begin
  if inherited ColorDepth <> Value then
  begin
    inherited ColorDepth := Value;
    PrepareFlags;
  end;
end;
{$ENDIF}

procedure TCustomFlagImageList.DefineProperties(Filer: TFiler);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    WriteData(Stream);
    Handle := 0;
    inherited DefineProperties(Filer);
    Stream.Position := 0;
    ReadData(Stream);
  finally
    Stream.Free;
  end;
end;

destructor TCustomFlagImageList.Destroy;
begin
  Regions.Free;
  inherited Destroy;
end;

procedure TCustomFlagImageList.NormalizeImage(var hImage: HBITMAP;
  TranparentPixel: TTransparentPixel);
var
  Image: TBitmap;
  NormalizedImage: TBitmap;
begin
  NormalizedImage := TBitmap.Create;
  try
    NormalizedImage.Canvas.Brush.Color := BlendColor;
    NormalizedImage.Width := Width;
    NormalizedImage.Height := Height;
    if hImage <> 0 then
    begin
      Image := TBitmap.Create;
      try
        Image.Handle := hImage;
        Image.Transparent := (TranparentPixel <> tpNone);
        case TranparentPixel of
          tpTopLeft:
            Image.TransparentColor := Image.Canvas.Pixels[0, 0];
          tpTopRight:
            Image.TransparentColor := Image.Canvas.Pixels[Image.Width - 1, 0];
          tpBottomRight:
            Image.TransparentColor := Image.Canvas.Pixels[Image.Width - 1, Image.Height - 1];
          tpBottomLeft:
            Image.TransparentColor := Image.Canvas.Pixels[0, Image.Height - 1];
        end;
        NormalizedImage.Canvas.Draw(
          (NormalizedImage.Width - Image.Width) div 2,
          (NormalizedImage.Height - Image.Height) div 2, Image);
      finally
        Image.Free;
      end;
    end;
    hImage := NormalizedImage.ReleaseHandle;
  finally
    NormalizedImage.Free;
  end;
end;

procedure TCustomFlagImageList.NormalizeFlag(var hImage: HBITMAP;
  const ISO_3166_2: String);
var
  Info: TBitmapInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.bmiHeader.biSize := SizeOf(Info.bmiHeader);
  GetDIBits(HelperDC, hImage, 0, 0, nil, Info, DIB_RGB_COLORS);
  if (Info.bmiHeader.biWidth <> Width) or (Info.bmiHeader.biHeight <> Height) then
  begin
    if ISO_3166_2 = 'NP' then            // Nepal's flag is triangular
      NormalizeImage(hImage, tpTopRight)
    else
      NormalizeImage(hImage);
  end;
end;

function TCustomFlagImageList.AddFlag(const ISO_3166_2: String;
  hImage: HBITMAP): Integer;
begin
  NormalizeFlag(hImage, ISO_3166_2);
  Result := ImageList_AddMasked(Handle, hImage, BlendColor);
  Regions.Add(ISO_3166_2, Result);
end;

function TCustomFlagImageList.ImageIndexOf(const ISO_3166_2: String): Integer;
begin
  if not Regions.Retrieve(UpperCase(ISO_3166_2), Result) then
    Result := -1;
end;

function TCustomFlagImageList.ImageIndexOf(Territory: TTerritoryInfo): Integer;
begin
  if not Assigned(Territory) or not Regions.Retrieve(Territory.Code2, Result) then
    Result := -1;
end;

function TCustomFlagImageList.ImageIndexOf(Culture: TCultureInfo): Integer;
begin
  if not Assigned(Culture) or not Assigned(Culture.Country) or
     not Regions.Retrieve(Culture.Country.Code2, Result)
  then
    Result := -1;
end;

function TCustomFlagImageList.ImageIndexOf(Currency: TCurrencyInfo): Integer;
var
  Code: String;
begin
  if Assigned(Currency) then
  begin
    if Assigned(Currency.OriginCountry) then
      Code := Currency.OriginCountry.Code2
    else
      Code := 'EU';
    if not Regions.Retrieve(Code, Result) then
      Result := -1;
  end
  else
    Result := -1;
end;

{ TCustomResFlagImageList }

function EnumBitmapsCallback(hModule: HMODULE; lpszType, lpszName: PChar;
  lParam: Integer): Integer; stdcall;
var
  IL: TCustomResFlagImageList absolute lParam;
  ISO_3166_2: String;
begin
  if (lpszType = RT_BITMAP) and (ULONG_PTR(lpszName) shr 16 <> 0) and
     (AnsiStrPos(lpszName, PChar(IL.GetResourcePrefix)) = lpszName) then
  begin
    SetString(ISO_3166_2, lpszName + Length(IL.GetResourcePrefix),
      StrLen(lpszName) - Cardinal(Length(IL.GetResourcePrefix)));
    IL.AddFlagFromResource(ISO_3166_2, lpszName, hModule);
  end;
  Result := 1;
end;

function TCustomResFlagImageList.AddFlagFromResource(const ISO_3166_2,
  ResName: String; hModule: HMODULE): Integer;
var
  hImage: HBITMAP;
begin
  if hModule = 0 then
    hModule := HInstance;
  hImage := LoadImage(hModule, PChar(ResName), IMAGE_BITMAP, 0, 0, 0);
  try
    Result := addFlag(UpperCase(ISO_3166_2), hImage);
  finally
    DeleteObject(hImage);
  end;
end;

procedure TCustomResFlagImageList.PrepareFlags;
begin
  with GetFlagSize do SetSize(cx, cy);
  EnumResourceNames(HInstance, RT_BITMAP, @EnumBitmapsCallback, Integer(Self));
end;

{ TFlagImageList }

function TFlagImageList.GetFlagSize: TSize;
begin
  Result.cx := 16;
  Result.cy := 11;
end;

function TFlagImageList.GetResourcePrefix: String;
begin
  Result := 'FLAG_';
end;

{ TCustomImageLabel }

constructor TCustomImageLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImagesLink := TChangeLink.Create;
  fImagesLink.OnChange := ImagesChanged;
  fPadding := TPadding.Create(Self);
  fPadding.OnChange := PaddingChanged;
  fBorderColor := clNone;
  fImageIndex := -1;
  fSpacing := 4;
end;

destructor TCustomImageLabel.Destroy;
begin
  Images := nil;
  fImagesLink.Free;
  fPadding.Free;
  inherited Destroy;
end;

procedure TCustomImageLabel.SetSpacing(Value: Integer);
begin
  if Spacing <> Value then
  begin
    fSpacing := Value;
    if Assigned(Images) and (ImageIndex >= 0) then
    begin
      AdjustBounds;
      Invalidate;
    end;
  end;
end;

procedure TCustomImageLabel.SetBorderWidth(Value: TBorderWidth);
begin
  if BorderWidth <> Value then
  begin
    fBorderWidth := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomImageLabel.SetBorderColor(Value: TColor);
begin
  if BorderColor <> Value then
  begin
    fBorderColor := Value;
    if BorderWidth <> 0 then
      Invalidate;
  end;
end;

function TCustomImageLabel.GetImageIndex: TImageIndex;
begin
  Result := fImageIndex;
end;

procedure TCustomImageLabel.SetImageIndex(Value: TImageIndex);
var
  NeedsAdjustment: Boolean;
begin
  if ImageIndex <> Value then
  begin
    NeedsAdjustment := (ImageIndex < 0) or (Value < 0);
    fImageIndex := Value;
    if Assigned(Images) and NeedsAdjustment then
      AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomImageLabel.SetImages(Value: TCustomImageList);
begin
  if Images <> Value then
  begin
    if Assigned(Images) then
    begin
      Images.RemoveFreeNotification(Self);
      Images.UnRegisterChanges(fImagesLink);
    end;
    fImages := Value;
    if Assigned(Images) then
    begin
      Images.FreeNotification(Self);
      Images.RegisterChanges(fImagesLink);
    end;
    fImagesLink.Change;
  end;
end;

procedure TCustomImageLabel.SetPadding(Value: TPadding);
begin
  Padding.Assign(Value);
end;

procedure TCustomImageLabel.ImagesChanged(Sender: TObject);
begin
  AdjustBounds;
  Invalidate;
end;

procedure TCustomImageLabel.PaddingChanged(Sender: TObject);
begin
  AdjustBounds;
  Invalidate;
end;

procedure TCustomImageLabel.CNVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Visible then
  begin
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomImageLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TCustomImageLabel.DoDrawText(var Rect: TRect; DrawFlags: Integer);
var
  ConsiderImage: Boolean;
  ConsiderText: Boolean;
  X, Y, I: Integer;
  tm: TTextMetric;
begin
  ConsiderText := (GetLabelText <> '');
  ConsiderImage := Assigned(Images) and (ImageIndex >= 0);
  if (DrawFlags and DT_CALCRECT) = DT_CALCRECT then
  begin
    if ConsiderText then
    begin
      if ConsiderImage then
        Dec(Rect.Right, Images.Width + Spacing);
      inherited DoDrawText(Rect, DrawFlags);
      if ConsiderImage then
      begin
        Inc(Rect.Right, Images.Width + Spacing);
        if Images.Height > (Rect.Bottom - Rect.Top) then
          Rect.Bottom := Rect.Top + Images.Height;
      end;
    end
    else if ConsiderImage then
    begin
      Rect.Right := Rect.Left + Images.Width;
      Rect.Bottom := Rect.Top + Images.Height;
    end
    else if not (csDesigning in ComponentState) then
    begin
      Rect.Right := Rect.Left;
      Rect.Bottom := Rect.Top;
    end;
  end
  else
  begin
    if ConsiderImage then
    begin
      if UseRightToLeftAlignment then
      begin
        X := Rect.Right - Images.Width;
        Dec(Rect.Right, Images.Width + Spacing);
      end
      else
      begin
        X := Rect.Left;
        Inc(Rect.Left, Images.Width + Spacing);
      end;
      if ConsiderText then
      begin
        GetTextMetrics(Canvas.Handle, tm);
        I := (tm.tmHeight - Images.Height) div 2;
        if I < 0 then
        begin
          Y := Rect.Top;
          if (Rect.Bottom - Rect.Top) = Images.Height then
            Inc(Rect.Top, -I);
        end
        else
          Y := Rect.Top + I;
        inherited DoDrawText(Rect, DrawFlags);
      end
      else
        Y := (Rect.Top + Rect.Bottom - Images.Height) div 2;
      Images.Draw(Canvas, X, Y, GetImageIndex, Enabled);
    end
    else if ConsiderText then
      inherited DoDrawText(Rect, DrawFlags)
  end;
end;

procedure TCustomImageLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WordBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
  TheAlignment: TAlignment;
begin
  if (([csReading, csLoading, csDestroying] * ComponentState) = []) and
     AutoSize and (Visible or (csDesigning in ComponentState)) then
  begin
    Rect := ClientRect;
    Dec(Rect.Right, Padding.Left + Padding.Right + 2 * BorderWidth);
    Dec(Rect.Bottom, Padding.Top + Padding.Bottom + 2 * BorderWidth);
    DC := GetDC(0);
    try
      Canvas.Handle := DC;
      DoDrawText(Rect, DT_EXPANDTABS or DT_CALCRECT or WordWraps[WordWrap]);
      Canvas.Handle := 0;
    finally
      ReleaseDC(0, DC);
    end;
    Inc(Rect.Right, Padding.Left + Padding.Right + 2 * BorderWidth);
    Inc(Rect.Bottom, Padding.Top + Padding.Bottom + 2 * BorderWidth);
    X := Left;
    TheAlignment := Alignment;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(TheAlignment);
    if TheAlignment = taRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TCustomImageLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WordBREAK);
var
  Rect, CalcRect: TRect;
  DrawStyle: Longint;
  TheAlignment: TAlignment;
begin
  with Canvas do
  begin
    Rect := ClientRect;
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      {$IFDEF COMPILER2010_UP}
      if not (csGlassPaint in ControlState) then
        FillRect(Rect)
      else
        FillGlassRect(Canvas, Rect);
      {$ELSE}
      FillRect(Rect);
      {$ENDIF}
    end;
    Brush.Style := bsClear;
    if BorderWidth <> 0 then
    begin
      if BorderColor <> clNone then
      begin
        if BorderColor = clDefault then
          Pen.Color := Font.Color
        else
          Pen.Color := BorderColor;
        Pen.Width := BorderWidth;
        Pen.Style := psInsideFrame;
        Rectangle(Rect);
      end;
      InflateRect(Rect, -BorderWidth, -BorderWidth);
    end;
    Inc(Rect.Left, Padding.Left);
    Inc(Rect.Top, Padding.Top);
    Dec(Rect.Right, Padding.Right);
    Dec(Rect.Bottom, Padding.Bottom);
    DrawStyle := DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment];
    if not AutoSize or (Align <> alNone) then
    begin
      CalcRect := Rect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      TheAlignment := Alignment;
      if UseRightToLeftAlignment then
        ChangeBiDiModeAlignment(TheAlignment);
      case TheAlignment of
        taRightJustify: OffsetRect(CalcRect, Rect.Right - CalcRect.Right, 0);
        taCenter: OffsetRect(CalcRect, (Rect.Right - CalcRect.Right) div 2, 0);
      end;
      case Layout of
        tlBottom: OffsetRect(CalcRect, 0, Rect.Bottom - CalcRect.Bottom);
        tlCenter: OffsetRect(CalcRect, 0, (Rect.Bottom - CalcRect.Bottom) div 2);
      end;
      DoDrawText(CalcRect, DrawStyle);
    end
    else
      DoDrawText(Rect, DrawStyle);
  end;
end;

{ TCustomFlagLabel }

constructor TCustomFlagLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if csDesigning in ComponentState then
    Flags := FindFlags(AOwner);
end;

procedure TCustomFlagLabel.SetFlags(Value: TCustomFlagImageList);
begin
  inherited Images := Value;
end;

function TCustomFlagLabel.GetFlags: TCustomFlagImageList;
begin
  Result := TCustomFlagImageList(inherited Images);
end;

{ TCustomFlagBox }

constructor TCustomFlagBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csDropDownList;
  fFlagsLink := TChangeLink.Create;
  if csDesigning in ComponentState then
    Flags := FindFlags(AOwner);
  fFlagsLink.OnChange := FlagsChanged;
  fItemIndex := -1;
end;

destructor TCustomFlagBox.Destroy;
begin
  Flags := nil;
  fFlagsLink.Free;
  inherited Destroy;
end;

procedure TCustomFlagBox.FlagsChanged(Sender: TObject);
begin
  ResetItemHeight;
  Invalidate;
end;

procedure TCustomFlagBox.SetSorted(Value: Boolean);
begin
  if Sorted <> Value then
  begin
    fSorted := Value;
    RebuildInternalItems;
  end;
end;

procedure TCustomFlagBox.SetFlagPosition(Value: TFlagPosition);
begin
  if FlagPosition <> Value then
  begin
    fFlagPosition := Value;
    Invalidate;
  end;
end;

procedure TCustomFlagBox.SetFlags(Value: TCustomFlagImageList);
begin
  if Flags <> Value then
  begin
    if Assigned(Flags) then
    begin
      Flags.RemoveFreeNotification(Self);
      Flags.UnRegisterChanges(fFlagsLink);
    end;
    fFlags := Value;
    if Assigned(Flags) then
    begin
      Flags.FreeNotification(Self);
      Flags.RegisterChanges(fFlagsLink);
    end;
    fFlagsLink.Change;
  end;
end;

function TCustomFlagBox.GetItemIndex: Integer;
begin
  if ItemsReady then
    Result := inherited GetItemIndex
  else
    Result := fItemIndex;
end;

procedure TCustomFlagBox.SetItemIndex(const Value: Integer);
begin
  if ItemsReady then
    inherited SetItemIndex(Value)
  else
    fItemIndex := Value;
end;

function TCustomFlagBox.DoDrawItemLabel(Index: Integer; var Rect: TRect;
  State: TOwnerDrawState; var TheLabel: String): Boolean;
begin
  Result := True;
  if Assigned(OnDrawItemLabel) then
    OnDrawItemLabel(Self, Index, Rect, State, TheLabel, Result);
end;

procedure TCustomFlagBox.DoItemsChange;
begin
  if Assigned(OnItemsChange) then
    OnItemsChange(Self);
end;

procedure TCustomFlagBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  TheLabel: String;
  X, Y: Integer;
begin
  Canvas.FillRect(Rect);
  SetBkMode(Canvas.Handle, TRANSPARENT);
  if Index >= 0 then
  begin
    if Assigned(Flags) then
    begin
      if UseRightToLeftAlignment xor (FlagPosition = fpAfterLabel) then
      begin
        X := Rect.Right - Flags.Width - HorzSpacing;
        Dec(Rect.Right, Flags.Width + 2 * HorzSpacing);
      end
      else
      begin
        X := Rect.Left + HorzSpacing;
        Inc(Rect.Left, Flags.Width + 2 * HorzSpacing);
      end;
      Y := (Rect.Top + Rect.Bottom - Flags.Height) div 2;
      Flags.Draw(Canvas, X, Y, GetImageIndex(Index));
    end;
    TheLabel := GetLabelText(Index);
    if DoDrawItemLabel(Index, Rect, State, TheLabel) then
    begin
      InflateRect(Rect, -HorzSpacing, 0);
      DrawText(Canvas.Handle, PChar(TheLabel), Length(TheLabel), Rect,
        DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or DT_EXPANDTABS or
          DT_SINGLELINE or DT_NOPREFIX));
    end;
  end;
end;

procedure TCustomFlagBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Flags) then
    Flags := nil;
end;

procedure TCustomFlagBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or CBS_OWNERDRAWFIXED or CBS_DROPDOWNLIST;
end;

procedure TCustomFlagBox.CreateWnd;
begin
  inherited CreateWnd;
  ResetItemHeight;
  if not (csRecreating in ControlState) then
  begin
    RebuildInternalItems;
    fItemsReady := True;
    if fItemIndex <> -1 then
      inherited ItemIndex := fItemIndex;
  end;
end;

procedure TCustomFlagBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
end;

procedure TCustomFlagBox.CMEnter(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

function TCustomFlagBox.GetLabelText(Index: Integer): String;
begin
  Result := Items[Index];
end;

function TCustomFlagBox.GetStandardItemHeight;
begin
  if WindowHandle = 0 then
    Result := Abs(Font.Height) + 2
  else
    Result := Canvas.TextHeight('|');
  if Assigned(Flags) and (Result < Flags.Height) then
    Result := Flags.Height;
  Inc(Result, 2 * VertSpacing);
end;

procedure TCustomFlagBox.ResetItemHeight;
begin
  fStandardItemHeight := GetStandardItemHeight;
  if (WindowHandle = 0) or (Style = csOwnerDrawVariable) then
    inherited ItemHeight := StandardItemHeight
  else
    SendMessage(WindowHandle, CB_SETITEMHEIGHT, 0, StandardItemHeight);
end;

{ TCustomFlagListBox }

constructor TCustomFlagListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
  fFlagsLink := TChangeLink.Create;
  if csDesigning in ComponentState then
    Flags := FindFlags(AOwner);
  fFlagsLink.OnChange := FlagsChanged;
  fItemIndex := -1;
end;

destructor TCustomFlagListBox.Destroy;
begin
  Flags := nil;
  fFlagsLink.Free;
  inherited Destroy;
end;

procedure TCustomFlagListBox.FlagsChanged(Sender: TObject);
begin
  ResetItemHeight;
  Invalidate;
end;

procedure TCustomFlagListBox.SetSorted(Value: Boolean);
begin
  if Sorted <> Value then
  begin
    fSorted := Value;
    RebuildInternalItems;
  end;
end;

procedure TCustomFlagListBox.SetFlagPosition(Value: TFlagPosition);
begin
  if FlagPosition <> Value then
  begin
    fFlagPosition := Value;
    Invalidate;
  end;
end;

procedure TCustomFlagListBox.SetFlags(Value: TCustomFlagImageList);
begin
  if Flags <> Value then
  begin
    if Assigned(Flags) then
    begin
      Flags.RemoveFreeNotification(Self);
      Flags.UnRegisterChanges(fFlagsLink);
    end;
    fFlags := Value;
    if Assigned(Flags) then
    begin
      Flags.FreeNotification(Self);
      Flags.RegisterChanges(fFlagsLink);
    end;
    fFlagsLink.Change;
  end;
end;

function TCustomFlagListBox.GetItemIndex: Integer;
begin
  if ItemsReady then
    Result := inherited GetItemIndex
  else
    Result := fItemIndex;
end;

procedure TCustomFlagListBox.SetItemIndex(const Value: Integer);
begin
  if ItemsReady then
    inherited SetItemIndex(Value)
  else
    fItemIndex := Value;
end;

function TCustomFlagListBox.GetLabelText(Index: Integer): String;
begin
  Result := Items[Index];
end;

procedure TCustomFlagListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  TheLabel: String;
  X, Y: Integer;
begin
  Canvas.FillRect(Rect);
  SetBkMode(Canvas.Handle, TRANSPARENT);
  if Index >= 0 then
  begin
    if Assigned(Flags) then
    begin
      if UseRightToLeftAlignment xor (FlagPosition = fpAfterLabel) then
      begin
        X := Rect.Right - Flags.Width - HorzSpacing;
        Dec(Rect.Right, Flags.Width + 2 * HorzSpacing);
      end
      else
      begin
        X := Rect.Left + HorzSpacing;
        Inc(Rect.Left, Flags.Width + 2 * HorzSpacing);
      end;
      Y := (Rect.Top + Rect.Bottom - Flags.Height) div 2;
      Flags.Draw(Canvas, X, Y, GetImageIndex(Index));
    end;
    TheLabel := GetLabelText(Index);
    if DoDrawItemLabel(Index, Rect, State, TheLabel) then
    begin
      InflateRect(Rect, -HorzSpacing, 0);
      DrawText(Canvas.Handle, PChar(TheLabel), Length(TheLabel), Rect,
        DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or DT_EXPANDTABS or
          DT_SINGLELINE or DT_NOPREFIX));
    end;
  end;
end;

procedure TCustomFlagListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Flags) then
    Flags := nil;
end;

procedure TCustomFlagListBox.CreateWnd;
begin
  inherited CreateWnd;
  ResetItemHeight;
  if not (csRecreating in ControlState) then
  begin
    RebuildInternalItems;
    fItemsReady := True;
    if fItemIndex <> -1 then
      inherited ItemIndex := fItemIndex;
  end;
end;

procedure TCustomFlagListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
end;

function TCustomFlagListBox.DoDrawItemLabel(Index: Integer; var Rect: TRect;
  State: TOwnerDrawState; var TheLabel: String): Boolean;
begin
  Result := True;
  if Assigned(OnDrawItemLabel) then
    OnDrawItemLabel(Self, Index, Rect, State, TheLabel, Result);
end;

procedure TCustomFlagListBox.DoItemsChange;
begin
  if Assigned(OnItemsChange) then
    OnItemsChange(Self);
end;

function TCustomFlagListBox.GetStandardItemHeight;
begin
  if WindowHandle = 0 then
    Result := Abs(Font.Height) + 2
  else
    Result := Canvas.TextHeight('|');
  if Assigned(Flags) and (Result < Flags.Height) then
    Result := Flags.Height;
  Inc(Result, 2 * VertSpacing);
end;

procedure TCustomFlagListBox.ResetItemHeight;
begin
  fStandardItemHeight := GetStandardItemHeight;
  if (WindowHandle = 0) or (Style = lbOwnerDrawVariable) then
    inherited ItemHeight := StandardItemHeight
  else
    SendMessage(WindowHandle, LB_SETITEMHEIGHT, 0, StandardItemHeight);
end;

{ TCheckListBoxDataWrapper }

type
  TCheckListBoxDataWrapper = class
  private
    fState: TCheckBoxState;
    fDisabled: Boolean;
    procedure SetChecked(Value: Boolean); inline;
    function GetChecked: Boolean; inline;
  protected
    Data: TListBoxItemData;
  public
    class function GetDefaultState: TCheckBoxState; static;
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read fState write fState;
    property Disabled: Boolean read fDisabled write fDisabled;
  end;

class function TCheckListBoxDataWrapper.GetDefaultState: TCheckBoxState;
begin
  Result := cbUnchecked;
end;

procedure TCheckListBoxDataWrapper.SetChecked(Value: Boolean);
begin
  fState := TCheckBoxState(Value);
end;

function TCheckListBoxDataWrapper.GetChecked: Boolean;
begin
  Result := (fState = cbChecked);
end;

{ TCheckListRecall }

constructor TCheckListRecall.Create(AReference: TPersistent);
begin
  CheckList := AReference;
  StateInfo := TKeyLookup<String,TStateRec>.Create;
  if Assigned(CheckList) then
    Assign(CheckList);
end;

destructor TCheckListRecall.Destroy;
begin
  if Assigned(CheckList) then
    AssignTo(CheckList);
  StateInfo.Free;
  inherited Destroy;
end;

procedure TCheckListRecall.Store;
begin
  if Assigned(CheckList) then
    Assign(CheckList);
end;

procedure TCheckListRecall.Forget;
begin
  CheckList := nil;
  StateInfo.Clear;
end;

procedure TCheckListRecall.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TCheckListRecall then
    StateInfo.CopyFrom(TCheckListRecall(Source).StateInfo)
  else if Source is TCustomCultureCheckListBox then
  begin
    StateInfo.Clear;
    with TCustomCultureCheckListBox(Source), StateInfo do
      for I := 0 to Items.Count - 1 do
        Add(Items[I].Locale, StateRec(State[I], not ItemEnabled[I]));
  end
  else if Source is TCustomTerritoryCheckListBox then
  begin
    StateInfo.Clear;
    with TCustomTerritoryCheckListBox(Source), StateInfo do
      for I := 0 to Items.Count - 1 do
        Add(Items[I].Code2, StateRec(State[I], not ItemEnabled[I]));
  end
  else if Source is TCustomCurrencyCheckListBox then
  begin
    StateInfo.Clear;
    with TCustomCurrencyCheckListBox(Source), StateInfo do
      for I := 0 to Items.Count - 1 do
        Add(Items[I].IntlSymbol, StateRec(State[I], not ItemEnabled[I]));
  end
  else if Source is TCheckListBox then
  begin
    StateInfo.Clear;
    with TCheckListBox(Source), StateInfo do
      for I := 0 to Items.Count - 1 do
        Add(Items[I], StateRec(State[I], not ItemEnabled[I]));
  end
  else
    inherited Assign(Source);
end;

procedure TCheckListRecall.AssignTo(Dest: TPersistent);
var
  I: Integer;
  Rec: TStateRec;
begin
  if Dest is TCustomCultureCheckListBox then
    with TCustomCultureCheckListBox(Dest) do
      for I := 0 to Items.Count - 1 do
      begin
        if StateInfo.Retrieve(Items[I].Locale, Rec) then
        begin
          State[I] := Rec.State;
          ItemEnabled[I] := not Rec.Disabled;
        end
        else
        begin
          State[I] := TCheckListBoxDataWrapper.GetDefaultState;
          ItemEnabled[I] := True;
        end;
      end
  else if Dest is TCustomTerritoryCheckListBox then
    with TCustomTerritoryCheckListBox(Dest) do
      for I := 0 to Items.Count - 1 do
      begin
        if StateInfo.Retrieve(Items[I].Code2, Rec) then
        begin
          State[I] := Rec.State;
          ItemEnabled[I] := not Rec.Disabled;
        end
        else
        begin
          State[I] := TCheckListBoxDataWrapper.GetDefaultState;
          ItemEnabled[I] := True;
        end;
      end
  else if Dest is TCustomCurrencyCheckListBox then
    with TCustomCurrencyCheckListBox(Dest) do
      for I := 0 to Items.Count - 1 do
      begin
        if StateInfo.Retrieve(Items[I].IntlSymbol, Rec) then
        begin
          State[I] := Rec.State;
          ItemEnabled[I] := not Rec.Disabled;
        end
        else
        begin
          State[I] := TCheckListBoxDataWrapper.GetDefaultState;
          ItemEnabled[I] := True;
        end;
      end
  else if Dest is TCheckListBox then
    with TCheckListBox(Dest) do
      for I := 0 to Items.Count - 1 do
      begin
        if StateInfo.Retrieve(Items[I], Rec) then
        begin
          State[I] := Rec.State;
          ItemEnabled[I] := not Rec.Disabled;
        end
        else
        begin
          State[I] := TCheckListBoxDataWrapper.GetDefaultState;
          ItemEnabled[I] := True;
        end;
      end
  else
    inherited AssignTo(Dest);
end;

function TCheckListRecall.StateRec(AState: TCheckBoxState; ADisabled: Boolean): TStateRec;
begin
  Result.State := AState;
  Result.Disabled := ADisabled;
end;

{ TCustomFlagCheckListBox }

constructor TCustomFlagCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWrapperList := TObjectList.Create;
  fCheckWidth := CheckBoxSize.cx + HorzSpacing;
  fFlat := True;
end;

destructor TCustomFlagCheckListBox.Destroy;
begin
  fWrapperList.Free;
  inherited Destroy;
end;

procedure TCustomFlagCheckListBox.SetFlat(Value: Boolean);
begin
  if Flat <> Value then
  begin
    fFlat := Value;
    Invalidate;
  end;
end;

function TCustomFlagCheckListBox.GetChecked(Index: Integer): Boolean;
begin
  if ItemsReady and HaveWrapper(Index) then
    Result := TCheckListBoxDataWrapper(GetWrapper(Index)).Checked
  else
    Result := False;
end;

procedure TCustomFlagCheckListBox.SetChecked(Index: Integer; Value: Boolean);
begin
  if ItemsReady and (Checked[Index] <> Value) then
  begin
    TCheckListBoxDataWrapper(GetWrapper(Index)).Checked := Value;
    InvalidateCheck(Index);
  end;
end;

function TCustomFlagCheckListBox.GetState(Index: Integer): TCheckBoxState;
begin
  if ItemsReady and HaveWrapper(Index) then
    Result := TCheckListBoxDataWrapper(GetWrapper(Index)).State
  else
    Result := TCheckListBoxDataWrapper.GetDefaultState;
end;

procedure TCustomFlagCheckListBox.SetState(Index: Integer; Value: TCheckBoxState);
begin
  if ItemsReady and (State[Index] <> Value) then
  begin
    TCheckListBoxDataWrapper(GetWrapper(Index)).State := Value;
    InvalidateCheck(Index);
  end;
end;

function TCustomFlagCheckListBox.GetItemEnabled(Index: Integer): Boolean;
begin
  if ItemsReady and HaveWrapper(Index) then
    Result := not TCheckListBoxDataWrapper(GetWrapper(Index)).Disabled
  else
    Result := True;
end;

procedure TCustomFlagCheckListBox.SetItemEnabled(Index: Integer; Value: Boolean);
begin
  if ItemsReady and (ItemEnabled[Index] <> Value) then
  begin
    TCheckListBoxDataWrapper(GetWrapper(Index)).Disabled := not Value;
    InvalidateCheck(Index);
  end;
end;

procedure TCustomFlagCheckListBox.ClickCheck;
begin
  if Assigned(fOnClickCheck) then
    fOnClickCheck(Self);
end;

procedure TCustomFlagCheckListBox.KeyPress(var Key: Char);
begin
  if Key = ' ' then
    ToggleClickCheck(ItemIndex);
  inherited KeyPress(Key);
end;

procedure TCustomFlagCheckListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    Index := ItemAtPos(Point(X,Y),True);
    if (Index >= 0) and GetItemEnabled(Index) then
      if UseRightToLeftAlignment then
      begin
        Dec(X, ItemRect(Index).Right - CheckWidth);
        if (X > 0) and (X < CheckWidth) then
          ToggleClickCheck(Index)
      end
      else
      begin
        if X - ItemRect(Index).Left < CheckWidth then
          ToggleClickCheck(Index)
      end;
  end;
end;

function TCustomFlagCheckListBox.CreateWrapper(Index: Integer): TObject;
begin
  Result := TCheckListBoxDataWrapper.Create;
  fWrapperList.Add(Result);
  inherited SetItemData(Index, TListBoxItemData(Result));
end;

function TCustomFlagCheckListBox.ExtractWrapper(Index: Integer): TObject;
begin
  Result := TCheckListBoxDataWrapper(inherited GetItemData(Index));
  if LB_ERR = Integer(Result) then
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
  if (Result <> nil) and not (Result is TCheckListBoxDataWrapper) then
    Result := nil;
end;

function TCustomFlagCheckListBox.HaveWrapper(Index: Integer): Boolean;
begin
  Result := ExtractWrapper(Index) <> nil;
end;

function TCustomFlagCheckListBox.GetWrapper(Index: Integer): TObject;
begin
  Result := ExtractWrapper(Index);
  if Result = nil then
    Result := CreateWrapper(Index);
end;

function TCustomFlagCheckListBox.InternalGetItemData(Index: Integer): TListBoxItemData;
begin
  Result := inherited GetItemData(Index);
end;

procedure TCustomFlagCheckListBox.InternalSetItemData(Index: Integer; AData: TListBoxItemData);
begin
  inherited SetItemData(Index, AData);
end;

function TCustomFlagCheckListBox.GetItemData(Index: Integer): TListBoxItemData;
begin
  Result := 0;
  if HaveWrapper(Index) then
    Result := TCheckListBoxDataWrapper(GetWrapper(Index)).Data;
end;

procedure TCustomFlagCheckListBox.SetItemData(Index: Integer; AData: TListBoxItemData);
var
  Wrapper: TCheckListBoxDataWrapper;
begin
  if HaveWrapper(Index) or (AData <> 0) then
  begin
    Wrapper := TCheckListBoxDataWrapper(GetWrapper(Index));
    Wrapper.Data := AData;
  end;
end;

procedure TCustomFlagCheckListBox.ResetContent;
begin
  fWrapperList.Clear;
  inherited ResetContent;
end;

procedure TCustomFlagCheckListBox.LoadRecreateItems(RecreateItems: TStrings);
var
  I, Index: Integer;
begin
  with RecreateItems do
  begin
    BeginUpdate;
    try
      Items.NameValueSeparator := NameValueSeparator;
      Items.QuoteChar := QuoteChar;
      Items.Delimiter := Delimiter;
      Items.LineBreak := LineBreak;
      for I := 0 to Count - 1 do
      begin
        Index := Items.Add(RecreateItems[I]);
        if Objects[I] <> nil then
          InternalSetItemData(Index, TListBoxItemData(Objects[I]));
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomFlagCheckListBox.SaveRecreateItems(RecreateItems: TStrings);
var
  I: Integer;
  Wrapper: TCheckListBoxDataWrapper;
begin
  fWrapperList.OwnsObjects := False;
  fWrapperList.Clear;
  fWrapperList.OwnsObjects := True;
  with RecreateItems do
  begin
    BeginUpdate;
    try
      NameValueSeparator := Items.NameValueSeparator;
      QuoteChar := Items.QuoteChar;
      Delimiter := Items.Delimiter;
      LineBreak := Items.LineBreak;
      for I := 0 to Items.Count - 1 do
      begin
        Wrapper := TCheckListBoxDataWrapper(ExtractWrapper(I));
        AddObject(Items[I], Wrapper);
        if Wrapper <> nil then
          fWrapperList.Add(Wrapper);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomFlagCheckListBox.DeleteString(Index: Integer);
var
  Wrapper: TCheckListBoxDataWrapper;
begin
  if HaveWrapper(Index) then
  begin
    Wrapper := TCheckListBoxDataWrapper(GetWrapper(Index));
    fWrapperList.Remove(Wrapper);
  end;
  inherited DeleteString(Index);
end;

procedure TCustomFlagCheckListBox.ToggleClickCheck;
begin
  if (Index >= 0) and (Index < Count) and ItemEnabled[Index] then
  begin
    case State[Index] of
      cbUnchecked:
        if AllowGrayed then
          State[Index] := cbGrayed
        else
          State[Index] := cbChecked;
      cbChecked:
        State[Index] := cbUnchecked;
      cbGrayed:
        State[Index] := cbChecked;
    end;
    ClickCheck;
  end;
end;

procedure TCustomFlagCheckListBox.InvalidateCheck(Index: Integer);
var
  Rect: TRect;
begin
  if WindowHandle <> 0 then
  begin
    Rect := ItemRect(Index);
    if UseRightToLeftAlignment then
      Rect.Left := Rect.Right - CheckWidth
    else
      Rect.Right := Rect.Left + CheckWidth;
    InvalidateRect(WindowHandle, Rect, not (csOpaque in ControlStyle));
    UpdateWindow(WindowHandle);
  end;
end;

procedure TCustomFlagCheckListBox.RestateAll(AState: TCheckBoxState; AllowGrayed: Boolean; AllowDisabled: Boolean);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if ItemEnabled[I] or (not ItemEnabled[I] and AllowDisabled) then
      if(AllowGrayed or (GetState(I) <> cbGrayed))then
        SetState(I, AState);
end;

function TCustomFlagCheckListBox.GetStandardItemHeight: Integer;
begin
  Result := inherited GetStandardItemHeight;
  if Result < CheckBoxSize.cy + 2 * VertSpacing then
    Result := CheckBoxSize.cy + 2 * VertSpacing;
end;

procedure TCustomFlagCheckListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  R: TRect;
  Enable: Boolean;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    R := Rect;
    if UseRightToLeftAlignment then
    begin
      R.Left := R.Right - CheckWidth;
      Dec(Rect.Right, CheckWidth);
    end
    else
    begin
      R.Right := R.Left + CheckWidth;
      Inc(Rect.Left, CheckWidth);
    end;
    Canvas.FillRect(R);
    Enable := Self.Enabled and ItemEnabled[Index];
    DrawCheck(R, GetState(Index), Enable);
    if IntersectRect(R, Rect, Canvas.ClipRect) then
    begin
      if not Enable then
        Canvas.Font.Color := clGrayText;
      inherited DrawItem(Index, Rect, State);
    end;
  end;
end;

procedure TCustomFlagCheckListBox.DrawCheck(const Rect: TRect;
  AState: TCheckBoxState; AEnabled: Boolean);
var
  DrawState: TPartDrawState;
begin
  if AEnabled then
    DrawState := pdsNormal
  else
    DrawState := pdsDisabled;
  DrawCheckBox(Canvas.Handle, Rect, AState, DrawState, Flat, ThemeControl(Self));
end;

{ TCustomCultureLabel }

constructor TCustomCultureLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDisplayName := cnEnglishDisplayName;
end;

destructor TCustomCultureLabel.Destroy;
begin
  Localizer := nil;
  inherited Destroy;
end;

procedure TCustomCultureLabel.ReadCulture(Reader: TReader);
begin
  Culture := CultureOf(Reader.ReadString);
end;

procedure TCustomCultureLabel.WriteCulture(Writer: TWriter);
begin
  Writer.WriteString(Culture.Locale);
end;

procedure TCustomCultureLabel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('CultureInfo', ReadCulture, WriteCulture,
    not Assigned(Localizer) and Assigned(Culture));
end;

procedure TCustomCultureLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Localizer) then
    Localizer := nil;
end;

function TCustomCultureLabel.GetImageIndex: TImageIndex;
begin
  Result := Flags.ImageIndexOf(Culture);
end;

function TCustomCultureLabel.GetLabelText: String;
begin
  if (DisplayName = cnNone) or not Assigned(Culture) then
    Result := Caption
  else
    Result := Culture.DisplayNames[DisplayName];
end;

procedure TCustomCultureLabel.SetDisplayName(Value: TCultureDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomCultureLabel.SetCulture(Value: TCultureInfo);
begin
  if Culture <> Value then
  begin
    fCulture := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomCultureLabel.SetLocalizer(Value: TLocalizer);
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

procedure TCustomCultureLabel.LocalizerNotify(Sender: TLocalizer;
  Reason: TLocalizerNotification);
begin
  if Reason = lnCultureChanged then
  begin
    if Assigned(Localizer) then
      Culture := Localizer.Culture
    else
      Culture := nil;
  end;
end;

function TCustomCultureLabel.IsManagedByLocalizer: Boolean;
begin
  Result := Assigned(Localizer);
end;

{ TCustomCultureBox }

constructor TCustomCultureBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCultures := TCultureList.Create;
  fCultures.OnChange := CulturesChanged;
  fDisplayName := cnEnglishDisplayName;
end;

destructor TCustomCultureBox.Destroy;
begin
  fCultures.OnChange := nil;
  Localizer := nil;
  fCultures.Free;
  inherited Destroy;
end;

procedure TCustomCultureBox.CulturesChanged(Sender: TObject);
begin
  if ItemsReady then
  begin
    fCultures.AssignLabelsTo(inherited Items, DisplayName);
    DoItemsChange;
  end;
end;

procedure TCustomCultureBox.LocalizerNotify(Sender: TLocalizer;
  Reason: TLocalizerNotification);
begin
  case Reason of
    lnCatalogChanged:
      if Assigned(Localizer) then
      begin
        fCultures.Assign(Localizer.Cultures);
        ItemSelected := Localizer.Culture;
      end
      else
        fCultures.Clear;
    lnCultureChanged:
      if Assigned(Localizer) then
        ItemSelected := Localizer.Culture
      else
        ItemSelected := nil;
  end;
end;

procedure TCustomCultureBox.SetLocalizer(Value: TLocalizer);
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
      LocalizerNotify(Localizer, lnCatalogChanged);
  end;
end;

procedure TCustomCultureBox.SetCultures(Value: TCultureList);
begin
  fCultures.Assign(Value);
end;

procedure TCustomCultureBox.SetItemSelected(Value: TCultureInfo);
begin
  ItemIndex := fCultures.IndexOf(Value);
end;

function TCustomCultureBox.GetItemSelected: TCultureInfo;
begin
  if Cardinal(ItemIndex) < Cardinal(fCultures.Count) then
    Result := fCultures[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomCultureBox.SetDisplayName(Value: TCultureDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    RebuildInternalItems;
  end;
end;

function TCustomCultureBox.GetImageIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fCultures.Count) and Assigned(Flags) then
    Result := Flags.ImageIndexOf(fCultures[Index])
  else
    Result := -1;
end;

function TCustomCultureBox.IsManagedByLocalizer: Boolean;
begin
  Result := Assigned(Localizer);
end;

procedure TCustomCultureBox.ReadItemSelected(Reader: TReader);
begin
  ItemSelected := CultureOf(Reader.ReadString);
end;

procedure TCustomCultureBox.WriteItemSelected(Writer: TWriter);
begin
  Writer.WriteString(ItemSelected.Locale);
end;

procedure TCustomCultureBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Selected', ReadItemSelected, WriteItemSelected,
    not Assigned(Localizer) and Assigned(ItemSelected));
end;

procedure TCustomCultureBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Localizer) then
    Localizer := nil;
end;

procedure TCustomCultureBox.Select;
begin
  inherited Select;
  if Assigned(Localizer) then
    Localizer.Culture := ItemSelected;
end;

function TCustomCultureBox.CollectAll: Boolean;
begin
  Result := False;
  if not Assigned(Localizer) then
  begin
    fCultures.Assign(World.Cultures);
    Result := True;
  end
end;

procedure TCustomCultureBox.RebuildInternalItems;
var
  Sel: TCultureInfo;
begin
  fCultures.OnChange := nil;
  try
    Sel := ItemSelected;
    if Sorted then
      fCultures.Sort(DisplayName);
    if WindowHandle <> 0 then
    begin
      fCultures.AssignLabelsTo(inherited Items, DisplayName);
      DoItemsChange;
    end;
    ItemSelected := Sel;
  finally
    fCultures.OnChange := CulturesChanged;
  end;
end;

{ TCustomCultureListBox }

constructor TCustomCultureListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCultures := TCultureList.Create;
  fCultures.OnChange := CulturesChanged;
  fDisplayName := cnEnglishDisplayName;
end;

destructor TCustomCultureListBox.Destroy;
begin
  fCultures.OnChange := nil;
  Localizer := nil;
  fCultures.Free;
  inherited Destroy;
end;

procedure TCustomCultureListBox.CulturesChanged(Sender: TObject);
begin
  if ItemsReady then
  begin
    fCultures.AssignLabelsTo(inherited Items, DisplayName);
    DoItemsChange;
  end;
end;

procedure TCustomCultureListBox.LocalizerNotify(Sender: TLocalizer;
  Reason: TLocalizerNotification);
begin
  case Reason of
    lnCatalogChanged:
      if Assigned(Localizer) then
      begin
        fCultures.Assign(Localizer.Cultures);
        ItemSelected := Localizer.Culture;
      end
      else
        fCultures.Clear;
    lnCultureChanged:
      if Assigned(Localizer) then
        ItemSelected := Localizer.Culture
      else
        ItemSelected := nil;
  end;
end;

procedure TCustomCultureListBox.SetLocalizer(Value: TLocalizer);
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
      MultiSelect := False;
      Localizer.FreeNotification(Self);
      Localizer.RegisterListener(Self);
    end;
    if not (csDestroying in ComponentState) then
      LocalizerNotify(Localizer, lnCatalogChanged);
  end;
end;

procedure TCustomCultureListBox.SetMultiSelect(Value: Boolean);
begin
  inherited SetMultiSelect(Value and not Assigned(Localizer));
end;

procedure TCustomCultureListBox.SetCultures(Value: TCultureList);
begin
  fCultures.Assign(Value);
end;

procedure TCustomCultureListBox.SetItemSelected(Value: TCultureInfo);
begin
  ItemIndex := fCultures.IndexOf(Value);
end;

function TCustomCultureListBox.GetItemSelected: TCultureInfo;
begin
  if Cardinal(ItemIndex) < Cardinal(fCultures.Count) then
    Result := fCultures[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomCultureListBox.SetDisplayName(Value: TCultureDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    RebuildInternalItems;
  end;
end;

function TCustomCultureListBox.GetImageIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fCultures.Count) and Assigned(Flags) then
    Result := Flags.ImageIndexOf(fCultures[Index])
  else
    Result := -1;
end;

function TCustomCultureListBox.IsManagedByLocalizer: Boolean;
begin
  Result := Assigned(Localizer);
end;

procedure TCustomCultureListBox.ReadItemSelected(Reader: TReader);
begin
  ItemSelected := CultureOf(Reader.ReadString);
end;

procedure TCustomCultureListBox.WriteItemSelected(Writer: TWriter);
begin
  Writer.WriteString(ItemSelected.Locale);
end;

procedure TCustomCultureListBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Selected', ReadItemSelected, WriteItemSelected,
    not Assigned(Localizer) and Assigned(ItemSelected));
end;

procedure TCustomCultureListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Localizer) then
    Localizer := nil;
end;

procedure TCustomCultureListBox.Click;
begin
  inherited Click;
  if Assigned(Localizer) then
    Localizer.Culture := ItemSelected;
end;

function TCustomCultureListBox.CollectAll: Boolean;
begin
  Result := False;
  if not Assigned(Localizer) then
  begin
    fCultures.Assign(World.Cultures);
    Result := True;
  end
end;

procedure TCustomCultureListBox.RebuildInternalItems;
var
  Sel: TCultureInfo;
begin
  fCultures.OnChange := nil;
  try
    Sel := ItemSelected;
    if Sorted then
      fCultures.Sort(DisplayName);
    if WindowHandle <> 0 then
    begin
      fCultures.AssignLabelsTo(inherited Items, DisplayName);
      DoItemsChange;
    end;
    ItemSelected := Sel;
  finally
    fCultures.OnChange := CulturesChanged;
  end;
end;

procedure TCustomCultureListBox.SetSelection(Source: TReadonlyCultureList);
var
  I: Integer;
begin
  if MultiSelect then
    for I := 0 to fCultures.Count - 1 do
      Selected[I] := Source.Exists(fCultures[I]);
end;

procedure TCustomCultureListBox.GetSelection(Dest: TCultureList);
var
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for I := 0 to fCultures.Count - 1 do
      if Selected[I] then
        Dest.Add(fCultures[I]);
  finally
    Dest.EndUpdate;
  end;
end;

{ TCustomCultureCheckListBox }

constructor TCustomCultureCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCultures := TCultureList.Create;
  fCultures.OnChange := CulturesChanged;
  fDisplayName := cnEnglishDisplayName;
end;

destructor TCustomCultureCheckListBox.Destroy;
begin
  fCultures.OnChange := nil;
  Localizer := nil;
  fCultures.Free;
  inherited Destroy;
end;

procedure TCustomCultureCheckListBox.CulturesChanged(Sender: TObject);
begin
  if ItemsReady then
  begin
    fCultures.AssignLabelsTo(inherited Items, DisplayName);
    DoItemsChange;
  end;
end;

procedure TCustomCultureCheckListBox.LocalizerNotify(Sender: TLocalizer;
  Reason: TLocalizerNotification);
begin
  if Reason = lnCatalogChanged then
  begin
    if Assigned(Localizer) then
      fCultures.Assign(Localizer.Cultures)
    else
      fCultures.Clear;
  end;
end;

procedure TCustomCultureCheckListBox.SetLocalizer(Value: TLocalizer);
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
      LocalizerNotify(Localizer, lnCatalogChanged);
  end;
end;

procedure TCustomCultureCheckListBox.SetCultures(Value: TCultureList);
begin
  fCultures.Assign(Value);
end;

procedure TCustomCultureCheckListBox.SetItemSelected(Value: TCultureInfo);
begin
  ItemIndex := fCultures.IndexOf(Value);
end;

function TCustomCultureCheckListBox.GetItemSelected: TCultureInfo;
begin
  if Cardinal(ItemIndex) < Cardinal(fCultures.Count) then
    Result := fCultures[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomCultureCheckListBox.SetDisplayName(Value: TCultureDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    RebuildInternalItems;
  end;
end;

function TCustomCultureCheckListBox.GetImageIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fCultures.Count) and Assigned(Flags) then
    Result := Flags.ImageIndexOf(fCultures[Index])
  else
    Result := -1;
end;

function TCustomCultureCheckListBox.IsManagedByLocalizer: Boolean;
begin
  Result := Assigned(Localizer);
end;

procedure TCustomCultureCheckListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Localizer) then
    Localizer := nil;
end;

function TCustomCultureCheckListBox.CollectAll: Boolean;
begin
  Result := False;
  if not Assigned(Localizer) then
  begin
    fCultures.Assign(World.Cultures);
    Result := True;
  end
end;

procedure TCustomCultureCheckListBox.RebuildInternalItems;
var
  Sel: TCultureInfo;
  RestorePoint: TCheckListRecall;
begin
  fCultures.OnChange := nil;
  try
    RestorePoint := TCheckListRecall.Create(Self);
    try
      Sel := ItemSelected;
      if Self.Sorted then
        fCultures.Sort(DisplayName);
      if WindowHandle <> 0 then
      begin
        fCultures.AssignLabelsTo(inherited Items, DisplayName);
        DoItemsChange;
      end;
      ItemSelected := Sel;
      Assign(RestorePoint);
    finally
      RestorePoint.Free;
    end;
  finally
    fCultures.OnChange := CulturesChanged;
  end;
end;

procedure TCustomCultureCheckListBox.GetSelection(Dest: TCultureList;
  AllowDisabled: Boolean);
var
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for I := 0 to Count - 1 do
      if Checked[I] and (AllowDisabled or ItemEnabled[I]) then
        Dest.Add(Items[I]);
  finally
    Dest.EndUpdate;
  end;
end;

procedure TCustomCultureCheckListBox.SetSelection(Source: TReadonlyCultureList;
  AllowDisabled: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AllowDisabled or ItemEnabled[I] then
      Checked[I] := Source.Exists(Items[I]);
end;

{ TCustomTerritoryLabel }

constructor TCustomTerritoryLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDisplayName := tnFriendlyName;
end;

procedure TCustomTerritoryLabel.ReadTerritory(Reader: TReader);
begin
  Territory := TerritoryOf(Reader.ReadString);
end;

procedure TCustomTerritoryLabel.WriteTerritory(Writer: TWriter);
begin
  Writer.WriteString(Territory.Code2);
end;

procedure TCustomTerritoryLabel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('TerritoryInfo', ReadTerritory, WriteTerritory, Assigned(Territory));
end;

function TCustomTerritoryLabel.GetImageIndex: TImageIndex;
begin
  Result := Flags.ImageIndexOf(Territory);
end;

function TCustomTerritoryLabel.GetLabelText: String;
begin
  if (DisplayName = tnNone) or not Assigned(Territory) then
    Result := Caption
  else
    Result := Territory.DisplayNames[DisplayName];
end;

procedure TCustomTerritoryLabel.SetDisplayName(Value: TTerritoryDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomTerritoryLabel.SetTerritory(Value: TTerritoryInfo);
begin
  if Territory <> Value then
  begin
    fTerritory := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

{ TCustomTerritoryBox }

constructor TCustomTerritoryBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTerritories := TTerritoryList.Create;
  fTerritories.OnChange := TerritoriesChanged;
  fDisplayName := tnFriendlyName;
end;

destructor TCustomTerritoryBox.Destroy;
begin
  fTerritories.OnChange := nil;
  fTerritories.Free;
  inherited Destroy;
end;

procedure TCustomTerritoryBox.TerritoriesChanged(Sender: TObject);
begin
  if ItemsReady then
  begin
    fTerritories.AssignLabelsTo(inherited Items, DisplayName);
    DoItemsChange;
  end;
end;

procedure TCustomTerritoryBox.ReadItemSelected(Reader: TReader);
begin
  ItemSelected := TerritoryOf(Reader.ReadString);
end;

procedure TCustomTerritoryBox.WriteItemSelected(Writer: TWriter);
begin
  Writer.WriteString(ItemSelected.Code2);
end;

procedure TCustomTerritoryBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Selected', ReadItemSelected, WriteItemSelected, Assigned(ItemSelected));
end;

procedure TCustomTerritoryBox.SetTerritories(Value: TTerritoryList);
begin
  fTerritories.Assign(Value);
end;

procedure TCustomTerritoryBox.SetItemSelected(Value: TTerritoryInfo);
begin
  ItemIndex := fTerritories.IndexOf(Value);
end;

function TCustomTerritoryBox.GetItemSelected: TTerritoryInfo;
begin
  if Cardinal(ItemIndex) < Cardinal(fTerritories.Count) then
    Result := fTerritories[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomTerritoryBox.SetDisplayName(Value: TTerritoryDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    RebuildInternalItems;
  end;
end;

function TCustomTerritoryBox.GetImageIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fTerritories.Count) and Assigned(Flags) then
    Result := Flags.ImageIndexOf(fTerritories[Index])
  else
    Result := -1;
end;

function TCustomTerritoryBox.CollectAll: Boolean;
begin
  fTerritories.Assign(World.Territories);
  Result := True;
end;

procedure TCustomTerritoryBox.RebuildInternalItems;
var
  Sel: TTerritoryInfo;
begin
  fTerritories.OnChange := nil;
  try
    Sel := ItemSelected;
    if Sorted then
      fTerritories.Sort(DisplayName);
    if WindowHandle <> 0 then
    begin
      fTerritories.AssignLabelsTo(inherited Items, DisplayName);
      DoItemsChange;
    end;
    ItemSelected := Sel;
  finally
    fTerritories.OnChange := TerritoriesChanged;
  end;
end;

{ TCustomTerritoryListBox }

constructor TCustomTerritoryListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTerritories := TTerritoryList.Create;
  fTerritories.OnChange := TerritoriesChanged;
  fDisplayName := tnFriendlyName;
end;

destructor TCustomTerritoryListBox.Destroy;
begin
  fTerritories.OnChange := nil;
  fTerritories.Free;
  inherited Destroy;
end;

procedure TCustomTerritoryListBox.TerritoriesChanged(Sender: TObject);
begin
  if ItemsReady then
  begin
    fTerritories.AssignLabelsTo(inherited Items, DisplayName);
    DoItemsChange;
  end;
end;

procedure TCustomTerritoryListBox.ReadItemSelected(Reader: TReader);
begin
  ItemSelected := TerritoryOf(Reader.ReadString);
end;

procedure TCustomTerritoryListBox.WriteItemSelected(Writer: TWriter);
begin
  Writer.WriteString(ItemSelected.Code2);
end;

procedure TCustomTerritoryListBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Selected', ReadItemSelected, WriteItemSelected, Assigned(ItemSelected));
end;
procedure TCustomTerritoryListBox.SetTerritories(Value: TTerritoryList);
begin
  fTerritories.Assign(Value);
end;

procedure TCustomTerritoryListBox.SetItemSelected(Value: TTerritoryInfo);
begin
  ItemIndex := fTerritories.IndexOf(Value);
end;

function TCustomTerritoryListBox.GetItemSelected: TTerritoryInfo;
begin
  if Cardinal(ItemIndex) < Cardinal(fTerritories.Count) then
    Result := fTerritories[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomTerritoryListBox.SetDisplayName(Value: TTerritoryDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    RebuildInternalItems;
  end;
end;

function TCustomTerritoryListBox.GetImageIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fTerritories.Count) and Assigned(Flags) then
    Result := Flags.ImageIndexOf(fTerritories[Index])
  else
    Result := -1;
end;

function TCustomTerritoryListBox.CollectAll: Boolean;
begin
  fTerritories.Assign(World.Territories);
  Result := True;
end;

procedure TCustomTerritoryListBox.RebuildInternalItems;
var
  Sel: TTerritoryInfo;
begin
  fTerritories.OnChange := nil;
  try
    Sel := ItemSelected;
    if Sorted then
      fTerritories.Sort(DisplayName);
    if WindowHandle <> 0 then
    begin
      fTerritories.AssignLabelsTo(inherited Items, DisplayName);
      DoItemsChange;
    end;
    ItemSelected := Sel;
  finally
    fTerritories.OnChange := TerritoriesChanged;
  end;
end;

procedure TCustomTerritoryListBox.SetSelection(Source: TReadonlyTerritoryList);
var
  I: Integer;
begin
  if MultiSelect then
    for I := 0 to fTerritories.Count - 1 do
      Selected[I] := Source.Exists(fTerritories[I]);
end;

procedure TCustomTerritoryListBox.GetSelection(Dest: TTerritoryList);
var
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for I := 0 to fTerritories.Count - 1 do
      if Selected[I] then
        Dest.Add(fTerritories[I]);
  finally
    Dest.EndUpdate;
  end;
end;

{ TCustomTerritoryCheckListBox }

constructor TCustomTerritoryCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTerritories := TTerritoryList.Create;
  fTerritories.OnChange := TerritoriesChanged;
  fDisplayName := tnFriendlyName;
end;

destructor TCustomTerritoryCheckListBox.Destroy;
begin
  fTerritories.OnChange := nil;
  fTerritories.Free;
  inherited Destroy;
end;

procedure TCustomTerritoryCheckListBox.TerritoriesChanged(Sender: TObject);
begin
  if ItemsReady then
  begin
    fTerritories.AssignLabelsTo(inherited Items, DisplayName);
    DoItemsChange;
  end;
end;

procedure TCustomTerritoryCheckListBox.SetTerritories(Value: TTerritoryList);
begin
  fTerritories.Assign(Value);
end;

procedure TCustomTerritoryCheckListBox.SetItemSelected(Value: TTerritoryInfo);
begin
  ItemIndex := fTerritories.IndexOf(Value);
end;

function TCustomTerritoryCheckListBox.GetItemSelected: TTerritoryInfo;
begin
  if Cardinal(ItemIndex) < Cardinal(fTerritories.Count) then
    Result := fTerritories[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomTerritoryCheckListBox.SetDisplayName(Value: TTerritoryDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    RebuildInternalItems;
  end;
end;

function TCustomTerritoryCheckListBox.GetImageIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fTerritories.Count) and Assigned(Flags) then
    Result := Flags.ImageIndexOf(fTerritories[Index])
  else
    Result := -1;
end;

function TCustomTerritoryCheckListBox.CollectAll: Boolean;
begin
  fTerritories.Assign(World.Territories);
  Result := True;
end;

procedure TCustomTerritoryCheckListBox.RebuildInternalItems;
var
  Sel: TTerritoryInfo;
  RestorePoint: TCheckListRecall;
begin
  fTerritories.OnChange := nil;
  try
    RestorePoint := TCheckListRecall.Create(Self);
    try
      Sel := ItemSelected;
      if Sorted then
        fTerritories.Sort(DisplayName);
      if WindowHandle <> 0 then
      begin
        fTerritories.AssignLabelsTo(inherited Items, DisplayName);
        DoItemsChange;
      end;
      ItemSelected := Sel;
    finally
      RestorePoint.Free;
    end;
  finally
    fTerritories.OnChange := TerritoriesChanged;
  end;
end;

procedure TCustomTerritoryCheckListBox.GetSelection(Dest: TTerritoryList;
  AllowDisabled: Boolean);
var
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for I := 0 to Count - 1 do
      if Checked[I] and (AllowDisabled or ItemEnabled[I]) then
        Dest.Add(Items[I]);
  finally
    Dest.EndUpdate;
  end;
end;

procedure TCustomTerritoryCheckListBox.SetSelection(Source: TReadonlyTerritoryList;
  AllowDisabled: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AllowDisabled or ItemEnabled[I] then
      Checked[I] := Source.Exists(Items[I]);
end;

{ TCustomCurrencyLabel }

constructor TCustomCurrencyLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDisplayName := crnEnglishName;
end;

procedure TCustomCurrencyLabel.ReadCurrency(Reader: TReader);
begin
  Currency := CurrencyOf(Reader.ReadString);
end;

procedure TCustomCurrencyLabel.WriteCurrency(Writer: TWriter);
begin
  Writer.WriteString(Currency.IntlSymbol);
end;

procedure TCustomCurrencyLabel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('CurrencyInfo', ReadCurrency, WriteCurrency, Assigned(Currency));
end;

function TCustomCurrencyLabel.GetImageIndex: TImageIndex;
begin
  Result := Flags.ImageIndexOf(Currency);
end;

function TCustomCurrencyLabel.GetLabelText: String;
begin
  if (DisplayName = crnNone) or not Assigned(Currency) then
    Result := Caption
  else
    Result := Currency.DisplayNames[DisplayName];
end;

procedure TCustomCurrencyLabel.SetDisplayName(Value: TCurrencyDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomCurrencyLabel.SetCurrency(Value: TCurrencyInfo);
begin
  if Currency <> Value then
  begin
    fCurrency := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

{ TCustomCurrencyBox }

constructor TCustomCurrencyBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCurrencies := TCurrencyList.Create;
  fCurrencies.OnChange := CurrenciesChanged;
  fDisplayName := crnEnglishName;
end;

destructor TCustomCurrencyBox.Destroy;
begin
  fCurrencies.OnChange := nil;
  fCurrencies.Free;
  inherited Destroy;
end;

procedure TCustomCurrencyBox.CurrenciesChanged(Sender: TObject);
begin
  if ItemsReady then
  begin
    fCurrencies.AssignLabelsTo(inherited Items, DisplayName);
    DoItemsChange;
  end;
end;

procedure TCustomCurrencyBox.ReadItemSelected(Reader: TReader);
begin
  ItemSelected := CurrencyOf(Reader.ReadString);
end;

procedure TCustomCurrencyBox.WriteItemSelected(Writer: TWriter);
begin
  Writer.WriteString(ItemSelected.IntlSymbol);
end;

procedure TCustomCurrencyBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Selected', ReadItemSelected, WriteItemSelected, Assigned(ItemSelected));
end;

procedure TCustomCurrencyBox.SetCurrencies(Value: TCurrencyList);
begin
  fCurrencies.Assign(Value);
end;

procedure TCustomCurrencyBox.SetItemSelected(Value: TCurrencyInfo);
begin
  ItemIndex := fCurrencies.IndexOf(Value);
end;

function TCustomCurrencyBox.GetItemSelected: TCurrencyInfo;
begin
  if Cardinal(ItemIndex) < Cardinal(fCurrencies.Count) then
    Result := fCurrencies[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomCurrencyBox.SetDisplayName(Value: TCurrencyDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    RebuildInternalItems;
  end;
end;

function TCustomCurrencyBox.GetImageIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fCurrencies.Count) and Assigned(Flags) then
    Result := Flags.ImageIndexOf(fCurrencies[Index])
  else
    Result := -1;
end;

function TCustomCurrencyBox.CollectAll: Boolean;
begin
  fCurrencies.Assign(World.Currencies);
  Result := True;
end;

procedure TCustomCurrencyBox.RebuildInternalItems;
var
  Sel: TCurrencyInfo;
begin
  fCurrencies.OnChange := nil;
  try
    Sel := ItemSelected;
    if Sorted then
      fCurrencies.Sort(DisplayName);
    if WindowHandle <> 0 then
    begin
      fCurrencies.AssignLabelsTo(inherited Items, DisplayName);
      DoItemsChange;
    end;
    ItemSelected := Sel;
  finally
    fCurrencies.OnChange := CurrenciesChanged;
  end;
end;

{ TCustomCurrencyListBox }

constructor TCustomCurrencyListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCurrencies := TCurrencyList.Create;
  fCurrencies.OnChange := CurrenciesChanged;
  fDisplayName := crnEnglishName;
end;

destructor TCustomCurrencyListBox.Destroy;
begin
  fCurrencies.OnChange := nil;
  fCurrencies.Free;
  inherited Destroy;
end;

procedure TCustomCurrencyListBox.CurrenciesChanged(Sender: TObject);
begin
  if ItemsReady then
  begin
    fCurrencies.AssignLabelsTo(inherited Items, DisplayName);
    DoItemsChange;
  end;
end;

procedure TCustomCurrencyListBox.ReadItemSelected(Reader: TReader);
begin
  ItemSelected := CurrencyOf(Reader.ReadString);
end;

procedure TCustomCurrencyListBox.WriteItemSelected(Writer: TWriter);
begin
  Writer.WriteString(ItemSelected.IntlSymbol);
end;

procedure TCustomCurrencyListBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Selected', ReadItemSelected, WriteItemSelected, Assigned(ItemSelected));
end;

procedure TCustomCurrencyListBox.SetCurrencies(Value: TCurrencyList);
begin
  fCurrencies.Assign(Value);
end;

procedure TCustomCurrencyListBox.SetItemSelected(Value: TCurrencyInfo);
begin
  ItemIndex := fCurrencies.IndexOf(Value);
end;

function TCustomCurrencyListBox.GetItemSelected: TCurrencyInfo;
begin
  if Cardinal(ItemIndex) < Cardinal(fCurrencies.Count) then
    Result := fCurrencies[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomCurrencyListBox.SetDisplayName(Value: TCurrencyDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    RebuildInternalItems;
  end;
end;

function TCustomCurrencyListBox.GetImageIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fCurrencies.Count) and Assigned(Flags) then
    Result := Flags.ImageIndexOf(fCurrencies[Index])
  else
    Result := -1;
end;

function TCustomCurrencyListBox.CollectAll: Boolean;
begin
  fCurrencies.Assign(World.Currencies);
  Result := True;
end;

procedure TCustomCurrencyListBox.RebuildInternalItems;
var
  Sel: TCurrencyInfo;
begin
  fCurrencies.OnChange := nil;
  try
    Sel := ItemSelected;
    if Sorted then
      fCurrencies.Sort(DisplayName);
    if WindowHandle <> 0 then
    begin
      fCurrencies.AssignLabelsTo(inherited Items, DisplayName);
      DoItemsChange;
    end;
    ItemSelected := Sel;
  finally
    fCurrencies.OnChange := CurrenciesChanged;
  end;
end;

procedure TCustomCurrencyListBox.SetSelection(Source: TReadonlyCurrencyList);
var
  I: Integer;
begin
  if MultiSelect then
    for I := 0 to fCurrencies.Count - 1 do
      Selected[I] := Source.Exists(fCurrencies[I]);
end;

procedure TCustomCurrencyListBox.GetSelection(Dest: TCurrencyList);
var
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for I := 0 to fCurrencies.Count - 1 do
      if Selected[I] then
        Dest.Add(fCurrencies[I]);
  finally
    Dest.EndUpdate;
  end;
end;

{ TCustomCurrencyCheckListBox }

constructor TCustomCurrencyCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCurrencies := TCurrencyList.Create;
  fCurrencies.OnChange := CurrenciesChanged;
  fDisplayName := crnEnglishName;
end;

destructor TCustomCurrencyCheckListBox.Destroy;
begin
  fCurrencies.OnChange := nil;
  fCurrencies.Free;
  inherited Destroy;
end;

procedure TCustomCurrencyCheckListBox.CurrenciesChanged(Sender: TObject);
begin
  if ItemsReady then
  begin
    fCurrencies.AssignLabelsTo(inherited Items, DisplayName);
    DoItemsChange;
  end;
end;

procedure TCustomCurrencyCheckListBox.SetCurrencies(Value: TCurrencyList);
begin
  fCurrencies.Assign(Value);
end;

procedure TCustomCurrencyCheckListBox.SetItemSelected(Value: TCurrencyInfo);
begin
  ItemIndex := fCurrencies.IndexOf(Value);
end;

function TCustomCurrencyCheckListBox.GetItemSelected: TCurrencyInfo;
begin
  if Cardinal(ItemIndex) < Cardinal(fCurrencies.Count) then
    Result := fCurrencies[ItemIndex]
  else
    Result := nil;
end;

procedure TCustomCurrencyCheckListBox.SetDisplayName(Value: TCurrencyDisplayName);
begin
  if DisplayName <> Value then
  begin
    fDisplayName := Value;
    RebuildInternalItems;
  end;
end;

function TCustomCurrencyCheckListBox.GetImageIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fCurrencies.Count) and Assigned(Flags) then
    Result := Flags.ImageIndexOf(fCurrencies[Index])
  else
    Result := -1;
end;

function TCustomCurrencyCheckListBox.CollectAll: Boolean;
begin
  fCurrencies.Assign(World.Currencies);
  Result := True;
end;

procedure TCustomCurrencyCheckListBox.RebuildInternalItems;
var
  Sel: TCurrencyInfo;
  RestorePoint: TCheckListRecall;
begin
  fCurrencies.OnChange := nil;
  try
    RestorePoint := TCheckListRecall.Create(Self);
    try
      Sel := ItemSelected;
      if Sorted then
        fCurrencies.Sort(DisplayName);
      if WindowHandle <> 0 then
      begin
        fCurrencies.AssignLabelsTo(inherited Items, DisplayName);
        DoItemsChange;
      end;
      ItemSelected := Sel;
    finally
      RestorePoint.Free;
    end;
  finally
    fCurrencies.OnChange := CurrenciesChanged;
  end;
end;

procedure TCustomCurrencyCheckListBox.GetSelection(Dest: TCurrencyList;
  AllowDisabled: Boolean);
var
  I: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for I := 0 to Count - 1 do
      if Checked[I] and (AllowDisabled or ItemEnabled[I]) then
        Dest.Add(Items[I]);
  finally
    Dest.EndUpdate;
  end;
end;

procedure TCustomCurrencyCheckListBox.SetSelection(Source: TReadonlyCurrencyList;
  AllowDisabled: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AllowDisabled or ItemEnabled[I] then
      Checked[I] := Source.Exists(Items[I]);
end;

initialization
  CheckBoxSize := CalcCheckBoxSize;
  DropDownSize := CalcDropDownSize;
end.
