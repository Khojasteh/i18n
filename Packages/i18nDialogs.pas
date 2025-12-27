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
/// This unit implements some common localizable dialogs.
/// </summary>
/// <remarks>
/// TODO: Font dialog, Color dialog, Find and replace dialog, Print dialog,
/// Page setup dialog, Open dialog, Save dialog.
/// </remarks>
unit i18nDialogs;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Classes, Graphics, Dialogs, Controls, Forms, StdCtrls, Menus;

type

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDlgBtnCaptions is the ancestor for all objects that provide
  /// localizable captions for common dialog buttons.
  /// </summary>
  {$endregion}
  TCustomDlgBtnCaptions = class(TPersistent)
  private
    fCaptions: array [TMsgDlgBtn] of String;
    function GetCaptions(DlgBtn: TMsgDlgBtn): String; inline;
    procedure SetCaptions(DlgBtn: TMsgDlgBtn; const Value: String); inline;
    function IsStoredCaptions(DlgBtn: TMsgDlgBtn): Boolean;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the contents from a source object.
    /// </summary>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates dimensions of a set of buttons that are arranged side by side.
    /// </summary>
    /// <param name="DC">
    /// The device context.
    /// </param>
    /// <param name="Buttons">
    /// The set of buttons.
    /// </param>
    /// <param name="Width">
    /// The calculated width of the buttons on the specified device context.
    /// </param>
    /// <param name="Height">
    /// The calculated height of the buttons on the specified device context.
    /// </param>
    /// <returns>
    /// The number of buttons.
    /// </returns>
    {$endregion}
    function GetMetrics(DC: HDC; Buttons: TMsgDlgButtons; var Width, Height: Integer): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists captions of the buttons.
    /// </summary>
    {$endregion}
    property Captions[DlgBtn: TMsgDlgBtn]: String read GetCaptions write SetCaptions; default;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Yes" button.
    /// </summary>
    {$endregion}
    property Yes: String index mbYes read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "No" button.
    /// </summary>
    {$endregion}
    property No: String index mbNo read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "OK" button.
    /// </summary>
    {$endregion}
    property OK: String index mbOK read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Cancel" button.
    /// </summary>
    {$endregion}
    property Cancel: String index mbCancel read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Abort" button.
    /// </summary>
    {$endregion}
    property Abort: String index mbAbort read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Retry" button.
    /// </summary>
    {$endregion}
    property Retry: String index mbRetry read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Ignore" button.
    /// </summary>
    {$endregion}
    property Ignore: String index mbIgnore read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "All" button.
    /// </summary>
    {$endregion}
    property All: String index mbAll read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "No to all" button.
    /// </summary>
    {$endregion}
    property NoToAll: String index mbNoToAll read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Yes to all" button.
    /// </summary>
    {$endregion}
    property YesToAll: String index mbYesToAll read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Help" button.
    /// </summary>
    {$endregion}
    property Help: String index mbHelp read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Close" button.
    /// </summary>
    {$endregion}
    property Close: String index mbClose read GetCaptions write SetCaptions stored IsStoredCaptions;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCustomDlgCheckBox is the ancestor for all objects that represent a check
  /// box control as a property.
  /// </summary>
  /// <remarks>
  /// Use TCustomDlgCheckBox as a base class when defining a new object that
  /// represents a check box as a published property of another object.
  /// </remarks>
  {$endregion}
  TCustomDlgCheckBox = class(TPersistent)
  private
    fCaption: String;
    fHint: String;
    fAllowGrayed: Boolean;
    fState: TCheckBoxState;
    fEnabled: Boolean;
    fVisible: Boolean;
    fControl: TCheckBox;
    procedure SetCaption(const Value: String);
    procedure SetHint(const Value: String);
    procedure SetAllowGrayed(Value: Boolean);
    procedure SetState(Value: TCheckBoxState);
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetControl(Value: TCheckBox);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the check box control that will be managed by the
    /// TCustomDlgCheckBox object.
    /// </summary>
    {$endregion}
    property Control: TCheckBox read fControl write SetControl;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box can be in a grayed state.
    /// </summary>
    {$endregion}
    property AllowGrayed: Boolean read fAllowGrayed write SetAllowGrayed default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets a text string that identifies the check box control to the user.
    /// </summary>
    {$endregion}
    property Caption: String read fCaption write SetCaption;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box is selected.
    /// </summary>
    {$endregion}
    property Checked: Boolean read GetChecked write SetChecked stored False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box control responds to keyboard, mouse, and timer events.
    /// </summary>
    {$endregion}
    property Enabled: Boolean read fEnabled write SetEnabled default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the text string that can appear when the user moves the mouse over
    /// the check box control.
    /// </summary>
    {$endregion}
    property Hint: String read fHint write SetHint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box is selected, deselected, or grayed.
    /// </summary>
    {$endregion}
    property State: TCheckBoxState read fState write SetState default cbUnchecked;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box control appears onscreen.
    /// </summary>
    {$endregion}
    property Visible: Boolean read fVisible write SetVisible default False;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the settings from another check box.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    /// <seealso cref="AssignTo"/>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the settings to another check box.
    /// </summary>
    /// <param name="Dest">
    /// The destination object.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure AssignTo(Dest: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates dimensions of the check box on a specified device context.
    /// </summary>
    /// <param name="DC">
    /// The device context.
    /// </param>
    /// <param name="MaxWidth">
    /// The maximum width that the check box can occupy on the specified device context.
    /// </param>
    /// <returns>
    /// The width and height of the check box on the specified device context.
    /// </returns>
    {$endregion}
    function GetMetrics(DC: HDC; MaxWidth: Integer): TSize;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TMsgDlgTypeIcons stores the icons that can appear on a message dialog.
  /// </summary>
  {$endregion}
  TMsgDlgTypeIcons = class(TPersistent)
  private
    fIcons: array [TMsgDlgType] of TIcon;
    function GetIcons(DlgType: TMsgDlgType): TIcon; inline;
    procedure SetIcons(DlgType: TMsgDlgType; Value: TIcon);
    function IsStoredIcons(DlgType: TMsgDlgType): Boolean;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the icons from another object.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the available icons for a message dialog.
    /// </summary>
    {$endregion}
    property Icons[DlgType: TMsgDlgType]: TIcon read GetIcons write SetIcons; default;
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the icon for a warning message dialog.
    /// </summary>
    {$endregion}
    property Warning: TIcon index mtWarning read GetIcons write SetIcons stored IsStoredIcons;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the icon for an error message dialog.
    /// </summary>
    {$endregion}
    property Error: TIcon index mtError read GetIcons write SetIcons stored IsStoredIcons;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the icon for an informative message dialog.
    /// </summary>
    {$endregion}
    property Information: TIcon index mtInformation read GetIcons write SetIcons stored IsStoredIcons;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the icon for an inquiry message dialog.
    /// </summary>
    {$endregion}
    property Confirmation: TIcon index mtConfirmation read GetIcons write SetIcons stored IsStoredIcons;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the icon for a custom message dialog.
    /// </summary>
    {$endregion}
    property Custom: TIcon index mtCustom read GetIcons write SetIcons stored IsStoredIcons;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TMsgDlgTypeCaptions stores the titles that a message dialog can have.
  /// </summary>
  {$endregion}
  TMsgDlgTypeCaptions = class(TPersistent)
  private
    fCaptions: array [TMsgDlgType] of String;
    function GetCaptions(DlgType: TMsgDlgType): String; inline;
    procedure SetCaptions(DlgType: TMsgDlgType; const Value: String); inline;
    function IsStoredCaptions(DlgType: TMsgDlgType): Boolean;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the titles from another object.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the available titles for a message dialog.
    /// </summary>
    {$endregion}
    property Captions[DlgType: TMsgDlgType]: String read GetCaptions write SetCaptions; default;
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the title for a warning message dialog.
    /// </summary>
    {$endregion}
    property Warning: String index mtWarning read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the title for an error message dialog.
    /// </summary>
    {$endregion}
    property Error: String index mtError read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the title for an informative message dialog.
    /// </summary>
    {$endregion}
    property Information: String index mtInformation read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the title for an inquiry message dialog.
    /// </summary>
    {$endregion}
    property Confirmation: String index mtConfirmation read GetCaptions write SetCaptions stored IsStoredCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the title for a custom message dialog.
    /// </summary>
    {$endregion}
    property Custom: String index mtCustom read GetCaptions write SetCaptions stored IsStoredCaptions;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TMsgDlgBtnCaptions stores the caption of buttons for a message dialog.
  /// </summary>
  {$endregion}
  TMsgDlgBtnCaptions = class(TCustomDlgBtnCaptions)
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Yes" button.
    /// </summary>
    {$endregion}
    property Yes;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "No" button.
    /// </summary>
    {$endregion}
    property No;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "OK" button.
    /// </summary>
    {$endregion}
    property OK;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Cancel" button.
    /// </summary>
    {$endregion}
    property Cancel;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Abort" button.
    /// </summary>
    {$endregion}
    property Abort;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Retry" button.
    /// </summary>
    {$endregion}
    property Retry;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Ignore" button.
    /// </summary>
    {$endregion}
    property Ignore;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "All" button.
    /// </summary>
    {$endregion}
    property All;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "No to all" button.
    /// </summary>
    {$endregion}
    property NoToAll;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Yes to all" button.
    /// </summary>
    {$endregion}
    property YesToAll;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Help" button.
    /// </summary>
    {$endregion}
    property Help;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Close" button.
    /// </summary>
    {$endregion}
    property Close;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TMsgDlgCheckBox represents a check box that can appear on a message dialog.
  /// </summary>
  {$endregion}
  TMsgDlgCheckBox = class(TCustomDlgCheckBox)
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box can be in a grayed state.
    /// </summary>
    {$endregion}
    property AllowGrayed;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets a text string that identifies the check box control to the user.
    /// </summary>
    {$endregion}
    property Caption;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box is selected.
    /// </summary>
    {$endregion}
    property Checked;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box control responds to keyboard, mouse, and timer events.
    /// </summary>
    {$endregion}
    property Enabled;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the text string that can appear when the user moves the mouse over
    /// the check box control.
    /// </summary>
    {$endregion}
    property Hint;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box is selected, deselected, or grayed.
    /// </summary>
    {$endregion}
    property State;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the check box control appears onscreen.
    /// </summary>
    {$endregion}
    property Visible;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TMessageDialog displays a localizable message dialog.
  /// </summary>
  /// <remarks>
  /// <para>
  /// Use TMessageDialog to display a modal Windows dialog box for displaying a message,
  /// and getting the user response. The dialog does not appear at runtime until it is
  /// activated by a call to one of its show methods.
  /// </para>
  /// <para>
  /// TMessageDialog has properties and methods to facilitate customization of the dialog
  /// and displaying messages.
  /// </para>
  /// </remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TMessageDialog = class(TComponent)
  private
    fAutoHotKeys: Boolean;
    fIcons: TMsgDlgTypeIcons;
    fCaptions: TMsgDlgTypeCaptions;
    fButtonCaptions: TMsgDlgBtnCaptions;
    fDialog: TForm;
    fFont: TFont;
    fBiDiMode: TBiDiMode;
    fParentFont: Boolean;
    fParentBiDiMode: Boolean;
    fCallbackTimer: Boolean;
    fCheckBox: TMsgDlgCheckBox;
    fShowTicks: Cardinal;
    fOnShow: TNotifyEvent;
    fOnClose: TNotifyEvent;
    fOnCallbackTimer: TNotifyEvent;
    procedure SetCaptions(Value: TMsgDlgTypeCaptions);
    procedure SetIcons(Value: TMsgDlgTypeIcons);
    procedure SetButtonCaptions(Value: TMsgDlgBtnCaptions);
    procedure SetFont(Value: TFont);
    function GetBiDiMode: TBiDiMode;
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetParentFont(Value: Boolean);
    procedure SetParentBiDiMode(Value: Boolean);
    procedure SetCheckBox(Value: TMsgDlgCheckBox);
    function GetShowDuration: Cardinal;
    function IsStoredFont: Boolean;
    function IsStoredBiDiMode: Boolean;
    procedure FontChange(Sender: TObject);
    procedure DialogShow(Sender: TObject);
    procedure DialogClose(Sender: TObject; var Action: TCloseAction);
    procedure DialogKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HelpButtonClick(Sender: TObject);
    procedure CheckBoxClicked(Sender: TObject);
    procedure CallbackTimerTrigger(Sender: TObject);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnShow"/> event.
    /// </summary>
    {$endregion}
    procedure DoShow; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnClose"/> event.
    /// </summary>
    {$endregion}
    procedure DoClose; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnCallbackTimer"/> event.
    /// </summary>
    {$endregion}
    procedure DoCallbackTimer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the content of the visible dialog as a string. GetDialogText is
    /// called when the user presses Ctrl+C keys to put the dialog's text in the
    /// Windows clipboard.
    /// </summary>
    /// <returns>
    /// Returns a string that represents the title, message, and buttons of the dialog.
    /// </returns>
    {$endregion}
    function GetDialogText: String; virtual;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the component.
    /// </summary>
    /// <param name="AOwner">
    /// The component that owns this component.
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
    /// Brings up a message dialog and obtains the user's response.
    /// </summary>
    /// <param name="Msg">
    /// The content of the message that appears on the dialog.
    /// </param>
    /// <param name="DlgType">
    /// Determines the purpose of the message.
    /// </param>
    /// <param name="Buttons">
    /// Specifies what buttons should appear on the dialog.
    /// </param>
    /// <param name="DefaultButton">
    /// Specifies which button from among those specified buttons is the default button
    /// for the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns the value of the button the user selected.
    /// </returns>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Inform"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="YesNoAllCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    function Show(const Msg: String; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn;
      HelpContext: THelpContext = 0): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Brings up a message dialog and obtains the user's response.
    /// </summary>
    /// <param name="Fmt">
    /// The format string that assembles the content of the message that appears
    /// on the dialog.
    /// </param>
    /// <param name="Args">
    /// The array of arguments to apply to the format specifiers in <paramref name="Fmt"/>.
    /// </param>
    /// <param name="DlgType">
    /// Determines the purpose of the message.
    /// </param>
    /// <param name="Buttons">
    /// Specifies what buttons should appear on the dialog.
    /// </param>
    /// <param name="DefaultButton">
    /// Specifies which button from among those specified buttons is the default button
    /// for the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns the value of the button the user selected.
    /// </returns>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Inform"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="YesNoAllCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    function Show(const Fmt: String; const Args: array of const;
      DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn;
      HelpContext: THelpContext = 0): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Brings up a message dialog and obtains the user's response.
    /// </summary>
    /// <param name="Msg">
    /// The content of the message that appears on the dialog.
    /// </param>
    /// <param name="DlgType">
    /// Determines the purpose of the message.
    /// </param>
    /// <param name="Buttons">
    /// Specifies what buttons should appear on the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns the value of the button the user selected.
    /// </returns>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Inform"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="YesNoAllCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    function Show(const Msg: String; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; HelpContext: THelpContext = 0): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Brings up a message dialog and obtains the user's response.
    /// </summary>
    /// <param name="Fmt">
    /// The format string that assembles the content of the message that appears
    /// on the dialog.
    /// </param>
    /// <param name="Args">
    /// The array of arguments to apply to the format specifiers in <paramref name="Fmt"/>.
    /// </param>
    /// <param name="DlgType">
    /// Determines the purpose of the message.
    /// </param>
    /// <param name="Buttons">
    /// Specifies what buttons should appear on the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns the value of the button the user selected.
    /// </returns>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Inform"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="YesNoAllCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    function Show(const Fmt: String; const Args: array of const;
      DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
      HelpContext: THelpContext = 0): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Displays a message dialog with custom title and icon.
    /// </summary>
    /// <param name="Msg">
    /// The content of the message that appears on the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <seealso cref="Show"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Inform"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="YesNoAllCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    procedure ShowMessage(const Msg: String; HelpContext: THelpContext = 0);
    {$region 'xmldoc'}
    /// <summary>
    /// Displays a warning message dialog.
    /// </summary>
    /// <param name="Msg">
    /// The content of the message that appears on the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <seealso cref="Show"/>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="Inform"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="YesNoAllCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    procedure ShowWarning(const Msg: String; HelpContext: THelpContext = 0);
    {$region 'xmldoc'}
    /// <summary>
    /// Displays an error message dialog.
    /// </summary>
    /// <param name="Msg">
    /// The content of the message that appears on the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <seealso cref="Show"/>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="YesNoAllCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    procedure ShowError(const Msg: String; HelpContext: THelpContext = 0);
    {$region 'xmldoc'}
    /// <summary>
    /// Displays an informative message dialog.
    /// </summary>
    /// <param name="Msg">
    /// The content of the message that appears on the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <seealso cref="Show"/>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="YesNoAllCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    procedure Inform(const Msg: String; HelpContext: THelpContext = 0);
    {$region 'xmldoc'}
    /// <summary>
    /// Displays an inquiry message dialog with "Yes" and "No" buttons.
    /// </summary>
    /// <param name="Msg">
    /// The content of the message that appears on the dialog.
    /// </param>
    /// <param name="DefaultButton">
    /// Specifies which button is the default button for the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the user selects "Yes" button, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Show"/>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Inform"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="YesNoAllCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    function Confirm(const Msg: String; DefaultButton: TMsgDlgBtn = mbYes; HelpContext: THelpContext = 0): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Displays an inquiry message dialog with "Yes" and "No", and "Cancel" buttons.
    /// </summary>
    /// <param name="Msg">
    /// The content of the message that appears on the dialog.
    /// </param>
    /// <param name="DefaultButton">
    /// Specifies which button is the default button for the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns the value of the button the user selected.
    /// </returns>
    /// <seealso cref="Show"/>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Inform"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoAllCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    function YesNoCancel(const Msg: String; DefaultButton: TMsgDlgBtn = mbYes; HelpContext: THelpContext = 0): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Displays an inquiry message dialog with "Yes" and "No", "Yes to all", "No to all",
    /// and "Cancel" buttons.
    /// </summary>
    /// <param name="Msg">
    /// The content of the message that appears on the dialog.
    /// </param>
    /// <param name="DefaultButton">
    /// Specifies which button is the default button for the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns the value of the button the user selected.
    /// </returns>
    /// <seealso cref="Show"/>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Inform"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="RetryIgnoreAbort"/>
    {$endregion}
    function YesNoAllCancel(const Msg: String; DefaultButton: TMsgDlgBtn = mbNo; HelpContext: THelpContext = 0): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Displays a warning message dialog with "Retry" and "Ignore", and "Abort"
    /// buttons.
    /// </summary>
    /// <param name="Msg">
    /// The content of the message that appears on the dialog.
    /// </param>
    /// <param name="DefaultButton">
    /// Specifies which button is the default button for the dialog.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns the value of the button the user selected.
    /// </returns>
    /// <seealso cref="Show"/>
    /// <seealso cref="ShowMessage"/>
    /// <seealso cref="ShowWarning"/>
    /// <seealso cref="Inform"/>
    /// <seealso cref="Confirm"/>
    /// <seealso cref="YesNoCancel"/>
    /// <seealso cref="YesNoAllCancel"/>
    {$endregion}
    function RetryIgnoreAbort(const Msg: String; DefaultButton: TMsgDlgBtn = mbRetry; HelpContext: THelpContext = 0): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides direct access to the underlying form object while the dialog is
    /// displayed.
    /// </summary>
    {$endregion}
    property Dialog: TForm read fDialog;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of milliseconds that the dialog is displayed.
    /// </summary>
    {$endregion}
    property ShowDuration: Cardinal read GetShowDuration;
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the hot key of the dialog's buttons should be determined
    /// automatically.
    /// </summary>
    {$endregion}
    property AutoHotKeys: Boolean read fAutoHotKeys write fAutoHotKeys default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the bidirectional mode for the dialog.
    /// </summary>
    {$endregion}
    property BiDiMode: TBiDiMode read GetBiDiMode write SetBiDiMode stored IsStoredBiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the captions of the dialog buttons.
    /// </summary>
    {$endregion}
    property ButtonCaptions: TMsgDlgBtnCaptions read fButtonCaptions write SetButtonCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the component should generate the periodic <see cref="OnCallbackTimer"/>
    /// event while a dialog is displayed.
    /// </summary>
    /// <remarks>
    /// <para>
    /// If CallbackTimer is <see langword="true"/> and a message dialog is displayed, the
    /// component will generate an <see cref="OnCallbackTimer"/> event on each second.
    /// </para>
    /// <para>
    /// You can use this event to take a default action if the user did not respond to the
    /// dialog in a reasonable amount of time.
    /// </para>
    /// </remarks>
    /// <seealso cref="ShowDuration"/>
    {$endregion}
    property CallbackTimer: Boolean read fCallbackTimer write fCallbackTimer default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the titles of the dialog for all different types of messages.
    /// </summary>
    {$endregion}
    property Captions: TMsgDlgTypeCaptions read fCaptions write SetCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets properties of the check box that can appear on the dialog.
    /// </summary>
    {$endregion}
    property CheckBox: TMsgDlgCheckBox read fCheckBox write SetCheckBox;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the attributes of text written on the dialog.
    /// </summary>
    {$endregion}
    property Font: TFont read fFont write SetFont stored IsStoredFont;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the icons of the dialog for all different types of messages.
    /// </summary>
    {$endregion}
    property Icons: TMsgDlgTypeIcons read fIcons write SetIcons;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the component uses its parent's <see cref="BiDiMode"/>.
    /// </summary>
    {$endregion}
    property ParentBiDiMode: Boolean read fParentBiDiMode write SetParentBiDiMode default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the component uses its parent's <see cref="Font"/>.
    /// </summary>
    {$endregion}
    property ParentFont: Boolean read fParentFont write SetParentFont default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the message dialog is shown.
    /// </summary>
    {$endregion}
    property OnShow: TNotifyEvent read fOnShow write fOnShow;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the message dialog closes.
    /// </summary>
    {$endregion}
    property OnClose: TNotifyEvent read fOnClose write fOnClose;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs every second while the dialog is displayed and the value of
    /// <see cref="CallbackTimer"/> property is <see langword="true"/>.</summary>
    {$endregion}
    property OnCallbackTimer: TNotifyEvent read fOnCallbackTimer write fOnCallbackTimer;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TInputQueryDlgBtnCaptions stores the caption of buttons for an input query
  /// dialog.
  /// </summary>
  {$endregion}
  TInputQueryDlgBtnCaptions = class(TCustomDlgBtnCaptions)
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "OK" button.
    /// </summary>
    {$endregion}
    property OK;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Cancel" button.
    /// </summary>
    {$endregion}
    property Cancel;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the caption for "Help" button.
    /// </summary>
    {$endregion}
    property Help;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TInputQueryDlgValidateEvent is the type for event handlers that respond when
  /// an input string needs to be validated.
  /// </summary>
  /// <param name="Sender">
  /// The object that generated the event.
  /// </param>
  /// <param name="Value">
  /// The string value to validate or modify.
  /// </param>
  /// <returns>
  /// Returns <see langword="true"/> if the string value is valid, otherwise returns
  /// <see langword="false"/>.
  /// </returns>
  {$endregion}
  TInputQueryDlgValidateEvent = function(Sender: TObject; var Value: String): Boolean of Object;

  {$region 'xmldoc'}
  /// <summary>
  /// TInputQueryDialog displays a localizable input dialog box that lets the user
  /// enter a string, double, or integer.
  /// </summary>
  /// <remarks>
  /// <para>
  /// Use TInputQueryDialog to display a modal Windows dialog box for requesting a string,
  /// double, or integer from the user. The dialog does not appear at runtime until it
  /// is activated by calling the <see cref="Execute"/> method.
  /// </para>
  /// <para>
  /// TInputQueryDialog has properties and methods to facilitate customization of the dialog
  /// and prompting for the user's input.
  /// </para>
  /// </remarks>
  /// <group name="Components"/>
  {$endregion}
  {$IFDEF COMPILER_XE2_UP}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TInputQueryDialog = class(TComponent)
  protected
    type
      {$region 'xmldoc'}
      /// <summary>
      /// This enumeration type identifies the types of inputs for an input query dialog.
      /// </summary>
      {$endregion}
      TInputType = (
        {$region 'xmldoc'}
        /// Multiple lines of text.
        {$endregion}
        itMultiLineText,
        {$region 'xmldoc'}
        /// A single line of text.
        {$endregion}
        itSingleLineText,
        {$region 'xmldoc'}
        /// An integer number.
        {$endregion}
        itInteger,
        {$region 'xmldoc'}
        /// A floating point number.
        {$endregion}
        itFloat
      );
  private
    fButtonCaptions: TInputQueryDlgBtnCaptions;
    fDialog: TForm;
    fFont: TFont;
    fBiDiMode: TBiDiMode;
    fParentFont: Boolean;
    fParentBiDiMode: Boolean;
    fReadOnly: Boolean;
    fInputType: TInputType;
    fEditBox: TCustomEdit;
    fOnShow: TNotifyEvent;
    fOnClose: TNotifyEvent;
    fOnValidate: TInputQueryDlgValidateEvent;
    procedure SetButtonCaptions(Value: TInputQueryDlgBtnCaptions);
    procedure SetFont(Value: TFont);
    function GetBiDiMode: TBiDiMode;
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetParentFont(Value: Boolean);
    procedure SetParentBiDiMode(Value: Boolean);
    function IsStoredFont: Boolean;
    function IsStoredBiDiMode: Boolean;
    procedure FontChange(Sender: TObject);
    procedure DialogShow(Sender: TObject);
    procedure DialogClose(Sender: TObject; var Action: TCloseAction);
    procedure DialogCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure HelpButtonClick(Sender: TObject);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnShow"/> event.
    /// </summary>
    {$endregion}
    procedure DoShow; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnClose"/> event.
    /// </summary>
    {$endregion}
    procedure DoClose; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Generates an <see cref="OnValidate"/> event.
    /// </summary>
    /// <param name="Value">
    /// The value to validate.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the value is valid, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    {$endregion}
    function DoValidate(var Value: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Brings up an input dialog box ready for the user to enter a string,
    /// double, or integer in its edit box.
    /// </summary>
    /// <param name="ACaption">
    /// The title of the dialog.
    /// </param>
    /// <param name="APrompt">
    /// The text that prompts the user to enter input in the edit box.
    /// </param>
    /// <param name="Value">
    /// The value that appears in the edit box when the dialog box first appears
    /// and which returns the value that the user enters.
    /// </param>
    /// <param name="AInputType">
    /// Specifies the type of input that the user can enter.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the user chooses OK, and <see langword="false"/>
    /// if the user chooses Cancel or presses the Esc key.
    /// </returns>
    {$endregion}
    function Prompt(const ACaption, APrompt: String; var Value: String;
      AInputType: TInputType; HelpContext: THelpContext = 0): Boolean;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the component.
    /// </summary>
    /// <param name="AOwner">
    /// The component that owns this component.
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
    /// Brings up an input dialog box ready for the user to enter a string in its
    /// edit box.
    /// </summary>
    /// <param name="ACaption">
    /// The title of the dialog.
    /// </param>
    /// <param name="APrompt">
    /// The text that prompts the user to enter input in the edit box.
    /// </param>
    /// <param name="Value">
    /// The value that appears in the edit box when the dialog box first appears
    /// and which returns the value that the user enters.
    /// </param>
    /// <param name="Multiline">
    /// Indicates whether the input is a single line string or has multiple lines
    /// of text.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the user chooses OK, and <see langword="false"/>
    /// if the user chooses Cancel or presses the Esc key.
    /// </returns>
    {$endregion}
    function Execute(const ACaption, APrompt: String; var Value: String;
      Multiline: Boolean = False; HelpContext: THelpContext = 0): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Brings up an input dialog box ready for the user to enter an integer number
    /// in its edit box.
    /// </summary>
    /// <param name="ACaption">
    /// The title of the dialog.
    /// </param>
    /// <param name="APrompt">
    /// The text that prompts the user to enter input in the edit box.
    /// </param>
    /// <param name="Value">
    /// The value that appears in the edit box when the dialog box first appears
    /// and which returns the value that the user enters.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the user chooses OK, and <see langword="false"/>
    /// if the user chooses Cancel or presses the Esc key.
    /// </returns>
    {$endregion}
    function Execute(const ACaption, APrompt: String; var Value: Integer;
      HelpContext: THelpContext = 0): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Brings up an input dialog box ready for the user to enter a floating point
    /// number in its edit box.
    /// </summary>
    /// <param name="ACaption">
    /// The title of the dialog.
    /// </param>
    /// <param name="APrompt">
    /// The text that prompts the user to enter input in the edit box.
    /// </param>
    /// <param name="Value">
    /// The value that appears in the edit box when the dialog box first appears
    /// and which returns the value that the user enters.
    /// </param>
    /// <param name="HelpContext">
    /// Specifies the context identifier for the help topic that should appear when
    /// the user clicks the help button or presses F1 while the dialog is displayed.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the user chooses OK, and <see langword="false"/>
    /// if the user chooses Cancel or presses the Esc key.
    /// </returns>
    {$endregion}
    function Execute(const ACaption, APrompt: String; var Value: Double;
      HelpContext: THelpContext = 0): Boolean; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides direct access to the underlying form object while the dialog is
    /// displayed.
    /// </summary>
    {$endregion}
    property Dialog: TForm read fDialog;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides direct access to the underlying edit control while the dialog is
    /// displayed.
    /// </summary>
    {$endregion}
    property EditBox: TCustomEdit read fEditBox;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the current input mode of the dialog onscreen.
    /// </summary>
    {$endregion}
    property InputType: TInputType read fInputType;
  published
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the bidirectional mode for the dialog.
    /// </summary>
    {$endregion}
    property BiDiMode: TBiDiMode read GetBiDiMode write SetBiDiMode stored IsStoredBiDiMode;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the captions of the dialog buttons.
    /// </summary>
    {$endregion}
    property ButtonCaptions: TInputQueryDlgBtnCaptions read fButtonCaptions write SetButtonCaptions;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the attributes of text written on the dialog.
    /// </summary>
    {$endregion}
    property Font: TFont read fFont write SetFont stored IsStoredFont;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the user can enter a value.
    /// </summary>
    {$endregion}
    property ReadOnly: Boolean read fReadOnly write fReadOnly default False;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the component uses its parent's <see cref="BiDiMode"/>.
    /// </summary>
    {$endregion}
    property ParentBiDiMode: Boolean read fParentBiDiMode write SetParentBiDiMode default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the component uses its parent's <see cref="Font"/>.
    /// </summary>
    {$endregion}
    property ParentFont: Boolean read fParentFont write SetParentFont default True;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dialog is shown.
    /// </summary>
    {$endregion}
    property OnShow: TNotifyEvent read fOnShow write fOnShow;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dialog closes.
    /// </summary>
    {$endregion}
    property OnClose: TNotifyEvent read fOnClose write fOnClose;
    {$region 'xmldoc'}
    /// <summary>
    /// Occurs when the dialog is about to close to validate the input value.
    /// </summary>
    {$endregion}
    property OnValidate: TInputQueryDlgValidateEvent read fOnValidate write fOnValidate;
  end;

implementation

uses
  {$IFDEF COMPILER_XE3_UP}
  UITypes,
  {$ENDIF}
  Types, SysUtils, Character, Clipbrd, ExtCtrls, i18nCtrls;

resourcestring
  SMsgDlgWarning     = 'Warning';
  SMsgDlgError       = 'Error';
  SMsgDlgInformation = 'Information';
  SMsgDlgConfirm     = 'Confirm';

resourcestring
  SMsgDlgYes         = 'Yes';
  SMsgDlgNo          = 'No';
  SMsgDlgOK          = 'OK';
  SMsgDlgCancel      = 'Cancel';
  SMsgDlgHelp        = 'Help';
  SMsgDlgAbort       = 'Abort';
  SMsgDlgRetry       = 'Retry';
  SMsgDlgIgnore      = 'Ignore';
  SMsgDlgAll         = 'All';
  SMsgDlgNoToAll     = 'No to All';
  SMsgDlgYesToAll    = 'Yes to All';
  SMsgDlgClose       = 'Close';

const
  DialogCaptions: array [TMsgDlgType] of Pointer =
    (@SMsgDlgWarning, @SMsgDlgError, @SMsgDlgInformation, @SMsgDlgConfirm, nil);
  IconIDs: array [TMsgDlgType] of PChar =
    (IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK, IDI_QUESTION, nil);
  ButtonCaptions_: array [TMsgDlgBtn] of Pointer =
    (@SMsgDlgYes, @SMsgDlgNo, @SMsgDlgOK, @SMsgDlgCancel, @SMsgDlgAbort,
     @SMsgDlgRetry, @SMsgDlgIgnore, @SMsgDlgAll, @SMsgDlgNoToAll,
     @SMsgDlgYesToAll, @SMsgDlgHelp {$IFDEF UNICODE}, @SMsgDlgClose {$ENDIF});
  ButtonNames: array [TMsgDlgBtn] of String =
    ('Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
     'YesToAll', 'Help' {$IFDEF UNICODE}, 'Close' {$ENDIF});
  ModalResults: array [TMsgDlgBtn] of Integer =
    (mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
     mrYesToAll, 0 {$IFDEF UNICODE}, mrClose {$ENDIF});

{ Helper Functions }

type TButtonHotKeys = array[TMsgDlgBtn] of Char;

function RemovePrefix(const Str: String): String;
var
  I: Integer;
begin
  Result := Str;
  I := Length(Result);
  while I > 0 do
  begin
    if Result[I] = '&' then
    begin
      if (I = 1) or (Result[I - 1] <> '&') then
        Delete(Result, I, 1)
      else
        Dec(I);
    end;
    Dec(I);
  end;
end;

function GetAutoHotKey(const Caption: String; Button: TMsgDlgBtn;
  var LastHotKeys: TButtonHotKeys): String;
var
  B: TMsgDlgBtn;
  HotKey: Char;
  I: Integer;
begin
  Result := RemovePrefix(Caption);
  if not (Button in [mbOK, mbCancel]) then
  begin
    for I := 1 to Length(Result) do
    begin
      if not IsLetterOrDigit(Result[I]) then
        Continue;
      HotKey := ToUpper(Result[I]);
      for B := Low(TMsgDlgBtn) to Pred(Button) do
        if HotKey = LastHotKeys[B] then
        begin
          HotKey := #0;
          Break;
        end;
      if HotKey <> #0 then
      begin
        Insert('&', Result, I);
        LastHotKeys[B] := HotKey;
        Break;
      end;
    end;
  end;
end;

{ TCustomDlgBtnCaptions }

constructor TCustomDlgBtnCaptions.Create;
var
  DlgBtn: TMsgDlgBtn;
begin
  for DlgBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    fCaptions[DlgBtn] := LoadResString(ButtonCaptions_[DlgBtn]);
end;

procedure TCustomDlgBtnCaptions.Assign(Source: TPersistent);
var
  DlgBtn: TMsgDlgBtn;
begin
  if Source is TCustomDlgBtnCaptions then
    for DlgBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      Captions[DlgBtn] := TCustomDlgBtnCaptions(Source).Captions[DlgBtn]
  else
    inherited Assign(Source);
end;

function TCustomDlgBtnCaptions.GetMetrics(DC: HDC; Buttons: TMsgDlgButtons;
  var Width, Height: Integer): Integer;
const
  HorzSpacing = 4;
  VertSpacing = 2;
var
  B: TMsgDlgBtn;
  TextRect: TRect;
begin
  Result := 0;
  for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if B in Buttons then
    begin
      Inc(Result);
      SetRect(TextRect, 0, 0, 0, 0);
      DrawText(DC, PChar(Captions[B]), Length(Captions[B]), TextRect,
        DT_CALCRECT or DT_SINGLELINE);
      if Width < TextRect.Right - TextRect.Left + 2 * HorzSpacing then
        Width := TextRect.Right - TextRect.Left + 2 * HorzSpacing;
      if Height < TextRect.Bottom - TextRect.Top + 2 * VertSpacing then
        Height := TextRect.Bottom - TextRect.Top + 2 * VertSpacing;
    end;
end;

function TCustomDlgBtnCaptions.GetCaptions(DlgBtn: TMsgDlgBtn): String;
begin
  Result := fCaptions[DlgBtn];
end;

procedure TCustomDlgBtnCaptions.SetCaptions(DlgBtn: TMsgDlgBtn;
  const Value: String);
begin
  fCaptions[DlgBtn] := Value;
end;

function TCustomDlgBtnCaptions.IsStoredCaptions(DlgBtn: TMsgDlgBtn): Boolean;
begin
  Result := (fCaptions[DlgBtn] <> LoadResString(ButtonCaptions_[DlgBtn]));
end;

{ TCustomDlgCheckBox }

constructor TCustomDlgCheckBox.Create;
begin
  inherited Create;
  fAllowGrayed := False;
  fState := cbUnchecked;
  fEnabled := True;
  fVisible := False;
end;

procedure TCustomDlgCheckBox.Assign(Source: TPersistent);
begin
  if Source is TCustomDlgCheckBox then
  begin
    Caption := TCustomDlgCheckBox(Source).Caption;
    Hint := TCustomDlgCheckBox(Source).Hint;
    AllowGrayed := TCustomDlgCheckBox(Source).AllowGrayed;
    State := TCustomDlgCheckBox(Source).State;
    Enabled := TCustomDlgCheckBox(Source).Enabled;
    Visible := TCustomDlgCheckBox(Source).Visible;
  end
  else if Source is TCheckBox then
  begin
    Caption := TCheckBox(Source).Caption;
    Hint := TCheckBox(Source).Hint;
    AllowGrayed := TCheckBox(Source).AllowGrayed;
    State := TCheckBox(Source).State;
    Enabled := TCheckBox(Source).Enabled;
    Visible := TCheckBox(Source).Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TCustomDlgCheckBox.AssignTo(Dest: TPersistent);
begin
  if Dest is TCheckBox then
  begin
    TCheckBox(Dest).Caption := Caption;
    TCheckBox(Dest).Hint := Hint;
    TCheckBox(Dest).AllowGrayed := AllowGrayed;
    TCheckBox(Dest).State := State;
    TCheckBox(Dest).Enabled := Enabled;
    TCheckBox(Dest).Visible := Visible;
  end
  else
    inherited AssignTo(Dest);
end;

function TCustomDlgCheckBox.GetMetrics(DC: HDC; MaxWidth: Integer): TSize;
const
  Spacing = 4;
var
  TextRect: TRect;
  CheckSize: TSize;
begin
  CheckSize := GetCheckBoxSize;
  SetRect(TextRect, 0, 0, MaxWidth - CheckSize.cx + Spacing, 0);
  DrawText(DC, PChar(Caption), Length(Caption), TextRect,
    DT_CALCRECT or DT_WORDBREAK);
  Result.cx := TextRect.Right - TextRect.Left + CheckSize.cx + Spacing;
  Result.cy := TextRect.Bottom - TextRect.Top;
  if Result.cy < CheckSize.cy then
    Result.cy := CheckSize.cy;
end;

procedure TCustomDlgCheckBox.SetCaption(const Value: String);
begin
  if Caption <> Value then
  begin
    fCaption := Value;
    if Assigned(Control) then
      Control.Caption := Caption;
  end;
end;

procedure TCustomDlgCheckBox.SetHint(const Value: String);
begin
  if Hint <> Value then
  begin
    fHint := Value;
    if Assigned(Control) then
      Control.Hint := Hint;
  end;
end;

procedure TCustomDlgCheckBox.SetAllowGrayed(Value: Boolean);
begin
  if AllowGrayed <> Value then
  begin
    fAllowGrayed := Value;
    if Assigned(Control) then
      Control.AllowGrayed := AllowGrayed;
  end;
end;

procedure TCustomDlgCheckBox.SetState(Value: TCheckBoxState);
begin
  if State <> Value then
  begin
    fState := Value;
    if Assigned(Control) then
      Control.State := State;
  end;
end;

function TCustomDlgCheckBox.GetChecked: Boolean;
begin
  Result := (State = cbChecked);
end;

procedure TCustomDlgCheckBox.SetChecked(Value: Boolean);
begin
  if Value then
    State := cbChecked
  else
    State := cbUnchecked;
end;

procedure TCustomDlgCheckBox.SetEnabled(Value: Boolean);
begin
  if Enabled <> Value then
  begin
    fEnabled := Value;
    if Assigned(Control) then
      Control.Enabled := Enabled;
  end;
end;

procedure TCustomDlgCheckBox.SetVisible(Value: Boolean);
begin
  if Visible <> Value then
  begin
    fVisible := Value;
    if Assigned(Control) then
      Control.Visible := Visible;
  end;
end;

procedure TCustomDlgCheckBox.SetControl(Value: TCheckBox);
begin
  if Control <> Value then
  begin
    fControl := Value;
    if Assigned(Control) then
      AssignTo(Control);
  end;
end;

{ TMsgDlgTypeIcons }

constructor TMsgDlgTypeIcons.Create;
var
  DlgType: TMsgDlgType;
begin
  for DlgType := Low(TMsgDlgType) to High(TMsgDlgType) do
  begin
    fIcons[DlgType] := TIcon.Create;
    if DlgType <> mtCustom then
      fIcons[DlgType].Handle := LoadIcon(0, IconIDs[DlgType]);
  end;
end;

destructor TMsgDlgTypeIcons.Destroy;
var
  DlgType: TMsgDlgType;
begin
  for DlgType := Low(TMsgDlgType) to High(TMsgDlgType) do
    fIcons[DlgType].Free;
  inherited Destroy;
end;

procedure TMsgDlgTypeIcons.Assign(Source: TPersistent);
var
  DlgType: TMsgDlgType;
begin
  if Source is TMsgDlgTypeIcons then
    for DlgType := Low(TMsgDlgType) to High(TMsgDlgType) do
      Icons[DlgType] := TMsgDlgTypeIcons(Source).Icons[DlgType]
  else
    inherited Assign(Source);
end;

function TMsgDlgTypeIcons.GetIcons(DlgType: TMsgDlgType): TIcon;
begin
  Result := fIcons[DlgType];
end;

procedure TMsgDlgTypeIcons.SetIcons(DlgType: TMsgDlgType; Value: TIcon);
begin
  if Assigned(Value) then
    fIcons[DlgType].Assign(Value)
  else if DlgType <> mtCustom then
    fIcons[DlgType].Handle := LoadIcon(0, IconIDs[DlgType])
  else
    fIcons[DlgType].Handle := 0;
end;

function TMsgDlgTypeIcons.IsStoredIcons(DlgType: TMsgDlgType): Boolean;
begin
  if fIcons[DlgType].Empty then
    Result := False
  else if DlgType <> mtCustom then
    Result := (fIcons[DlgType].Handle <> LoadIcon(0, IconIDs[DlgType]))
  else
    Result := True;
end;

{ TMsgDlgTypeCaptions }

constructor TMsgDlgTypeCaptions.Create;
var
  DlgType: TMsgDlgType;
begin
  for DlgType := Low(TMsgDlgType) to High(TMsgDlgType) do
    if DlgType <> mtCustom then
      fCaptions[DlgType] := LoadResString(DialogCaptions[DlgType])
    else
      fCaptions[DlgType] := '';
end;

procedure TMsgDlgTypeCaptions.Assign(Source: TPersistent);
var
  DlgType: TMsgDlgType;
begin
  if Source is TMsgDlgTypeCaptions then
    for DlgType := Low(TMsgDlgType) to High(TMsgDlgType) do
      Captions[DlgType] := TMsgDlgTypeCaptions(Source).Captions[DlgType]
  else
    inherited Assign(Source);
end;

function TMsgDlgTypeCaptions.GetCaptions(DlgType: TMsgDlgType): String;
begin
  Result := fCaptions[DlgType];
end;

procedure TMsgDlgTypeCaptions.SetCaptions(DlgType: TMsgDlgType;
  const Value: String);
begin
  fCaptions[DlgType] := Value;
end;

function TMsgDlgTypeCaptions.IsStoredCaptions(DlgType: TMsgDlgType): Boolean;
begin
  if DlgType <> mtCustom then
    Result := (fCaptions[DlgType] <> LoadResString(DialogCaptions[DlgType]))
  else
    Result := (fCaptions[DlgType] <> '');
end;

{ TMessageDialog }

constructor TMessageDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAutoHotKeys := True;
  fCheckBox := TMsgDlgCheckBox.Create;
  fIcons := TMsgDlgTypeIcons.Create;
  fCaptions := TMsgDlgTypeCaptions.Create;
  fButtonCaptions := TMsgDlgBtnCaptions.Create;
  fFont := TFont.Create;
  fFont.OnChange := FontChange;
  ParentBiDiMode := True;
  ParentFont := True;
end;

destructor TMessageDialog.Destroy;
begin
  fIcons.Free;
  fCaptions.Free;
  fButtonCaptions.Free;
  fCheckBox.Free;
  fFont.Free;
  inherited Destroy;
end;

function TMessageDialog.GetShowDuration: Cardinal;
begin
  if Assigned(Dialog) then
    Result := GetTickCount - fShowTicks
  else
    Result := fShowTicks;
end;

procedure TMessageDialog.SetCheckBox(Value: TMsgDlgCheckBox);
begin
  fCheckBox.Assign(Value);
end;

procedure TMessageDialog.SetIcons(Value: TMsgDlgTypeIcons);
begin
  fIcons.Assign(Value);
end;

procedure TMessageDialog.SetCaptions(Value: TMsgDlgTypeCaptions);
begin
  fCaptions.Assign(Value);
end;

procedure TMessageDialog.SetButtonCaptions(Value: TMsgDlgBtnCaptions);
begin
  fButtonCaptions.Assign(Value);
end;

procedure TMessageDialog.SetFont(Value: TFont);
begin
  fFont.Assign(Value);
end;

function TMessageDialog.GetBiDiMode: TBiDiMode;
begin
  if ParentBiDiMode then
    fBiDiMode := Application.BiDiMode;
  Result := fBiDiMode;
end;

procedure TMessageDialog.SetBiDiMode(Value: TBiDiMode);
begin
  fBiDiMode := Value;
  fParentBiDiMode := False;
end;

procedure TMessageDialog.SetParentFont(Value: Boolean);
begin
  if ParentFont <> Value then
  begin
    if Value then
      fFont.Assign(Application.DefaultFont);
    fParentFont := Value;
  end;
end;

procedure TMessageDialog.SetParentBiDiMode(Value: Boolean);
begin
  if ParentBiDiMode <> Value then
  begin
    if Value then
      fBiDiMode := Application.BiDiMode;
    fParentBiDiMode := Value;
  end;
end;

function TMessageDialog.IsStoredFont: Boolean;
begin
  Result := not ParentFont;
end;

function TMessageDialog.IsStoredBiDiMode: Boolean;
begin
  Result := not ParentBiDiMode;
end;

procedure TMessageDialog.FontChange(Sender: TObject);
begin
  fParentFont := False;
end;

procedure TMessageDialog.DialogShow(Sender: TObject);
begin
  DoShow;
end;

procedure TMessageDialog.DialogClose(Sender: TObject; var Action: TCloseAction);
begin
  DoClose;
end;

procedure TMessageDialog.DialogKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Word('C')) then
  begin
    Beep;
    Clipboard.AsText := GetDialogText;
  end;
end;

procedure TMessageDialog.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(Dialog.HelpContext);
end;

procedure TMessageDialog.CheckBoxClicked(Sender: TObject);
begin
  if Assigned(CheckBox.Control) then
    CheckBox.State := CheckBox.Control.State;
end;

procedure TMessageDialog.CallbackTimerTrigger(Sender: TObject);
begin
  DoCallbackTimer;
end;

procedure TMessageDialog.DoShow;
begin
  if Assigned(OnShow) then
    OnShow(Self);
end;

procedure TMessageDialog.DoClose;
begin
  if Assigned(OnClose) then
    OnClose(Self);
end;

procedure TMessageDialog.DoCallbackTimer;
begin
  if Assigned(OnCallbackTimer) then
    OnCallbackTimer(Self);
end;

function TMessageDialog.GetDialogText: String;
var
  DividerLine, ButtonCaptions, MessageCaption: String;
  I: Integer;
begin
  if Assigned(Dialog) then
  begin
    ButtonCaptions := '';
    MessageCaption := '';
    DividerLine := StringOfChar('-', 27) + sLineBreak;
    for I := 0 to Dialog.ComponentCount - 1 do
      if Dialog.Components[I] is TButton then
        ButtonCaptions := ButtonCaptions + TButton(Dialog.Components[I])
          .Caption + StringOfChar(' ', 3)
      else if Dialog.Components[I] is TStaticText then
        MessageCaption := TStaticText(Dialog.Components[I]).Caption;
    ButtonCaptions := StringReplace(ButtonCaptions, '&', '', [rfReplaceAll]);
    Result := DividerLine + Dialog.Caption + sLineBreak + DividerLine +
      MessageCaption + sLineBreak + DividerLine + ButtonCaptions + sLineBreak +
      DividerLine;
  end
  else
    Result := '';
end;

function TMessageDialog.Show(const Msg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn;
  HelpContext: THelpContext): Integer;
const
  mcHorzMargin = 8;
  mcVertMargin = 6;
  mcHorzSpacing = 8;
  mcVertSpacing = 5;
  mcButtonWidth = 60;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
  mcMinDialogWidth = 260;
var
  DC: HDC;
  tm: TTextMetric;
  TextRect: TRect;
  TextSize, IconSize, CheckSize: TSize;
  HorzMargin, VertMargin: Integer;
  HorzSpacing, VertSpacing: Integer;
  ButtonWidth, ButtonHeight, ButtonSpacing: Integer;
  ButtonCount, ButtonGroupWidth: Integer;
  IconTextWidth, IconTextHeight: Integer;
  B, CancelButton: TMsgDlgBtn;
  X, MinDialogWidth: Integer;
  ButtonsPanel: TPanel;
  Button: TButton;
  HotKeys: TButtonHotKeys;
begin
  fDialog := TForm.CreateNew(Application);
  try
    // init dialog
    Dialog.BiDiMode := BiDiMode;
    Dialog.Color := clWindow;
    Dialog.Font := Font;
    Dialog.ShowHint := True;
    Dialog.KeyPreview := True;
    Dialog.BorderStyle := bsToolWindow;
    Dialog.Position := poScreenCenter;
    Dialog.HelpContext := HelpContext;
    Dialog.Icon := Application.Icon;
    Dialog.OnShow := DialogShow;
    Dialog.OnClose := DialogClose;
    Dialog.OnKeyDown := DialogKeyDown;
    Dialog.Canvas.Font := Dialog.Font;
    Dialog.Caption := Captions[DlgType];
    if Dialog.Caption = '' then
      Dialog.Caption := Application.Title;
    // calculate metrics
    DC := Dialog.Canvas.Handle;
    GetTextMetrics(DC, tm);
    HorzMargin := MulDiv(mcHorzMargin, tm.tmAveCharWidth, 4);
    VertMargin := MulDiv(mcVertMargin, tm.tmHeight, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, tm.tmAveCharWidth, 4);
    VertSpacing := MulDiv(mcVertSpacing, tm.tmHeight, 8);
    ButtonWidth := MulDiv(mcButtonWidth, tm.tmAveCharWidth, 4);
    ButtonHeight := MulDiv(mcButtonHeight, tm.tmHeight, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, tm.tmAveCharWidth, 4);
    MinDialogWidth := MulDiv(mcMinDialogWidth, tm.tmAveCharWidth, 4);
    // calculate size of buttons
    ButtonCount := ButtonCaptions.GetMetrics(DC, Buttons, ButtonWidth, ButtonWidth);
    if ButtonCount > 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount + ButtonSpacing * (ButtonCount - 1)
    else
      ButtonGroupWidth := 0;
    // calculate size of message
    SetRect(TextRect, 0, 0, Screen.Width div 3, 0);
    DrawText(DC, PChar(Msg), Length(Msg), TextRect,
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK);
    OffsetRect(TextRect, HorzMargin, VertMargin);
    TextSize.cx := TextRect.Right - TextRect.Left;
    TextSize.cy := TextRect.Bottom - TextRect.Top;
    IconTextWidth := TextSize.cx + 2 * HorzMargin;
    IconTextHeight := TextSize.cy + 2 * VertMargin;
    // calculate size of icon
    if not Icons[DlgType].Empty then
    begin
      IconSize.cx := Icons[DlgType].Width;
      IconSize.cy := Icons[DlgType].Height;
    end
    else
    begin
      IconSize.cx := 0;
      IconSize.cy := 0;
    end;
    if IconSize.cx <> 0 then
    begin
      OffsetRect(TextRect, IconSize.cx + HorzSpacing, 0);
      Inc(IconTextWidth, IconSize.cx + HorzSpacing);
      if IconSize.cy > TextSize.cy then
      begin
        OffsetRect(TextRect, 0, (IconSize.cy - TextSize.cy) div 2);
        Inc(IconTextHeight, IconSize.cy - TextSize.cy);
      end;
    end;
    // adjust size
    if IconTextWidth < ButtonGroupWidth + 2 * HorzMargin then
    begin
      Inc(TextSize.cx, ButtonGroupWidth + 2 * HorzMargin - IconTextWidth);
      IconTextWidth := ButtonGroupWidth + 2 * HorzMargin;
    end;
    if IconTextWidth < MinDialogWidth then
    begin
      Inc(TextSize.cx, MinDialogWidth - IconTextWidth);
      IconTextWidth := MinDialogWidth;
    end;
    // calculate size of check box
    if CheckBox.Visible then
    begin
      CheckSize := CheckBox.GetMetrics(DC, TextSize.cx);
      if CheckSize.cx > TextSize.cx then
      begin
        Inc(IconTextWidth, CheckSize.cx - TextSize.cx);
        Inc(TextSize.cx, CheckSize.cx - TextSize.cx);
      end;
    end
    else
    begin
      CheckSize.cx := 0;
      CheckSize.cy := 0;
    end;
    // create icon
    if IconSize.cx <> 0 then
      with TImage.Create(Dialog) do
      begin
        Name := 'Image';
        Parent := Dialog;
        Center := True;
        Proportional := True;
        Picture.Icon.Assign(Icons[DlgType]);
        SetBounds(HorzMargin, VertMargin, IconSize.cx, IconSize.cy);
      end;
    // create message
    if Msg <> '' then
      with TStaticText.Create(Dialog) do
      begin
        Name := 'Message';
        Parent := Dialog;
        AutoSize := False;
        ShowAccelChar := False;
        Caption := Msg;
        BoundsRect := TextRect;
      end;
    // create checkbox
    if CheckBox.Visible then
    begin
      CheckBox.Control := TCheckBox.Create(Dialog);
      with CheckBox.Control do
      begin
        Parent := Dialog;
        WordWrap := True;
        OnClick := CheckBoxClicked;
        SetBounds(TextRect.Left, IconTextHeight, CheckSize.cx, CheckSize.cy);
        Inc(IconTextHeight, VertMargin + CheckSize.cy);
      end;
    end;
    // create buttons
    if ButtonCount > 0 then
    begin
      ButtonsPanel := TPanel.Create(Dialog);
      with ButtonsPanel do
      begin
        Name := 'ButtonsPanel';
        Parent := Dialog;
        Caption := '';
        ShowCaption := False;
        BevelOuter := bvNone;
        ParentBackground := False;
        SetBounds(0, IconTextHeight, IconTextWidth, ButtonHeight + 2 * VertSpacing - 1);
      end;
      with TBevel.Create(Dialog) do
      begin
        Parent := ButtonsPanel;
        SetBounds(0, 0, ButtonsPanel.Width, 1);
        Shape := bsTopLine;
      end;
      FillChar(HotKeys, SizeOf(HotKeys), 0);
      if mbCancel in Buttons then
        CancelButton := mbCancel
      else if mbNo in Buttons then
        CancelButton := mbNo
      else
        CancelButton := mbOK;
      X := ButtonsPanel.ClientWidth - ButtonGroupWidth - HorzMargin;
      for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
        if B in Buttons then
        begin
          Button := TButton.Create(Dialog);
          with Button do
          begin
            Name := ButtonNames[B];
            Parent := ButtonsPanel;
            if AutoHotKeys then
              Caption := GetAutoHotKey(ButtonCaptions[B], B, HotKeys)
            else
              Caption := ButtonCaptions[B];
            ModalResult := ModalResults[B];
            if B = DefaultButton then
            begin
              Default := True;
              Dialog.ActiveControl := Button;
            end;
            if B = CancelButton then
              Cancel := True;
            if B = mbHelp then
              OnClick := HelpButtonClick;
            SetBounds(X, VertSpacing, ButtonWidth, ButtonHeight);
            Inc(X, ButtonWidth + ButtonSpacing);
          end;
        end;
    end;
    // adjust dialog size
    Dialog.ClientWidth := IconTextWidth;
    if ButtonCount > 0 then
      Dialog.ClientHeight := IconTextHeight + ButtonHeight + 2 * VertSpacing
    else
      Dialog.ClientHeight := IconTextHeight;
    // do the flip if needed
    if Dialog.UseRightToLeftAlignment then
      Dialog.FlipChildren(True);
    // create callback timer
    if CallbackTimer then
      with TTimer.Create(Dialog) do
      begin
        Name := 'Timer';
        OnTimer := CallbackTimerTrigger;
      end;
    // show the dialog
    fShowTicks := GetTickCount;
    Result := Dialog.ShowModal;
    fShowTicks := GetTickCount - fShowTicks;
  finally
    CheckBox.Control := nil;
    FreeAndNil(fDialog);
  end;
end;

function TMessageDialog.Show(const Fmt: String; const Args: array of const;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn;
  HelpContext: THelpContext): Integer;
begin
  Result := Show(Format(Fmt, Args), DlgType, Buttons, DefaultButton, HelpContext);
end;

function TMessageDialog.Show(const Msg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpContext: THelpContext): Integer;
var
  DefaultButton: TMsgDlgBtn;
begin
  if mbOK in Buttons then
    DefaultButton := mbOK
  else if mbYes in Buttons then
    DefaultButton := mbYes
  else
    DefaultButton := mbRetry;
  Result := Show(Msg, DlgType, Buttons, DefaultButton, HelpContext);
end;

function TMessageDialog.Show(const Fmt: String; const Args: array of const;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpContext: THelpContext): Integer;
begin
  Result := Show(Format(Fmt, Args), DlgType, Buttons, HelpContext)
end;

procedure TMessageDialog.ShowMessage(const Msg: String;
  HelpContext: THelpContext);
var
  Buttons: TMsgDlgButtons;
begin
  Buttons := [mbOK];
  if HelpContext <> 0 then
    Include(Buttons, mbHelp);
  Show(Msg, mtCustom, Buttons, HelpContext);
end;

procedure TMessageDialog.ShowWarning(const Msg: String;
  HelpContext: THelpContext);
var
  Buttons: TMsgDlgButtons;
begin
  Buttons := [mbOK];
  if HelpContext <> 0 then
    Include(Buttons, mbHelp);
  Show(Msg, mtWarning, Buttons, HelpContext);
end;

procedure TMessageDialog.ShowError(const Msg: String;
  HelpContext: THelpContext);
var
  Buttons: TMsgDlgButtons;
begin
  Buttons := [mbOK];
  if HelpContext <> 0 then
    Include(Buttons, mbHelp);
  Show(Msg, mtError, Buttons, HelpContext);
end;

procedure TMessageDialog.Inform(const Msg: String; HelpContext: THelpContext);
var
  Buttons: TMsgDlgButtons;
begin
  Buttons := [mbOK];
  if HelpContext <> 0 then
    Include(Buttons, mbHelp);
  Show(Msg, mtInformation, Buttons, HelpContext);
end;

function TMessageDialog.Confirm(const Msg: String; DefaultButton: TMsgDlgBtn;
  HelpContext: THelpContext): Boolean;
var
  Buttons: TMsgDlgButtons;
begin
  Buttons := [mbYes, mbNo];
  if HelpContext <> 0 then
    Include(Buttons, mbHelp);
  Result := (Show(Msg, mtConfirmation, Buttons, DefaultButton, HelpContext) = mrYes);
end;

function TMessageDialog.YesNoCancel(const Msg: String;
  DefaultButton: TMsgDlgBtn; HelpContext: THelpContext): Integer;
var
  Buttons: TMsgDlgButtons;
begin
  Buttons := [mbYes, mbNo, mbCancel];
  if HelpContext <> 0 then
    Include(Buttons, mbHelp);
  Result := Show(Msg, mtConfirmation, Buttons, DefaultButton, HelpContext);
end;

function TMessageDialog.YesNoAllCancel(const Msg: String;
  DefaultButton: TMsgDlgBtn; HelpContext: THelpContext): Integer;
var
  Buttons: TMsgDlgButtons;
begin
  Buttons := [mbYes, mbNo, mbYesToAll, mbNoToAll, mbCancel];
  if HelpContext <> 0 then
    Include(Buttons, mbHelp);
  Result := Show(Msg, mtConfirmation, Buttons, DefaultButton, HelpContext);
end;

function TMessageDialog.RetryIgnoreAbort(const Msg: String;
  DefaultButton: TMsgDlgBtn; HelpContext: THelpContext): Integer;
var
  Buttons: TMsgDlgButtons;
begin
  Buttons := [mbRetry, mbIgnore, mbAbort];
  if HelpContext <> 0 then
    Include(Buttons, mbHelp);
  Result := Show(Msg, mtWarning, Buttons, DefaultButton, HelpContext);
end;

{ TInputDialog }

constructor TInputQueryDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fButtonCaptions := TInputQueryDlgBtnCaptions.Create;
  fFont := TFont.Create;
  fFont.OnChange := FontChange;
  ParentBiDiMode := True;
  ParentFont := True;
end;

destructor TInputQueryDialog.Destroy;
begin
  fButtonCaptions.Free;
  fFont.Free;
  inherited Destroy;
end;

procedure TInputQueryDialog.SetButtonCaptions(Value: TInputQueryDlgBtnCaptions);
begin
  fButtonCaptions.Assign(Value);
end;

procedure TInputQueryDialog.SetFont(Value: TFont);
begin
  fFont.Assign(Value);
end;

function TInputQueryDialog.GetBiDiMode: TBiDiMode;
begin
  if ParentBiDiMode then
    fBiDiMode := Application.BiDiMode;
  Result := fBiDiMode;
end;

procedure TInputQueryDialog.SetBiDiMode(Value: TBiDiMode);
begin
  fBiDiMode := Value;
  fParentBiDiMode := False;
end;

procedure TInputQueryDialog.SetParentFont(Value: Boolean);
begin
  if ParentFont <> Value then
  begin
    if Value then
      fFont.Assign(Application.DefaultFont);
    fParentFont := Value;
  end;
end;

procedure TInputQueryDialog.SetParentBiDiMode(Value: Boolean);
begin
  if ParentBiDiMode <> Value then
  begin
    fBiDiMode := Application.BiDiMode;
    fParentBiDiMode := Value;
  end;
end;

function TInputQueryDialog.IsStoredFont: Boolean;
begin
  Result := not ParentFont;
end;

function TInputQueryDialog.IsStoredBiDiMode: Boolean;
begin
  Result := not ParentBiDiMode;
end;

procedure TInputQueryDialog.FontChange(Sender: TObject);
begin
  fParentFont := False;
end;

procedure TInputQueryDialog.DialogShow(Sender: TObject);
begin
  DoShow;
  if EditBox.ReadOnly then
    Windows.SetFocus(GetNextDlgTabItem(Dialog.Handle, EditBox.Handle, False));
end;

procedure TInputQueryDialog.DialogClose(Sender: TObject; var Action: TCloseAction);
begin
  DoClose;
end;

procedure TInputQueryDialog.DialogCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Value: String;
begin
  if (Dialog.ModalResult = mrOk) and not EditBox.ReadOnly then
  begin
    Value := EditBox.Text;
    if not DoValidate(Value) then
    begin
      EditBox.SetFocus;
      CanClose := False;
    end;
    EditBox.Text := Value;
  end;
end;

procedure TInputQueryDialog.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(Dialog.HelpContext);
end;

procedure TInputQueryDialog.DoShow;
begin
  if Assigned(OnShow) then
    OnShow(Self);
end;

procedure TInputQueryDialog.DoClose;
begin
  if Assigned(OnClose) then
    OnClose(Self);
end;

function TInputQueryDialog.DoValidate(var Value: String): Boolean;
begin
  if Assigned(OnValidate) then
    Result := OnValidate(Self, Value)
  else
  begin
    try
      case InputType of
        itInteger: StrToInt(Value);
        itFloat: StrToFloat(Value);
      end;
      Result := True;
    except
      Result := False;
    end;
  end;
end;

function TInputQueryDialog.Prompt(const ACaption, APrompt: String;
  var Value: String; AInputType: TInputType; HelpContext: THelpContext): Boolean;
const
  mcHorzMargin = 8;
  mcVertMargin = 6;
  mcButtonWidth = 60;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
  mcEditBoxWidth = 300;
var
  DC: HDC;
  tm: TTextMetric;
  TextRect: TRect;
  HorzMargin, VertMargin: Integer;
  ButtonWidth, ButtonHeight, ButtonSpacing: Integer;
  ButtonCount, ButtonGroupWidth: Integer;
  EditBoxWidth, TextEditHeight: Integer;
  Buttons: TMsgDlgButtons;
  B: TMsgDlgBtn;
  X: Integer;
  ButtonAnchors: TAnchors;
begin
  Result := False;
  fInputType := AInputType;
  fDialog := TForm.CreateNew(Application);
  try
    // init dialog
    Dialog.Caption := ACaption;
    Dialog.BiDiMode := BiDiMode;
    Dialog.Font := Font;
    if InputType = itMultiLineText then
    begin
      Dialog.BorderStyle := bsSizeable;
      Dialog.BorderIcons := [biSystemMenu, biMaximize];
    end
    else
      Dialog.BorderStyle := bsDialog;
    Dialog.ShowHint := True;
    Dialog.Position := poScreenCenter;
    Dialog.HelpContext := HelpContext;
    Dialog.OnShow := DialogShow;
    Dialog.OnClose := DialogClose;
    Dialog.OnCloseQuery := DialogCloseQuery;
    Dialog.Canvas.Font := Dialog.Font;
    // calculate metrics
    DC := Dialog.Canvas.Handle;
    GetTextMetrics(DC, tm);
    HorzMargin := MulDiv(mcHorzMargin, tm.tmAveCharWidth, 4);
    VertMargin := MulDiv(mcVertMargin, tm.tmHeight, 8);
    ButtonWidth := MulDiv(mcButtonWidth, tm.tmAveCharWidth, 4);
    ButtonHeight := MulDiv(mcButtonHeight, tm.tmHeight, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, tm.tmAveCharWidth, 4);
    EditBoxWidth := MulDiv(mcEditBoxWidth, tm.tmAveCharWidth, 4);
    if InputType = itMultiLineText then
      EditBoxWidth := MulDiv(EditBoxWidth, 150, 100);
    // calculate size of buttons
    Buttons := [mbOK];
    if not ReadOnly then
      Include(Buttons, mbCancel);
    if HelpContext <> 0 then
      Include(Buttons, mbHelp);
    ButtonCount := ButtonCaptions.GetMetrics(DC, Buttons, ButtonWidth,  ButtonHeight);
    ButtonGroupWidth := ButtonWidth * ButtonCount + ButtonSpacing * (ButtonCount - 1);
    if EditBoxWidth < ButtonGroupWidth then
      EditBoxWidth := ButtonGroupWidth;
    // calculate size of prompt
    SetRect(TextRect, 0, 0, EditBoxWidth, 0);
    DrawText(DC, PChar(APrompt), Length(APrompt), TextRect,
      DT_CALCRECT or DT_WORDBREAK or Dialog.DrawTextBiDiModeFlagsReadingOnly);
    TextEditHeight := TextRect.Bottom;
    OffsetRect(TextRect, HorzMargin, VertMargin);
    // create edit box
    if InputType = itMultiLineText then
      fEditBox := TMemo.Create(Dialog)
    else
    begin
      fEditBox := TEdit.Create(Dialog);
      if InputType in [itInteger, itFloat] then
      begin
        TEdit(fEditBox).NumbersOnly := True;
        if BiDiMode = bdRightToLeft then
          fEditBox.BiDiMode := bdRightToLeftNoAlign;
      end;
    end;
    EditBox.ReadOnly := ReadOnly;
    with EditBox do
    begin
      Name := 'EditBox';
      Parent := Dialog;
      Text := Value;
      Left := TextRect.Left;
      Top := TextRect.Bottom + tm.tmInternalLeading;
      Width := EditBoxWidth;
      if InputType = itMultiLineText then
      begin
        ClientHeight := (tm.tmHeight + tm.tmInternalLeading) * 15;
        TMemo(EditBox).WantReturns := True;
        TMemo(EditBox).WordWrap := False;
        TMemo(EditBox).ScrollBars := ssVertical;
      end;
      Inc(TextEditHeight, Height + tm.tmInternalLeading);
    end;
    // create prompt
    if APrompt <> '' then
      with TStaticText.Create(Dialog) do
      begin
        Name := 'EditBoxLabel';
        Parent := Dialog;
        AutoSize := False;
        Caption := APrompt;
        FocusControl := EditBox;
        BoundsRect := TextRect;
      end;
    // set dialog's size and adjust anchors
    Dialog.ClientWidth := EditBoxWidth + 2 * HorzMargin;
    Dialog.ClientHeight := TextEditHeight + 3 * VertMargin + ButtonHeight;
    Dialog.Constraints.MinWidth := ButtonWidth + ButtonGroupWidth + 2 *
      HorzMargin + Dialog.Width - Dialog.ClientWidth;
    Dialog.Constraints.MinHeight := Dialog.Height -
      (tm.tmHeight + tm.tmInternalLeading) * 10;
    EditBox.Anchors := [akLeft, akTop, akRight, akBottom];
    if Dialog.UseRightToLeftAlignment then
      ButtonAnchors := [akLeft, akBottom]
    else
      ButtonAnchors := [akRight, akBottom];
    // create buttons
    X := Dialog.ClientWidth - ButtonGroupWidth - HorzMargin;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then
      begin
        with TButton.Create(Dialog) do
        begin
          Name := ButtonNames[B];
          Parent := Dialog;
          Caption := ButtonCaptions[B];
          ModalResult := ModalResults[B];
          Anchors := ButtonAnchors;
          if B = mbOK then
            Default := True;
          if B = mbCancel then
            Cancel := True;
          if B = mbHelp then
            OnClick := HelpButtonClick;
          SetBounds(X, TextEditHeight + 2 * VertMargin, ButtonWidth,
            ButtonHeight);
          Inc(X, ButtonWidth + ButtonSpacing);
        end;
      end;
    // do the flip if needed
    if Dialog.UseRightToLeftAlignment then
      Dialog.FlipChildren(True);
    // show the dialog
    if Dialog.ShowModal = mrOk then
    begin
      Value := EditBox.Text;
      Result := True;
    end;
  finally
    FreeAndNil(fDialog);
  end;
end;

function TInputQueryDialog.Execute(const ACaption, APrompt: String;
  var Value: String; Multiline: Boolean; HelpContext: THelpContext): Boolean;
begin
  if Multiline then
    Result := Prompt(ACaption, APrompt, Value, itMultiLineText, HelpContext)
  else
    Result := Prompt(ACaption, APrompt, Value, itSingleLineText, HelpContext);
end;

function TInputQueryDialog.Execute(const ACaption, APrompt: String;
  var Value: Integer; HelpContext: THelpContext): Boolean;
var
  StrValue: String;
begin
  Result := False;
  StrValue := IntToStr(Value);
  if Prompt(ACaption, APrompt, StrValue, itInteger, HelpContext) then
  begin
    try
      Value := StrToInt(StrValue);
      Result := True;
    except
      // no exception
    end;
  end;
end;

function TInputQueryDialog.Execute(const ACaption, APrompt: String;
  var Value: Double; HelpContext: THelpContext): Boolean;
var
  StrValue: String;
begin
  Result := False;
  StrValue := Format('%g', [Value]);
  if Prompt(ACaption, APrompt, StrValue, itInteger, HelpContext) then
  begin
    try
      Value := StrToFloat(StrValue);
      Result := True;
    except
      // no exception
    end;
  end;
end;

end.
