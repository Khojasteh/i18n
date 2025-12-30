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
/// This unit implements classes and functions to format text in Rich Edit
/// controls using BBCode-style tags.
/// </summary>
unit i18nBBCode;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, Classes, Types, Contnrs, Windows, Messages, Graphics,
  RichEdit, ComCtrls;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This is a class reference for the <see cref="TBBCode"/> class or for one of
  /// its descendants.
  /// </summary>
  /// <remarks>
  /// TBBCodeClass is the metaclass for <see cref="TBBCode"/>. Its value is the
  /// class reference for <see cref="TBBCode"/> or for one of its descendants.
  /// </remarks>
  {$endregion}
  TBBCodeClass = class of TBBCode;

  {$region 'xmldoc'}
  /// <summary>
  /// This class is the base class for classes that represent a BBCode tag.
  /// </summary>
  /// <remarks>
  /// <para>
  /// TBBCode is an abstract class that cannot be instantiated. Descendant
  /// BBCode classes override many of the methods of the TBBCode class to address
  /// the particular character and paragraph format settings of the Rich Edit
  /// controls.
  /// </para>
  /// <para>
  /// To add support for a new BBCode tag, subclass the TBBCode class and override
  /// at least the <see cref="TBBCode.GetTagName"/> and <see cref="TBBCode.Prepare"/>
  /// methods. Then, register the new class using the <see cref="BBCodes.Register"/>
  /// procedure.
  /// </para>
  /// </remarks>
  {$endregion}
  TBBCode = class abstract(TObject)
  private
    fSelStart: Integer;
    fSelEnd: Integer;
    fAttribute: String;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the BBCode tag that is represented by this BBCode object.
    /// </summary>
    /// <returns>
    /// The BBCode tag.
    /// </returns>
    {$endregion}
    class function GetTagName: String; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the internal offset of a character in a Rich Edit control by
    /// considering CR/LF pair of characters as only one character.
    /// </summary>
    /// <param name="hRichEdit">
    /// Window handle of the Rich Edit control.
    /// </param>
    /// <param name="CharPos">
    /// The logical offset of the character.
    /// </param>
    /// <returns>
    /// The actual zero-based offset of the character.
    /// </returns>
    {$endregion}
    function RealCharPos(hRichEdit: THandle; CharPos: Integer): Integer; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Selects a range of characters in a Rich Edit control.
    /// </summary>
    /// <param name="hRichEdit">
    /// Window handle of the Rich Edit control.
    /// </param>
    /// <param name="ASelStart">
    /// Start of the selection range in character position.
    /// </param>
    /// <param name="ASelEnd">
    /// End of the selection range in character position.
    /// </param>
    {$endregion}
    procedure SelectRange(hRichEdit: THandle; ASelStart, ASelEnd: Integer); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the value of the paragraph and character format settings according to
    /// the BBCode requirements.
    /// </summary>
    /// <param name="ParaFormat">
    /// Paragraph format settings.
    /// </param>
    /// <param name="CharFormat">
    /// Character format settings.
    /// </param>
    {$endregion}
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the raw parameters of the BBCode tag.
    /// </summary>
    {$endregion}
    property Attribute: String read fAttribute write fAttribute;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Applies the BBCode to a Rich Edit control at a specified text range.
    /// </summary>
    /// <param name="hRichEdit">
    /// Window handle of the Rich Edit control.
    /// </param>
    /// <param name="ASelStart">
    /// Start of the selection range in character position.
    /// </param>
    /// <param name="ASelEnd">
    /// End of the selection range in character position.
    /// </param>
    {$endregion}
    procedure Apply(hRichEdit: THandle; ASelStart, ASelEnd: Integer); overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Applies the BBCode to a Rich Edit control at the text range specified
    /// by the <see cref="SelStart"/> and <see cref="SelEnd"/> properties.
    /// </summary>
    /// <param name="hRichEdit">
    /// Window handle of the Rich Edit control.
    /// </param>
    /// <param name="Offset">
    /// Optional amount of characters to offset the text range.
    /// </param>
    {$endregion}
    procedure Apply(hRichEdit: THandle; Offset: Integer = 0); overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// In a specified Rich Edit control, surrounds a specified text range with
    /// the BBCode's open and close tags.
    /// </summary>
    /// <param name="hRichEdit">
    /// Window handle of the Rich Edit control.
    /// </param>
    /// <param name="ASelStart">
    /// Start of the selection range in character position.
    /// </param>
    /// <param name="ASelEnd">
    /// End of the selection range in character position.
    /// </param>
    {$endregion}
    function Mark(hRichEdit: THandle; ASelStart, ASelEnd: Integer): Integer; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// In a specified Rich Edit control, surrounds the text range specified by
    /// the <see cref="SelStart"/> and <see cref="SelEnd"/> properties with the BBCode's
    /// open and close tags.
    /// </summary>
    /// <param name="hRichEdit">
    /// Window handle of the Rich Edit control.
    /// </param>
    /// <param name="Offset">
    /// Optional amount of characters to offset the text range.
    /// </param>
    {$endregion}
    function Mark(hRichEdit: THandle; Offset: Integer = 0): Integer; overload; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the BBCode tag name.
    /// </summary>
    {$endregion}
    property TagName: String read GetTagName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the start of the text range, in character position.
    /// </summary>
    {$endregion}
    property SelStart: Integer read fSelStart write fSelStart;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the end of the text range, in character position.
    /// </summary>
    {$endregion}
    property SelEnd: Integer read fSelEnd write fSelEnd;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TBBCodeObjectList maintains a list of (owned) <see cref="TBBCode"/> objects.
  /// </summary>
  /// <remarks>
  /// Use TBBCodeObjectList to store and maintain a list of <see cref="TBBCode"/>
  /// objects and provides properties and methods to add, delete, rearrange, locate,
  /// access, and sort them.
  /// </remarks>
  {$endregion}
  TBBCodeObjectList = class(TObjectList)
  private
    function GetItems(Index: Integer): TBBCode; inline;
    procedure SetItems(Index: Integer; Value: TBBCode);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the first <see cref="TBBCode"/> object in the <see cref="Items"/>
    /// array.
    /// </summary>
    /// <returns>
    /// The first item in the list.
    /// </returns>
    /// <seealso cref="Last"/>
    {$endregion}
    function First: TBBCode; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the last <see cref="TBBCode"/> object in the
    /// <see cref="Items"/> array.
    /// </summary>
    /// <returns>
    /// The last item in the list.
    /// </returns>
    /// <seealso cref="First"/>
    {$endregion}
    function Last: TBBCode; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Inserts the <paramref name="Item"/> parameter at the end of the
    /// <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TBBCode"/> object to add.
    /// </param>
    /// <returns>
    /// The index of the new item.
    /// </returns>
    /// <seealso cref="Insert"/>
    {$endregion}
    function Add(Item: TBBCode): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Inserts the <paramref name="Item"/> parameter in the
    /// <see cref="Items"/> array, at the position specified
    /// by <paramref name="Index"/>.
    /// </summary>
    /// <param name="Index">
    /// Position of the item in the <see cref="Items"/> array.
    /// </param>
    /// <param name="Item">
    /// The <see cref="TBBCode"/> object to insert.
    /// </param>
    /// <seealso cref="Add"/>
    {$endregion}
    procedure Insert(Index: Integer; Item: TBBCode); inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes the first reference to the <paramref name="Item"/> parameter from
    /// the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TBBCode"/> object to remove.
    /// </param>
    /// <returns>
    /// The index of the removed item, or -1 if the item is not found.
    /// </returns>
    /// <seealso cref="RemoveItem"/>
    {$endregion}
    function Remove(Item: TBBCode): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes the first or last reference to the <paramref name="Item"/> parameter
    /// from the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TBBCode"/> object to remove.
    /// </param>
    /// <param name="Direction">
    /// The direction of search for locating the item.
    /// </param>
    /// <returns>
    /// The index of the removed item, or -1 if the item is not found.
    /// </returns>
    /// <seealso cref="Remove"/>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    function RemoveItem(Item: TBBCode; Direction: TList.TDirection): Integer;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes the first reference to the <paramref name="Item"/> parameter from
    /// the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TBBCode"/> object to extract.
    /// </param>
    /// <returns>
    /// The extracted item, or <see langword="nil"/> if the item is not found.
    /// </returns>
    /// <seealso cref="ExtractItem"/>
    {$endregion}
    function Extract(Item: TBBCode): TBBCode; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes the first or last reference to the <paramref name="Item"/> parameter
    /// from the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TBBCode"/> object to extract.
    /// </param>
    /// <param name="Direction">
    /// The direction of search for locating the item.
    /// </param>
    /// <returns>
    /// The extracted item, or <see langword="nil"/> if the item is not found.
    /// </returns>
    /// <seealso cref="Extract"/>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    function ExtractItem(Item: TBBCode; Direction: TList.TDirection): TBBCode; inline;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the index of the first reference to the <paramref name="Item"/> parameter
    /// in the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TBBCode"/> object to locate.
    /// </param>
    /// <returns>
    /// The index of the item, or -1 if the item is not found.
    /// </returns>
    /// <seealso cref="IndexOfItem"/>
    {$endregion}
    function IndexOf(Item: TBBCode): Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the index of the first or last reference to the <paramref name="Item"/>
    /// parameter in the <see cref="Items"/> array.
    /// </summary>
    /// <param name="Item">
    /// The <see cref="TBBCode"/> object to locate.
    /// </param>
    /// <param name="Direction">
    /// The direction of search for locating the item.
    /// </param>
    /// <returns>
    /// The index of the item, or -1 if the item is not found.
    /// </returns>
    /// <seealso cref="IndexOf"/>
    {$endregion}
    {$IFDEF COMPILER2010_UP}
    function IndexOfItem(Item: TBBCode; Direction: TList.TDirection): Integer; inline;
    {$ENDIF}
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TBBCode"/> objects.
    /// </summary>
    {$endregion}
    property Items[Index: Integer]: TBBCode read GetItems write SetItems; default;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class is the base class for classes that represent a BBCode tag that
  /// toggles a feature on or off.
  /// </summary>
  /// <remarks>
  /// TSwitchableBBCode is an abstract class that cannot be instantiated. This class
  /// is the ancestor for BBCode classes that represent a BBCode tag, which can be toggled
  /// on or off using an optional parameter.
  /// </remarks>
  {$endregion}
  TSwitchableBBCode = class abstract(TBBCode)
  private
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the BBCode is on or off.
    /// </summary>
    {$endregion}
    property Active: Boolean read GetActive write SetActive;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the bold attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeBold class represents the <c>[B]</c> BBCode tag.
  /// <para>
  /// The <c>[B]</c> BBCode tag accepts one optional parameter, that can be one
  /// of the values listed in the following table:
  /// </para>
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Marks characters as bold (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the bold formatting of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeBold = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the italic attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeItalic class represents the <c>[I]</c> BBCode tag.
  /// <para>
  /// The <c>[I]</c> BBCode tag accepts one optional parameter, that can be one
  /// of the values listed in the following table:
  /// </para>
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Marks characters as italic (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the italic format of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeItalic = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the underline attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeUnderline class represents the <c>[U]</c> BBCode tag.
  /// <para>
  /// The <c>[U]</c> BBCode tag accepts two optional parameters, separated by comma.
  /// The order of parameters is not important.
  /// </para>
  /// <para>
  /// The <c>type</c> parameter determines the type of underline and can be one of the
  /// following values:
  /// </para>
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>None</term>
  ///     <description>No underline</description>
  ///   </item>
  ///   <item>
  ///     <term>Normal</term>
  ///     <description>Single-line solid underline (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Word</term>
  ///     <description>Single-line underline broken between words</description>
  ///   </item>
  ///   <item>
  ///     <term>Double</term>
  ///     <description>Double-line underline</description>
  ///   </item>
  ///   <item>
  ///     <term>Dotted</term>
  ///     <description>Dotted pattern underline</description>
  ///   </item>
  ///   <item>
  ///     <term>Dash</term>
  ///     <description>Dashed pattern underline</description>
  ///   </item>
  ///   <item>
  ///     <term>DashDot</term>
  ///     <description>Dash-dot pattern underline</description>
  ///   </item>
  ///   <item>
  ///     <term>DashDotDot</term>
  ///     <description>Dash-dot-dot pattern underline</description>
  ///   </item>
  ///   <item>
  ///     <term>Wave</term>
  ///     <description>Single-line wave style underline</description>
  ///   </item>
  ///   <item>
  ///     <term>Thick</term>
  ///     <description>Single-line solid underline with extra thickness</description>
  ///   </item>
  ///   <item>
  ///     <term>HairLine</term>
  ///     <description>Single-line solid underline with less thickness</description>
  ///   </item>
  ///   <item>
  ///     <term>DoubleWave</term>
  ///     <description>Double-line wave style underline</description>
  ///   </item>
  ///   <item>
  ///     <term>HeavyWave</term>
  ///     <description>Single-line wave style underline with extra thickness</description>
  ///   </item>
  ///   <item>
  ///     <term>LongDash</term>
  ///     <description>Long dash pattern underline</description>
  ///   </item>
  /// </list>
  /// <para>
  /// The <c>color</c> parameter determines the color of the underline and can be one of the
  /// following values:
  /// </para>
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>Auto</term>
  ///     <description>Text's foreground color (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Blue</term>
  ///     <description>Blue</description>
  ///   </item>
  ///   <item>
  ///     <term>Aqua</term>
  ///     <description>Aqua</description>
  ///   </item>
  ///   <item>
  ///     <term>Lime</term>
  ///     <description>Lime</description>
  ///   </item>
  ///   <item>
  ///     <term>Fuchsia</term>
  ///     <description>Fuchsia</description>
  ///   </item>
  ///   <item>
  ///     <term>Red</term>
  ///     <description>Red</description>
  ///   </item>
  ///   <item>
  ///     <term>Yellow</term>
  ///     <description>Yellow</description>
  ///   </item>
  ///   <item>
  ///     <term>White</term>
  ///     <description>White</description>
  ///   </item>
  ///   <item>
  ///     <term>Navy</term>
  ///     <description>Navy</description>
  ///   </item>
  ///   <item>
  ///     <term>Teal</term>
  ///     <description>Teal</description>
  ///   </item>
  ///   <item>
  ///     <term>Green</term>
  ///     <description>Green</description>
  ///   </item>
  ///   <item>
  ///     <term>Purple</term>
  ///     <description>Purple</description>
  ///   </item>
  ///   <item>
  ///     <term>Maroon</term>
  ///     <description>Maroon</description>
  ///   </item>
  ///   <item>
  ///     <term>Olive</term>
  ///     <description>Olive</description>
  ///   </item>
  ///   <item>
  ///     <term>Gray</term>
  ///     <description>Gray</description>
  ///   </item>
  ///   <item>
  ///     <term>Silver</term>
  ///     <description>Silver</description>
  ///   </item>
  /// </list>
  /// <para>
  /// Note: Some underline types may have no effect on some versions of
  /// the Rich Edit control.
  /// </para>
  /// </remarks>
  {$endregion}
  TBBCodeUnderline = class(TBBCode)
  public
    type
      {$region 'xmldoc'}
      /// <summary>
      /// This enumeration type identifies the styles of an underline.
      /// </summary>
      {$endregion}
      TUnderlineType = (
        {$region 'xmldoc'}
        /// <summary>
        /// No underline
        /// </summary>
        {$endregion}
        ulNone,
        {$region 'xmldoc'}
        /// <summary>
        /// Single-line solid underline
        /// </summary>
        {$endregion}
        ulNormal,
        {$region 'xmldoc'}
        /// <summary>
        /// Single-line underline broken between words
        /// </summary>
        {$endregion}
        ulWord,
        {$region 'xmldoc'}
        /// <summary>
        /// Double-line underline
        /// </summary>
        {$endregion}
        ulDouble,
        {$region 'xmldoc'}
        /// <summary>
        /// Dotted pattern underline
        /// </summary>
        {$endregion}
        ulDotted,
        {$region 'xmldoc'}
        /// <summary>
        /// Dashed pattern underline
        /// </summary>
        {$endregion}
        ulDash,
        {$region 'xmldoc'}
        /// <summary>
        /// Dash-dot pattern underline
        /// </summary>
        {$endregion}
        ulDashDot,
        {$region 'xmldoc'}
        /// <summary>
        /// Dash-dot-dot pattern underline
        /// </summary>
        {$endregion}
        ulDashDotDot,
        {$region 'xmldoc'}
        /// <summary>
        /// Single-line wave style underline
        /// </summary>
        {$endregion}
        ulWave,
        {$region 'xmldoc'}
        /// <summary>
        /// Single-line solid underline with extra thickness
        /// </summary>
        {$endregion}
        ulThick,
        {$region 'xmldoc'}
        /// <summary>
        /// Single-line solid underline with less thickness
        /// </summary>
        {$endregion}
        ulHairLine,
        {$region 'xmldoc'}
        /// <summary>
        /// Double-line wave style underline
        /// </summary>
        {$endregion}
        ulDoubleWave,
        {$region 'xmldoc'}
        /// <summary>
        /// Single-line wave style underline with extra thickness
        /// </summary>
        {$endregion}
        ulHeavyWave,
        {$region 'xmldoc'}
        /// <summary>
        /// Long dash pattern underline
        /// </summary>
        {$endregion}
        ulLongDash
      );
      {$region 'xmldoc'}
      /// <summary>
      /// This enumeration type identifies the colors of an underline.
      /// </summary>
      {$endregion}
      TUnderlineColor = (
        {$region 'xmldoc'}
        /// <summary>
        /// Same color as the foreground color
        /// </summary>
        {$endregion}
        ucAuto,
        {$region 'xmldoc'}
        /// <summary>
        /// Blue color
        /// </summary>
        {$endregion}
        ucBlue,
        {$region 'xmldoc'}
        /// <summary>
        /// Aqua color
        /// </summary>
        {$endregion}
        ucAqua,
        {$region 'xmldoc'}
        /// <summary>
        /// Lime color
        /// </summary>
        {$endregion}
        ucLime,
        {$region 'xmldoc'}
        /// <summary>
        /// Fuchsia color
        /// </summary>
        {$endregion}
        ucFuchsia,
        {$region 'xmldoc'}
        /// <summary>
        /// Red color
        /// </summary>
        {$endregion}
        ucRed,
        {$region 'xmldoc'}
        /// <summary>
        /// Yellow color
        /// </summary>
        {$endregion}
        ucYellow,
        {$region 'xmldoc'}
        /// <summary>
        /// White color
        /// </summary>
        {$endregion}
        ucWhite,
        {$region 'xmldoc'}
        /// <summary>
        /// Navy color
        /// </summary>
        {$endregion}
        ucNavy,
        {$region 'xmldoc'}
        /// <summary>
        /// Teal color
        /// </summary>
        {$endregion}
        ucTeal,
        {$region 'xmldoc'}
        /// <summary>
        /// Green color
        /// </summary>
        {$endregion}
        ucGreen,
        {$region 'xmldoc'}
        /// <summary>
        /// Purple color
        /// </summary>
        {$endregion}
        ucPurple,
        {$region 'xmldoc'}
        /// <summary>
        /// Maroon color
        /// </summary>
        {$endregion}
        ucMaroon,
        {$region 'xmldoc'}
        /// <summary>
        /// Olive color
        /// </summary>
        {$endregion}
        ucOlive,
        {$region 'xmldoc'}
        /// <summary>
        /// Gray color
        /// </summary>
        {$endregion}
        ucGray,
        {$region 'xmldoc'}
        /// <summary>
        /// Silver color
        /// </summary>
        {$endregion}
        ucSilver
      );
  private
    function GetUnderLineType: TUnderlineType;
    procedure SetUnderlineType(Value: TUnderlineType);
    function GetUnderLineColor: TUnderlineColor;
    procedure SetUnderlineColor(Value: TUnderlineColor);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the type of underline.
    /// </summary>
    {$endregion}
    property UnderlineType: TUnderlineType read GetUnderLineType write SetUnderlineType;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the color of the underline.
    /// </summary>
    {$endregion}
    property UnderlineColor: TUnderlineColor read GetUnderLineColor write SetUnderlineColor;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the strikeout attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeStrikeout class represents the <c>[S]</c> BBCode tag.
  /// <para>
  /// The <c>[S]</c> BBCode tag accepts one optional parameter, that can be one
  /// of the values listed in the following table:
  /// </para>
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Marks characters with a line straight through the middle (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the strikeout format of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeStrikeout = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the font name attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeFontFace class represents the <c>[FONT]</c> BBCode tag.
  /// The <c>[FONT]</c> BBCode tag has one required parameter, which is the name of
  /// the font to use for displaying the text.
  /// </remarks>
  {$endregion}
  TBBCodeFontFace = class(TBBCode)
  private
    function GetFontName: String;
    procedure SetFontName(const Value: String);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the name of font.
    /// </summary>
    {$endregion}
    property FontName: String read GetFontName write SetFontName;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the font size attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeFontSize class represents the <c>[SIZE]</c> BBCode tag.
  /// The <c>[SIZE]</c> BBCode tag has one required parameter, which is an integer
  /// value that determines the size of the font to use for displaying the text.
  /// </remarks>
  {$endregion}
  TBBCodeFontSize = class(TBBCode)
  private
    function GetSize: Integer;
    procedure SetSize(Value: Integer);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the size of font, in points.
    /// </summary>
    {$endregion}
    property Size: Integer read GetSize write SetSize;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the foreground color attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeFontColor class represents the <c>[COLOR]</c> BBCode tag.
  /// The <c>[COLOR]</c> BBCode tag has one required parameter, which is the foreground
  /// color to use for displaying the text.
  /// The color can be expressed by name (e.g. Red), web format (e.g. #F00 or #FF0000),
  /// or hexadecimal value (e.g. $000000FF).
  /// </remarks>
  {$endregion}
  TBBCodeFontColor = class(TBBCode)
  private
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the foreground color.
    /// </summary>
    {$endregion}
    property Color: TColor read GetColor write SetColor;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the background color attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeBackColor class represents the <c>[BACKCOLOR]</c> BBCode tag.
  /// The <c>[BACKCOLOR]</c> BBCode tag has one required parameter, which is the background
  /// color to use for displaying the text.
  /// The color can be expressed by name (e.g. Red), web format (e.g. #F00 or #FF0000),
  /// or hexadecimal value (e.g. $000000FF).
  /// </remarks>
  {$endregion}
  TBBCodeBackColor = class(TBBCode)
  private
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the background color.
    /// </summary>
    {$endregion}
    property Color: TColor read GetColor write SetColor;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the link attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeLink class represents the <c>[LINK]</c> BBCode tag.
  /// A click on the link will cause the Rich Edit control to generate a notification
  /// of EN_LINK.
  /// The <c>[LINK]</c> BBCode tag accepts one optional parameter, that can be one
  /// of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Marks up the text as a link (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Breaks the link</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeLink = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the subscript attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeSubscript class represents the <c>[SUB]</c> BBCode tag.
  /// The <c>[SUB]</c> BBCode tag accepts one optional parameter, that can be one
  /// of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Marks characters as subscript (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the subscript attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeSubscript = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the superscript attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeSuperscript class represents the <c>[SUP]</c> BBCode tag.
  /// The <c>[SUP]</c> BBCode tag accepts one optional parameter, that can be one
  /// of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Marks characters as superscript (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the superscript attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeSuperscript = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the disabled attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeDisabled class represents the <c>[DISABLED]</c> BBCode tag.
  /// The <c>[DISABLED]</c> BBCode tag accepts one optional parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Displays characters as disabled (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the disabled attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeDisabled = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the protected attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeProtected class represents the <c>[PROTECTED]</c> BBCode tag.
  /// An attempt to modify protected text will cause the Rich Edit control to generate
  /// a notification of EN_PROTECTED.
  /// The <c>[PROTECTED]</c> BBCode tag accepts one optional parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Marks characters as protected (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the protected attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeProtected = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the hidden attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeHidden class represents the <c>[HIDDEN]</c> BBCode tag.
  /// The <c>[HIDDEN]</c> BBCode tag accepts one optional parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Does not display characters (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the hidden attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeHidden = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the outline attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeOutline class represents the <c>[OUTLINE]</c> BBCode tag.
  /// The <c>[OUTLINE]</c> BBCode tag accepts one optional parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Displays characters as outlined characters (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the outline attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeOutline = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the shadow attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeShadow class represents the <c>[SHADOW]</c> BBCode tag.
  /// The <c>[SHADOW]</c> BBCode tag accepts one optional parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Displays characters as shadowed characters (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the shadow attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeShadow = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the emboss attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeEmboss class represents the <c>[EMBOSS]</c> BBCode tag.
  /// The <c>[EMBOSS]</c> BBCode tag accepts one optional parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Displays characters as embossed characters (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the emboss attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeEmboss = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the imprint attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeImprint class represents the <c>[IMPRINT]</c> BBCode tag.
  /// The <c>[IMPRINT]</c> BBCode tag accepts one optional parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Displays characters as imprinted characters (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the imprint attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeImprint = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the small caps attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeSmallCaps class represents the <c>[SMALLCAPS]</c> BBCode tag.
  /// The <c>[SMALLCAPS]</c> BBCode tag accepts one optional parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Displays characters in small capital letters (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the small caps attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeSmallCaps = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the all caps attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeAllCaps class represents the <c>[ALLCAPS]</c> BBCode tag.
  /// The <c>[ALLCAPS]</c> BBCode tag accepts one optional parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Displays characters all in capital letters (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the all caps attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeAllCaps = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the revised attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeRevised class represents the <c>[REVISED]</c> BBCode tag.
  /// The <c>[REVISED]</c> BBCode tag accepts one optional parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>On | Yes</term>
  ///     <description>Marks up the text as revised (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Off | No</term>
  ///     <description>Removes the revised attribute of the characters</description>
  ///   </item>
  /// </list>
  /// </remarks>
  /// <seealso cref="TBBCodeRevAuthor"/>
  {$endregion}
  TBBCodeRevised = class(TSwitchableBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the revision author attribute of characters in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeRevAuthor class represents the <c>[AUTHOR]</c> BBCode tag.
  /// The <c>[AUTHOR]</c> BBCode tag has one required parameter, which is an index that
  /// identifies the author making a revision. The Rich Edit control uses different text
  /// colors for each different author index. The index can be a value between 0 and 255.
  /// </remarks>
  /// <seealso cref="TBBCodeRevised"/>
  {$endregion}
  TBBCodeRevAuthor = class(TBBCode)
  private
    function GetAuthor: Byte;
    procedure SetAuthor(Value: Byte);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the index that identifies the author making a revision.
    /// </summary>
    {$endregion}
    property Author: Byte read GetAuthor write SetAuthor;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class sets the font of characters in a Rich Edit control to a monospace font.
  /// </summary>
  /// <remarks>
  /// TBBCodeCode class represents the <c>[CODE]</c> BBCode tag.
  /// The <c>[CODE]</c> BBCode tag has no parameter. Characters marked with this tag will
  /// be displayed using monospace font.
  /// </remarks>
  /// <seealso cref="TBBCodeFontFace"/>
  {$endregion}
  TBBCodeCode = class(TBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class centers paragraphs in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeCenter class represents the <c>[CENTER]</c> BBCode tag.
  /// The <c>[CENTER]</c> BBCode tag has no parameter. Paragraphs marked with this tag will
  /// be centered.
  /// </remarks>
  /// <seealso cref="TBBCodeAlign"/>
  {$endregion}
  TBBCodeCenter = class(TBBCode)
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the alignment of paragraphs in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeAlign class represents the <c>[ALIGN]</c> BBCode tag.
  /// The <c>[ALIGN]</c> BBCode tag has one required parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>Left</term>
  ///     <description>Paragraphs are aligned with the left margin</description>
  ///   </item>
  ///   <item>
  ///     <term>Right</term>
  ///     <description>Paragraphs are aligned with the right margin</description>
  ///   </item>
  ///   <item>
  ///     <term>Center</term>
  ///     <description>Paragraphs are centered</description>
  ///   </item>
  ///   <item>
  ///     <term>Justify</term>
  ///     <description>Paragraphs are justified (some versions of Rich Edit control support this value)</description>
  ///   </item>
  ///   <item>
  ///     <term>FullInterWord</term>
  ///     <description>Paragraphs are justified by expanding the blanks alone</description>
  ///   </item>
  /// </list>
  /// </remarks>
  /// <seealso cref="TBBCodeCenter"/>
  {$endregion}
  TBBCodeAlign = class(TBBCode)
  public
    type
      {$region 'xmldoc'}
      /// <summary>
      /// This enumeration type identifies the alignments of text for paragraphs.
      /// </summary>
      {$endregion}
      TParaAlign = (
        {$region 'xmldoc'}
        /// Left aligned text
        {$endregion}
        paLeft,
        {$region 'xmldoc'}
        /// Right aligned text
        {$endregion}
        paRight,
        {$region 'xmldoc'}
        /// Centered text
        {$endregion}
        paCenter,
        {$region 'xmldoc'}
        /// Justified text
        {$endregion}
        paJustify,
        {$region 'xmldoc'}
        /// Justified text by expanding the blanks alone
        {$endregion}
        paFullInterWord
      );
  private
    function GetAlignment: TParaAlign;
    procedure SetAlignment(Value: TParaAlign);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the alignment for the paragraphs.
    /// </summary>
    {$endregion}
    property Alignment: TParaAlign read GetAlignment write SetAlignment;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the line spacing of paragraphs in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeLineSpacing class represents the <c>[LS]</c> BBCode tag.
  /// The <c>[LS]</c> BBCode tag has one required parameter, that can be
  /// one of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>1</term>
  ///     <description>Single spacing</description>
  ///   </item>
  ///   <item>
  ///     <term>1.5</term>
  ///     <description>One-and-a-half spacing</description>
  ///   </item>
  ///   <item>
  ///     <term>2</term>
  ///     <description>Double spacing</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeLineSpacing = class(TBBCode)
  public
    type
      {$region 'xmldoc'}
      /// <summary>
      /// This enumeration type identifies the types of line spacing for paragraphs.
      /// </summary>
      {$endregion}
      TLineSpacing = (
        {$region 'xmldoc'}
        /// Single spacing
        {$endregion}
        lsSingle,
        {$region 'xmldoc'}
        /// One-and-a-half spacing
        {$endregion}
        lsOneAndHalf,
        {$region 'xmldoc'}
        /// Double spacing
        {$endregion}
        lsDouble
      );
  private
    function GetSpacing: TLineSpacing;
    procedure SetSpacing(Value: TLineSpacing);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the line spacing for the paragraphs.
    /// </summary>
    {$endregion}
    property Spacing: TLineSpacing read GetSpacing write SetSpacing;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the amount of spacing before paragraphs in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeParagraphSpaceBefore class represents the <c>[PSB]</c> BBCode tag.
  /// The <c>[PSB]</c> BBCode tag has one required parameter, which is an integer value
  /// that represents the amount of spacing before paragraphs, in points.
  /// </remarks>
  /// <seealso cref="TBBCodeParagraphSpaceAfter"/>
  {$endregion}
  TBBCodeParagraphSpaceBefore = class(TBBCode)
  private
    function GetSpacing: Integer;
    procedure SetSpacing(Value: Integer);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the amount of spacing, in points.
    /// </summary>
    {$endregion}
    property Spacing: Integer read GetSpacing write SetSpacing;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the amount of spacing after paragraphs in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeParagraphSpaceAfter class represents the <c>[PSA]</c> BBCode tag.
  /// The <c>[PSA]</c> BBCode tag has one required parameter, which is an integer value
  /// that represents the amount of spacing after paragraphs, in points.
  /// </remarks>
  /// <seealso cref="TBBCodeParagraphSpaceBefore"/>
  {$endregion}
  TBBCodeParagraphSpaceAfter = class(TBBCode)
  private
    function GetSpacing: Integer;
    procedure SetSpacing(Value: Integer);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the amount of spacing, in points.
    /// </summary>
    {$endregion}
    property Spacing: Integer read GetSpacing write SetSpacing;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the amount of indent for paragraphs in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeIndent class represents the <c>[INDENT]</c> BBCode tag.
  /// The <c>[INDENT]</c> BBCode tag has one required parameter, which is an integer value
  /// that represents the amount of indent, in points.
  /// </remarks>
  /// <seealso cref="TBBCodeRightIndent"/>
  {$endregion}
  TBBCodeIndent = class(TBBCode)
  private
    function GetIndent: Integer;
    procedure SetIndent(Value: Integer);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the amount of indent, in points.
    /// </summary>
    {$endregion}
    property Indent: Integer read GetIndent write SetIndent;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the amount of right indent for paragraphs in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeRightIndent class represents the <c>[RINDENT]</c> BBCode tag.
  /// The <c>[RINDENT]</c> BBCode tag has one required parameter, which is an integer value
  /// that represents the amount of right indent, in points.
  /// </remarks>
  /// <seealso cref="TBBCodeIndent"/>
  {$endregion}
  TBBCodeRightIndent = class(TBBCode)
  private
    function GetIndent: Integer;
    procedure SetIndent(Value: Integer);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the amount of indent, in points.
    /// </summary>
    {$endregion}
    property Indent: Integer read GetIndent write SetIndent;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the text direction of paragraphs in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeDir class represents the <c>[DIR]</c> BBCode tag.
  /// The <c>[DIR]</c> BBCode tag has one required parameter, that can be one
  /// of the values listed in the following table:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>LTR</term>
  ///     <description>Displays text in left-to-right direction</description>
  ///   </item>
  ///   <item>
  ///     <term>RTL</term>
  ///     <description>Displays text in right-to-left direction</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  TBBCodeDir = class(TBBCode)
  public
    type
      {$region 'xmldoc'}
      /// <summary>
      /// The TTextDirection determines the direction of text for bi-directional languages.
      /// </summary>
      {$endregion}
      TTextDirection = (
        {$region 'xmldoc'}
        /// Left-To-Right direction
        {$endregion}
        LTR,
        {$region 'xmldoc'}
        /// Right-To-Left direction
        {$endregion}
        RTL
      );
  private
    function GetDirection: TTextDirection;
    procedure SetDirection(Value: TTextDirection);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the direction of text.
    /// </summary>
    {$endregion}
    property Direction: TTextDirection read GetDirection write SetDirection;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls bulleted or numbered paragraphs in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeList class represents the <c>[LIST]</c> BBCode tag.
  /// The <c>[LIST]</c> BBCode tag accepts four optional parameters: <c>type</c>,
  /// <c>start</c>, <c>style</c>, and <c>tab</c>. The parameters are separated by
  /// comma and their order of appearance in the parameter list is not important.
  /// The <c>type</c> parameter specifies whether the list is bulleted or numbered. And,
  /// if the list is numbered, which numbering type is used. The <c>type</c> parameter
  /// can have one of the following values:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>None</term>
  ///     <description>No paragraph numbering or bullets</description>
  ///   </item>
  ///   <item>
  ///     <term>Bullet</term>
  ///     <description>Uses bullets (default)</description>
  ///   </item>
  ///   <item>
  ///     <term>Number</term>
  ///     <description>Uses Arabic numbers (1, 2, 3, ...)</description>
  ///   </item>
  ///   <item>
  ///     <term>LowerLetter</term>
  ///     <description>Uses lowercase letters (a, b, c, ...)</description>
  ///   </item>
  ///   <item>
  ///     <term>UpperLetter</term>
  ///     <description>Uses uppercase letters (A, B, C, ...)</description>
  ///   </item>
  ///   <item>
  ///     <term>LowerRoman</term>
  ///     <description>Uses lowercase Roman numerals (i, ii, iii, ...)</description>
  ///   </item>
  ///   <item>
  ///     <term>UpperRoman</term>
  ///     <description>Uses uppercase Roman numerals (I, II, III, ...)</description>
  ///   </item>
  ///   <item>
  ///     <term>Custom</term>
  ///     <description>Uses a sequence of characters beginning with the specified Unicode character</description>
  ///   </item>
  /// </list>
  /// The <c>start</c> parameter specifies the first number for numbering the list. If
  /// the <c>type</c> parameter is custom, the <c>start</c> parameter indicates the
  /// Unicode character that is the first character in the sequence of characters for
  /// numbering. If the <c>type</c> parameter is omitted, the <c>start</c> parameter
  /// can be any character to represent both numbering type and start.
  /// The <c>style</c> parameter specifies the style of numbering lists, and can have
  /// one of the following values:
  /// <list type="table">
  ///   <listheader>
  ///     <term>Value</term>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>Parenthesis</term>
  ///     <description>Follows the number with a period (default).</description>
  ///   </item>
  ///   <item>
  ///     <term>Enclosed</term>
  ///     <description>Encloses the number in parentheses.</description>
  ///   </item>
  ///   <item>
  ///     <term>Period</term>
  ///     <description>Follows the number with a period.</description>
  ///   </item>
  ///   <item>
  ///     <term>Simple</term>
  ///     <description>Displays only the number.</description>
  ///   </item>
  /// </list>
  /// The <c>tab</c> parameter is an integer value followed by an at sign (@) to specify
  /// the minimum space between a paragraph bullet or number and the paragraph text, in
  /// points.
  /// Note: Some numbering types may have no effect on some versions of Rich Edit control.
  /// </remarks>
  {$endregion}
  TBBCodeList = class(TBBCode)
  public
    type
      {$region 'xmldoc'}
      /// <summary>
      /// This enumeration type identifies the types of numbering for a list.
      /// </summary>
      {$endregion}
      TListNumbering = (
        {$region 'xmldoc'}
        /// No paragraph numbering or bullets
        {$endregion}
        lnNone,
        {$region 'xmldoc'}
        /// Uses bullets
        {$endregion}
        lnBullet,
        {$region 'xmldoc'}
        /// Uses Arabic numbers (1, 2, 3, ...)
        {$endregion}
        lnNumber,
        {$region 'xmldoc'}
        /// Uses lowercase letters (a, b, c, ...)
        {$endregion}
        lnLowerLetter,
        {$region 'xmldoc'}
        /// Uses uppercase letters (A, B, C, ...)
        {$endregion}
        lnUpperLetter,
        {$region 'xmldoc'}
        /// Uses lowercase Roman numerals (i, ii, iii, ...)
        {$endregion}
        lnLowerRoman,
        {$region 'xmldoc'}
        /// Uses uppercase Roman numerals (I, II, III, ...)
        {$endregion}
        lnUpperRoman,
        {$region 'xmldoc'}
        /// Uses a sequence of characters beginning with the specified Unicode character
        {$endregion}
        lnCustom
      );
      {$region 'xmldoc'}
      /// <summary>
      /// This enumeration type identifies the styles of numbering for a list.
      /// </summary>
      {$endregion}
      TListNumberingStyle = (
        {$region 'xmldoc'}
        /// Follows the number with a right parenthesis
        {$endregion}
        nsParenthesis,
        {$region 'xmldoc'}
        /// Encloses the number in parentheses
        {$endregion}
        nsEnclosed,
        {$region 'xmldoc'}
        /// Follows the number with a period
        {$endregion}
        nsPeriod,
        {$region 'xmldoc'}
        /// Displays only the number
        {$endregion}
        nsSimple
      );
  private
    function GetNumbering: TListNumbering;
    procedure SetNumbering(Value: TListNumbering);
    function GetStyle: TListNumberingStyle;
    procedure SetStyle(Value: TListNumberingStyle);
    function GetStart: Word;
    procedure SetStart(Value: Word);
    function GetTab: Integer;
    procedure SetTab(Value: Integer);
    procedure DecodeAttribute(out TheNumbering: TListNumbering; out TheStart: Word;
      out TheStyle: TListNumberingStyle; out TheTab: Integer);
    procedure EncodeAttribute(TheNumbering: TListNumbering; TheStart: Word;
      TheStyle: TListNumberingStyle; TheTab: Integer);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the list is bulleted or numbered, and which numbering type is used.
    /// </summary>
    {$endregion}
    property Numbering: TListNumbering read GetNumbering write SetNumbering;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the style of the numbering.
    /// </summary>
    {$endregion}
    property Style: TListNumberingStyle read GetStyle write SetStyle;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the starting number or Unicode value used for the numbering.
    /// </summary>
    {$endregion}
    property Start: Word read GetStart write SetStart;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the minimum space between a paragraph number and the paragraph text, in points.
    /// </summary>
    {$endregion}
    property Tab: Integer read GetTab write SetTab;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This class controls the tab stops in a Rich Edit control.
  /// </summary>
  /// <remarks>
  /// TBBCodeTabs class represents the <c>[TABS]</c> BBCode tag.
  /// The <c>[TABS]</c> BBCode tag needs one or more integer values as the parameter,
  /// separated by comma. Each value specifies the absolute tab stop, in points.
  /// </remarks>
  {$endregion}
  TBBCodeTabs = class(TBBCode)
  private
    function GetTabStops: String;
    procedure SetTabStops(const Value: String);
  protected
    class function GetTagName: String; override;
    procedure Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the comma separated list of tab stops, in points.
    /// </summary>
    {$endregion}
    property TabStops: String read GetTabStops write SetTabStops;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// BBCodes class provides some class methods to set content of the Rich Edit
  /// controls using BBCode formatted text.
  /// </summary>
  /// <remarks>
  /// Use methods of the BBCodes class to display BBCode formatted text on the Rich
  /// Edit control.
  /// <para>
  /// The following table lists the recognized BBCode tags. Case is ignored in tags.
  /// To know about the attributes of each tag, please refer to the documentation
  /// of its class.
  /// </para>
  /// <list type="table">
  ///   <listheader>
  ///     <term>BBCode Tag</term>
  ///     <description>Class</description>
  ///     <description>Description</description>
  ///   </listheader>
  ///   <item>
  ///     <term>B</term>
  ///     <description><see cref="TBBCodeBold"/></description>
  ///     <description>Displays text as bold.</description>
  ///   </item>
  ///   <item>
  ///     <term>I</term>
  ///     <description><see cref="TBBCodeItalic"/></description>
  ///     <description>Displays the text as italic.</description>
  ///   </item>
  ///   <item>
  ///     <term>U</term>
  ///     <description><see cref="TBBCodeUnderline"/></description>
  ///     <description>Displays the text with underline.</description>
  ///   </item>
  ///   <item>
  ///     <term>Strikeout</term>
  ///     <description><see cref="TBBCodeStrikeout"/></description>
  ///     <description>Displays the text with a line straight through the middle.</description>
  ///   </item>
  ///   <item>
  ///     <term>FONT</term>
  ///     <description><see cref="TBBCodeFontFace"/></description>
  ///     <description>Determines the name of the font to use for displaying the text.</description>
  ///   </item>
  ///   <item>
  ///     <term>SIZE</term>
  ///     <description><see cref="TBBCodeFontSize"/></description>
  ///     <description>Determines the size of the font to use for displaying the text.</description>
  ///   </item>
  ///   <item>
  ///     <term>COLOR</term>
  ///     <description><see cref="TBBCodeFontColor"/></description>
  ///     <description>Determines the color of the font to use for displaying the text.</description>
  ///   </item>
  ///   <item>
  ///     <term>BACKCOLOR</term>
  ///     <description><see cref="TBBCodeBackColor"/></description>
  ///     <description>Determines the background color to use for displaying the text.</description>
  ///   </item>
  ///   <item>
  ///     <term>LINK</term>
  ///     <description><see cref="TBBCodeLink"/></description>
  ///     <description>Marks up the text as a link.</description>
  ///   </item>
  ///   <item>
  ///     <term>SUB</term>
  ///     <description><see cref="TBBCodeSubscript"/></description>
  ///     <description>Displays the text as subscript.</description>
  ///   </item>
  ///   <item>
  ///     <term>SUP</term>
  ///     <description><see cref="TBBCodeSuperscript"/></description>
  ///     <description>Displays the text as superscript.</description>
  ///   </item>
  ///   <item>
  ///     <term>DISABLED</term>
  ///     <description><see cref="TBBCodeDisabled"/></description>
  ///     <description>Displays the text as disabled.</description>
  ///   </item>
  ///   <item>
  ///     <term>PROTECTED</term>
  ///     <description><see cref="TBBCodeProtected"/></description>
  ///     <description>Prevents modification of the text.</description>
  ///   </item>
  ///   <item>
  ///     <term>HIDDEN</term>
  ///     <description><see cref="TBBCodeHidden"/></description>
  ///     <description>Does not display the text.</description>
  ///   </item>
  ///   <item>
  ///     <term>OUTLINE</term>
  ///     <description><see cref="TBBCodeOutline"/></description>
  ///     <description>Displays the text with outlined characters.</description>
  ///   </item>
  ///   <item>
  ///     <term>SHADOW</term>
  ///     <description><see cref="TBBCodeShadow"/></description>
  ///     <description>Displays the text as shadowed.</description>
  ///   </item>
  ///   <item>
  ///     <term>EMBOSS</term>
  ///     <description><see cref="TBBCodeEmboss"/></description>
  ///     <description>Displays the text as embossed.</description>
  ///   </item>
  ///   <item>
  ///     <term>IMPRINT</term>
  ///     <description><see cref="TBBCodeImprint"/></description>
  ///     <description>Displays the text as imprinted.</description>
  ///   </item>
  ///   <item>
  ///     <term>SMALLCAPS</term>
  ///     <description><see cref="TBBCodeSmallCaps"/></description>
  ///     <description>Displays the text in small capital letters.</description>
  ///   </item>
  ///   <item>
  ///     <term>REVISED</term>
  ///     <description><see cref="TBBCodeRevised"/></description>
  ///     <description>Marks up the text as revised.</description>
  ///   </item>
  ///   <item>
  ///     <term>AUTHOR</term>
  ///     <description><see cref="TBBCodeRevAuthor"/></description>
  ///     <description>Uses different text colors for each different author.</description>
  ///   </item>
  ///   <item>
  ///     <term>CODE</term>
  ///     <description><see cref="TBBCodeCode"/></description>
  ///     <description>Displays the text using monospace font.</description>
  ///   </item>
  ///   <item>
  ///     <term>CENTER</term>
  ///     <description><see cref="TBBCodeCenter"/></description>
  ///     <description>Centers the paragraphs.</description>
  ///   </item>
  ///   <item>
  ///     <term>ALIGN</term>
  ///     <description><see cref="TBBCodeAlign"/></description>
  ///     <description>Determines alignment of the paragraphs.</description>
  ///   </item>
  ///   <item>
  ///     <term>LS</term>
  ///     <description><see cref="TBBCodeLineSpacing"/></description>
  ///     <description>Determines spacing between lines of the paragraphs.</description>
  ///   </item>
  ///   <item>
  ///     <term>PSB</term>
  ///     <description><see cref="TBBCodeParagraphSpaceBefore"/></description>
  ///     <description>Determines the amount of spacing before the paragraphs.</description>
  ///   </item>
  ///   <item>
  ///     <term>PSA</term>
  ///     <description><see cref="TBBCodeParagraphSpaceAfter"/></description>
  ///     <description>Determines the amount of spacing after the paragraphs.</description>
  ///   </item>
  ///   <item>
  ///     <term>INDENT</term>
  ///     <description><see cref="TBBCodeIndent"/></description>
  ///     <description>Determines the amount of left indent for the paragraphs.</description>
  ///   </item>
  ///   <item>
  ///     <term>RINDENT</term>
  ///     <description><see cref="TBBCodeRightIndent"/></description>
  ///     <description>Determines the amount of right indent for the paragraphs.</description>
  ///   </item>
  ///   <item>
  ///     <term>DIR</term>
  ///     <description><see cref="TBBCodeDir"/></description>
  ///     <description>Displays the paragraphs in either right-to-left or left-to-right direction.</description>
  ///   </item>
  ///   <item>
  ///     <term>LIST</term>
  ///     <description><see cref="TBBCodeList"/></description>
  ///     <description>Displays the paragraphs as bulleted or numbered list.</description>
  ///   </item>
  ///   <item>
  ///     <term>TABS</term>
  ///     <description><see cref="TBBCodeTabs"/></description>
  ///     <description>Specifies the absolute tab stops.</description>
  ///   </item>
  /// </list>
  /// </remarks>
  {$endregion}
  BBCodes = class
  private
    class var BBCodeClasses: TStringList;
    class procedure Cleanup; static;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Converts the string representation of a color to a color value. The string
    /// representation of the color can be expressed by name (e.g. Red), web format
    /// (e.g. #F00 or #FF0000), or hexadecimal value (e.g. $000000FF).
    /// </summary>
    /// <param name="Str">
    /// The string representation of the color.
    /// </param>
    /// <param name="DefColor">
    /// The default color if the conversion failed.
    /// </param>
    /// <returns>
    /// Returns the corresponding color value of the color string, or <paramref name="DefColor"/>
    /// if the string does not represent a color.
    /// </returns>
    {$endregion}
    class function StrToColor(const Str: String; DefColor: TColor): TColor; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a color value to its string representation.
    /// </summary>
    /// <param name="Color">
    /// The color to be converted.
    /// </param>
    /// <returns>
    /// The string representation of the color.
    /// </returns>
    {$endregion}
    class function ColorToStr(Color: TColor): String; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a Roman numeral to an integer value.
    /// </summary>
    /// <param name="Roman">
    /// Roman numeral as a string.
    /// </param>
    /// <returns>
    /// The integer value of the Roman numeral, or zero if the conversion failed.
    /// </returns>
    /// <seealso cref="NumberToRoman"/>
    {$endregion}
    class function RomanToNumber(const Roman: String): Integer; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts an integer value to a Roman numeral.
    /// </summary>
    /// <param name="Number">
    /// The integer value to convert.
    /// </param>
    /// <returns>
    /// The Roman numeral as a string.
    /// </returns>
    /// <seealso cref="RomanToNumber"/>
    {$endregion}
    class function NumberToRoman(Number: Integer): String; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts a sequence of letters to an integer value. For example, 'A' represents 1,
    /// 'Z' represents 26, and 'AA' represents 27.
    /// </summary>
    /// <param name="Letter">
    /// The sequence of letters.
    /// </param>
    /// <returns>
    /// The integer value of the sequence of letters, or zero if the conversion failed.
    /// </returns>
    /// <seealso cref="NumberToLetter"/>
    {$endregion}
    class function LetterToNumber(const Letter: String): Integer; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Converts an integer value to a sequence of letters. For example, 1 represents 'A',
    /// 26 represents 'Z', and 27 represents 'AA'.
    /// </summary>
    /// <param name="Number">
    /// The integer value to convert.
    /// </param>
    /// <returns>
    /// The sequence of letters corresponding to the integer value.
    /// </returns>
    /// <seealso cref="LetterToNumber"/>
    {$endregion}
    class function NumberToLetter(Number: Integer): String; static;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Registers a BBCode class for subsequent use in calls to <see cref="FindBBCodeClass"/>,
    /// <see cref="ParseBBCode"/> and <see cref="ApplyBBCode"/> functions.
    /// </summary>
    /// <param name="BBCodeClass">
    /// The class to register.
    /// </param>
    /// <seealso cref="Unregister"/>
    /// <seealso cref="Find"/>
    /// <seealso cref="Parse"/>
    /// <seealso cref="Apply"/>
    {$endregion}
    class procedure Register(BBCodeClass: TBBCodeClass); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Unregisters a BBCode class.
    /// </summary>
    /// <param name="BBCodeClass">
    /// The class to unregister.
    /// </param>
    /// <seealso cref="Register"/>
    {$endregion}
    class procedure Unregister(BBCodeClass: TBBCodeClass); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Determines whether a BBCode class is registered or not.
    /// </summary>
    /// <param name="BBCodeClass">
    /// The class to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the class is registered, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Register"/>
    {$endregion}
    class function IsRegistered(BBCodeClass: TBBCodeClass): Boolean; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches in the registered BBCode classes to find a class with the specified
    /// tag name. The search is case-insensitive.
    /// </summary>
    /// <param name="TagName">
    /// BBCode tag name to search.
    /// </param>
    /// <returns>
    /// The BBCode class with the specified tag name, or <see langword="nil"/>
    /// if the tag name could not be found.
    /// </returns>
    /// <seealso cref="Register"/>
    {$endregion}
    class function Find(const TagName: String): TBBCodeClass; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Parses a BBCode formatted text to extract the plain text and BBCode objects
    /// used for formatting the text. The BBCode tags without any registered
    /// BBCode class will be ignored.
    /// </summary>
    /// <param name="BBCodeText">
    /// The BBCode formatted text.
    /// </param>
    /// <param name="PlainText">
    /// Plain text extracted from the BBCode formatted text.
    /// </param>
    /// <param name="BBCodes">
    /// List of BBCode objects extracted from the BBCode formatted text.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the function is succeeded, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Register"/>
    {$endregion}
    class function Parse(const BBCodeText: String; out PlainText: String; BBCodes: TBBCodeObjectList): Boolean; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets content of a Rich Edit control using a BBCode formatted text.
    /// </summary>
    /// <param name="hRichEdit">
    /// Window handle of the target Rich Edit control.
    /// </param>
    /// <param name="BBCodeText">
    /// BBCode formatted text.
    /// </param>
    /// <param name="Append">
    /// Determines whether the new text should be appended to the old content of the Rich Edit control or replace it.
    /// </param>
    {$endregion}
    class procedure Apply(hRichEdit: THandle; const BBCodeText: String; Append: Boolean = False); overload; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets content of a Rich Edit control using a BBCode formatted text.
    /// </summary>
    /// <param name="RichEdit">
    /// Target Rich Edit control
    /// </param>
    /// <param name="BBCodeText">
    /// BBCode formatted text.
    /// </param>
    /// <param name="Append">
    /// Determines whether the new text should be appended to the old content of the Rich Edit control or replace it.
    /// </param>
    {$endregion}
    class procedure Apply(RichEdit: TCustomRichEdit; const BBCodeText: String; Append: Boolean = False); overload; static;
  end;

implementation

{ TBBCode }

function TBBCode.RealCharPos(hRichEdit: THandle; CharPos: Integer): Integer;
var
  FindText: TFindText;
  NumLineBreaks: Integer;
  LineIndex: Integer;
  LineCount: Integer;
  LineStart: Integer;
  LineLength: Integer;
  CharCount: Integer;
begin
  Result := CharPos;
  FindText.chrg.cpMin := 0;
  FindText.chrg.cpMax := -1;
  FindText.lpstrText := #13#10;
  if SendMessage(hRichEdit, EM_FINDTEXT, 0, LPARAM(@FindText)) = -1 then
  begin
    CharCount := 0;
    NumLineBreaks := 0;
    LineCount := SendMessage(hRichEdit, EM_GETLINECOUNT, 0, 0);
    for LineIndex := 0 to LineCount - 1 do
    begin
      LineStart := SendMessageW(hRichEdit, EM_LINEINDEX, LineIndex, 0);
      if CharPos < (LineStart + NumLineBreaks) then
        Break;
      if LineStart > CharCount then
      begin
        Inc(NumLineBreaks);
        Inc(CharCount);
      end;
      LineLength := SendMessageW(hRichEdit, EM_LINELENGTH, LineStart, 0);
      Inc(CharCount, LineLength);
      if (CharPos >= (LineStart + NumLineBreaks)) and
         (CharPos < (LineStart + LineLength + NumLineBreaks))
      then
        break;
    end;
    Dec(Result, NumLineBreaks); {deflate CR/LF -> CR}
  end;
end;

procedure TBBCode.SelectRange(hRichEdit: THandle;
  ASelStart, ASelEnd: Integer);
var
  Range: TCharRange;
begin
  Range.cpMin := RealCharPos(hRichEdit, ASelStart);
  Range.cpMax := RealCharPos(hRichEdit, ASelEnd);
  SendMessage(hRichEdit, EM_EXSETSEL, 0, LPARAM(@Range));
end;

procedure TBBCode.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  FillChar(ParaFormat, SizeOf(ParaFormat), 0);
  ParaFormat.cbSize := SizeOf(ParaFormat);
  FillChar(CharFormat, SizeOf(CharFormat), 0);
  CharFormat.cbSize := SizeOf(CharFormat);
end;

procedure TBBCode.Apply(hRichEdit: THandle; ASelStart, ASelEnd: Integer);
var
  ParaFormat: TParaFormat2;
  CharFormat: TCharFormat2;
begin
  SelectRange(hRichEdit, ASelStart, ASelEnd);
  Prepare(ParaFormat, CharFormat);
  if ParaFormat.dwMask <> 0 then
    SendMessage(hRichEdit, EM_SETPARAFORMAT, 0, LPARAM(@ParaFormat));
  if CharFormat.dwMask <> 0 then
    SendMessage(hRichEdit, EM_SETCHARFORMAT, SCF_SELECTION, LPARAM(@CharFormat));
end;

procedure TBBCode.Apply(hRichEdit: THandle; Offset: Integer);
begin
  Apply(hRichEdit, SelStart + Offset, SelEnd + Offset);
end;

function TBBCode.Mark(hRichEdit: THandle; ASelStart, ASelEnd: Integer): Integer;
var
  OpenTag, CloseTag: String;
begin
  if Attribute = '' then
    OpenTag := '[' + TagName + ']'
  else
    OpenTag := '[' + TagName + '=' + Attribute + ']';
  CloseTag := '[/' + TagName + ']';
  SelectRange(hRichEdit, ASelEnd, ASelEnd);
  SendMessage(hRichEdit, EM_REPLACESEL, 0, LPARAM(PChar(CloseTag)));
  SelectRange(hRichEdit, ASelStart, ASelStart);
  SendMessage(hRichEdit, EM_REPLACESEL, 0, LPARAM(PChar(OpenTag)));
  Result := Length(OpenTag);
end;

function TBBCode.Mark(hRichEdit: THandle; Offset: Integer = 0): Integer;
begin
  Result := Mark(hRichEdit, SelStart + Offset, SelEnd + Offset);
end;

{ TBBCodeObjectList }

function TBBCodeObjectList.GetItems(Index: Integer): TBBCode;
begin
  Result := TBBCode(Get(Index));
end;

procedure TBBCodeObjectList.SetItems(Index: Integer; Value: TBBCode);
begin
  Put(Index, Value);
end;

function TBBCodeObjectList.First: TBBCode;
begin
  Result := TBBCode(inherited First);
end;

function TBBCodeObjectList.Last: TBBCode;
begin
  Result := TBBCode(inherited Last);
end;

function TBBCodeObjectList.Add(Item: TBBCode): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TBBCodeObjectList.Insert(Index: Integer; Item: TBBCode);
begin
  inherited Insert(Index, Item);
end;

function TBBCodeObjectList.Remove(Item: TBBCode): Integer;
begin
  Result := inherited Remove(Item);
end;

{$IFDEF COMPILER2010_UP}
function TBBCodeObjectList.RemoveItem(Item: TBBCode;
  Direction: TList.TDirection): Integer;
begin
  Result := inherited RemoveItem(Item, Direction);
end;
{$ENDIF}

function TBBCodeObjectList.Extract(Item: TBBCode): TBBCode;
begin
  Result := TBBCode(inherited Extract(Item));
end;

{$IFDEF COMPILER2010_UP}
function TBBCodeObjectList.ExtractItem(Item: TBBCode;
  Direction: TList.TDirection): TBBCode;
begin
  Result := TBBCode(inherited ExtractItem(Item, Direction));
end;
{$ENDIF}

function TBBCodeObjectList.IndexOf(Item: TBBCode): Integer;
begin
  Result := inherited IndexOf(Item);
end;

{$IFDEF COMPILER2010_UP}
function TBBCodeObjectList.IndexOfItem(Item: TBBCode;
  Direction: TList.TDirection): Integer;
begin
  Result := inherited IndexOfItem(Item, Direction);
end;
{$ENDIF}

{ TSwitchableBBCode }

const
  SInactives: array[1..2] of String = ('Off', 'No');

function TSwitchableBBCode.GetActive: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := Low(SInactives) to High(SInactives) do
    if SameText(Attribute, SInactives[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

procedure TSwitchableBBCode.SetActive(Value: Boolean);
begin
  if not Value then
    Attribute := SInactives[1]
  else
    Attribute := '';
end;

{ TBBCodeBold }

class function TBBCodeBold.GetTagName: String;
begin
  Result := 'B';
end;

procedure TBBCodeBold.Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_BOLD;
    if Active then
      dwEffects := dwEffects or CFE_BOLD
    else
      dwEffects := dwEffects and not CFE_BOLD;
  end;
end;

{ TBBCodeItalic }

class function TBBCodeItalic.GetTagName: String;
begin
  Result := 'I';
end;

procedure TBBCodeItalic.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_ITALIC;
    if Active then
      dwEffects := dwEffects or CFE_ITALIC
    else
      dwEffects := dwEffects and not CFE_ITALIC;
  end;
end;

{ TBBCodeUnderline }

class function TBBCodeUnderline.GetTagName: String;
begin
  Result := 'U';
end;

procedure TBBCodeUnderline.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_UNDERLINE;
    if UnderlineType <> ulNone then
    begin
      dwEffects := dwEffects or CFE_UNDERLINE;
      dwMask := dwMask or CFM_UNDERLINETYPE;
      bUnderlineType := Ord(UnderlineType) or (Ord(UnderlineColor) shl 4);
    end
    else
      dwEffects := dwEffects and not CFE_UNDERLINE;
  end;
end;

const
  SUnderlineType: array[TBBCodeUnderline.TUnderlineType] of String =
    ('None', 'Normal', 'Word', 'Double', 'Dotted', 'Dash',
     'DashDot', 'DashDotDot', 'Wave', 'Thick', 'HairLine',
     'DoubleWave', 'HeavyWave', 'LongDash');
  SUnderlineColor: array[TBBCodeUnderline.TUnderlineColor] of String =
    ('Auto', 'Blue', 'Aqua', 'Lime', 'Fuchsia', 'Red',
     'Yellow', 'White', 'Navy', 'Teal', 'Green', 'Purple',
     'Maroon', 'Olive', 'Gray', 'Silver');

function TBBCodeUnderline.GetUnderLineType: TUnderlineType;
var
  T: TUnderlineType;
  Attr1, Attr2: String;
  I: Integer;
begin
  Result := ulNormal;
  I := Pos(',', Attribute);
  if I <> 0 then
  begin
    Attr1 := Trim(Copy(Attribute, 1, I - 1));
    Attr2 := Trim(Copy(Attribute, I + 1, Length(Attribute) - I));
    if Attr1 = '' then
      Attribute := Attr2
    else if Attr2 = '' then
      Attribute := Attr1
    else
    begin
      for T := Low(TUnderlineType) to High(TUnderlineType) do
        if SameText(Attr1, SUnderlineType[T]) or
           SameText(Attr2, SUnderlineType[T]) then
        begin
          Result := T;
          Exit;
        end;
      Exit;
    end
  end;
  if Attribute <> '' then
    for T := Low(TUnderlineType) to High(TUnderlineType) do
      if SameText(Attribute, SUnderlineType[T]) then
      begin
        Result := T;
        Exit;
      end;
end;

procedure TBBCodeUnderline.SetUnderlineType(Value: TUnderlineType);
begin
  if UnderlineType <> Value then
  begin
    if UnderlineColor <> ucAuto then
      Attribute := SUnderlineColor[UnderlineColor]
    else
      Attribute := '';
    if Value <> ulNormal then
    begin
      if Attribute <> '' then
        Attribute := ',' + Attribute;
      Attribute := SUnderlineType[Value] + Attribute;
    end;
  end;
end;

function TBBCodeUnderline.GetUnderLineColor: TUnderlineColor;
var
  C: TUnderlineColor;
  Attr1, Attr2: String;
  I: Integer;
begin
  Result := ucAuto;
  I := Pos(',', Attribute);
  if I <> 0 then
  begin
    Attr1 := Trim(Copy(Attribute, 1, I - 1));
    Attr2 := Trim(Copy(Attribute, I + 1, Length(Attribute) - I));
    if Attr1 = '' then
      Attribute := Attr2
    else if Attr2 = '' then
      Attribute := Attr1
    else
    begin
      for C := Low(TUnderlineColor) to High(TUnderlineColor) do
        if SameText(Attr1, SUnderlineColor[C]) or
           SameText(Attr2, SUnderlineColor[C]) then
        begin
          Result := C;
          Exit;
        end;
      Exit;
    end
  end;
  if Attribute <> '' then
    for C := Low(TUnderlineColor) to High(TUnderlineColor) do
      if SameText(Attribute, SUnderlineColor[C]) then
      begin
        Result := C;
        Exit;
      end;
end;

procedure TBBCodeUnderline.SetUnderlineColor(Value: TUnderlineColor);
begin
  if UnderlineColor <> Value then
  begin
    if UnderlineType <> ulNormal then
      Attribute := SUnderlineType[UnderlineType]
    else
      Attribute := '';
    if Value <> ucAuto then
    begin
      if Attribute <> '' then
        Attribute := Attribute + ',';
      Attribute := Attribute + SUnderlineColor[Value]
    end;
  end;
end;

{ TBBCodeStrikeout }

class function TBBCodeStrikeout.GetTagName: String;
begin
  Result := 'S';
end;

procedure TBBCodeStrikeout.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_STRIKEOUT;
    if Active then
      dwEffects := dwEffects or CFE_STRIKEOUT
    else
      dwEffects := dwEffects and not CFE_STRIKEOUT;
  end;
end;

{ TBBCodeFont }

class function TBBCodeFontFace.GetTagName: String;
begin
  Result := 'FONT';
end;

procedure TBBCodeFontFace.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_FACE;
    StrLCopy(szFaceName, PChar(FontName), High(szFaceName) - 1);
  end;
end;

function TBBCodeFontFace.GetFontName: String;
begin
  Result := Attribute;
end;

procedure TBBCodeFontFace.SetFontName(const Value: String);
begin
  Attribute := Value;
end;

{ TBBCodeSize }

class function TBBCodeFontSize.GetTagName: String;
begin
  Result := 'SIZE';
end;

procedure TBBCodeFontSize.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_SIZE;
    yHeight := Size * 20;
  end;
end;

function TBBCodeFontSize.GetSize: Integer;
begin
  Result := StrToIntDef(Attribute, 10);
end;

procedure TBBCodeFontSize.SetSize(Value: Integer);
begin
  Attribute := IntToStr(Value);
end;

{ TBBCodeColor }

class function TBBCodeFontColor.GetTagName: String;
begin
  Result := 'COLOR';
end;

procedure TBBCodeFontColor.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
var
  C: TColor;
begin
  inherited Prepare(ParaFormat, CharFormat);
  C := Color;
  with CharFormat do
  begin
    dwMask := dwMask or CFM_COLOR;
    if C = clWindowText then
      dwEffects := dwEffects or CFE_AUTOCOLOR
    else
    begin
      dwEffects := dwEffects and not CFE_AUTOCOLOR;
      crTextColor := ColorToRGB(C);
    end;
  end;
end;

function TBBCodeFontColor.GetColor: TColor;
begin
  Result := BBCodes.StrToColor(Attribute, clWindowText);
end;

procedure TBBCodeFontColor.SetColor(Value: TColor);
begin
  Attribute := BBCodes.ColorToStr(Value);
end;

{ TBBCodeBackColor }

class function TBBCodeBackColor.GetTagName: String;
begin
  Result := 'BACKCOLOR';
end;

procedure TBBCodeBackColor.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
var
  C: TColor;
begin
  inherited Prepare(ParaFormat, CharFormat);
  C := Color;
  with CharFormat do
  begin
    dwMask := dwMask or CFM_BACKCOLOR;
    if C = clWindow then
      dwEffects := dwEffects or CFE_AUTOBACKCOLOR
    else
    begin
      dwEffects := dwEffects and not CFE_AUTOBACKCOLOR;
      crBackColor := ColorToRGB(C);
    end;
  end;
end;

function TBBCodeBackColor.GetColor: TColor;
begin
  Result := BBCodes.StrToColor(Attribute, clWindow);
end;

procedure TBBCodeBackColor.SetColor(Value: TColor);
begin
  Attribute := BBCodes.ColorToStr(Value);
end;

{ TBBCodeLink }

class function TBBCodeLink.GetTagName: String;
begin
  Result := 'LINK';
end;

procedure TBBCodeLink.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_LINK;
    if Active then
      dwEffects := dwEffects or CFE_LINK
    else
      dwEffects := dwEffects and not CFE_LINK;
  end;
end;

{ TBBCodeSubscript }

class function TBBCodeSubscript.GetTagName: String;
begin
  Result := 'SUB';
end;

procedure TBBCodeSubscript.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_SUBSCRIPT;
    if Active then
      dwEffects := dwEffects or CFE_SUBSCRIPT
    else
      dwEffects := dwEffects and not CFE_SUBSCRIPT;
  end;
end;

{ TBBCodeSuperscript }

class function TBBCodeSuperscript.GetTagName: String;
begin
  Result := 'SUP';
end;

procedure TBBCodeSuperscript.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_SUPERSCRIPT;
    if Active then
      dwEffects := dwEffects or CFE_SUPERSCRIPT
    else
      dwEffects := dwEffects and not CFE_SUPERSCRIPT;
  end;
end;

{ TBBCodeDisabled }

class function TBBCodeDisabled.GetTagName: String;
begin
  Result := 'DISABLED';
end;

procedure TBBCodeDisabled.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_DISABLED;
    if Active then
      dwEffects := dwEffects or CFE_DISABLED
    else
      dwEffects := dwEffects and not CFE_DISABLED;
  end;
end;

{ TBBCodeProtected }

class function TBBCodeProtected.GetTagName: String;
begin
  Result := 'PROTECTED';
end;

procedure TBBCodeProtected.Prepare(var ParaFormat: TParaFormat2; var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_PROTECTED;
    if Active then
      dwEffects := dwEffects or CFE_PROTECTED
    else
      dwEffects := dwEffects and not CFE_PROTECTED;
  end;
end;

{ TBBCodeHidden }

class function TBBCodeHidden.GetTagName: String;
begin
  Result := 'HIDDEN';
end;

procedure TBBCodeHidden.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_HIDDEN;
    if Active then
      dwEffects := dwEffects or CFE_HIDDEN
    else
      dwEffects := dwEffects and not CFE_HIDDEN;
  end;
end;

{ TBBCodeOutline }

class function TBBCodeOutline.GetTagName: String;
begin
  Result := 'OUTLINE';
end;

procedure TBBCodeOutline.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_OUTLINE;
    if Active then
      dwEffects := dwEffects or CFE_OUTLINE
    else
      dwEffects := dwEffects and not CFE_OUTLINE;
  end;
end;

{ TBBCodeShadow }

class function TBBCodeShadow.GetTagName: String;
begin
  Result := 'SHADOW';
end;

procedure TBBCodeShadow.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_SHADOW;
    if Active then
      dwEffects := dwEffects or CFE_SHADOW
    else
      dwEffects := dwEffects and not CFE_SHADOW;
  end;
end;

{ TBBCodeEmboss }

class function TBBCodeEmboss.GetTagName: String;
begin
  Result := 'EMBOSS';
end;

procedure TBBCodeEmboss.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_EMBOSS;
    if Active then
      dwEffects := dwEffects or CFE_EMBOSS
    else
      dwEffects := dwEffects and not CFE_EMBOSS;
  end;
end;

{ TBBCodeImprint }

class function TBBCodeImprint.GetTagName: String;
begin
  Result := 'IMPRINT';
end;

procedure TBBCodeImprint.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_IMPRINT;
    if Active then
      dwEffects := dwEffects or CFE_IMPRINT
    else
      dwEffects := dwEffects and not CFE_IMPRINT;
  end;
end;

{ TBBCodeSmallCaps }

class function TBBCodeSmallCaps.GetTagName: String;
begin
  Result := 'SMALLCAPS';
end;

procedure TBBCodeSmallCaps.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_SMALLCAPS;
    if Active then
      dwEffects := dwEffects or CFE_SMALLCAPS
    else
      dwEffects := dwEffects and not CFE_SMALLCAPS;
  end;
end;

{ TBBCodeAllCaps }

class function TBBCodeAllCaps.GetTagName: String;
begin
  Result := 'CAPS';
end;

procedure TBBCodeAllCaps.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_ALLCAPS;
    if Active then
      dwEffects := dwEffects or CFE_ALLCAPS
    else
      dwEffects := dwEffects and not CFE_ALLCAPS;
  end;
end;

{ TBBCodeRevised }

class function TBBCodeRevised.GetTagName: String;
begin
  Result := 'REVISED';
end;

procedure TBBCodeRevised.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_REVISED;
    if Active then
      dwEffects := dwEffects or CFE_REVISED
    else
      dwEffects := dwEffects and not CFE_REVISED;
  end;
end;

{ TBBCodeAuthor }

class function TBBCodeRevAuthor.GetTagName: String;
begin
  Result := 'AUTHOR';
end;

procedure TBBCodeRevAuthor.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_REVAUTHOR;
    bRevAuthor := Author;
  end;
end;

function TBBCodeRevAuthor.GetAuthor: Byte;
begin
  Result := StrToIntDef(Attribute, 0);
end;

procedure TBBCodeRevAuthor.SetAuthor(Value: Byte);
begin
  Attribute := IntToStr(Value);
end;

{ TBBCodeCode }

class function TBBCodeCode.GetTagName: String;
begin
  Result := 'CODE';
end;

procedure TBBCodeCode.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with CharFormat do
  begin
    dwMask := dwMask or CFM_FACE;
    StrCopy(szFaceName, 'Courier New');
  end;
end;

{ TBBCodeCenter }

class function TBBCodeCenter.GetTagName: String;
begin
  Result := 'CENTER';
end;

procedure TBBCodeCenter.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with ParaFormat do
  begin
    dwMask := dwMask or PFM_ALIGNMENT;
    wAlignment := PFA_CENTER;
  end;
end;

{ TBBCodeAlign }

class function TBBCodeAlign.GetTagName: String;
begin
  Result := 'ALIGN';
end;

procedure TBBCodeAlign.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with ParaFormat do
  begin
    dwMask := dwMask or PFM_ALIGNMENT;
    wAlignment := Ord(Alignment) + 1;
  end;
end;

const
  SParaAlign: array[TBBCodeAlign.TParaAlign] of String =
    ('Left', 'Right', 'Center', 'Justify', 'FullInterWord');

function TBBCodeAlign.GetAlignment: TParaAlign;
var
  ParaAlign: TParaAlign;
begin
  if Attribute <> '' then
  begin
    for ParaAlign := Low(TParaAlign) to High(TParaAlign) do
      if SameText(Attribute, SParaAlign[ParaAlign]) then
      begin
        Result := ParaAlign;
        Exit;
      end;
  end;
  Result := paLeft;
end;

procedure TBBCodeAlign.SetAlignment(Value: TParaAlign);
begin
  Attribute := SParaAlign[Value];
end;

{ TBBCodeLineSpacing }

class function TBBCodeLineSpacing.GetTagName: String;
begin
  Result := 'LS';
end;

procedure TBBCodeLineSpacing.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with ParaFormat do
  begin
    dwMask := dwMask or PFM_LINESPACING;
    bLineSpacingRule := Ord(Spacing);
  end;
end;

function TBBCodeLineSpacing.GetSpacing: TLineSpacing;
var
  Spacing: Single;
begin
  Spacing := StrToFloatDef(Attribute, 1);
  if Spacing <= 1 then
    Result := lsSingle
  else if Spacing >= 2 then
    Result := lsDouble
  else
    Result := lsOneAndHalf;
end;

procedure TBBCodeLineSpacing.SetSpacing(Value: TLineSpacing);
begin
  case Value of
    lsSingle: Attribute := '1';
    lsOneAndHalf: Attribute := '1.5';
    lsDouble: Attribute := '2';
  end;
end;

{ TBBCodeParagraphSpaceBefore }

class function TBBCodeParagraphSpaceBefore.GetTagName: String;
begin
  Result := 'PSB';
end;

procedure TBBCodeParagraphSpaceBefore.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with ParaFormat do
  begin
    dwMask := dwMask or PFM_SPACEBEFORE;
    dySpaceBefore := Spacing * 20;
  end;
end;

function TBBCodeParagraphSpaceBefore.GetSpacing: Integer;
begin
  Result := StrToIntDef(Attribute, 0);
end;

procedure TBBCodeParagraphSpaceBefore.SetSpacing(Value: Integer);
begin
  Attribute := IntToStr(Value);
end;

{ TBBCodeParagraphSpaceAfter }

class function TBBCodeParagraphSpaceAfter.GetTagName: String;
begin
  Result := 'PSA';
end;

procedure TBBCodeParagraphSpaceAfter.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with ParaFormat do
  begin
    dwMask := dwMask or PFM_SPACEAFTER;
    dySpaceAfter := Spacing * 20;
  end;
end;

function TBBCodeParagraphSpaceAfter.GetSpacing: Integer;
begin
  Result := StrToIntDef(Attribute, 0);
end;

procedure TBBCodeParagraphSpaceAfter.SetSpacing(Value: Integer);
begin
  Attribute := IntToStr(Value);
end;

{ TBBCodeIndent }

class function TBBCodeIndent.GetTagName: String;
begin
  Result := 'INDENT';
end;

procedure TBBCodeIndent.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with ParaFormat do
  begin
    dwMask := dwMask or PFM_OFFSETINDENT or PFM_OFFSET;
    dxStartIndent := Indent * 20;
    dxOffset := 0;
  end;
end;

function TBBCodeIndent.GetIndent: Integer;
begin
  Result := StrToIntDef(Attribute, 16);
end;

procedure TBBCodeIndent.SetIndent(Value: Integer);
begin
  Attribute := IntToStr(Value);
end;

{ TBBCodeRightIndent }

class function TBBCodeRightIndent.GetTagName: String;
begin
  Result := 'RINDENT';
end;

procedure TBBCodeRightIndent.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with ParaFormat do
  begin
    dwMask := dwMask or PFM_RIGHTINDENT;
    dxRightIndent := Indent * 20;
  end;
end;

function TBBCodeRightIndent.GetIndent: Integer;
begin
  Result := StrToIntDef(Attribute, 16);
end;

procedure TBBCodeRightIndent.SetIndent(Value: Integer);
begin
  Attribute := IntToStr(Value);
end;

{ TBBCodeDir }

class function TBBCodeDir.GetTagName: String;
begin
  Result := 'DIR';
end;

procedure TBBCodeDir.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
begin
  inherited Prepare(ParaFormat, CharFormat);
  with ParaFormat do
  begin
    dwMask := dwMask or PFM_RTLPARA;
    if Direction = RTL then
      wReserved := wReserved or PFE_RTLPARA
    else
      wReserved := wReserved and not PFE_RTLPARA
  end;
end;

const
  STextDirection: array[TBBCodeDir.TTextDirection] of String =
    ('LTR', 'RTL');

function TBBCodeDir.GetDirection: TTextDirection;
var
  TextDir: TTextDirection;
begin
  if Attribute <> '' then
  begin
    for TextDir := Low(TTextDirection) to High(TTextDirection) do
      if SameText(Attribute, STextDirection[TextDir]) then
      begin
        Result := TextDir;
        Exit;
      end;
  end;
  Result := TTextDirection.LTR;
end;

procedure TBBCodeDir.SetDirection(Value: TTextDirection);
begin
  Attribute := STextDirection[Value];
end;

{ TBBCodeList }

class function TBBCodeList.GetTagName: String;
begin
  Result := 'LIST';
end;

procedure TBBCodeList.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
var
  TheNumbering: TListNumbering;
  TheStart: Word;
  TheStyle: TListNumberingStyle;
  TheTab: Integer;
begin
  DecodeAttribute(TheNumbering, TheStart, TheStyle, TheTab);
  inherited Prepare(ParaFormat, CharFormat);
  with ParaFormat do
  begin
    dwMask := dwMask or PFM_NUMBERING or PFM_NUMBERINGSTART or
      PFM_NUMBERINGSTYLE or PFM_NUMBERINGTAB or PFM_OFFSETINDENT or PFM_OFFSET;
    dxStartIndent := 8 * 20;
    dxOffset := 0;
    wNumbering := Ord(TheNumbering);
    wNumberingStyle := Ord(TheStyle) shl 8;
    wNumberingStart := TheStart;
    wNumberingTab := TheTab * 20;
  end;
end;

const
  SNone = 'None';
  SListNumberingStyle: array[TBBCodeList.TListNumberingStyle] of String =
    ('Parenthesis', 'Enclosed', 'Period', 'Simple');

procedure TBBCodeList.DecodeAttribute(out TheNumbering: TListNumbering;
  out TheStart: Word; out TheStyle: TListNumberingStyle; out TheTab: Integer);
var
  I: Integer;
  Attr1, Attr2, Attr3: String;
  S: TListNumberingStyle;
begin
  I := Pos(',', Attribute);
  if I <> 0 then
  begin
    Attr1 := Trim(Copy(Attribute, 1, I - 1));
    Attr2 := Trim(Copy(Attribute, I + 1, Length(Attribute) - I));
    I := Pos(',', Attr2);
    if I <> 0 then
    begin
      Attr3 := Trim(Copy(Attr2, I + 1, Length(Attr2) - I));
      Attr2 := Trim(Copy(Attr2, 1, I - 1));
    end
    else
      Attr3 := '';
    if Attr1 = '' then
    begin
      Attr1 := Attr2;
      Attr2 := '';
    end;
    if Attr2 = '' then
    begin
      Attr2 := Attr3;
      Attr3 := '';
    end;
  end
  else
  begin
    Attr1 := Attribute;
    Attr2 := '';
    Attr3 := '';
  end;
  TheStyle := nsParenthesis;
  TheNumbering := lnBullet;
  TheStart := 1;
  TheTab := 0;
  if Attr1 <> '' then
  begin
    // style
    for S := Low(TListNumberingStyle) to High(TListNumberingStyle) do
      if SameText(Attr1, SListNumberingStyle[S]) then
      begin
        TheStyle := S;
        Attr1 := Attr2;
        Attr2 := Attr3;
        Attr3 := '';
        Break;
      end
      else if (Attr2 <> '') and SameText(Attr2, SListNumberingStyle[S]) then
      begin
        TheStyle := S;
        Attr2 := Attr3;
        Attr3 := '';
        Break;
      end
      else if (Attr3 <> '') and SameText(Attr3, SListNumberingStyle[S]) then
      begin
        TheStyle := S;
        Attr3 := '';
        Break;
      end;
    // tab
    if (Length(Attr1) > 1) and (Pos('@', Attr1) = 1) then
    begin
      TheTab := StrToIntDef(Copy(Attr1, 2, Length(Attr1) - 1), 0);
      Attr1 := Attr2;
      Attr2 := Attr3;
      Attr3 := '';
    end
    else if (Length(Attr2) > 1) and (Pos('@', Attr2) = 1) then
    begin
      TheTab := StrToIntDef(Copy(Attr2, 2, Length(Attr2) - 1), 0);
      Attr2 := Attr3;
      Attr3 := '';
    end
    else if (Length(Attr3) > 1) and (Pos('@', Attr3) = 1) then
    begin
      TheTab := StrToIntDef(Copy(Attr3, 2, Length(Attr3) - 1), 0);
      Attr3 := '';
    end;
    // numbering & start
    if Attr1 = '' then
      TheNumbering := lnBullet
    else if SameText(Attr1, SNone) then
      TheNumbering := lnNone
    else
    begin
      TheStart := StrToIntDef(Attr1, 0);
      if TheStart <> 0 then
        TheNumbering := lnNumber
      else
      begin
        TheStart := BBCodes.LetterToNumber(Attr1);
        if TheStart <> 0 then
        begin
          TheNumbering := lnUpperLetter;
          if (Length(Attr1) > 1) or (Pos(UpCase(Attr1[1]), 'IVX') <> 0) then
          begin
            I := BBCodes.RomanToNumber(Attr1);
            if I <> 0 then
            begin
              TheStart := I;
              TheNumbering := lnUpperRoman;
            end;
          end;
          if IsCharLower(Attr1[1]) then
            Dec(TheNumbering);
        end
        else
        begin
          TheStart := Ord(Attr1[1]);
          TheNumbering := lnCustom;
        end;
      end;
    end;
  end;
end;

procedure TBBCodeList.EncodeAttribute(TheNumbering: TListNumbering;
  TheStart: Word; TheStyle: TListNumberingStyle; TheTab: Integer);
begin
  if TheNumbering <> lnNone then
  begin
    Attribute := '';
    if (TheNumbering <> lnBullet) and (TheStart <> 0) then
    begin
      case TheNumbering of
        lnNumber: Attribute := IntToStr(TheStart);
        lnLowerLetter: Attribute := LowerCase(BBCodes.NumberToLetter(TheStart));
        lnUpperLetter: Attribute := BBCodes.NumberToLetter(TheStart);
        lnLowerRoman: Attribute := LowerCase(BBCodes.NumberToRoman(TheStart));
        lnUpperRoman: Attribute := BBCodes.NumberToRoman(TheStart);
        lnCustom: Attribute := Chr(TheStart);
      end;
      if TheStyle <> nsParenthesis then
        Attribute := Attribute + ',' + SListNumberingStyle[TheStyle];
    end;
    if TheTab <> 0 then
    begin
      if Attribute <> '' then
        Attribute := Attribute + ',';
      Attribute := Attribute + '@' + IntToStr(TheTab);
    end;
  end
  else
    Attribute := SNone;
end;

function TBBCodeList.GetNumbering: TListNumbering;
var
  TheNumbering: TListNumbering;
  TheStart: Word;
  TheStyle: TListNumberingStyle;
  TheTab: Integer;
begin
  DecodeAttribute(TheNumbering, TheStart, TheStyle, TheTab);
  Result := TheNumbering;
end;

procedure TBBCodeList.SetNumbering(Value: TListNumbering);
var
  TheNumbering: TListNumbering;
  TheStart: Word;
  TheStyle: TListNumberingStyle;
  TheTab: Integer;
begin
  DecodeAttribute(TheNumbering, TheStart, TheStyle, TheTab);
  EncodeAttribute(Value, TheStart, TheStyle, TheTab);
end;

function TBBCodeList.GetStart: Word;
var
  TheNumbering: TListNumbering;
  TheStart: Word;
  TheStyle: TListNumberingStyle;
  TheTab: Integer;
begin
  DecodeAttribute(TheNumbering, TheStart, TheStyle, TheTab);
  Result := TheStart;
end;

procedure TBBCodeList.SetStart(Value: Word);
var
  TheNumbering: TListNumbering;
  TheStart: Word;
  TheStyle: TListNumberingStyle;
  TheTab: Integer;
begin
  DecodeAttribute(TheNumbering, TheStart, TheStyle, TheTab);
  EncodeAttribute(TheNumbering, Value, TheStyle, TheTab);
end;

function TBBCodeList.GetStyle: TListNumberingStyle;
var
  TheNumbering: TListNumbering;
  TheStart: Word;
  TheStyle: TListNumberingStyle;
  TheTab: Integer;
begin
  DecodeAttribute(TheNumbering, TheStart, TheStyle, TheTab);
  Result := TheStyle;
end;

procedure TBBCodeList.SetStyle(Value: TListNumberingStyle);
var
  TheNumbering: TListNumbering;
  TheStart: Word;
  TheStyle: TListNumberingStyle;
  TheTab: Integer;
begin
  DecodeAttribute(TheNumbering, TheStart, TheStyle, TheTab);
  EncodeAttribute(TheNumbering, TheStart, Value, TheTab);
end;

function TBBCodeList.GetTab: Integer;
var
  TheNumbering: TListNumbering;
  TheStart: Word;
  TheStyle: TListNumberingStyle;
  TheTab: Integer;
begin
  DecodeAttribute(TheNumbering, TheStart, TheStyle, TheTab);
  Result := TheTab;
end;

procedure TBBCodeList.SetTab(Value: Integer);
var
  TheNumbering: TListNumbering;
  TheStart: Word;
  TheStyle: TListNumberingStyle;
  TheTab: Integer;
begin
  DecodeAttribute(TheNumbering, TheStart, TheStyle, TheTab);
  EncodeAttribute(TheNumbering, TheStart, TheStyle, Value);
end;

{ TBBCodeTabs }

class function TBBCodeTabs.GetTagName: String;
begin
  Result := 'TABS';
end;

procedure TBBCodeTabs.Prepare(var ParaFormat: TParaFormat2;
  var CharFormat: TCharFormat2);
var
  Tabs, Tab: String;
  TabStop: Integer;
  I: Integer;
begin
  inherited Prepare(ParaFormat, CharFormat);
  with ParaFormat do
  begin
    dwMask := dwMask or PFM_TABSTOPS;
    cTabCount := 0;
    while (Tabs <> '') and (cTabCount <= High(rgxTabs)) do
    begin
      I := Pos(',', Tabs);
      if I <> 0 then
      begin
        Tab := Copy(Tabs, 1, I - 1);
        Delete(Tabs, 1, I);
      end
      else
      begin
        Tab := Tabs;
        Tabs := '';
      end;
      TabStop := StrToIntDef(Tab, 0);
      if TabStop <> 0 then
      begin
        rgxTabs[cTabCount] := TabStop * 20;
        Inc(cTabCount);
      end;
    end;
  end;
end;

function TBBCodeTabs.GetTabStops: String;
begin
  Result := Attribute;
end;

procedure TBBCodeTabs.SetTabStops(const Value: String);
begin
  Attribute := Value;
end;

{ BBCodes }

class function BBCodes.StrToColor(const Str: String; DefColor: TColor): TColor;
begin
  if Pos('#', Str) = 1 then
  begin
    if Length(Str) = 7 then
      try
        Result := RGB(
          StrToInt('$' + Copy(Str, 2, 2)),
          StrToInt('$' + Copy(Str, 4, 2)),
          StrToInt('$' + Copy(Str, 6, 2)));
      except
        Result := DefColor;
      end
    else if Length(Str) = 4 then
      try
        Result := RGB(
          (StrToInt('$' + Copy(Str, 2, 1)) shl 4) or StrToInt('$' + Copy(Str, 2, 1)),
          (StrToInt('$' + Copy(Str, 3, 1)) shl 4) or StrToInt('$' + Copy(Str, 3, 1)),
          (StrToInt('$' + Copy(Str, 4, 1)) shl 4) or StrToInt('$' + Copy(Str, 4, 1)))
      except
        Result := DefColor;
      end
    else
      Result := DefColor;
  end
  else if Pos('$', Str) = 1 then
    Result := StrToIntDef(Str, DefColor)
  else if not IdentToColor('cl' + Str, Integer(Result)) then
    Result := DefColor;
end;

class function BBCodes.ColorToStr(Color: TColor): String;
var
  RGB: Integer;
begin
  if ColorToIdent(Color, Result) then
    Delete(Result, 1, 2)
  else
  begin
    RGB := ColorToRGB(Color);
    Result := '#'
            + IntToHex(GetRValue(RGB), 2)
            + IntToHex(GetGValue(RGB), 2)
            + IntToHex(GetBValue(RGB), 2);
  end;
end;

class function BBCodes.RomanToNumber(const Roman: String): Integer;
const
  Numbers: array[1..7] of Integer = (1, 5, 10, 50, 100, 500, 1000);
  Romans = 'IVXLCDMivxlcdm';
var
  I, N: Integer;
  NewValue, OldValue: Integer;
begin
  Result := 0;
  OldValue := 0 ;
  for I := Length(Roman) downto 1 do
  begin
    N := Pos(Roman[I], Romans);
    if N = 0 then
    begin
      Result := 0;
      Exit;
    end;
    if N > High(Numbers) then
      NewValue := Numbers[N - High(Numbers)]
    else
      NewValue := Numbers[N];
    if NewValue < OldValue then
      Dec(Result, NewValue)
    else
      Inc(Result, NewValue);
    OldValue := NewValue;
  end;
end;

class function BBCodes.NumberToRoman(Number: Integer): String;
// Taken from http://www.swissdelphicenter.ch/torry/showcode.php?id=766
const
  Numbers: array[1..13] of Integer =
    (1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000);
  Romans: array[1..13] of String =
    ('I', 'IV', 'V', 'IX', 'X', 'XL', 'L', 'XC', 'C', 'CD', 'D', 'CM', 'M');
var
  I: Integer;
begin
  Result := '';
  for I := 13 downto 1 do
    while Number >= Numbers[I] do
    begin
      Number := Number - Numbers[I];
      Result := Result + Romans[I];
    end;
end;

class function BBCodes.LetterToNumber(const Letter: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Letter) do
  begin
    if (Letter[I] >= 'A') and (Letter[I] <= 'Z') then
      Result := Result * 26 + Ord(Letter[I]) - Ord('A') + 1
    else if (Letter[I] >= 'a') and (Letter[I] <= 'z') then
      Result := Result * 26 + Ord(Letter[I]) - Ord('a') + 1
    else
    begin
      Result := 0;
      Exit;
    end;
  end;
end;

class function BBCodes.NumberToLetter(Number: Integer): String;
begin
  Result := '';
  while Number > 0 do
  begin
    Result := Chr(Ord('A') + (Number - 1) mod 26) + Result;
    Number := (Number - 1) div 26;
  end;
end;

class procedure BBCodes.Cleanup;
begin
  if Assigned(BBCodeClasses) then
    FreeAndNil(BBCodeClasses);
end;

class procedure BBCodes.Register(BBCodeClass: TBBCodeClass);
var
  Index: Integer;
begin
  if not Assigned(BBCodeClasses) then
  begin
    BBCodeClasses := TStringList.Create;
    BBCodeClasses.CaseSensitive := False;
    BBCodeClasses.Sorted := True;
  end;
  if BBCodeClasses.Find(BBCodeClass.GetTagName, Index) then
    BBCodeClasses.Objects[Index] := TObject(BBCodeClass)
  else
    BBCodeClasses.AddObject(BBCodeClass.GetTagName, TObject(BBCodeClass));
end;

class procedure BBCodes.Unregister(BBCodeClass: TBBCodeClass);
var
  Index: Integer;
begin
  if Assigned(BBCodeClasses) and
     BBCodeClasses.Find(BBCodeClass.GetTagName, Index) and
    (BBCodeClasses.Objects[Index] = TObject(BBCodeClass))
  then
    BBCodeClasses.Delete(Index);
end;

class function BBCodes.IsRegistered(BBCodeClass: TBBCodeClass): Boolean;
var
  Index: Integer;
begin
  Result := Assigned(BBCodeClasses)
        and BBCodeClasses.Find(BBCodeClass.GetTagName, Index)
        and (BBCodeClasses.Objects[Index] = TObject(BBCodeClass));
end;

class function BBCodes.Find(const TagName: String): TBBCodeClass;
var
  Index: Integer;
begin
  if Assigned(BBCodeClasses) and BBCodeClasses.Find(UpperCase(TagName), Index) then
    Result := TBBCodeClass(BBCodeClasses.Objects[Index])
  else
    Result := nil;
end;

class function BBCodes.Parse(const BBCodeText: String; out PlainText: String;
  BBCodes: TBBCodeObjectList): Boolean;
var
  Stack: TList;
  BBCode: TBBCodeClass;
  Current: TBBCode;
  IsClosingTag: Boolean;
  Tag, Attr: String;
  P, S, T: PChar;
  Len: Integer;
begin
  Result := True;
  Current := nil;
  SetString(PlainText, nil, Length(BBCodeText));
  Stack := TList.Create;
  try
    S := PChar(BBCodeText);
    P := S;
    Len := 0;
    while P^ <> #0 do
    begin
      if P^ = '[' then
      begin
        // more plain text?
        if P <> S then
        begin
          StrLCopy(@PlainText[Len + 1], S, P - S);
          Inc(Len, P - S);
          S := P;
        end;
        Inc(P);
        // is it the escaped '[' character?
        if P^ = '[' then
        begin
          Inc(Len);
          PlainText[Len] := '[';
        end
        else
        begin
          // is it a closing tag?
          if P^ = '/' then
          begin
            IsClosingTag := True;
            Inc(P);
          end
          else
            IsClosingTag := False;
          // get the tag name
          T := P;
          while IsCharAlphaNumeric(P^) do
            Inc(P);
          SetString(Tag, T, P - T);
          // skip blanks
          while (P^ = ' ') or (P^ = #13) or (P^ = #10) or (P^ = #9) do
            Inc(P);
          // is it the closing tag?
          if IsClosingTag then
          begin
            if (P^ = ']') and Assigned(Current) and SameText(Current.TagName, Tag) then
            begin
              Current.SelEnd := Len;
              BBCodes.Add(Current);
              Current := nil;
              // go up one level
              if Stack.Count <> 0 then
              begin
                Current := TBBCode(Stack.Last);
                Stack.Delete(Stack.Count - 1);
              end;
            end
            else
            begin
              Inc(Len);
              PlainText[Len] := '[';
              P := S;
              Result := False;
            end;
          end
          else
          begin
            // is there an attribute
            Attr := '';
            if P^ = '=' then
            begin
              Inc(P);
              T := P;
              while (P^ <> #0) and (P^ <> ']') do
                Inc(P);
              SetString(Attr, T, P - T);
            end;
            BBCode := Find(Tag);
            if (P^ = ']') and Assigned(BBCode) then
            begin
              // put the current BBCode on the stack
              if Assigned(Current) then
                Stack.Add(Current);
              // create new BBCode
              Current := BBCode.Create;
              Current.SelStart := Len;
              Current.Attribute := Trim(Attr);
            end
            else
            begin
              Inc(Len);
              PlainText[Len] := '[';
              P := S;
              Result := False;
            end;
          end;
        end;
        S := P + 1;
        if ((Len = 0) or (PlainText[Len] = #10)) and (S^ = #13) then
        begin
          Dec(Len);
          Inc(S);
        end;
      end;
      Inc(P);
    end;
    // more plain text?
    if P <> S then
    begin
      StrLCopy(@PlainText[Len + 1], S, P - S);
      Inc(Len, P - S);
    end;
    SetLength(PlainText, Len);
    // any open tag?
    while Assigned(Current) do
    begin
      Current.SelEnd := Len;
      BBCodes.Add(Current);
      Current := nil;
      if Stack.Count <> 0 then
      begin
        Current := TBBCode(Stack.Last);
        Stack.Delete(Stack.Count - 1);
      end;
      Result := False;
    end;
  finally
    Stack.Free;
  end;
end;

class procedure BBCodes.Apply(hRichEdit: THandle;
  const BBCodeText: String; Append: Boolean);
var
  BBCodes: TBBCodeObjectList;
  Range: TCharRange;
  PlainText: String;
  I, Offset: Integer;
begin
  BBCodes := TBBCodeObjectList.Create(True);
  try
    Parse(BBCodeText, PlainText, BBCodes);
    SendMessage(hRichEdit, WM_SETREDRAW, 0, 0);
    try
      if not Append then
        SetWindowText(hRichEdit, '');
      Offset := GetWindowTextLength(hRichEdit);
      Range.cpMin := Offset;
      Range.cpMax := Offset;
      SendMessage(hRichEdit, EM_EXSETSEL, 0, LPARAM(@Range));
      SendMessage(hRichEdit, EM_REPLACESEL, 0, LPARAM(PChar(PlainText)));
      for I := BBCodes.Count - 1 downto 0 do
        BBCodes[I].Apply(hRichEdit, Offset);
      SendMessage(hRichEdit, EM_SETSEL, 0, 0);
      SendMessage(hRichEdit, EM_EMPTYUNDOBUFFER, 0, 0);
    finally
      SendMessage(hRichEdit, WM_SETREDRAW, 1, 0);
      InvalidateRect(hRichEdit, nil, True);
    end;
  finally
    BBCodes.Free;
  end;
end;

class procedure BBCodes.Apply(RichEdit: TCustomRichEdit;
  const BBCodeText: String; Append: Boolean);
begin
  Apply(RichEdit.Handle, BBCodeText, Append);
end;

initialization
  BBCodes.Register(TBBCodeBold);
  BBCodes.Register(TBBCodeItalic);
  BBCodes.Register(TBBCodeUnderline);
  BBCodes.Register(TBBCodeStrikeout);
  BBCodes.Register(TBBCodeFontFace);
  BBCodes.Register(TBBCodeFontSize);
  BBCodes.Register(TBBCodeFontColor);
  BBCodes.Register(TBBCodeBackColor);
  BBCodes.Register(TBBCodeLink);
  BBCodes.Register(TBBCodeSubscript);
  BBCodes.Register(TBBCodeSuperscript);
  BBCodes.Register(TBBCodeDisabled);
  BBCodes.Register(TBBCodeProtected);
  BBCodes.Register(TBBCodeHidden);
  BBCodes.Register(TBBCodeOutline);
  BBCodes.Register(TBBCodeShadow);
  BBCodes.Register(TBBCodeEmboss);
  BBCodes.Register(TBBCodeImprint);
  BBCodes.Register(TBBCodeSmallCaps);
  BBCodes.Register(TBBCodeAllCaps);
  BBCodes.Register(TBBCodeRevised);
  BBCodes.Register(TBBCodeRevAuthor);
  BBCodes.Register(TBBCodeCode);
  BBCodes.Register(TBBCodeCenter);
  BBCodes.Register(TBBCodeAlign);
  BBCodes.Register(TBBCodeLineSpacing);
  BBCodes.Register(TBBCodeParagraphSpaceBefore);
  BBCodes.Register(TBBCodeParagraphSpaceAfter);
  BBCodes.Register(TBBCodeIndent);
  BBCodes.Register(TBBCodeRightIndent);
  BBCodes.Register(TBBCodeDir);
  BBCodes.Register(TBBCodeList);
  BBCodes.Register(TBBCodeTabs);
finalization
  BBCodes.Cleanup;
end.
