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
/// This unit implements classes for managing translation catalogs and
/// translation repositories.
/// </summary>
unit i18nCatalog;

{$I DELPHIAREA.INC}

interface

uses
  Windows, SysUtils, Classes, Contnrs, XMLIntf,
  i18nHashList, i18nCore, i18nPlurals, i18nUtils, i18nUnicode;

const

  {$region 'xmldoc'}
  /// <summary>
  /// This constant represents the default extension of translation files.
  /// </summary>
  {$endregion}
  i18nCatalogFileExt = '.i18n';

  {$region 'xmldoc'}
  /// <summary>
  /// This constant represents the default extension of translation repository files.
  /// </summary>
  {$endregion}
  i18nRepositoryFileExt = '.m17n';

type

  {$region 'xmldoc'}
  /// <summary>
  /// ETranslationCatalogError is the exception class for problems that occur while
  /// loading a translation catalog into memory.
  /// </summary>
  /// <seealso cref="TTranslationCatalog"/>
  {$endregion}
  ETranslationCatalogError = class(Exception);

  {$region 'xmldoc'}
  /// <summary>
  /// ETranslationRepositoryError is the exception class for problems that occur
  /// while loading a translation repository into memory.
  /// </summary>
  /// <seealso cref="TTranslationRepository"/>
  {$endregion}
  ETranslationRepositoryError = class(Exception);

  {$region 'xmldoc'}
  /// <summary>
  /// TXMLPersistentHashItem is the base class for all hash item objects that are
  /// able to be saved to or loaded from an XML node.
  /// </summary>
  /// <seealso cref="TXMLPersistentHashList"/>
  {$endregion}
  TXMLPersistentHashItem = class abstract(THashItem)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Reads the object's content from an XML node.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <seealso cref="Save"/>
    {$endregion}
    procedure Load(const Node: IXMLNode); virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Writes the object's content to an XML node.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <seealso cref="Save"/>
    {$endregion}
    procedure Save(const Node: IXMLNode); virtual; abstract;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TXMLPersistentHashList is the base class for all hash list objects that are
  /// able to add an item directly from an XML node.
  /// </summary>
  /// <seealso cref="TXMLPersistentHashItem"/>
  {$endregion}
  TXMLPersistentHashList = class abstract(THashList)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TXMLPersistentHashItem"/> class that represents
    /// individual items of the list.
    /// </summary>
    /// <returns>
    /// The <see cref="TXMLPersistentHashItem"/> class or one of its descendants.
    /// </returns>
    {$endregion}
    function GetItemClass: THashItemClass; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a new item from a specified XML node, and then adds the item to the list.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <returns>
    /// The <see cref="TXMLPersistentHashItem"/> object that is newly added to the list.
    /// </returns>
    {$endregion}
    function Add(const Node: IXMLNode): TXMLPersistentHashItem; overload; virtual; abstract;
  end;

  TTextItems = class;

  {$region 'xmldoc'}
  /// <summary>
  /// TTextItem represents a text string including its translation.
  /// </summary>
  /// <remarks>
  /// <para>
  /// TTextItem stores the original value of a text string and comments regarding its
  /// value. If the text is translated, it also provides the translated version of
  /// the text.
  /// </para>
  /// <para>
  /// TTextItem does not store any of the original and translated languages. If
  /// you need to know about those languages, use <see cref="TTextDefinition"/>
  /// class instead.
  /// </para>
  /// </remarks>
  /// <seealso cref="TTextItems"/>
  /// <seealso cref="TTextDefinition"/>
  {$endregion}
  TTextItem = class(TXMLPersistentHashItem)
  private
    fID: String;
    fComment: String;
    fOriginalValue: String;
    fTranslatedValue: String;
    fIsTranslated: Boolean;
    fTag: Integer;
    procedure SetOriginalValue(const Value: String);
    procedure SetTranslatedValue(const Value: String);
    procedure SetIsTranslated(Value: Boolean);
    function GetValue: String;
    procedure SetValue(const Value: String);
    function GetHasPluralForms: Boolean; inline;
    function GetPluralCount: Integer; inline;
    function GetPlurals(Index: Integer): String; inline;
    procedure SetPlurals(Index: Integer; const PluralForm: String); inline;
    function GetOwnerList: TTextItems; inline;
    function GetPrev: TTextItem; inline;
    function GetNext: TTextItem; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a unique key that identifies the object.
    /// </summary>
    /// <returns>
    /// Returns the unique key of the item.
    /// </returns>
    {$endregion}
    function GetKey: String; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Reads properties of the object from an XML node.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <seealso cref="Save"/>
    {$endregion}
    procedure Load(const Node: IXMLNode); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Writes properties of the object to an XML node.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <seealso cref="Load"/>
    {$endregion}
    procedure Save(const Node: IXMLNode); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    /// <param name="AID">
    /// The unique identifier of the new instance.
    /// </param>
    {$endregion}
    constructor Create(const AID: String); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies another object to this object.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the unique identifier of this object.
    /// </summary>
    {$endregion}
    property ID: String read fID;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the original (untranslated) value of text string.
    /// </summary>
    {$endregion}
    property OriginalValue: String read fOriginalValue write SetOriginalValue;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the translated value of the text string.
    /// </summary>
    {$endregion}
    property TranslatedValue: String read fTranslatedValue write SetTranslatedValue;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets either the original or translated value of the text string.
    /// </summary>
    {$endregion}
    property Value: String read GetValue write SetValue;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the text string is a list of null character separated
    /// sub-strings, which represent plural forms of the text.
    /// </summary>
    /// <seealso cref="PluralCount"/>
    /// <seealso cref="Plurals"/>
    {$endregion}
    property HasPluralForms: Boolean read GetHasPluralForms;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of plural forms that the text string have.
    /// </summary>
    /// <seealso cref="HasPluralForms"/>
    /// <seealso cref="Plurals"/>
    {$endregion}
    property PluralCount: Integer read GetPluralCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the plural forms of the text string.
    /// </summary>
    /// <seealso cref="HasPluralForms"/>
    /// <seealso cref="PluralCount"/>
    {$endregion}
    property Plurals[Index: Integer]: String read GetPlurals write SetPlurals;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets comments regarding the text string.
    /// </summary>
    {$endregion}
    property Comment: String read fComment write fComment;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the text string is translated.
    /// </summary>
    {$endregion}
    property IsTranslated: Boolean read fIsTranslated write SetIsTranslated;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the integer value that is stored as part of the object.
    /// </summary>
    {$endregion}
    property Tag: Integer read fTag write fTag;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TTextItems"/> object that owns this object.
    /// </summary>
    {$endregion}
    Property Owner: TTextItems read GetOwnerList;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the object in the owner <see cref="TTextItems"/> object that is placed
    /// just before this object.
    /// </summary>
    /// <seealso cref="Next"/>
    {$endregion}
    property Prev: TTextItem read GetPrev;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the object in the owner <see cref="TTextItems"/> object that is placed
    /// just after this object.
    /// </summary>
    /// <seealso cref="Prev"/>
    {$endregion}
    property Next: TTextItem read GetNext;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TTextItems maintains a list of <see cref="TTextItem"/> objects.
  /// </summary>
  {$endregion}
  TTextItems = class(TXMLPersistentHashList)
  private
    function GetItems(const ID: String): TTextItem; inline;
    function GetFirst: TTextItem; inline;
    function GetLast: TTextItem; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TXMLPersistentHashItem"/> class that represents
    /// individual items of the list.
    /// </summary>
    /// <returns>
    /// The <see cref="TXMLPersistentHashItem"/> class or one of its descendants.
    /// </returns>
    {$endregion}
    function GetItemClass: THashItemClass; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a new item from a specified XML node, and then adds the item to the list.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <returns>
    /// The <see cref="TXMLPersistentHashItem"/> object that is newly added to the list.
    /// </returns>
    {$endregion}
    function Add(const Node: IXMLNode): TXMLPersistentHashItem; overload; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Removes the translations from all items.
    /// </summary>
    {$endregion}
    procedure DiscardTranslations;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the text strings that are represented by items of this list to a
    /// <see cref="TStrings"/> object.
    /// </summary>
    /// <param name="Dest">
    /// The target <see cref="TStrings"/> object.
    /// </param>
    {$endregion}
    procedure AssignTo(Dest: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Loads the list from a specified <see cref="TReader"/> object.
    /// </summary>
    /// <param name="Reader">
    /// The <see cref="TReader"/> object to load items from.
    /// </param>
    /// <seealso cref="Write"/>
    {$endregion}
    procedure Read(Reader: TReader);
    {$region 'xmldoc'}
    /// <summary>
    /// Saves the list to a specified <see cref="TWriter"/> object.
    /// </summary>
    /// <param name="Writer">
    /// The <see cref="TWriter"/> object to save items to.
    /// </param>
    /// <seealso cref="Read"/>
    {$endregion}
    procedure Write(Writer: TWriter);
    {$region 'xmldoc'}
    /// <summary>
    /// Looks for an item with a specified identifier.
    /// </summary>
    /// <param name="ID">
    /// The identifier of the item to find.
    /// </param>
    /// <returns>
    /// Returns the found <see cref="TTextItem"/> object, or <see langword="nil"/>
    /// if the identifier is not found.
    /// </returns>
    {$endregion}
    function Find(const ID: String): TTextItem; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a new item with a specified identifier and adds it to the list.
    /// </summary>
    /// <param name="ID">
    /// The unique identifier of the new item.
    /// </param>
    /// <returns>
    /// Returns a <see cref="TTextItem"/> object that is either a newly created
    /// object or an already existing object with the specified identifier.
    /// </returns>
    {$endregion}
    function Add(const ID: String): TTextItem; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes an item with a specified identifier from the list.
    /// </summary>
    /// <param name="ID">
    /// The identifier of the item to remove.
    /// </param>
    /// <returns>
    /// Returns the <see cref="TTextItem"/> that is removed from the list, or
    /// <see langword="nil"/> if the item is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Extract(const ID: String): TTextItem; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes an item with a specified identifier from the list and destroys its
    /// object.
    /// </summary>
    /// <param name="ID">
    /// The identifier of the item to remove.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the item is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Extract"/>
    {$endregion}
    function Delete(const ID: String): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the first item in the list.
    /// </summary>
    {$endregion}
    property First: TTextItem read GetFirst;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the last item in the list.
    /// </summary>
    {$endregion}
    property Last: TTextItem read GetLast;
    {$region 'xmldoc'}
    /// <summary>
    /// Provides access to items by their identifier.
    /// </summary>
    {$endregion}
    property Items[const ID: String]: TTextItem read GetItems; default;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the options for merging the translation
  /// catalogs.
  /// </summary>
  {$endregion}
  TMergeOption = (
    {$region 'xmldoc'}
    /// If an entry exists in both source and target, use the source entry.
    {$endregion}
    moPreferSource,
    {$region 'xmldoc'}
    /// If a text domain no longer exists in the source, remove it from the target.
    {$endregion}
    moDeleteDomainsNotInSource,
    {$region 'xmldoc'}
    /// If a text definition no longer exists in the source, remove it from the target.
    {$endregion}
    moDeleteDefinitionsNotInSource
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This data type represents a set of <see cref="TMergeOption"/> values.
  /// </summary>
  {$endregion}
  TMergeOptions = set of TMergeOption;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the options for compacting the translation
  /// catalogs.
  /// </summary>
  {$endregion}
  TCompactOption = (
    {$region 'xmldoc'}
    /// Removes developer's comments.
    {$endregion}
    coDropComments,
    {$region 'xmldoc'}
    /// Removes translator's notes.
    {$endregion}
    coDropNotes,
    {$region 'xmldoc'}
    /// Removes items that do not affect the translation.
    {$endregion}
    coUsablesOnly
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This data type represents a set of <see cref="TCompactOption"/> values.
  /// </summary>
  {$endregion}
  TCompactOptions = set of TCompactOption;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the possible states of a translation.
  /// </summary>
  {$endregion}
  TTranslationState = (
    {$region 'xmldoc'}
    /// The text is not translated.
    {$endregion}
    tsNone,
    {$region 'xmldoc'}
    /// The text is somehow translated, but the translation is not approved yet.
    {$endregion}
    tsFuzzy,
    {$region 'xmldoc'}
    /// The text is automatically translated by the Google's online translator service.
    {$endregion}
    tsGoogle,
    {$region 'xmldoc'}
    /// The text is automatically translated using the knowledge of the past translations.
    {$endregion}
    tsAuto,
    {$region 'xmldoc'}
    /// The text is translated and approved by a translator person.
    {$endregion}
    tsUser
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This data type represents a set of <see cref="TTranslationState"/> values.
  /// </summary>
  {$endregion}
  TTranslationStates = set of TTranslationState;

  {$region 'xmldoc'}
  /// <summary>
  /// This data type stores the statistical information about a translation set.
  /// </summary>
  {$endregion}
  TTranslationStats = record
    {$region 'xmldoc'}
    /// The total number of text entries.
    {$endregion}
    Total: Integer;
    {$region 'xmldoc'}
    /// The number of text entries for each translation state.
    {$endregion}
    SubTotal: array [TTranslationState] of Integer;
    {$region 'xmldoc'}
    /// Returns the number of entries with approved translation.
    {$endregion}
    function Approved: Integer;
    {$region 'xmldoc'}
    /// Returns the ratio of approved translations to total entries.
    {$endregion}
    function Progress: Double;
  end;

  TTextTranslations = class;

  {$region 'xmldoc'}
  /// <summary>
  /// TTextTranslation represents the translation of a text string for a single
  /// language.
  /// </summary>
  /// <remarks>
  /// TTextTranslation stores the translation of a text string along with its state and
  /// translator's comments for a single language.
  /// </remarks>
  /// <seealso cref="TTextTranslations"/>
  /// <seealso cref="TTextDefinition"/>
  {$endregion}
  TTextTranslation = class(TXMLPersistentHashItem)
  private
    fLocale: String;
    fValue: String;
    fState: TTranslationState;
    fNote: String;
    function GetPluralCount: Integer; inline;
    function GetPlurals(Index: Integer): String; inline;
    procedure SetPlurals(Index: Integer; const PluralForm: String); inline;
    function GetOwnerList: TTextTranslations; inline;
    function GetPrev: TTextTranslation; inline;
    function GetNext: TTextTranslation; inline;
  protected
    const
      {$region 'xmldoc'}
      /// <summary>
      /// This constant specifies those <see cref="TTranslationState"/> values that
      /// state a translation acceptable.
      /// </summary>
      /// <seealso cref="IsApproved"/>
      {$endregion}
      AcceptedTranslationStates = [tsUser];
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a unique key that identifies the object.
    /// </summary>
    /// <returns>
    /// Returns the unique key of the item.
    /// </returns>
    {$endregion}
    function GetKey: String; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Reads properties of the object from an XML node.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <seealso cref="Save"/>
    {$endregion}
    procedure Load(const Node: IXMLNode); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Writes properties of the object to an XML node.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <seealso cref="Load"/>
    {$endregion}
    procedure Save(const Node: IXMLNode); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for a specific locale.
    /// </summary>
    /// <param name="ALocale">
    /// The locale that identifies the instance and specifies the language of the
    /// translation.
    /// </param>
    {$endregion}
    constructor Create(const ALocale: String); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies another object to this object.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies another object to this object based on the given <paramref name="Options"/>.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    /// <param name="Options">
    /// Determines how the source object should be copied to this object.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure Merge(Source: TTextTranslation; Options: TMergeOptions = []);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the translation is approved. An approved translation
    /// has <see cref="State"/> of tsUser.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the translation is approved, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="State"/>
    {$endregion}
    function IsApproved: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the locale that identifies the object and determines the language
    /// of the translation.
    /// </summary>
    {$endregion}
    property Locale: String read fLocale;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the translated text.
    /// </summary>
    {$endregion}
    property Value: String read fValue write fValue;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of plural forms in the translation.
    /// </summary>
    /// <seealso cref="Plurals"/>
    {$endregion}
    property PluralCount: Integer read GetPluralCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the plural forms in the translation.
    /// </summary>
    /// <seealso cref="PluralCount"/>
    {$endregion}
    property Plurals[Index: Integer]: String read GetPlurals write SetPlurals;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the state of the translation.
    /// </summary>
    {$endregion}
    property State: TTranslationState read fState write fState;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the translator's notes regarding the translation.
    /// </summary>
    {$endregion}
    property Note: String read fNote write fNote;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TTextTranslations"/> object that owns this object.
    /// </summary>
    {$endregion}
    Property Owner: TTextTranslations read GetOwnerList;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the object in the owner <see cref="TTextTranslations"/> object that is
    /// placed just before this object.
    /// </summary>
    /// <seealso cref="Next"/>
    {$endregion}
    property Prev: TTextTranslation read GetPrev;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the object in the owner <see cref="TTextTranslations"/> object that is
    /// placed just after this object.
    /// </summary>
    /// <seealso cref="Prev"/>
    {$endregion}
    property Next: TTextTranslation read GetNext;
  end;

  TTextDefinition = class;

  {$region 'xmldoc'}
  /// <summary>
  /// TTextItems represents translations of a text string in different languages
  /// by maintaining a list of <see cref="TTextTranslation"/> objects.
  /// </summary>
  {$endregion}
  TTextTranslations = class(TXMLPersistentHashList)
  private
    fDefinition: TTextDefinition;
    function GetItems(const Locale: String): TTextTranslation; inline;
    function GetFirst: TTextTranslation; inline;
    function GetLast: TTextTranslation; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TXMLPersistentHashItem"/> class that represents
    /// individual items of the list.
    /// </summary>
    /// <returns>
    /// The <see cref="TXMLPersistentHashItem"/> class or one of its descendants.
    /// </returns>
    {$endregion}
    function GetItemClass: THashItemClass; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a new item from a specified XML node, and then adds the item to the list.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <returns>
    /// The <see cref="TXMLPersistentHashItem"/> object that is newly added to the list.
    /// </returns>
    {$endregion}
    function Add(const Node: IXMLNode): TXMLPersistentHashItem; overload; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for a specified text.
    /// </summary>
    /// <param name="ADefinition">
    /// The <see cref="TTextDefinition"/> object that represents the text in its
    /// original language.
    /// </param>
    {$endregion}
    constructor Create(ADefinition: TTextDefinition); reintroduce;
    {$region 'xmldoc'}
    /// <summary>
    /// Combines items of another object with items of this object based on the
    /// given options.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    /// <param name="Options">
    /// Determines how the source object should be combined by this object.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure Merge(Source: TTextTranslations; Options: TMergeOptions = []);
    {$region 'xmldoc'}
    /// <summary>
    /// Looks up for an item with a specified locale.
    /// </summary>
    /// <param name="Locale">
    /// The locale of the item to find.
    /// </param>
    /// <returns>
    /// Returns the found <see cref="TTextTranslation"/> object, or <see langword="nil"/>
    /// if the locale is not found.
    /// </returns>
    /// <seealso cref="NearestTo"/>
    {$endregion}
    function Find(const Locale: String): TTextTranslation; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a new item with a specified locale and adds it to the list.
    /// </summary>
    /// <param name="Locale">
    /// The locale of the new item.
    /// </param>
    /// <returns>
    /// Returns a <see cref="TTextTranslation"/> object that is either a newly created
    /// object or an already existing object with the specified locale.
    /// </returns>
    {$endregion}
    function Add(const Locale: String): TTextTranslation; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes an item with a specified locale from the list.
    /// </summary>
    /// <param name="Locale">
    /// The locale of the item to remove.
    /// </param>
    /// <returns>
    /// Returns the <see cref="TTextTranslation"/> that is removed from the list,
    /// or <see langword="nil"/> if the item is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Extract(const Locale: String): TTextTranslation; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes an item with a specified locale from the list and destroys its
    /// object.
    /// </summary>
    /// <param name="Locale">
    /// The locale of the item to remove.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the item is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Extract"/>
    {$endregion}
    function Delete(const Locale: String): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Looks up for an item with the nearest language match to a specified locale.
    /// </summary>
    /// <param name="Locale">
    /// The locale to look for.
    /// </param>
    /// <returns>
    /// Returns the matched <see cref="TTextTranslation"/> object, or <see langword="nil"/>
    /// if no match is found.
    /// </returns>
    /// <seealso cref="Find"/>
    {$endregion}
    function NearestTo(const Locale: String): TTextTranslation;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets state of all translations to fuzzy.
    /// </summary>
    {$endregion}
    procedure DisapproveAll;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TTextDefinition"/> object that represents the original
    /// text of this set of translations.
    /// </summary>
    {$endregion}
    property Definition: TTextDefinition read fDefinition;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the item on top of the list.
    /// </summary>
    {$endregion}
    property First: TTextTranslation read GetFirst;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the item on bottom of the list.
    /// </summary>
    {$endregion}
    property Last: TTextTranslation read GetLast;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the items with their locale.
    /// </summary>
    {$endregion}
    property Items[const Locale: String]: TTextTranslation read GetItems; default;
  end;

  TTextDictionary = class;

  {$region 'xmldoc'}
  /// <summary>
  /// TTextDefinition represents a text string and its translations.
  /// </summary>
  /// <remarks>
  /// TTextDefinition stores a text string along with its comments and translations
  /// for different languages.
  /// </remarks>
  /// <seealso cref="TTextTranslation"/>
  /// <seealso cref="TTextDictionary"/>
  {$endregion}
  TTextDefinition = class(TXMLPersistentHashItem)
  private
    fID: String;
    fComment: String;
    fValue: String;
    fTranslations: TTextTranslations;
    function GetLocale: String;
    function GetHasPluralForms: Boolean; inline;
    function GetPluralCount: Integer; inline;
    function GetPlurals(Index: Integer): String; inline;
    procedure SetPlurals(Index: Integer; const PluralForm: String); inline;
    function GetDictionary: TTextDictionary; inline;
    function GetPrev: TTextDefinition; inline;
    function GetNext: TTextDefinition; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a unique key that identifies the object.
    /// </summary>
    /// <returns>
    /// Returns the unique key of the item.
    /// </returns>
    {$endregion}
    function GetKey: String; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Reads properties of the object from an XML node.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <seealso cref="Save"/>
    {$endregion}
    procedure Load(const Node: IXMLNode); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Writes properties of the object to an XML node.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <seealso cref="Load"/>
    {$endregion}
    procedure Save(const Node: IXMLNode); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    /// <param name="AID">
    /// The unique identifier of the instance.
    /// </param>
    {$endregion}
    constructor Create(const AID: String); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies another object to this object.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    /// <seealso cref="Merge"/>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Combines another object with this object based on the given <paramref name="Options"/>.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    /// <param name="Options">
    /// Determines how the source object should be combined by this object.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure Merge(Source: TTextDefinition; Options: TMergeOptions = []);
    {$region 'xmldoc'}
    /// <summary>
    /// Updates this object with the information provided by a <see cref="TTextItem"/>
    /// object.
    /// </summary>
    /// <param name="Source">
    /// The source <see cref="TTextItem"/> object.
    /// </param>
    {$endregion}
    procedure Revise(Source: TTextItem); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the unique identifier of this object.
    /// </summary>
    {$endregion}
    property ID: String read fID;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the locale of the original language of the text string.
    /// </summary>
    {$endregion}
    property Locale: String read GetLocale;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the original value of the text string.
    /// </summary>
    {$endregion}
    property Value: String read fValue write fValue;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the text string is a list of null character separated
    /// sub-strings, which represent plural forms of the text.
    /// </summary>
    /// <seealso cref="PluralCount"/>
    /// <seealso cref="Plurals"/>
    {$endregion}
    property HasPluralForms: Boolean read GetHasPluralForms;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of plural forms that the text string have.
    /// </summary>
    /// <seealso cref="HasPluralForms"/>
    /// <seealso cref="Plurals"/>
    {$endregion}
    property PluralCount: Integer read GetPluralCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the plural forms of the text string.
    /// </summary>
    /// <seealso cref="HasPluralForms"/>
    /// <seealso cref="PluralCount"/>
    {$endregion}
    property Plurals[Index: Integer]: String read GetPlurals write SetPlurals;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the translations of the text string.
    /// </summary>
    {$endregion}
    property Translations: TTextTranslations read fTranslations;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets comments regarding the text string.
    /// </summary>
    {$endregion}
    property Comment: String read fComment write fComment;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TTextDictionary"/> object that owns this object.
    /// </summary>
    {$endregion}
    Property Dictionary: TTextDictionary read GetDictionary;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the object in the owner <see cref="TTextDefinition"/> object that is
    /// placed just before this object.
    /// </summary>
    /// <seealso cref="Next"/>
    {$endregion}
    property Prev: TTextDefinition read GetPrev;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the object in the owner <see cref="TTextDefinition"/> object that is
    /// placed just after this object.
    /// </summary>
    /// <seealso cref="Prev"/>
    {$endregion}
    property Next: TTextDefinition read GetNext;
  end;

  TTextDomain = class;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the different kinds of usage for
  /// <see cref="TTextDictionary"/> objects.
  /// </summary>
  {$endregion}
  TTextDitionaryKind = (
    {$region 'xmldoc'}
    /// The <see cref="TTextDictionary"/> object contains information regarding
    /// properties.
    {$endregion}
    dkProperty,
    {$region 'xmldoc'}
    /// The <see cref="TTextDictionary"/> object contains information regarding
    /// string literals and string constants.
    {$endregion}
    dkLiteral
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TTextDictionary maintains a list of <see cref="TTextDefinition"/> objects.
  /// </summary>
  {$endregion}
  TTextDictionary = class(TXMLPersistentHashList)
  private
    fTextDomain: TTextDomain;
    fKind: TTextDitionaryKind;
    function GetItems(const ID: String): TTextDefinition; inline;
    function GetFirst: TTextDefinition; inline;
    function GetLast: TTextDefinition; inline;
  protected
    const
      {$region 'xmldoc'}
      /// <summary>
      /// This constant specifies the maximum percentage of changes in a string, so
      /// that the new string still can be expressed as a modified version of the
      /// original one.
      /// </summary>
      /// <seealso cref="Merge"/>
      /// <seealso cref="Revise"/>
      {$endregion}
      AcceptedSimilarityRatio = 0.80;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TXMLPersistentHashItem"/> class that represents
    /// individual items of the list.
    /// </summary>
    /// <returns>
    /// The <see cref="TXMLPersistentHashItem"/> class or one of its descendants.
    /// </returns>
    {$endregion}
    function GetItemClass: THashItemClass; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a new item from a specified XML node, and then adds the item to the list.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <returns>
    /// The <see cref="TXMLPersistentHashItem"/> object that is newly added to the list.
    /// </returns>
    {$endregion}
    function Add(const Node: IXMLNode): TXMLPersistentHashItem; overload; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the object.
    /// </summary>
    /// <param name="ATextDomain">
    /// The <see cref="TTextDomain"/> object that owns this object.
    /// </param>
    /// <param name="AKind">
    /// Determines whether the object provides information about property or
    /// litral strings.
    /// </param>
    {$endregion}
    constructor Create(ATextDomain: TTextDomain; AKind: TTextDitionaryKind); reintroduce;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies another object to this object.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    /// <seealso cref="Merge"/>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Combines items of another object with items of this object based on the
    /// given options.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    /// <param name="Options">
    /// Determines how the source object should be combined by this object.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure Merge(Source: TTextDictionary; Options: TMergeOptions = []);
    {$region 'xmldoc'}
    /// <summary>
    /// Updates this object with the information provided by a <see cref="TTextItems"/>
    /// object.
    /// </summary>
    /// <param name="Source">
    /// The source <see cref="TTextItems"/> object.
    /// </param>
    /// <param name="KeepRemovedEntries">
    /// Indicates whether to keep the entries that are not in the source.
    /// </param>
    {$endregion}
    procedure Revise(Source: TTextItems; KeepRemovedEntries: Boolean);
    {$region 'xmldoc'}
    /// <summary>
    /// Looks up for an item with a specified identifier.
    /// </summary>
    /// <param name="ID">
    /// The identifier of the item to find.
    /// </param>
    /// <returns>
    /// Returns the found <see cref="TTextDefinition"/> object, or <see langword="nil"/>
    /// if the identifier is not found.
    /// </returns>
    {$endregion}
    function Find(const ID: String): TTextDefinition; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a new item with a specified identifier and adds it to the list.
    /// </summary>
    /// <param name="ID">
    /// The unique identifier of the new item.
    /// </param>
    /// <returns>
    /// Returns a <see cref="TTextDefinition"/> object that is either a newly created
    /// object or an already existing object with the specified identifier.
    /// </returns>
    {$endregion}
    function Add(const ID: String): TTextDefinition; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes an item with a specified identifier from the list.
    /// </summary>
    /// <param name="ID">
    /// The identifier of the item to remove.
    /// </param>
    /// <returns>
    /// Returns the <see cref="TTextDefinition"/> that is removed from the list,
    /// or <see langword="nil"/> if the item is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Extract(const ID: String): TTextDefinition; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes an item with a specified identifier from the list and destroys its
    /// object.
    /// </summary>
    /// <param name="ID">
    /// The identifier of the item to remove.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the item is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Extract"/>
    {$endregion}
    function Delete(const ID: String): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Tries to reduce memory usage of items by purging unwanted information.
    /// </summary>
    /// <param name="Options">
    /// Determines how to compact the object.
    /// </param>
    {$endregion}
    procedure Compact(Options: TCompactOptions);
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the translation in a specified language to the original definition, for
    /// all the items.
    /// NOTE: Do not call this method directly.
    /// </summary>
    /// <param name="Locale">
    /// The locale that specifies the language of the source translation.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if all the items has an acceptable translation
    /// for the specified locale. Otherwise, returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="OrigialToTranslation"/>
    {$endregion}
    function TranslationToOriginal(const Locale: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies the original definition to the translation in a specified language, for
    /// all the items.
    /// NOTE: Do not call this method directly.
    /// </summary>
    /// <param name="Locale">
    /// The locale that specifies the language of the target translation.
    /// </param>
    /// <seealso cref="TranslationToOriginal"/>
    {$endregion}
    procedure OrigialToTranslation(const Locale: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a translation entry for a specified locale to all the items.
    /// </summary>
    /// <param name="Locale">
    /// The locale that specifies the language of the translation entry.
    /// </param>
    /// <seealso cref="Undefine"/>
    {$endregion}
    procedure Define(const Locale: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Remove the translation entry for a specified locale from all the items.
    /// </summary>
    /// <param name="Locale">
    /// The locale that specifies the language of the translation entry.
    /// </param>
    /// <seealso cref="Define"/>
    {$endregion}
    procedure Undefine(const Locale: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the list of languages that the items have a translation entry
    /// for them.
    /// </summary>
    /// <param name="Dest">
    /// The list that saves the translation languages as <see cref="TCultureInfo"/>
    /// objects.
    /// </param>
    {$endregion}
    procedure RetrieveCultures(Dest: TCultureList);
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the statistical information about the translation entries of a
    /// specific language.
    /// </summary>
    /// <param name="Locale">
    /// The locale that determines the entries in interest.
    /// </param>
    /// <param name="Stats">
    /// The <see cref="TTranslationStats"/> records that represents the result.
    /// This fields of this record must be initialized.
    /// </param>
    {$endregion}
    procedure UpdateStats(const Locale: String; var Stats: TTranslationStats);
    {$region 'xmldoc'}
    /// <summary>
    /// Finds the translation entry for a specific item in a specific language.
    /// </summary>
    /// <param name="ID">
    /// The unique identifier of the item.
    /// </param>
    /// <param name="Locale">
    /// The locale of the translation entry.
    /// </param>
    /// <returns>
    /// Returns the found <see cref="TTextTranslation"/> object or <see langword="nil"/>
    /// if the translation is not found.
    /// </returns>
    /// <seealso cref="Translate"/>
    {$endregion}
    function TranslationOf(const ID, Locale: String): TTextTranslation;
    {$region 'xmldoc'}
    /// <summary>
    /// Uses this object's information to translate a list of text strings given
    /// by <paramref name="TextItems"/> into a specified language.
    /// </summary>
    /// <param name="TextItems">
    /// The text items to translate.
    /// </param>
    /// <param name="Locale">
    /// The locale that specifies the target translation language.
    /// </param>
    /// <seealso cref="TranslationOf"/>
    {$endregion}
    procedure Translate(TextItems: TTextItems; Locale: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TextDomain"/> object that owns this object.
    /// </summary>
    {$endregion}
    property TextDomain: TTextDomain read fTextDomain;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the items of the list represent translatable properties or
    /// translatable string literals.
    /// </summary>
    {$endregion}
    property Kind: TTextDitionaryKind read fKind;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the item on top of the list.
    /// </summary>
    {$endregion}
    property First: TTextDefinition read GetFirst;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the item on bottom of the list.
    /// </summary>
    {$endregion}
    property Last: TTextDefinition read GetLast;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the items with their identifier.
    /// </summary>
    {$endregion}
    property Items[const ID: String]: TTextDefinition read GetItems; default;
  end;

  TTextDomains = class;

  {$region 'xmldoc'}
  /// <summary>
  /// TTextDomain represents a set of translatable strings in a translation catalog.
  /// </summary>
  /// <remarks>
  /// <para>
  /// TTextDomain maintains two lists of translatable strings, one for properties
  /// and another for string literals. These strings are usually used within the same
  /// Pascal unit.
  /// </para>
  /// </remarks>
  /// <seealso cref="TTextDomains"/>
  /// <seealso cref="TTranslationCatalog"/>
  {$endregion}
  TTextDomain = class(TXMLPersistentHashItem)
  private
    fName: String;
    fProperties: TTextDictionary;
    fLiterals: TTextDictionary;
    function GetOwnerList: TTextDomains; inline;
    function GetPrev: TTextDomain; inline;
    function GetNext: TTextDomain; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a unique key that identifies the object.
    /// </summary>
    /// <returns>
    /// Returns the unique key of the item.
    /// </returns>
    {$endregion}
    function GetKey: String; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Reads properties of the object from an XML node.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <seealso cref="Save"/>
    {$endregion}
    procedure Load(const Node: IXMLNode); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Writes properties of the object to an XML node.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <seealso cref="Load"/>
    {$endregion}
    procedure Save(const Node: IXMLNode); override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of a class.
    /// </summary>
    /// <param name="AName">
    /// The unique name of the instance.
    /// </param>
    {$endregion}
    constructor Create(const AName: String); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies another object to this object.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies another object to this object based on the given <paramref name="Options"/>.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    /// <param name="Options">
    /// Determines how the source object should be copied to this object.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure Merge(Source: TTextDomain; Options: TMergeOptions = []);
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the statistical information about the translations in a specific
    /// language.
    /// </summary>
    /// <param name="Locale">
    /// The locale that determines the language in interest.
    /// </param>
    /// <returns>
    /// Returns a <see cref="TTranslationStats"/> record.
    /// </returns>
    {$endregion}
    function StatsOf(const Locale: String): TTranslationStats;
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the name of components that have translatable property in this domain.
    /// </summary>
    /// <param name="dstList">
    /// The <see cref="TStrings"/> object that receives the name of components.
    /// </param>
    /// <seealso cref="HasComponent"/>
    /// <seealso cref="DeleteComponent"/>
    /// <seealso cref="RenameComponent"/>
    {$endregion}
    procedure GetComponents(dstList: TStrings);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a component has a translatable property in this domain.
    /// </summary>
    /// <param name="ComponentName">
    /// The name of component to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the component has a translatable property
    /// in this domain. Otherwise, returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="GetComponents"/>
    /// <seealso cref="DeleteComponent"/>
    /// <seealso cref="RenameComponent"/>
    {$endregion}
    function HasComponent(const ComponentName: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes properties of a specified component from this domain.
    /// </summary>
    /// <param name="ComponentName">
    /// The name of component to remove its properties.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the component had a translatable property
    /// in this domain. Otherwise, returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="HasComponent"/>
    /// <seealso cref="GetComponents"/>
    /// <seealso cref="RenameComponent"/>
    {$endregion}
    function DeleteComponent(const ComponentName: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates path of properties in the domain for a specified component to reflect
    /// the new name of the component.
    /// </summary>
    /// <param name="ComponentName">
    /// The old name of the component.
    /// </param>
    /// <param name="NewComponentName">
    /// The new name of the component. This name must be a valid Pascal identifier
    /// and the domain must not have already a property for the new component name.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the component renamed successfully. Otherwise,
    /// returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Rename"/>
    /// <seealso cref="HasComponent"/>
    /// <seealso cref="GetComponents"/>
    /// <seealso cref="DeleteComponent"/>
    {$endregion}
    function RenameComponent(const ComponentName, NewComponentName: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// <para>
    /// Changes the unique name of the domain.
    /// </para>
    /// <para>
    /// NOTE: This method destroys the current instance and creates a new one. Therefore,
    /// after calling this method, any access to the old object raises an AV exception.
    /// </para>
    /// </summary>
    /// <param name="NewName">
    /// The new name of the domain. This name must be a valid Pascal identifier and
    /// the <see cref="Owner"/> object must not have already a domain with this name.
    /// </param>
    /// <returns>
    /// Returns the <see cref="TTextDomain"/> object with the new name, or <see langword="nil"/>
    /// if the rename is not possible.
    /// </returns>
    /// <seealso cref="RenameComponent"/>
    {$endregion}
    function Rename(const NewName: String): TTextDomain;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the domain maintains any translatable string.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the domain has any translatable string,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Empty: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the unique name of the domain.
    /// </summary>
    {$endregion}
    property Name: String read fName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the translatable properties in the domain.
    /// </summary>
    {$endregion}
    property Properties: TTextDictionary read fProperties;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the translatable string literals in the domain.
    /// </summary>
    {$endregion}
    property Literals: TTextDictionary read fLiterals;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TTextDomains"/> object that owns this domain.
    /// </summary>
    {$endregion}
    Property Owner: TTextDomains read GetOwnerList;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the domain in the owner <see cref="TTextDomains"/> object that is placed
    /// just before this domain.
    /// </summary>
    /// <seealso cref="Next"/>
    {$endregion}
    property Prev: TTextDomain read GetPrev;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the domain in the owner <see cref="TTextDomains"/> object that is placed
    /// just after this domain.
    /// </summary>
    /// <seealso cref="Prev"/>
    {$endregion}
    property Next: TTextDomain read GetNext;
  end;

  TTranslationCatalog = class;

  {$region 'xmldoc'}
  /// <summary>
  /// TTextDomains maintains a list of <see cref="TTextDomain"/> objects.
  /// </summary>
  {$endregion}
  TTextDomains = class(TXMLPersistentHashList)
  private
    fCatalog: TTranslationCatalog;
    function GetItems(const Name: String): TTextDomain;
    function GetFirst: TTextDomain; inline;
    function GetLast: TTextDomain; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="TXMLPersistentHashItem"/> class that represents
    /// individual items of the list.
    /// </summary>
    /// <returns>
    /// The <see cref="TXMLPersistentHashItem"/> class or one of its descendants.
    /// </returns>
    {$endregion}
    function GetItemClass: THashItemClass; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a new item from a specified XML node, and then adds the item to the list.
    /// </summary>
    /// <param name="Node">
    /// The XML node.
    /// </param>
    /// <returns>
    /// The <see cref="TXMLPersistentHashItem"/> object that is newly added to the list.
    /// </returns>
    {$endregion}
    function Add(const Node: IXMLNode): TXMLPersistentHashItem; overload; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    /// <param name="ACatalog">
    /// The translation catalog that owns the domains.
    /// </param>
    {$endregion}
    constructor Create(ACatalog: TTranslationCatalog); reintroduce;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies another object to this object based on the given <paramref name="Options"/>.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    /// <param name="Options">
    /// Determines how the source object should be copied to this object.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure Merge(Source: TTextDomains; Options: TMergeOptions = []);
    {$region 'xmldoc'}
    /// <summary>
    /// Looks up for a domain with a specified name.
    /// </summary>
    /// <param name="Name">
    /// The name of the domain to find.
    /// </param>
    /// <returns>
    /// Returns the found <see cref="TTextDomain"/> object, or <see langword="nil"/>
    /// if the name is not found.
    /// </returns>
    {$endregion}
    function Find(const Name: String): TTextDomain; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a new domain with a specified name and adds it to the list.
    /// </summary>
    /// <param name="Name">
    /// The name of the new domain.
    /// </param>
    /// <returns>
    /// Returns a <see cref="TTextDomain"/> object that is either a newly created
    /// domain or an already existing domain with the specified name.
    /// </returns>
    {$endregion}
    function Add(const Name: String): TTextDomain; overload; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes a domain with a specified name from the list.
    /// </summary>
    /// <param name="Name">
    /// The name of the domain to remove.
    /// </param>
    /// <returns>
    /// Returns the <see cref="TTextDomain"/> that is removed from the list, or
    /// <see langword="nil"/> if the domain is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Extract(const Name: String): TTextDomain; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes a domain with a specified name from the list and destroys its object.
    /// </summary>
    /// <param name="Name">
    /// The name of the domain to remove.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the item is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Extract"/>
    {$endregion}
    function Delete(const Name: String): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the translation catalog that owns this domains.
    /// </summary>
    {$endregion}
    property Catalog: TTranslationCatalog read fCatalog;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the domain in top of the list.
    /// </summary>
    {$endregion}
    property First: TTextDomain read GetFirst;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the domain in bottom of the list.
    /// </summary>
    {$endregion}
    property Last: TTextDomain read GetLast;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the domains by their names.
    /// </summary>
    {$endregion}
    property Items[const Name: String]: TTextDomain read GetItems; default;
  end;

  TTranslationRepository = class;

  {$region 'xmldoc'}
  /// <summary>
  /// TTranslationCatalog represents a translation catalog, which contains information
  /// to translate strings of an application.
  /// </summary>
  /// <remarks>
  /// TTranslationCatalog stores information about a translation set. This information
  /// includes original and translated strings, available translation languages, and
  /// plural forms of the languages. Usually each application uses only one translation
  /// catalog.
  /// </remarks>
  {$endregion}
  TTranslationCatalog = class(TPersistent)
  private
    fNativeCulture: TCultureInfo;
    fCultures: TCultureList;
    fTextDomains: TTextDomains;
    fCustomPluralRules: TKeyLookup<TCultureInfo,String>;
    fCompressed: Boolean;
    fUpdateCount: Integer;
    fRepository: TTranslationRepository;
    function GetCultures: TReadonlyCultureList; inline;
    procedure SetNativeCulture(Value: TCultureInfo);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Ensures all text definitions in all text domains have the same set of
    /// translation entries. It also deletes empty text domains.
    /// </summary>
    {$endregion}
    procedure NormalizeTranslations;
    {$region 'xmldoc'}
    /// <summary>
    /// Ensures the <see cref="Cultures"/> property lists all all the available
    /// translation languages.
    /// </summary>
    {$endregion}
    procedure NormalizeCultures;
    {$region 'xmldoc'}
    /// <summary>
    /// Gives access to the custom rules of plural forms for the translation languages.
    /// </summary>
    {$endregion}
    property CustomPluralRules: TKeyLookup<TCultureInfo,String> read fCustomPluralRules;
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
    /// Indicates whether the translation catalog contains any information.
    /// </summary>
    /// <returns>
    /// returns <see langword="true"/> if the translation catalog is empty, otherwise
    /// returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Empty: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Clears all the translation catalog.
    /// </summary>
    {$endregion}
    procedure Clear;
    {$region 'xmldoc'}
    /// <summary>
    /// Prevents rearranging and normalizing content until the <see cref="EndUpdate"/>
    /// method is called.
    /// </summary>
    /// <seealso cref="EndUpdate"/>
    {$endregion}
    procedure BeginUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Reenables rearranging and normalizing content that was turned off with the
    /// <see cref="BeginUpdate"/> method.
    /// </summary>
    /// <seealso cref="BeginUpdate"/>
    {$endregion}
    procedure EndUpdate;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes unwanted information from the translation catalog.
    /// </summary>
    /// <param name="Options">
    /// Specifies which content should be purged and which one should be kept.
    /// </param>
    {$endregion}
    procedure Compact(Options: TCompactOptions);
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a new language to the translation catalog.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object that represents the new language.
    /// </param>
    {$endregion}
    procedure Add(Culture: TCultureInfo);
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes a translation language and all of its related information from the
    /// translation catalog.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object that represents the language to delete.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the language was in the list of available
    /// translation languages, and removed. Otherwise, returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Remove(Culture: TCultureInfo): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies another translation catalog to this one.
    /// </summary>
    /// <param name="Source">
    /// The source object.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Combines another translation catalog with this one based on the given
    /// <paramref name="Options"/>.
    /// </summary>
    /// <param name="Source">
    /// The source translation catalog.
    /// </param>
    /// <param name="Options">
    /// Determines how the two catalogs should be merged.
    /// </param>
    /// <seealso cref="Assign"/>
    {$endregion}
    procedure Merge(Source: TTranslationCatalog; Options: TMergeOptions = []);
    {$region 'xmldoc'}
    /// <summary>
    /// Retrieves the statistical information about the translations in a specific
    /// language.
    /// </summary>
    /// <param name="Locale">
    /// The <see cref="TCultureInfo"/> object that represents the language in
    /// interest.
    /// </param>
    /// <returns>
    /// Returns a <see cref="TTranslationStats"/> record.
    /// </returns>
    {$endregion}
    function StatsOf(Culture: TCultureInfo): TTranslationStats;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the rule for the plural forms of a specific translation language.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object that represents the language in
    /// interest.
    /// </param>
    /// <returns>
    /// The rule of plural forms of the language as a steing.
    /// </returns>
    /// <seealso cref="ChangePluralRuleOf"/>
    /// <seealso cref="TPluralForms.Rule"/>
    {$endregion}
    function PluralRuleOf(Culture: TCultureInfo): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Sets a new rule for the plural forms of a specific translation language.
    /// NOTE: This method does not validate the plural rule.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object that represents the language in
    /// interest.
    /// </param>
    /// <param name="PluralRule">
    /// The rule of plural forms of the language as a steing.
    /// </param>
    /// <seealso cref="PluralRuleOf"/>
    /// <seealso cref="TPluralForms.Rule"/>
    {$endregion}
    procedure ChangePluralRuleOf(Culture: TCultureInfo; const PluralRule: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Uses the translation in a specified language for all instances of a specified
    /// string in the translation catalog.
    /// </summary>
    /// <param name="OriginalText">
    /// The text to use the translatin.
    /// </param>
    /// <param name="TranslatedText">
    /// The translation of the text given by <paramref name="OriginalText"/>
    /// in the language specified by <paramref name="Culture"/>.
    /// </param>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object that represents the language of the
    /// translation.
    /// </param>
    /// <param name="AffectedTranslations">
    /// The list of <see cref="TTextTranslation"/> objects that are affected by
    /// this method.
    /// </param>
    /// <returns>
    /// The number of affected <see cref="TTextTranslation"/> object.
    /// </returns>
    /// <seealso cref="UseRepository"/>
    {$endregion}
    function UseTranslation(const OriginalText: String;
      const TranslatedText: String; Culture: TCultureInfo;
      AffectedTranslations: TList = nil): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Uses the linked translation repository to translate all the untranslated
    /// strings in the translation catalog.
    /// </summary>
    /// <param name="AffectedTranslations">
    /// The list of <see cref="TTextTranslation"/> objects that are affected by
    /// this method.
    /// </param>
    /// <returns>
    /// The number of affected <see cref="TTextTranslation"/> object.
    /// </returns>
    /// <seealso cref="UseTranslation"/>
    /// <seealso cref="Repository"/>
    {$endregion}
    function UseRepository(AffectedTranslations: TList = nil): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Uses the linked translation repository to translate the untranslated
    /// strings of a given language in the translation catalog.
    /// </summary>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object that represents the language in
    /// interest.
    /// </param>
    /// <param name="AffectedTranslations">
    /// The list of <see cref="TTextTranslation"/> objects that are affected by
    /// this method.
    /// </param>
    /// <returns>
    /// The number of affected <see cref="TTextTranslation"/> object.
    /// </returns>
    /// <seealso cref="UseTranslation"/>
    /// <seealso cref="Repository"/>
    {$endregion}
    function UseRepository(Culture: TCultureInfo; AffectedTranslations: TList = nil): Integer; overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Writes the translation catalog to an XML document.
    /// </summary>
    /// <param name="Document">
    /// The target XML document.
    /// </param>
    /// <seealso cref="SaveToStream"/>
    /// <seealso cref="SaveToFile"/>
    {$endregion}
    procedure SaveToXML(const Document: IXMLDocument);
    {$region 'xmldoc'}
    /// <summary>
    /// Writes the translation catalog to a stream.
    /// </summary>
    /// <param name="Stream">
    /// The target stream.
    /// </param>
    /// <seealso cref="SaveToXML"/>
    /// <seealso cref="SaveToFile"/>
    /// <seealso cref="Compressed"/>
    {$endregion}
    procedure SaveToStream(Stream: TStream);
    {$region 'xmldoc'}
    /// <summary>
    /// Writes the translation catalog to a file.
    /// </summary>
    /// <param name="FileName">
    /// The path to the target file. The path can use environment variables.
    /// For convenience, a relative path is considered ro be relative to the
    /// application's directory rather than the current directory.
    /// </param>
    /// <seealso cref="SaveToXML"/>
    /// <seealso cref="SaveToStream"/>
    /// <seealso cref="Compressed"/>
    {$endregion}
    procedure SaveToFile(const FileName: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Reads the translation catalog from an XML document.
    /// </summary>
    /// <param name="Document">
    /// The source XML document.
    /// </param>
    /// <seealso cref="LoadFromStream"/>
    /// <seealso cref="LoadFromResource"/>
    /// <seealso cref="LoadFromFile"/>
    /// <seealso cref="LoadFromFiles"/>
    /// <seealso cref="LoadFromDirectory"/>
    /// <seealso cref="Open"/>
    /// <exception cref="ETranslationCatalogError">
    /// Occurs when the method encounters a problem while loading the translation
    /// catalog.
    /// </exception>
    {$endregion}
    procedure LoadFromXML(const Document: IXMLDocument);
    {$region 'xmldoc'}
    /// <summary>
    /// Reads the translation catalog from a stream.
    /// </summary>
    /// <param name="Stream">
    /// The source stream.
    /// </param>
    /// <seealso cref="LoadFromXML"/>
    /// <seealso cref="LoadFromResource"/>
    /// <seealso cref="LoadFromFile"/>
    /// <seealso cref="LoadFromFiles"/>
    /// <seealso cref="LoadFromDirectory"/>
    /// <seealso cref="Open"/>
    /// <exception cref="ETranslationCatalogError">
    /// Occurs when the method encounters a problem while loading the translation
    /// catalog.
    /// </exception>
    {$endregion}
    procedure LoadFromStream(Stream: TStream);
    {$region 'xmldoc'}
    /// <summary>
    /// Reads the translation catalog from a file.
    /// </summary>
    /// <param name="FileName">
    /// The path to the source file. The path can use environment variables.
    /// For convenience, a relative path is considered ro be relative to the
    /// application's directory rather than the current directory.
    /// </param>
    /// <seealso cref="LoadFromXML"/>
    /// <seealso cref="LoadFromStream"/>
    /// <seealso cref="LoadFromResource"/>
    /// <seealso cref="LoadFromFiles"/>
    /// <seealso cref="LoadFromDirectory"/>
    /// <seealso cref="Open"/>
    /// <exception cref="ETranslationCatalogError">
    /// Occurs when the method encounters a problem while loading the translation
    /// catalog.
    /// </exception>
    {$endregion}
    procedure LoadFromFile(const FileName: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Reads the translation catalog from a particular resource name and type.
    /// </summary>
    /// <param name="hInstance">
    /// The instance handle associated with the executable or shared library that
    /// contains the resource
    /// </param>
    /// <param name="ResName">
    /// The name of the resource.
    /// </param>
    /// <param name="ResType">
    /// The type of the resource.
    /// </param>
    /// <seealso cref="LoadFromXML"/>
    /// <seealso cref="LoadFromStream"/>
    /// <seealso cref="LoadFromFile"/>
    /// <seealso cref="LoadFromFiles"/>
    /// <seealso cref="LoadFromDirectory"/>
    /// <seealso cref="Open"/>
    /// <exception cref="ETranslationCatalogError">
    /// Occurs when the method encounters a problem while loading the translation
    /// catalog.
    /// </exception>
    {$endregion}
    procedure LoadFromResource(hInstance: HINST; const ResName: String;
      ResType: PChar = RT_RCDATA); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Reads the translation catalog from a particular resource ID and type.
    /// </summary>
    /// <param name="hInstance">
    /// The instance handle associated with the executable or shared library that
    /// contains the resource
    /// </param>
    /// <param name="ResID">
    /// The ID of the resource.
    /// </param>
    /// <param name="ResType">
    /// The type of the resource.
    /// </param>
    /// <seealso cref="LoadFromXML"/>
    /// <seealso cref="LoadFromStream"/>
    /// <seealso cref="LoadFromFile"/>
    /// <seealso cref="LoadFromFiles"/>
    /// <seealso cref="LoadFromDirectory"/>
    /// <seealso cref="Open"/>
    /// <exception cref="ETranslationCatalogError">
    /// Occurs when the method encounters a problem while loading the translation
    /// catalog.
    /// </exception>
    {$endregion}
    procedure LoadFromResource(hInstance: HINST; ResID: Integer;
      ResType: PChar = RT_RCDATA); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Reads a set of translation catalogs from a specified list of files and
    /// merge them together.
    /// </summary>
    /// <param name="FileNames">
    /// The list of file names. The file paths can use environment variables.
    /// For convenience, a relative path is considered ro be relative to the
    /// application's directory rather than the current directory.
    /// </param>
    /// <param name="IgnoreErrors">
    /// Indicates whether failure in reading any file should fail the entire
    /// process.
    /// </param>
    /// <seealso cref="LoadFromXML"/>
    /// <seealso cref="LoadFromStream"/>
    /// <seealso cref="LoadFromResource"/>
    /// <seealso cref="LoadFromFile"/>
    /// <seealso cref="LoadFromDirectory"/>
    /// <seealso cref="Open"/>
    /// <exception cref="ETranslationCatalogError">
    /// Occurs when the method encounters a problem while loading the translation
    /// catalog.
    /// </exception>
    {$endregion}
    procedure LoadFromFiles(FileNames: TStrings; IgnoreErrors: Boolean);
    {$region 'xmldoc'}
    /// <summary>
    /// Searches in a directory for translation catalogs and merge them together.
    /// </summary>
    /// <param name="Path">
    /// The path to the directory. The path can use environment variables.
    /// For convenience, a relative path is considered ro be relative to the
    /// application's directory rather than the current directory.
    /// </param>
    /// <param name="IncludeSubDirs">
    /// Indicates whether the search should follow the sub-directories.
    /// </param>
    /// <param name="FileMask">
    /// Specifies the file mask for selecting the files in interest.
    /// </param>
    /// <param name="IgnoreErrors">
    /// Indicates whether failure in reading any file should fail the entire
    /// process.
    /// </param>
    /// <seealso cref="LoadFromXML"/>
    /// <seealso cref="LoadFromStream"/>
    /// <seealso cref="LoadFromResource"/>
    /// <seealso cref="LoadFromFile"/>
    /// <seealso cref="LoadFromFiles"/>
    /// <seealso cref="Open"/>
    /// <exception cref="ETranslationCatalogError">
    /// Occurs when the method encounters a problem while loading the translation
    /// catalog.
    /// </exception>
    {$endregion}
    procedure LoadFromDirectory(const Path: String;
      IncludeSubDirs: Boolean = False; const FileMask: String = '';
      IgnoreErrors: Boolean = True);
    {$region 'xmldoc'}
    /// <summary>
    /// Loads a translation catalog form a specified location.
    /// </summary>
    /// <remarks>
    /// The <paramref name="URI"/> parameter determines the location of the translation
    /// catalog to load. It can be simply a path to a translation catalog file, but it
    /// can also address a directory or an application's resource. The file and directory
    /// paths can use environment variables.
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
    ///     </code>
    ///     </example>
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
    ///     </code>
    ///     </example>
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
    ///     </code>
    ///     </example>
    ///   </item>
    ///   <item>
    ///     <term>
    ///     [file:]&lt;FilePath&gt;</term>
    ///     <description>
    ///     Load the translation catalog from a specified file.</description>
    ///     <example><code>
    ///     Localizer1.URI := 'file:langs\MyApp.i18n';
    ///     Localizer2.URI := 'langs\MyApp.i18n';
    ///     </code>
    ///     </example>
    ///   </item>
    /// </list>
    /// NOTE: For convenience, a relative file or directory is considered to be
    /// relative to the application's directory instead of the current directory.
    /// </remarks>
    /// <example><code>
    /// Localizer1.URI := 'dir:%ALLUSERSPROFILE%\MyApp\';
    /// Localizer2.URI := '%USERPROFILE%\MyApp\myapp.i18n';
    /// </code>
    /// </example>
    /// <param name="URL">
    /// The address that specifies the location of a catalog to load.
    /// </param>
    /// <seealso cref="LoadFromXML"/>
    /// <seealso cref="LoadFromStream"/>
    /// <seealso cref="LoadFromResource"/>
    /// <seealso cref="LoadFromFile"/>
    /// <seealso cref="LoadFromFiles"/>
    /// <seealso cref="LoadFromDirectory"/>
    /// <exception cref="ETranslationCatalogError">
    /// Occurs when the method encounters a problem while loading the translation
    /// catalog.
    /// </exception>
    {$endregion}
    procedure Open(const URI: String); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the text domain in the translation catalog.
    /// </summary>
    {$endregion}
    property TextDomains: TTextDomains read fTextDomains;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the <see cref="TCultureInfo"/> objects that represent the available
    /// translation languages of the translation catalog.
    /// </summary>
    /// <seealso cref="NativeCulture"/>
    {$endregion}
    property Cultures: TReadonlyCultureList read GetCultures;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TCultureInfo"/> object that represents the original language
    /// of strings in the translation catalog.
    /// </summary>
    /// <seealso cref="Cultures"/>
    {$endregion}
    property NativeCulture: TCultureInfo read fNativeCulture write SetNativeCulture;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the translation catalog should be saved in compressed
    /// format.
    /// </summary>
    {$endregion}
    property Compressed: Boolean read fCompressed write fCompressed;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the translation repository that is linked to this translation
    /// catalog.
    /// </summary>
    {$endregion}
    property Repository: TTranslationRepository read fRepository write fRepository;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TTranslationRepository represents a translation repository, which contains
  /// information about the past translations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// TTranslationRepository maintains a persistent history of the translations.
  /// This information can be used later to automatically translate strings that
  /// were recorded.
  /// </para>
  /// <para>
  /// TTranslationRepository provides some methods to add new entries to the
  /// translation repository and use the translation history to translate new
  /// text.
  /// </para>
  /// </remarks>
  {$endregion}
  TTranslationRepository = class(TObject)
  private
    type
      TTranslationRepositoryItem = class
      public
        Key: String;
        Culture: TCultureInfo;
        Text: String;
        Hits: Integer;
        Relatives: TStringList;         { Locale => TTranslationRepositoryItem }
      end;
  private
    Items: TStringList;                 { Key => TTranslationRepositoryItem }
    Relations: TObjectList;             { List of all TTranslationRepositoryItem.Relatives }
    fModified: Boolean;
    function GetPhraseCount: Integer;
    function GetRelationCount: Integer;
    function CreateRelationship: TStringList;
    function KeyOf(const Text: String; Culture: TCultureInfo): String;
    function Find(const Text: String; Culture: TCultureInfo;
      out Item: TTranslationRepositoryItem): Boolean;
    function FindOrCreate(const Text: String;
      Culture: TCultureInfo): TTranslationRepositoryItem; overload;
    function FindOrCreate(const Key, Text: String;
      Culture: TCultureInfo): TTranslationRepositoryItem; overload;
    procedure Relate(Item1, Item2: TTranslationRepositoryItem);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    {$endregion}
    constructor Create; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the class and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Clears content of the translation repository.
    /// </summary>
    {$endregion}
    procedure Clear;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the translation repository contains any information.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the translation repository is empty,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Empty: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes the translation history of a text string in a specific language.
    /// </summary>
    /// <param name="Text">
    /// The text to remove its translation history.
    /// </param>
    /// <param name="Culture">
    /// The <see cref="TCultureInfo"/> object that represents the language of
    /// the given <paramref name="Text"/>.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the text string had any history to remove,
    /// otherwise returns <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Remove(const Text: String; Culture: TCultureInfo): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Defines two text strings in different (or same) languages as synonyms.
    /// </summary>
    /// <param name="Text1">
    /// The first text string.
    /// </param>
    /// <param name="Culture1">
    /// The <see cref="TCultureInfo"/> object that represents the language of
    /// the given <paramref name="Text1"/>.
    /// </param>
    /// <param name="Text2">
    /// The second text string.
    /// </param>
    /// <param name="Culture2">
    /// The <see cref="TCultureInfo"/> object that represents the language of
    /// the given <paramref name="Text2"/>.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the two text strings are defined synonyms
    /// successfully. Otherwise, returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="HasSynonym"/>
    /// <seealso cref="FindSynonym"/>
    /// <seealso cref="FindAllSynonyms"/>
    {$endregion}
    function DefineSynonym(const Text1: String; Culture1: TCultureInfo;
      const Text2: String; Culture2: TCultureInfo): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a text string in a specified language has a synonym
    /// in another specified language.
    /// </summary>
    /// <param name="SourceText">
    /// The text string to examine.
    /// </param>
    /// <param name="SourceCulture">
    /// The <see cref="TCultureInfo"/> object that represents the language of
    /// the given <paramref name="SourceText"/>.
    /// </param>
    /// <param name="TargetCulture">
    /// The <see cref="TCultureInfo"/> object that represents the language of
    /// the requested synonym.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the text strings has a synonym in the
    /// specified language, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefineSynonym"/>
    /// <seealso cref="FindSynonym"/>
    /// <seealso cref="FindAllSynonyms"/>
    {$endregion}
    function HasSynonym(const SourceText: String;
      SourceCulture, TargetCulture: TCultureInfo): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// For a specified text in a specified language, retrieves its synonym in
    /// another specified language.
    /// </summary>
    /// <param name="SourceText">
    /// The text string to find its synonym.
    /// </param>
    /// <param name="SourceCulture">
    /// The <see cref="TCultureInfo"/> object that represents the language of
    /// the given <paramref name="SourceText"/>.
    /// </param>
    /// <param name="TargetCulture">
    /// The <see cref="TCultureInfo"/> object that represents the language of
    /// the returned <paramref name="Synonym"/>.
    /// </param>
    /// <param name="Synonym">
    /// Returns the found synonym.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the text strings has a synonym in the
    /// specified language, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="DefineSynonym"/>
    /// <seealso cref="HasSynonym"/>
    /// <seealso cref="FindAllSynonyms"/>
    {$endregion}
    function FindSynonym(const SourceText: String;
      SourceCulture, TargetCulture: TCultureInfo;
      out Synonym: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// For a specified text in a specified language, retrieves all of its synonyms
    /// in another specified language.
    /// </summary>
    /// <param name="SourceText">
    /// The text string to find its synonyms.
    /// </param>
    /// <param name="SourceCulture">
    /// The <see cref="TCultureInfo"/> object that represents the language of
    /// the given <paramref name="SourceText"/>.
    /// </param>
    /// <param name="TargetCulture">
    /// The <see cref="TCultureInfo"/> object that represents the language of
    /// the returned <paramref name="Synonyms"/>.
    /// </param>
    /// <param name="Synonyms">
    /// Returns the list of found synonyms.
    /// </param>
    /// <returns>
    /// Returns the number of found synonyms for the given text string.
    /// </returns>
    /// <remarks>
    /// Each individual synonym in the <paramref name="Synonyms"/> list is
    /// escaped using <see cref="EscapeString"/> global function.
    /// </remarks>
    /// <seealso cref="DefineSynonym"/>
    /// <seealso cref="HasSynonym"/>
    /// <seealso cref="FindSynonym"/>
    {$endregion}
    function FindAllSynonyms(const SourceText: String;
      SourceCulture, TargetCulture: TCultureInfo;
      Synonyms: TStrings): Integer;
    {$region 'xmldoc'}
    /// <summary>
    /// Improves the translation repository using the translated strings of a
    /// translation catalog.
    /// </summary>
    /// <param name="Catalog">
    /// The source translation catalog to learn from.
    /// </param>
    /// <seealso cref="Merge"/>
    {$endregion}
    procedure Learn(Catalog: TTranslationCatalog);
    {$region 'xmldoc'}
    /// <summary>
    /// Combines information of another translation repository with this one.
    /// </summary>
    /// <param name="Repository">
    /// The source translation repository.
    /// </param>
    /// <seealso cref="Learn"/>
    {$endregion}
    procedure Merge(Repository: TTranslationRepository);
    {$region 'xmldoc'}
    /// <summary>
    /// Writes the translation repository to an XML document.
    /// </summary>
    /// <param name="Document">
    /// The target XML document.
    /// </param>
    /// <seealso cref="SaveToStream"/>
    /// <seealso cref="SaveToFile"/>
    {$endregion}
    procedure SaveToXML(const Document: IXMLDocument);
    {$region 'xmldoc'}
    /// <summary>
    /// Writes the translation repository to a stream.
    /// </summary>
    /// <param name="Stream">
    /// The target stream.
    /// </param>
    /// <seealso cref="SaveToXML"/>
    /// <seealso cref="SaveToFile"/>
    {$endregion}
    procedure SaveToStream(Stream: TStream);
    {$region 'xmldoc'}
    /// <summary>
    /// Writes the translation repository to a file.
    /// </summary>
    /// <param name="FileName">
    /// The path to the target file. The path can use environment variables.
    /// For convenience, a relative path is considered ro be relative to the
    /// application's directory rather than the current directory.
    /// </param>
    /// <seealso cref="SaveToXML"/>
    /// <seealso cref="SaveToStream"/>
    {$endregion}
    procedure SaveToFile(const FileName: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Reads the translation repository from an XML document.
    /// </summary>
    /// <param name="Document">
    /// The source XML document.
    /// </param>
    /// <seealso cref="LoadFromStream"/>
    /// <seealso cref="LoadFromFile"/>
    /// <exception cref="ETranslationRepositoryError">
    /// Occurs when the method encounters a problem while loading the translation
    /// repository.
    /// </exception>
    {$endregion}
    procedure LoadFromXML(const Document: IXMLDocument);
    {$region 'xmldoc'}
    /// <summary>
    /// Reads the translation repository from a stream.
    /// </summary>
    /// <param name="Stream">
    /// The source stream.
    /// </param>
    /// <seealso cref="LoadFromXML"/>
    /// <seealso cref="LoadFromFile"/>
    /// <exception cref="ETranslationRepositoryError">
    /// Occurs when the method encounters a problem while loading the translation
    /// repository.
    /// </exception>
    {$endregion}
    procedure LoadFromStream(Stream: TStream);
    {$region 'xmldoc'}
    /// <summary>
    /// Reads the translation repository from a file.
    /// </summary>
    /// <param name="FileName">
    /// The path to the source file. The path can use environment variables.
    /// For convenience, a relative path is considered ro be relative to the
    /// application's directory rather than the current directory.
    /// </param>
    /// <seealso cref="LoadFromXML"/>
    /// <seealso cref="LoadFromStream"/>
    /// <exception cref="ETranslationRepositoryError">
    /// Occurs when the method encounters a problem while loading the translation
    /// repository.
    /// </exception>
    {$endregion}
    procedure LoadFromFile(const FileName: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of text strings in the translation repository.
    /// </summary>
    {$endregion}
    property PhraseCount: Integer read GetPhraseCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of relationships in the translation repository.
    /// </summary>
    {$endregion}
    property RelationCount: Integer read GetRelationCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets whether the translation repository is modified.
    /// </summary>
    {$endregion}
    property Modified: Boolean read fModified write fModified;
  end;

implementation

uses
  Types, XMLDoc, ZLib, StrUtils, Character, i18nMD5, i18nZStrList;

resourcestring
  SCatalogLoadError1    = 'The translation catalog cannot be loaded because its format is not supported';
  SCatalogLoadError2    = 'Unable to load the translation catalog: %s';
  SRepositoryLoadError1 = 'The translation repository cannot be loaded because its format is not supported';
  SRepositoryLoadError2 = 'Unable to load the translation repository: %s';
  SDirNotFoundError     = 'Directory not found: %s';
  SSchemaError          = '''%s:'' is not supported';

{ Helper Functions }

function NormalizePath(const Path: String): String;
begin
  Result := ExpandEnvStr(Path);
  if (ExtractFileDrive(Result) = '') and (Pos('\', ExtractFileDir(Result)) <> 1) then
    Result := ExtractFilePath(ParamStr(0)) + Result;
end;

function IsTrivial(C: Char): Boolean;
begin
  Result := (C <> #0)
        and (CharIsWhiteSpace(C) or CharIsSeparator(C) or
             CharInSet(C, ['(', ')', '[', ']', '<', '>', '|', ':', '.']));
end;

function TrimTrivials(const Str: String): String;
var
  I, J: Integer;
begin
  I := 1;
  J := Length(Str);
  while (I <= J) and IsTrivial(Str[I]) do
    Inc(I);
  while (J >= I) and IsTrivial(Str[J]) do
    Dec(J);
  Result := Copy(Str, I, J - I + 1);
end;

function ModelTrivials(const Str, Template: String): String;
var
  Leading, Trailing: String;
  I, J: Integer;
begin
  I := 1;
  J := Length(Template);
  while (I <= J) and IsTrivial(Template[I]) do
    Inc(I);
  while (J >= I) and IsTrivial(Template[J]) do
    Dec(J);
  Leading := Copy(Template, 1, I - 1);
  Trailing := Copy(Template, J + 1, MaxInt);
  I := 1;
  J := Length(Str);
  while (I <= J) and IsTrivial(Str[I]) do
    Inc(I);
  while (J >= I) and IsTrivial(Str[J]) do
    Dec(J);
  Result := Leading + Copy(Str, I, J - I + 1) + Trailing;
end;

function LettersOnly(const Str: String): String;
var
  I, N: Integer;
begin
  SetString(Result, nil, Length(Str));
  N := 0;
  for I := 1 to Length(Str) do
    if CharIsLetter(Str[I]) or (Str[I] = #0) then
    begin
      Inc(N);
      Result[N] := CharToUpper(Str[I]);
    end;
  SetLength(Result, N);
end;

function IsXML(Stream: TStream): Boolean;

  function Swap(WC: WideChar): WideChar; inline;
  begin
    Result := WideChar(System.Swap(Word(WC)));
  end;

const
  UNICODE_BOM: WideChar = #$FEFF;
  UNICODE_BOM_SWAPPED: WideChar = #$FFFE;
  UTF8_BOM: PAnsiChar = #$EF#$BB#$BF;
const
  xmlSignW: PWideChar = '<?xml';
  xmlSignA: PAnsiChar = '<?xml';
var
  BufferW: array [1..7] of WideChar;
  BufferA: array [1..9] of AnsiChar absolute BufferW;
  BOM: WideChar absolute BufferW;
  BytesRead, I: Integer;
begin
  Result := False;
  BytesRead := Stream.Read(BufferW, SizeOf(BufferW));
  try
    if BytesRead = SizeOf(BufferW) then
    begin
      if (BOM = UNICODE_BOM_SWAPPED) or (BufferW[1] = Swap('<')) then
      begin
        for I := 1 to BytesRead div 2 do
          BufferW[I] := Swap(BufferW[I]);
      end;
      if BOM = UNICODE_BOM then
      begin
        BufferW[Length(xmlSignW) + 2] := #0;
        Result := (lstrcmpiW(@BufferW[2], xmlSignW) = 0);
      end
      else if CompareMem(@BufferA[1], UTF8_BOM, 3) then
      begin
        BufferA[Length(xmlSignA) + 4] := #0;
        Result := (lstrcmpiA(@BufferA[4], xmlSignA) = 0);
      end
      else
      begin
        if BufferA[1] = '<' then
        begin
          BufferA[Length(xmlSignA) + 1] := #0;
          Result := (lstrcmpiA(@BufferA[1], xmlSignA) = 0);
        end
        else if BufferW[1] = '<' then
        begin
          BufferW[Length(xmlSignW) + 1] := #0;
          Result := (lstrcmpiW(@BufferW[1], xmlSignW) = 0);
        end;
      end;
    end;
  finally
    Stream.Seek(-BytesRead, soFromCurrent);
  end;
end;

procedure WriteTextNode(Node: IXMLNode; HasPluralForms: Boolean; const Value: String);
var
  I: Integer;
begin
  if HasPluralForms then
  begin
    for I := 0 to ZStrings.Count(Value) - 1 do
      Node.AddChild('PluralForm').Text := EscapeString(ZStrings.GetSubStrAt(Value, I));
  end
  else
    Node.Text := EscapeString(Value);
end;

function ReadTextNode(Node: IXMLNode; HasPluralForms: Boolean): String;
var
  ChildNode: IXMLNode;
begin
  if Node.IsTextElement or not HasPluralForms then
    Result := UnescapeString(Node.Text)
  else
  begin
    Result := '';
    ChildNode := Node.ChildNodes.First;
    while ChildNode <> nil do
    begin
      if SameText(ChildNode.NodeName, 'PluralForm') then
        ZStrings.Add(Result, UnescapeString(ChildNode.Text));
      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;

{ TXMLPersistentHashList }

function TXMLPersistentHashList.GetItemClass: THashItemClass;
begin
  Result := TXMLPersistentHashItem;
end;

{ TTextItem }

constructor TTextItem.Create(const AID: String);
begin
  fID := AID;
end;

function TTextItem.GetKey: String;
begin
  Result := fID;
end;

procedure TTextItem.Load(const Node: IXMLNode);
var
  ChildNode: IXMLNode;
begin
  if Node.HasChildNodes then
  begin
    ChildNode := Node.ChildNodes.FindNode('Comment');
    if ChildNode <> nil then
      fComment := UnescapeString(ChildNode.Text);
    ChildNode := Node.ChildNodes.FindNode('OriginalValue');
    if ChildNode <> nil then
    begin
      fOriginalValue := ReadTextNode(ChildNode, HasPluralForms);
      ChildNode := Node.ChildNodes.FindNode('TranslatedValue');
      if ChildNode <> nil then
      begin
        fTranslatedValue := ReadTextNode(ChildNode, HasPluralForms);
        fIsTranslated := True;
      end;
    end;
  end;
end;

procedure TTextItem.Save(const Node: IXMLNode);
begin
  Node.Attributes['ID'] := ID;
  if Comment <> '' then
    Node.AddChild('Comment').Text := EscapeString(Comment);
  WriteTextNode(Node.AddChild('OriginalValue'), HasPluralForms, OriginalValue);
  if IsTranslated then
    WriteTextNode(Node.AddChild('TranslatedValue'), HasPluralForms, TranslatedValue);
end;

procedure TTextItem.SetOriginalValue(const Value: String);
begin
  if OriginalValue <> Value then
  begin
    fOriginalValue := Value;
    fTranslatedValue := '';
    fIsTranslated := False;
  end;
end;

procedure TTextItem.SetTranslatedValue(const Value: String);
begin
  if fTranslatedValue <> Value then
  begin
    fTranslatedValue := Value;
    fIsTranslated := True;
  end;
end;

procedure TTextItem.SetIsTranslated(Value: Boolean);
begin
  if IsTranslated <> Value then
  begin
    fIsTranslated := Value;
    if not fIsTranslated then
      fTranslatedValue := '';
  end;
end;

function TTextItem.GetValue: String;
begin
  if IsTranslated then
    Result := TranslatedValue
  else
    Result := OriginalValue;
end;

procedure TTextItem.SetValue(const Value: String);
begin
  if OriginalValue <> Value then
    TranslatedValue := Value
  else
    IsTranslated := False;
end;

function TTextItem.GetHasPluralForms: Boolean;
begin
  Result := (PChar(ID)^ = '#');
end;

function TTextItem.GetPluralCount: Integer;
begin
  Result := ZStrings.Count(Value);
end;

function TTextItem.GetPlurals(Index: Integer): String;
begin
  Result := ZStrings.GetSubStrAt(Value, Index);
end;

procedure TTextItem.SetPlurals(Index: Integer; const PluralForm: String);
var
  NewValue: String;
begin
  NewValue := Value;
  ZStrings.SetSubStrAt(NewValue, Index, PluralForm);
  Value := NewValue;
end;

procedure TTextItem.Assign(Source: TPersistent);
begin
  if Source is TTextItem then
  begin
    fOriginalValue := TTextItem(Source).fOriginalValue;
    fTranslatedValue := TTextItem(Source).fTranslatedValue;
    fIsTranslated := TTextItem(Source).fIsTranslated;
    fComment := TTextItem(Source).fComment;
  end
  else
    inherited Assign(Source);
end;

function TTextItem.GetOwnerList: TTextItems;
begin
  Result := TTextItems(inherited Owner);
end;

function TTextItem.GetNext: TTextItem;
begin
  Result := TTextItem(inherited Next);
end;

function TTextItem.GetPrev: TTextItem;
begin
  Result := TTextItem(inherited Prev);
end;

{ TTextItems }

function TTextItems.GetItemClass: THashItemClass;
begin
  Result := TTextItem;
end;

function TTextItems.GetItems(const ID: String): TTextItem;
begin
  Result := TTextItem(inherited Items[ID]);
end;

function TTextItems.GetFirst: TTextItem;
begin
  Result := TTextItem(inherited First);
end;

function TTextItems.GetLast: TTextItem;
begin
  Result := TTextItem(inherited Last);
end;

procedure TTextItems.DiscardTranslations;
var
  Item: TTextItem;
begin
  Item := First;
  while Item <> nil do
  begin
    Item.IsTranslated := False;
    Item := Item.Next;
  end;
end;

function TTextItems.Find(const ID: String): TTextItem;
begin
  Result := TTextItem(inherited Find(ID));
end;

function TTextItems.Add(const ID: String): TTextItem;
begin
  Result := TTextItem(inherited Add(ID));
end;

function TTextItems.Add(const Node: IXMLNode): TXMLPersistentHashItem;
begin
  Result := Add(Node.Attributes['ID']);
  Result.Load(Node);
end;

function TTextItems.Extract(const ID: String): TTextItem;
begin
  Result := TTextItem(inherited Extract(ID));
end;

function TTextItems.Delete(const ID: String): Boolean;
begin
  Result := inherited Delete(ID);
end;

procedure TTextItems.AssignTo(Dest: TPersistent);
var
  Item: TTextItem;
begin
  if Dest is TStrings then
  begin
    TStrings(Dest).BeginUpdate;
    try
      TStrings(Dest).Clear;
      Item := First;
      while Item <> nil do
      begin
        TStrings(Dest).Values[Item.ID] := EscapeString(Item.Value);
        Item := Item.Next;
      end;
    finally
      TStrings(Dest).EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TTextItems.Read(Reader: TReader);
var
  Data: String;
  ID, Comment: String;
  P: Integer;
begin
  Clear;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    Data := Reader.ReadString;
    P := Pos('=', Data);
    if P <> 0 then
    begin
      ID := Copy(Data, 1, P - 1);
      Comment := Copy(Data, P + 1, Length(Data) - P);
    end
    else
    begin
      ID := Data;
      Comment := '';
    end;
    Add(ID).Comment := Comment;
  end;
  Reader.ReadListEnd;
end;

procedure TTextItems.Write(Writer: TWriter);
var
  Item: TTextItem;
  Data: String;
begin
  Writer.WriteListBegin;
  Item := First;
  while Item <> nil do
  begin
    Data := Item.ID;
    if Item.Comment <> '' then
      Data := Data + '=' + Item.Comment;
    Writer.WriteString(Data);
    Item := Item.Next;
  end;
  Writer.WriteListEnd;
end;

{ TTranslationStats }

function TTranslationStats.Approved: Integer;
var
  State: TTranslationState;
begin
  Result := 0;
  for State := Low(TTranslationState) to High(TTranslationState) do
    if State in TTextTranslation.AcceptedTranslationStates then
      Inc(Result, SubTotal[State]);
end;

function TTranslationStats.Progress: Double;
begin
  if Total <> 0 then
    Result := 100 * Approved / Total
  else
    Result := 100;
end;

{ TTextTranslation }

constructor TTextTranslation.Create(const ALocale: String);
begin
  fLocale := ALocale;
end;

function TTextTranslation.GetKey: String;
begin
  Result := fLocale;
end;

procedure TTextTranslation.Load(const Node: IXMLNode);
var
  StateStr: String;
begin
  if Node.HasAttribute('State') then
  begin
    StateStr := Node.Attributes['State'];
    State := TTranslationState(AnsiIndexText(StateStr, ['Fuzzy', 'Google', 'Auto', 'User']) + 1);
  end
  else
    State := tsUser;
  if Node.HasAttribute('Note') then
    Note := UnescapeString(Node.Attributes['Note']);
  Value := ReadTextNode(Node, Owner.Definition.HasPluralForms);
end;

procedure TTextTranslation.Save(const Node: IXMLNode);
begin
  Node.Attributes['Locale'] := Locale;
  case State of
    tsNone: Node.Attributes['State'] := 'None';
    tsFuzzy: Node.Attributes['State'] := 'Fuzzy';
    tsGoogle: Node.Attributes['State'] := 'Google';
    tsAuto: Node.Attributes['State'] := 'Auto';
  end;
  if Note <> '' then
    Node.Attributes['Note'] := EscapeString(Note);
  WriteTextNode(Node, Owner.Definition.HasPluralForms, Value);
end;

procedure TTextTranslation.Assign(Source: TPersistent);
begin
  if Source is TTextTranslation then
  begin
    State := TTextTranslation(Source).State;
    Value := TTextTranslation(Source).Value;
    Note := TTextTranslation(Source).Note;
  end
  else
    inherited Assign(Source);
end;

procedure TTextTranslation.Merge(Source: TTextTranslation; Options: TMergeOptions);
begin
  if (State < Source.State) or (moPreferSource in Options) then
    Assign(Source);
end;

function TTextTranslation.IsApproved: Boolean;
begin
  Result := (State in AcceptedTranslationStates);
end;

function TTextTranslation.GetPluralCount: Integer;
begin
  Result := ZStrings.Count(Value);
end;

function TTextTranslation.GetPlurals(Index: Integer): String;
begin
  Result := ZStrings.GetSubStrAt(Value, Index);
end;

procedure TTextTranslation.SetPlurals(Index: Integer; const PluralForm: String);
begin
  ZStrings.SetSubStrAt(fValue, Index, PluralForm);
end;

function TTextTranslation.GetOwnerList: TTextTranslations;
begin
  Result := TTextTranslations(inherited Owner);
end;

function TTextTranslation.GetNext: TTextTranslation;
begin
  Result := TTextTranslation(inherited Next);
end;

function TTextTranslation.GetPrev: TTextTranslation;
begin
  Result := TTextTranslation(inherited Prev);
end;

{ TTextTranslations }

constructor TTextTranslations.Create(ADefinition: TTextDefinition);
begin
  inherited Create(32);
  fDefinition := ADefinition;
end;

function TTextTranslations.GetItemClass: THashItemClass;
begin
  Result := TTextTranslation;
end;

function TTextTranslations.GetItems(const Locale: String): TTextTranslation;
begin
  Result := TTextTranslation(inherited Items[Locale]);
end;

function TTextTranslations.GetFirst: TTextTranslation;
begin
  Result := TTextTranslation(inherited First);
end;

function TTextTranslations.GetLast: TTextTranslation;
begin
  Result := TTextTranslation(inherited Last);
end;

procedure TTextTranslations.Merge(Source: TTextTranslations;
  Options: TMergeOptions);
var
  Translation: TTextTranslation;
begin
  Translation := Source.First;
  while Translation <> nil do
  begin
    Add(Translation.Locale).Merge(Translation, Options);
    Translation := Translation.Next;
  end;
end;

function TTextTranslations.Find(const Locale: String): TTextTranslation;
begin
  Result := TTextTranslation(inherited Find(Locale));
end;

function TTextTranslations.Add(const Locale: String): TTextTranslation;
begin
  Result := TTextTranslation(inherited Add(Locale));
end;

function TTextTranslations.Add(const Node: IXMLNode): TXMLPersistentHashItem;
begin
  Result := Add(Node.Attributes['Locale']);
  Result.Load(Node);
end;

function TTextTranslations.Extract(const Locale: String): TTextTranslation;
begin
  Result := TTextTranslation(inherited Extract(Locale));
end;

function TTextTranslations.Delete(const Locale: String): Boolean;
begin
  Result := inherited Delete(Locale);
end;

function TTextTranslations.NearestTo(const Locale: String): TTextTranslation;
var
  Culture: TCultureInfo;
  Translation: TTextTranslation;
begin
  Result := Find(Locale);
  if Result = nil then
  begin
    Culture := CultureOf(Locale, False);
    if Culture <> nil then
    begin
      Translation := First;
      while Translation <> nil do
      begin
        if Culture.IsDialectOf(Translation.Locale) then
        begin
          if (Result = nil) or (Result.State < Translation.State) then
            Result := Translation;
          if Result.State = tsUser then
            Exit;
        end;
        Translation := Translation.Next;
      end;
    end;
  end;
end;

procedure TTextTranslations.DisapproveAll;
var
  Translation: TTextTranslation;
begin
  Translation := First;
  while Translation <> nil do
  begin
    if Translation.IsApproved then
      Translation.State := tsFuzzy;
    Translation := Translation.Next;
  end;
end;

{ TTextDefinition }

constructor TTextDefinition.Create(const AID: String);
begin
  fTranslations := TTextTranslations.Create(Self);
  fID := AID;
end;

destructor TTextDefinition.Destroy;
begin
  fTranslations.Free;
  inherited Destroy;
end;

function TTextDefinition.GetKey: String;
begin
  Result := fID;
end;

procedure TTextDefinition.Load(const Node: IXMLNode);
var
  ChildNode: IXMLNode;
begin
  if Node.HasChildNodes then
  begin
    ChildNode := Node.ChildNodes.First;
    while ChildNode <> nil do
    begin
      case AnsiIndexText(ChildNode.NodeName, ['Comment', 'Value', 'Translation']) of
        0: Comment := UnescapeString(ChildNode.Text);
        1: Value := ReadTextNode(ChildNode, HasPluralForms);
        2: Translations.Add(ChildNode);
      end;
      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;

procedure TTextDefinition.Save(const Node: IXMLNode);
var
  Translation: TTextTranslation;
begin
  Node.Attributes['ID'] := ID;
  if Comment <> '' then
    Node.AddChild('Comment').Text := EscapeString(Comment);
  if Value <> '' then
    WriteTextNode(Node.AddChild('Value'), HasPluralForms, Value);
  Translation := Translations.First;
  while Translation <> nil do
  begin
    if (Translation.State <> tsNone) or (Translation.Value <> '') then
      Translation.Save(Node.AddChild('Translation'));
    Translation := Translation.Next;
  end;
end;

procedure TTextDefinition.Assign(Source: TPersistent);
begin
  if Source is TTextDefinition then
  begin
    fComment := TTextDefinition(Source).Comment;
    fValue := TTextDefinition(Source).Value;
    Translations.Assign(TTextDefinition(Source).Translations);
  end
  else
    inherited Assign(Source);
end;

procedure TTextDefinition.Merge(Source: TTextDefinition; Options: TMergeOptions);
begin
  if Source.Locale = Locale then
  begin
    if moPreferSource in Options then
    begin
      if Value <> Source.Value then
      begin
        Translations.DisapproveAll;
        fValue := Source.Value;
      end;
      fComment := Source.Comment;
    end
    else if (Source.Comment <> '') and (Pos(Source.Comment, Comment) = 0) then
    begin
      if Comment = '' then
        Comment := Source.Comment
      else
        Comment := Comment + #13#10 + Source.Comment;
    end;
  end
  else if Source.Value <> '' then
  begin
    with Translations.Add(Source.Locale) do
    begin
      Value := Source.Value;
      State := tsUser;
      Note := Comment;
    end;
  end;
  Translations.Merge(Source.Translations, Options);
end;

procedure TTextDefinition.Revise(Source: TTextItem);
begin
  if Value <> Source.Value then
  begin
    Translations.DisapproveAll;
    fValue := Source.Value;
  end;
  fComment := Source.Comment;
end;

function TTextDefinition.GetLocale: String;
var
  Culture: TCultureInfo;
begin
  Culture := Dictionary.TextDomain.Owner.Catalog.NativeCulture;
  if Assigned(Culture) then
    Result := Culture.Locale
  else
    Result := '';
end;

function TTextDefinition.GetHasPluralForms: Boolean;
begin
  Result := (PChar(ID)^ = '#');
end;

function TTextDefinition.GetPluralCount: Integer;
begin
  Result := ZStrings.Count(Value);
end;

function TTextDefinition.GetPlurals(Index: Integer): String;
begin
  Result := ZStrings.GetSubStrAt(Value, Index);
end;

procedure TTextDefinition.SetPlurals(Index: Integer; const PluralForm: String);
begin
  ZStrings.SetSubStrAt(fValue, Index, PluralForm);
end;

function TTextDefinition.GetDictionary: TTextDictionary;
begin
  Result := TTextDictionary(inherited Owner);
end;

function TTextDefinition.GetNext: TTextDefinition;
begin
  Result := TTextDefinition(inherited Next);
end;

function TTextDefinition.GetPrev: TTextDefinition;
begin
  Result := TTextDefinition(inherited Prev);
end;

{ TTextDictionary }

constructor TTextDictionary.Create(ATextDomain: TTextDomain;
  AKind: TTextDitionaryKind);
begin
  inherited Create(256);
  fTextDomain := ATextDomain;
  fKind := AKind;
end;

function TTextDictionary.GetItemClass: THashItemClass;
begin
  Result := TTextDefinition;
end;

function TTextDictionary.GetItems(const ID: String): TTextDefinition;
begin
  Result := TTextDefinition(inherited Items[ID]);
end;

function TTextDictionary.GetFirst: TTextDefinition;
begin
  Result := TTextDefinition(inherited First);
end;

function TTextDictionary.GetLast: TTextDefinition;
begin
  Result := TTextDefinition(inherited Last);
end;

procedure TTextDictionary.Assign(Source: TPersistent);
begin
  if Source is TTextDictionary then
    fKind := TTextDictionary(Source).Kind;
  inherited Assign(Source);
end;

procedure TTextDictionary.Merge(Source: TTextDictionary; Options: TMergeOptions);
var
  SrcDefinition, Definition: TTextDefinition;
  Added, Removed: TTextDefinition;
  NewDefinitions: TList;
  Ratio, BestRatio: Double;
  I, SimilarIndex: Integer;
begin
  NewDefinitions := TList.Create;
  try
    // Add new or update exiting definitions
    SrcDefinition := Source.First;
    while SrcDefinition <> nil do
    begin
      Definition := Find(SrcDefinition.ID);
      if Definition = nil then
      begin
        Definition := Add(SrcDefinition.ID);
        NewDefinitions.Add(Definition);
        Definition.Assign(SrcDefinition);
      end
      else
        Definition.Merge(SrcDefinition, Options);
      SrcDefinition := SrcDefinition.Next;
    end;
    // A new definition can be a renamed or slightly modified version of an old one
    Removed := nil;
    Definition := First;
    while Definition <> nil do
    begin
      if not Source.Exists(Definition.ID) then
        Removed := Definition;
      Definition := Definition.Next;
      if Removed <> nil then
      begin
        // Attempt to find a new definition similar to the removed one
        BestRatio := 0;
        SimilarIndex := -1;
        for I := 0 to NewDefinitions.Count - 1 do
        begin
          Added := TTextDefinition(NewDefinitions[I]);
          if Added.HasPluralForms = Removed.HasPluralForms then
          begin
            Ratio := TextSimilarityRatio(Added.Value, Removed.Value);
            if (Ratio >= AcceptedSimilarityRatio) and (Ratio > BestRatio)then
            begin
              BestRatio := Ratio;
              SimilarIndex := I;
            end;
          end;
        end;
        // If there is a new and similar definition, it's a revised version of the removed one
        if SimilarIndex >= 0 then
        begin
          Added := TTextDefinition(NewDefinitions[SimilarIndex]);
          if Added.Value <> Removed.Value then
            Removed.Translations.DisapproveAll;
          Added.Translations.Merge(Removed.Translations);
          NewDefinitions.Delete(SimilarIndex);
          Removed.Delete;
        end
        else if moDeleteDefinitionsNotInSource in Options then
          Removed.Delete;
        Removed := nil;
      end;
    end;
  finally
    NewDefinitions.Free;
  end;
end;

procedure TTextDictionary.Revise(Source: TTextItems; KeepRemovedEntries: Boolean);
var
  Item: TTextItem;
  Definition, Added, Removed: TTextDefinition;
  NewDefinitions: TList;
  Ratio, BestRatio: Double;
  I, SimilarIndex: Integer;
begin
  NewDefinitions := TList.Create;
  try
    // Add new or update exiting definitions
    Item := Source.First;
    while Item <> nil do
    begin
      Definition := Find(Item.ID);
      if Definition = nil then
      begin
        Definition := Add(Item.ID);
        NewDefinitions.Add(Definition);
      end;
      Definition.Revise(Item);
      Item := Item.Next;
    end;
    // A new definition can be a renamed or slightly modified version of an old one
    Removed := nil;
    Definition := First;
    while Definition <> nil do
    begin
      if not Source.Exists(Definition.ID) then
        Removed := Definition;
      Definition := Definition.Next;
      if Removed <> nil then
      begin
        // Attempt to find a new definition similar to the removed one
        BestRatio := 0;
        SimilarIndex := -1;
        for I := 0 to NewDefinitions.Count - 1 do
        begin
          Added := TTextDefinition(NewDefinitions[I]);
          if Added.HasPluralForms = Removed.HasPluralForms then
          begin
            Ratio := TextSimilarityRatio(Added.Value, Removed.Value);
            if (Ratio >= AcceptedSimilarityRatio) and (Ratio > BestRatio) then
            begin
              BestRatio := Ratio;
              SimilarIndex := I;
            end;
          end;
        end;
        // If there is a new and similar definition, it's a revised version of the removed one
        if SimilarIndex >= 0 then
        begin
          Added := TTextDefinition(NewDefinitions[SimilarIndex]);
          Added.Translations.Assign(Removed.Translations);
          if Added.Value <> Removed.Value then
            Added.Translations.DisapproveAll;
          NewDefinitions.Delete(SimilarIndex);
          Removed.Delete;
        end
        else if not KeepRemovedEntries then
          Removed.Delete;
        Removed := nil;
      end;
    end;
  finally
    NewDefinitions.Free;
  end;
end;

function TTextDictionary.Find(const ID: String): TTextDefinition;
begin
  Result := TTextDefinition(inherited Find(ID));
end;

function TTextDictionary.Add(const ID: String): TTextDefinition;
begin
  Result := TTextDefinition(inherited Add(ID));
end;

function TTextDictionary.Add(const Node: IXMLNode): TXMLPersistentHashItem;
begin
  Result := Add(Node.Attributes['ID']);
  Result.Load(Node);
end;

function TTextDictionary.Extract(const ID: String): TTextDefinition;
begin
  Result := TTextDefinition(inherited Extract(ID));
end;

function TTextDictionary.Delete(const ID: String): Boolean;
begin
  Result := inherited Delete(ID);
end;

procedure TTextDictionary.Compact(Options: TCompactOptions);
var
  Definition: TTextDefinition;
  Translation: TTextTranslation;
  NotUsedTranslation: TTextTranslation;
begin
  Definition := First;
  while Definition <> nil do
  begin
    if coDropComments in Options then
      Definition.Comment := '';
    NotUsedTranslation := nil;
    Translation := Definition.Translations.First;
    while Translation <> nil do
    begin
      if (coUsablesOnly in Options) and
         (not Translation.IsApproved or (Translation.Value = Definition.Value))
      then
        NotUsedTranslation := Translation
      else if coDropNotes in Options then
        Translation.Note := '';
      Translation := Translation.Next;
      if NotUsedTranslation <> nil then
      begin
        NotUsedTranslation.Delete;
        NotUsedTranslation := nil;
      end;
    end;
    Definition := Definition.Next;
  end;
end;

function TTextDictionary.TranslationToOriginal(const Locale: String): Boolean;
var
  Definition: TTextDefinition;
  Translation: TTextTranslation;
begin
  Result := True;
  Definition := First;
  while Definition <> nil do
  begin
    Translation := Definition.Translations.NearestTo(Locale);
    if (Translation <> nil) and Translation.IsApproved then
    begin
      Definition.Value := Translation.Value;
      Definition.Comment := Translation.Note;
    end
    else
      Result := False;
    Definition := Definition.Next;
  end;
end;

procedure TTextDictionary.OrigialToTranslation(const Locale: String);
var
  Definition: TTextDefinition;
  Translation: TTextTranslation;
begin
  Definition := First;
  while Definition <> nil do
  begin
    Translation := Definition.Translations.Add(Locale);
    Translation.Value := Definition.Value;
    Translation.Note := Definition.Comment;
    Translation.State := tsUser;
    Definition := Definition.Next;
  end;
end;

procedure TTextDictionary.Define(const Locale: String);

  function AutoTranslate(const Text: String; out TranslatedText: String): Boolean;
  begin
    with TextDomain.Owner.Catalog do
      Result := (NativeCulture <> nil) and (Repository <> nil) and
        Repository.FindSynonym(Text, NativeCulture, CultureOf(Locale), TranslatedText);
  end;

var
  CanUseOriginal, IsOriginal: Boolean;
  Definition: TTextDefinition;
  Translation: TTextTranslation;
  NearestTranslation: TTextTranslation;
  TranslatedText: String;
begin
  CanUseOriginal := (TextDomain.Owner.Catalog.NativeCulture <> nil)
    and TextDomain.Owner.Catalog.NativeCulture.IsDialectOf(Locale);
  IsOriginal := CanUseOriginal
    and SameText(TextDomain.Owner.Catalog.NativeCulture.Locale, Locale);
  Definition := First;
  while Definition <> nil do
  begin
    if not Definition.Translations.Exists(Locale) then
    begin
      NearestTranslation := Definition.Translations.NearestTo(Locale);
      Translation := Definition.Translations.Add(Locale);
      if (NearestTranslation <> nil) and NearestTranslation.IsApproved then
      begin
        Translation.Value := NearestTranslation.Value;
        Translation.State := tsAuto;
      end
      else if CanUseOriginal then
      begin
        Translation.Value := Definition.Value;
        if IsOriginal then
          Translation.State := tsUser
        else
          Translation.State := tsAuto;
      end
      else if AutoTranslate(Definition.Value, TranslatedText) then
      begin
        Translation.Value := TranslatedText;
        Translation.State := tsAuto;
      end;
    end;
    Definition := Definition.Next;
  end;
end;

procedure TTextDictionary.Undefine(const Locale: String);
var
  Definition: TTextDefinition;
begin
  Definition := First;
  while Definition <> nil do
  begin
    Definition.Translations.Delete(Locale);
    Definition := Definition.Next;
  end;
end;

procedure TTextDictionary.RetrieveCultures(Dest: TCultureList);
var
  Definition: TTextDefinition;
  Translation: TTextTranslation;
begin
  Definition := First;
  while Definition <> nil do
  begin
    Translation := Definition.Translations.First;
    while Translation <> nil do
    begin
      Dest.Add(Translation.Locale);
      Translation := Translation.Next;
    end;
    Definition := Definition.Next;
  end;
end;

procedure TTextDictionary.UpdateStats(const Locale: String;
  var Stats: TTranslationStats);
var
  Definition: TTextDefinition;
  Translation: TTextTranslation;
begin
  Inc(Stats.Total, Count);
  Definition := First;
  while Definition <> nil do
  begin
    Translation := Definition.Translations.Find(Locale);
    if Translation <> nil then
      Inc(Stats.SubTotal[Translation.State]);
    Definition := Definition.Next;
  end;
end;

function TTextDictionary.TranslationOf(const ID, Locale: String): TTextTranslation;
var
  Definition: TTextDefinition;
begin
  Definition := Find(ID);
  if Definition <> nil then
    Result := Definition.Translations.NearestTo(Locale)
  else
    Result := nil;
end;

procedure TTextDictionary.Translate(TextItems: TTextItems; Locale: String);
var
  TextItem: TTextItem;
  Translation: TTextTranslation;
begin
  TextItem := TextItems.First;
  while TextItem <> nil do
  begin
    Translation := TranslationOf(TextItem.ID, Locale);
    if (Translation <> nil) and Translation.IsApproved then
      TextItem.TranslatedValue := Translation.Value
    else
      TextItem.IsTranslated := False;
    TextItem := TextItem.Next;
  end;
end;

{ TTextDomain }

constructor TTextDomain.Create(const AName: String);
begin
  fProperties := TTextDictionary.Create(Self, dkProperty);
  fLiterals := TTextDictionary.Create(Self, dkLiteral);
  fName := AName;
end;

destructor TTextDomain.Destroy;
begin
  fProperties.Free;
  fLiterals.Free;
  inherited Destroy;
end;

function TTextDomain.GetKey: String;
begin
  Result := fName;
end;

procedure TTextDomain.Load(const Node: IXMLNode);
var
  ChildNode: IXMLNode;
begin
  if Node.HasChildNodes then
  begin
    ChildNode := Node.ChildNodes.First;
    while ChildNode <> nil do
    begin
      case AnsiIndexText(ChildNode.NodeName, ['Property', 'Literal']) of
        0: Properties.Add(ChildNode);
        1: Literals.Add(ChildNode);
      end;
      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;

procedure TTextDomain.Save(const Node: IXMLNode);
var
  Definition: TTextDefinition;
begin
  Node.Attributes['Name'] := Name;
  Definition := Properties.First;
  while Definition <> nil do
  begin
    Definition.Save(Node.AddChild('Property'));
    Definition := Definition.Next;
  end;
  Definition := Literals.First;
  while Definition <> nil do
  begin
    Definition.Save(Node.AddChild('Literal'));
    Definition := Definition.Next;
  end;
end;

procedure TTextDomain.Assign(Source: TPersistent);
begin
  if Source is TTextDomain then
  begin
    Properties.Assign(TTextDomain(Source).Properties);
    Literals.Assign(TTextDomain(Source).Literals);
  end
  else
    inherited Assign(Source);
end;

procedure TTextDomain.Merge(Source: TTextDomain; Options: TMergeOptions);
begin
  Properties.Merge(Source.Properties, Options);
  Literals.Merge(Source.Literals, Options);
end;

function TTextDomain.GetOwnerList: TTextDomains;
begin
  Result := TTextDomains(inherited Owner);
end;

function TTextDomain.GetNext: TTextDomain;
begin
  Result := TTextDomain(inherited Next);
end;

function TTextDomain.GetPrev: TTextDomain;
begin
  Result := TTextDomain(inherited Prev);
end;

function TTextDomain.Empty: Boolean;
begin
  Result := (Properties.Count = 0) and (Literals.Count = 0);
end;

function TTextDomain.StatsOf(const Locale: String): TTranslationStats;
begin
  FillChar(Result, SizeOf(Result), 0);
  Properties.UpdateStats(Locale, Result);
  Literals.UpdateStats(Locale, Result);
end;

procedure TTextDomain.GetComponents(dstList: TStrings);
var
  Prop: TTextDefinition;
  ComponentName: String;
begin
  dstList.BeginUpdate;
  try
    dstList.Clear;
    Prop := Properties.First;
    while Prop <> nil do
    begin
      ComponentName := ExtractComponentName(Prop.ID);
      if (ComponentName <> '') and (dstList.IndexOf(ComponentName) < 0) then
        dstList.Add(ComponentName);
      Prop := Prop.Next;
    end;
  finally
    dstList.EndUpdate;
  end;
end;

function TTextDomain.HasComponent(const ComponentName: String): Boolean;
var
  Prop: TTextDefinition;
begin
  Result := False;
  Prop := Properties.First;
  while Prop <> nil do
  begin
    if AnsiStartsText(ComponentName, Prop.ID) and
       CharInSet(Prop.ID[Length(ComponentName) + 1], ['.', '[']) then
    begin
      Result := True;
      Exit;
    end;
    Prop := Prop.Next;
  end;
end;

function TTextDomain.DeleteComponent(const ComponentName: String): Boolean;
var
  Prop: TTextDefinition;
  TargetProp: TTextDefinition;
begin
  Result := False;
  TargetProp := nil;
  Prop := Properties.First;
  while Prop <> nil do
  begin
    if AnsiStartsText(ComponentName, Prop.ID) and
       CharInSet(Prop.ID[Length(ComponentName) + 1], ['.', '['])
    then
      TargetProp := Prop;
    Prop := Prop.Next;
    if TargetProp <> nil then
    begin
      TargetProp.Delete;
      TargetProp := nil;
      Result := True;
    end;
  end;
end;

function TTextDomain.RenameComponent(const ComponentName,
  NewComponentName: String): Boolean;
var
  Prop: TTextDefinition;
  TargetProp: TTextDefinition;
  FieldPath: String;
begin
  Result := False;
  if ComponentName = NewComponentName then
    Result := True
  else if SameText(ComponentName, NewComponentName) then
  begin
    Prop := Properties.First;
    while Prop <> nil do
    begin
      if AnsiStartsText(ComponentName, Prop.ID) and
         CharInSet(Prop.ID[Length(ComponentName) + 1], ['.', '[']) then
      begin
        FieldPath := Copy(Prop.ID, Length(ComponentName) + 1, MaxInt);
        Prop.fID := NewComponentName + FieldPath;
        Result := True;
      end;
      Prop := Prop.Next;
    end;
  end
  else if not HasComponent(NewComponentName) then
  begin
    TargetProp := nil;
    Prop := Properties.First;
    while Prop <> nil do
    begin
      if AnsiStartsText(ComponentName, Prop.ID) and
         CharInSet(Prop.ID[Length(ComponentName) + 1], ['.', '[']) then
        TargetProp := Prop;
      Prop := Prop.Next;
      if TargetProp <> nil then
      begin
        FieldPath := Copy(TargetProp.ID, Length(ComponentName) + 1, MaxInt);
        with Properties.Add(NewComponentName + FieldPath) do
        begin
          Assign(TargetProp);
          MoveBefore(TargetProp);
        end;
        TargetProp.Delete;
        TargetProp := nil;
        Result := True;
      end;
    end;
  end;
end;

function TTextDomain.Rename(const NewName: String): TTextDomain;
begin
  if SameText(NewName, Name) then
  begin
    fName := NewName;
    Result := Self;
  end
  else if not Owner.Exists(NewName) then
  begin
    Result := Owner.Add(NewName);
    Result.Assign(Self);
    Result.MoveBefore(Self);
    Self.Delete;
  end
  else
    Result := nil;
end;

{ TTextDomains }

constructor TTextDomains.Create(ACatalog: TTranslationCatalog);
begin
  inherited Create(32);
  fCatalog := ACatalog;
end;

function TTextDomains.GetItemClass: THashItemClass;
begin
  Result := TTextDomain;
end;

function TTextDomains.GetItems(const Name: String): TTextDomain;
begin
  Result := TTextDomain(inherited Items[Name]);
end;

function TTextDomains.GetFirst: TTextDomain;
begin
  Result := TTextDomain(inherited First);
end;

function TTextDomains.GetLast: TTextDomain;
begin
  Result := TTextDomain(inherited Last);
end;

procedure TTextDomains.Merge(Source: TTextDomains; Options: TMergeOptions);
var
  TextDomain, Removed: TTextDomain;
begin
  if moDeleteDomainsNotInSource in Options then
  begin
    Removed := nil;
    TextDomain := First;
    while TextDomain <> nil do
    begin
      if not Source.Exists(TextDomain.Name) then
        Removed := TextDomain;
      TextDomain := TextDomain.Next;
      if Removed <> nil then
      begin
        Removed.Delete;
        Removed := nil;
      end;
    end;
  end;
  TextDomain := Source.First;
  while TextDomain <> nil do
  begin
    Add(TextDomain.Name).Merge(TextDomain, Options);
    TextDomain := TextDomain.Next;
  end;
end;

function TTextDomains.Find(const Name: String): TTextDomain;
begin
  Result := TTextDomain(inherited Find(Name));
end;

function TTextDomains.Add(const Name: String): TTextDomain;
begin
  Result := TTextDomain(inherited Add(Name));
end;

function TTextDomains.Add(const Node: IXMLNode): TXMLPersistentHashItem;
begin
  Result := Add(Node.Attributes['Name']);
  Result.Load(Node);
end;

function TTextDomains.Extract(const Name: String): TTextDomain;
begin
  Result := TTextDomain(inherited Extract(Name));
end;

function TTextDomains.Delete(const Name: String): Boolean;
begin
  Result := inherited Delete(Name);
end;

{ TTranslationCatalog }

constructor TTranslationCatalog.Create;
begin
  fTextDomains := TTextDomains.Create(Self);
  fCultures := TCultureList.Create;
  fCustomPluralRules := TKeyLookup<TCultureInfo,String>.Create;
end;

destructor TTranslationCatalog.Destroy;
begin
  fCultures.Free;
  fTextDomains.Free;
  fCustomPluralRules.Free;
  inherited Destroy;
end;

function TTranslationCatalog.GetCultures: TReadonlyCultureList;
begin
  Result := fCultures;
end;

procedure TTranslationCatalog.SetNativeCulture(Value: TCultureInfo);
var
  TextDomain: TTextDomain;
begin
  if NativeCulture <> Value then
  begin
    TextDomain := TextDomains.First;
    while TextDomain <> nil do
    begin
      if NativeCulture <> nil then
      begin
        TextDomain.Properties.OrigialToTranslation(NativeCulture.Locale);
        TextDomain.Literals.OrigialToTranslation(NativeCulture.Locale);
      end;
      if Value <> nil then
      begin
        TextDomain.Properties.TranslationToOriginal(Value.Locale);
        TextDomain.Literals.TranslationToOriginal(Value.Locale);
      end;
      TextDomain := TextDomain.Next;
    end;
    fNativeCulture := Value;
  end;
end;

procedure TTranslationCatalog.NormalizeCultures;
var
  TextDomain: TTextDomain;
begin
  if fUpdateCount = 0 then
  begin
    fCultures.Clear;
    TextDomain := TextDomains.First;
    while TextDomain <> nil do
    begin
      TextDomain.Properties.RetrieveCultures(fCultures);
      TextDomain.Literals.RetrieveCultures(fCultures);
      TextDomain := TextDomain.Next;
    end;
    if (Repository = nil) and (NativeCulture <> nil) then
      fCultures.Add(NativeCulture);
  end;
end;

procedure TTranslationCatalog.NormalizeTranslations;
var
  TextDomain, ToDelete: TTextDomain;
  I: Integer;
begin
  if fUpdateCount = 0 then
  begin
    ToDelete := nil;
    TextDomain := TextDomains.First;
    while TextDomain <> nil do
    begin
      if TextDomain.Empty then
        ToDelete := TextDomain
      else
        for I := 0 to Cultures.Count - 1 do
        begin
          TextDomain.Properties.Define(Cultures[I].Locale);
          TextDomain.Literals.Define(Cultures[I].Locale);
        end;
      TextDomain := TextDomain.Next;
      if ToDelete <> nil then
      begin
        ToDelete.Delete;
        ToDelete := nil;
      end;
    end;
  end;
end;

procedure TTranslationCatalog.Clear;
begin
  fNativeCulture := nil;
  fCultures.Clear;
  fTextDomains.Clear;
  fCustomPluralRules.Clear;
  fCompressed := False;
end;

function TTranslationCatalog.Empty: Boolean;
begin
  Result := (TextDomains.Count = 0);
end;

procedure TTranslationCatalog.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TTranslationCatalog.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then
  begin
    NormalizeCultures;
    if Assigned(Repository) then
      NormalizeTranslations;
  end;
end;

procedure TTranslationCatalog.Compact(Options: TCompactOptions);
var
  TextDomain: TTextDomain;
begin
  BeginUpdate;
  try
    TextDomain := TextDomains.First;
    while TextDomain <> nil do
    begin
      TextDomain.Properties.Compact(Options);
      TextDomain.Literals.Compact(Options);
      TextDomain := TextDomain.Next;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TTranslationCatalog.Add(Culture: TCultureInfo);
var
  TextDomain: TTextDomain;
begin
  if Culture <> nil then
  begin
    fCultures.Add(Culture);
    TextDomain := TextDomains.First;
    while TextDomain <> nil do
    begin
      TextDomain.Properties.Define(Culture.Locale);
      TextDomain.Literals.Define(Culture.Locale);
      TextDomain := TextDomain.Next;
    end;
  end;
end;

function TTranslationCatalog.Remove(Culture: TCultureInfo): Boolean;
var
  TextDomain: TTextDomain;
begin
  Result := False;
  if (Culture <> nil) and (fCultures.Remove(Culture) >= 0) then
  begin
    TextDomain := TextDomains.First;
    while TextDomain <> nil do
    begin
      TextDomain.Properties.Undefine(Culture.Locale);
      TextDomain.Literals.Undefine(Culture.Locale);
      TextDomain := TextDomain.Next;
    end;
    Result := True;
  end;
end;

procedure TTranslationCatalog.Assign(Source: TPersistent);
begin
  if Source is TTranslationCatalog then
  begin
    fTextDomains.Assign(TTranslationCatalog(Source).TextDomains);
    fCultures.Assign(TTranslationCatalog(Source).Cultures);
    fNativeCulture := TTranslationCatalog(Source).NativeCulture;
    fCustomPluralRules.CopyFrom(TTranslationCatalog(Source).CustomPluralRules);
  end
  else
    inherited Assign(Source);
end;

procedure TTranslationCatalog.Merge(Source: TTranslationCatalog;
  Options: TMergeOptions);
begin
  BeginUpdate;
  try
    TextDomains.Merge(Source.TextDomains, Options);
    Source.CustomPluralRules.ForEach(
      function(const Culture: TCultureInfo; const PluralRule: String): Boolean
      begin
        if moPreferSource in Options then
          CustomPluralRules.AddOrUpdate(Culture, PluralRule)
        else
          CustomPluralRules.Add(Culture, PluralRule);
        Result := True;
      end);
  finally
    EndUpdate;
  end;
end;

function TTranslationCatalog.StatsOf(Culture: TCultureInfo): TTranslationStats;
var
  TextDomain: TTextDomain;
begin
  FillChar(Result, SizeOf(Result), 0);
  TextDomain := TextDomains.First;
  while TextDomain <> nil do
  begin
    if Culture <> nil then
    begin
      TextDomain.Properties.UpdateStats(Culture.Locale, Result);
      TextDomain.Literals.UpdateStats(Culture.Locale, Result);
    end
    else
    begin
      Inc(Result.Total, TextDomain.Properties.Count);
      Inc(Result.Total, TextDomain.Literals.Count);
    end;
    TextDomain := TextDomain.Next;
  end;
end;

function TTranslationCatalog.PluralRuleOf(Culture: TCultureInfo): String;
begin
  if Culture = nil then
  begin
    if NativeCulture = nil then
      Culture := GetApplicationDefaultCulture
    else
      Culture := NativeCulture;
  end;
  if not CustomPluralRules.Retrieve(Culture, Result) then
    Result := Culture.PluralRule;
end;

procedure TTranslationCatalog.ChangePluralRuleOf(Culture: TCultureInfo;
  const PluralRule: String);
begin
  if Culture <> nil then
    CustomPluralRules.AddOrUpdate(Culture, PluralRule);
end;

function TTranslationCatalog.UseTranslation(const OriginalText: String;
  const TranslatedText: String; Culture: TCultureInfo;
  AffectedTranslations: TList): Integer;
var
  OriginalLettersOnly: String;
  Count: Integer;

  procedure ApplyTo(Dictionary: TTextDictionary);
  var
    Definition: TTextDefinition;
    Translation: TTextTranslation;
  begin
    Definition := Dictionary.First;
    while Definition <> nil do
    begin
      Translation := Definition.Translations.Find(Culture.Locale);
      if (Translation <> nil) and (Translation.State = tsNone) and
         (LettersOnly(Definition.Value) = OriginalLettersOnly) then
      begin
        Translation.Value := ModelTrivials(TranslatedText, OriginalText);
        Translation.State := tsAuto;
        if AffectedTranslations <> nil then
          AffectedTranslations.Add(Translation);
        Inc(Count);
      end;
      Definition := Definition.Next;
    end;
  end;

var
  TextDomain: TTextDomain;
begin
  Count := 0;
  if (NativeCulture <> nil) and (OriginalText <> '') then
  begin
    OriginalLettersOnly := LettersOnly(OriginalText);
    TextDomain := TextDomains.First;
    while TextDomain <> nil do
    begin
      ApplyTo(TextDomain.Properties);
      ApplyTo(TextDomain.Literals);
      TextDomain := TextDomain.Next;
    end;
    if Repository <> nil then
      Repository.DefineSynonym(OriginalText, NativeCulture, TranslatedText, Culture);
  end;
  Result := Count;
end;

function TTranslationCatalog.UseRepository(AffectedTranslations: TList): Integer;
var
  Count: Integer;

  procedure Process(Dictionary: TTextDictionary);
  var
    Definition: TTextDefinition;
    Translation: TTextTranslation;
    TranslatedText: String;
    TargetLocale: TCultureInfo;
  begin
    TargetLocale := nil;
    Definition := Dictionary.First;
    while Definition <> nil do
    begin
      Translation := Definition.Translations.First;
      while Translation <> nil do
      begin
        if not Translation.IsApproved and (Translation.State <> tsAuto) then
        begin
          if (TargetLocale = nil) or (TargetLocale.Locale <> Translation.Locale) then
            TargetLocale := CultureOf(Translation.Locale);
          if Repository.FindSynonym(Definition.Value, NativeCulture, TargetLocale, TranslatedText) then
          begin
            Translation.Value := TranslatedText;
            Translation.State := tsAuto;
            if AffectedTranslations <> nil then
              AffectedTranslations.Add(Translation);
            Inc(Count);
          end;
        end;
        Translation := Translation.Next;
      end;
      Definition := Definition.Next;
    end;
  end;

var
  TextDomain: TTextDomain;
begin
  Count := 0;
  if (NativeCulture <> nil) and (Repository <> nil) then
  begin
    TextDomain := TextDomains.First;
    while TextDomain <> nil do
    begin
      Process(TextDomain.Properties);
      Process(TextDomain.Literals);
      TextDomain := TextDomain.Next;
    end;
  end;
  Result := Count;
end;

function TTranslationCatalog.UseRepository(Culture: TCultureInfo;
  AffectedTranslations: TList): Integer;
var
  Count: Integer;

  procedure Process(Dictionary: TTextDictionary);
  var
    Definition: TTextDefinition;
    Translation: TTextTranslation;
    TranslatedText: String;
  begin
    Definition := Dictionary.First;
    while Definition <> nil do
    begin
      Translation := Definition.Translations.Find(Culture.Locale);
      if (Translation <> nil) and not Translation.IsApproved and (Translation.State <> tsAuto) and
         Repository.FindSynonym(Definition.Value, NativeCulture, Culture, TranslatedText) then
      begin
        Translation.Value := TranslatedText;
        Translation.State := tsAuto;
        if AffectedTranslations <> nil then
          AffectedTranslations.Add(Translation);
        Inc(Count);
      end;
      Definition := Definition.Next;
    end;
  end;

var
  TextDomain: TTextDomain;
begin
  Count := 0;
  if (NativeCulture <> nil) and (Culture <> nil) and (Repository <> nil) then
  begin
    TextDomain := TextDomains.First;
    while TextDomain <> nil do
    begin
      Process(TextDomain.Properties);
      Process(TextDomain.Literals);
      TextDomain := TextDomain.Next;
    end;
  end;
  Result := Count;
end;

procedure TTranslationCatalog.SaveToXML(const Document: IXMLDocument);
var
  TextDomain: TTextDomain;
begin
  Document.DocumentElement := Document.CreateElement('i18nCatalog', '');
  with Document.DocumentElement do
  begin
    Attributes['Version'] := '1.0';
    if NativeCulture <> nil then
      Attributes['Locale'] := NativeCulture.Locale;
    CustomPluralRules.ForEach(
      function(const Culture: TCultureInfo; const PluralRule: String): Boolean
      begin
        if Cultures.Exists(Culture) and (Culture.PluralRule <> PluralRule) then
          with AddChild('PluralForm') do
          begin
            Attributes['Locale'] := Culture.Locale;
            Attributes['Rule'] := PluralRule;
          end;
        Result := True;
      end);
    TextDomain := TextDomains.First;
    while TextDomain <> nil do
    begin
      TextDomain.Save(AddChild('Domain'));
      TextDomain := TextDomain.Next;
    end;
  end;
end;

procedure TTranslationCatalog.SaveToStream(Stream: TStream);
var
  Document: IXMLDocument;
  CompressionStream: TCompressionStream;
begin
  Document := NewXMLDocument();
  try
    Document.Options := [doNodeAutoIndent];
    Document.StandAlone := 'yes';
    Document.Encoding := 'utf-8';
    SaveToXML(Document);
    if Compressed then
    begin
      CompressionStream := TCompressionStream.Create(clDefault, Stream);
      try
        Document.SaveToStream(CompressionStream);
      finally
        CompressionStream.Free;
      end;
    end
    else
      Document.SaveToStream(Stream);
  finally
    Document := nil;
  end;
end;

procedure TTranslationCatalog.SaveToFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(NormalizePath(FileName), fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTranslationCatalog.LoadFromXML(const Document: IXMLDocument);
var
  Culture: TCultureInfo;
  ChildNode: IXMLNode;
  Locale: String;
begin
  Clear;
  if (Document.DocumentElement = nil) or
     not SameText(Document.DocumentElement.NodeName, 'i18nCatalog')
  then
    raise ETranslationCatalogError.CreateRes(@SCatalogLoadError1);
  BeginUpdate;
  try
    try
      if Document.DocumentElement.HasAttribute('Locale') then
      begin
        Locale := Document.DocumentElement.Attributes['Locale'];
        fNativeCulture := CultureOf(Locale);
      end;
      if Document.DocumentElement.HasChildNodes then
      begin
        ChildNode := Document.DocumentElement.ChildNodes.First;
        while ChildNode <> nil do
        begin
          case AnsiIndexText(ChildNode.NodeName, ['Domain', 'PluralForm']) of
            0: TextDomains.Add(ChildNode);
            1: with TPluralForms.Create(ChildNode.Attributes['Rule']) do
                 try
                   Culture := CultureOf(String(ChildNode.Attributes['Locale']));
                   if Assigned(Culture) and (Culture.PluralRule <> Rule) then
                     CustomPluralRules.Add(Culture, Rule);
                 finally
                   Free;
                 end;
          end;
          ChildNode := ChildNode.NextSibling;
        end;
      end;
    except
      on E: ETranslationCatalogError do
        raise;
      on E: Exception do
      begin
        Clear;
        raise ETranslationCatalogError.CreateResFmt(@SCatalogLoadError2, [E.Message]);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TTranslationCatalog.LoadFromStream(Stream: TStream);
var
  Document: IXMLDocument;
  DecompressionStream: TDecompressionStream;
  IsCompressed: Boolean;
begin
  IsCompressed := False;
  Document := TXMLDocument.Create(nil);
  try
    try
      if not IsXML(Stream) then
      begin
        DecompressionStream := TDecompressionStream.Create(Stream);
        try
          Document.LoadFromStream(DecompressionStream);
          IsCompressed := True;
        finally
          DecompressionStream.Free;
        end;
      end
      else
        Document.LoadFromStream(Stream);
    except
      on E: Exception do
      begin
        Clear;
        raise ETranslationCatalogError.CreateResFmt(@SCatalogLoadError2, [E.Message]);
      end;
    end;
    LoadFromXML(Document);
    Compressed := IsCompressed;
  finally
    Document := nil;
  end;
end;

procedure TTranslationCatalog.LoadFromFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(NormalizePath(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTranslationCatalog.LoadFromResource(hInstance: HINST;
  const ResName: String; ResType: PChar);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(hInstance, ResName, ResType);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTranslationCatalog.LoadFromResource(hInstance: HINST;
  ResID: Integer; ResType: PChar);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.CreateFromID(hInstance, ResID, ResType);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTranslationCatalog.LoadFromFiles(FileNames: TStrings;
  IgnoreErrors: Boolean);
var
  SubCatalog: TTranslationCatalog;
  I: Integer;
begin
  Clear;
  if FileNames.Count <> 0 then
  begin
    BeginUpdate;
    try
      SubCatalog := TTranslationCatalog.Create;
      try
        SubCatalog.EndUpdate; // intentionally without BeginUpdate
        for I := 0 to FileNames.Count - 1 do
        begin
          try
            SubCatalog.LoadFromFile(FileNames[I]);
          except
            on E: Exception do
            begin
              if not (E is ETranslationCatalogError) or not IgnoreErrors then
              begin
                Clear;
                raise;
              end;
              Continue;
            end;
          end;
          if NativeCulture = nil then
            NativeCulture := SubCatalog.NativeCulture;
          Merge(SubCatalog);
        end;
      finally
        SubCatalog.Free;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TTranslationCatalog.LoadFromDirectory(const Path: String;
  IncludeSubDirs: Boolean; const FileMask: String; IgnoreErrors: Boolean);
var
  Files: TStringList;
  Mask: String;

  procedure Search(const Dir: String);
  var
    F: TSearchRec;
  begin
    if FindFirst(Dir + Mask, faAnyFile and not faDirectory, F) = 0 then
    begin
      repeat
        Files.Add(Dir + F.Name);
      until FindNext(F) <> 0;
      FindClose(F);
    end;
    if IncludeSubDirs and (FindFirst(Dir + '*.*', faDirectory, F) = 0) then
    begin
      repeat
        if LongBool(F.Attr and faDirectory) and (F.Name <> '.') and (F.Name <> '..') then
          Search(Dir + F.Name + '\');
      until FindNext(F) <> 0;
      FindClose(F);
    end;
  end;

var
  SearchPath: String;
begin
  SearchPath := IncludeTrailingPathDelimiter(NormalizePath(Path));
  if not DirectoryExists(SearchPath) then
  begin
    {$IFDEF COMPILER2010_UP}
    raise EDirectoryNotFoundException.CreateFmt(SDirNotFoundError, [SearchPath]);
    {$ELSE}
    raise Exception.CreateFmt(SDirNotFoundError, [SearchPath]);
    {$ENDIF}
  end;
  if FileMask = '' then
    Mask := '*' + i18nCatalogFileExt
  else
    Mask := FileMask;
  Files := TStringList.Create;
  try
    Search(SearchPath);
    LoadFromFiles(Files, IgnoreErrors);
  finally
    Files.Free;
  end;
end;

procedure TTranslationCatalog.Open(const URI: String);
var
  Schema, Path: String;
  FileDir, FileMask: String;
  ResName, ResType: String;
begin
  if Pos(':', URI) > 2 then
  begin
    Schema := LowerCase(Copy(URI, 1, Pos(':', URI) - 1));
    Path := Copy(URI, Length(Schema) + 2, Length(URI) - Length(Schema) - 1);
  end
  else
  begin
    Schema := 'file';
    Path := URI;
  end;
  try
    if Schema = 'file' then
      LoadFromFile(Path)
    else if (Schema = 'dir') or (Schema = 'dirx') then
    begin
      FileDir := ExtractFilePath(Path);
      FileMask := ExtractFileName(Path);
      LoadFromDirectory(FileDir, Schema = 'dirx', FileMask);
    end
    else if Schema = 'res' then
    begin
      if Pos('.', Path) = 0 then
        LoadFromResource(hInstance, Path)
      else
      begin
        ResName := Copy(Path, 1, Pos('.', Path) - 1);
        ResType := Copy(Path, Pos('.', Path) + 1, MaxInt);
        LoadFromResource(hInstance, ResName, PChar(ResType));
      end;
    end
    else
    begin
      {$IFDEF COMPILER2010_UP}
      raise ENotSupportedException.CreateFmt(SSchemaError, [Schema]);
      {$ELSE}
      raise Exception.CreateFmt(SSchemaError, [Schema]);
      {$ENDIF}
    end;
  except
    Clear;
    raise;
  end;
end;

{ TTranslationRepository }

constructor TTranslationRepository.Create;
begin
  Relations := TObjectList.Create(True);
  Items := TStringList.Create;
  Items.OwnsObjects := True;
  Items.CaseSensitive := True;
  Items.Duplicates := dupAccept;
  Items.Sorted := True;
end;

destructor TTranslationRepository.Destroy;
begin
  Items.Free;
  Relations.Free;
  inherited Destroy;
end;

function TTranslationRepository.GetPhraseCount: Integer;
begin
  Result := Items.Count;
end;

function TTranslationRepository.GetRelationCount: Integer;
begin
  Result := Relations.Count;
end;

procedure TTranslationRepository.Clear;
begin
  Items.Clear;
  Relations.Clear;
  fModified := False;
end;

function TTranslationRepository.Empty: Boolean;
begin
  Result := (Relations.Count = 0);
end;

function TTranslationRepository.CreateRelationship: TStringList;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupAccept;
  Result.CaseSensitive := True;
  Result.Sorted := True;
  Relations.Add(Result);
end;

function TTranslationRepository.KeyOf(const Text: String; Culture: TCultureInfo): String;
begin
  Result := Culture.Language2 + MD5(LettersOnly(Text));
end;

function TTranslationRepository.Find(const Text: String; Culture: TCultureInfo;
  out Item: TTranslationRepositoryItem): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if Culture <> nil then
  begin
    if Items.Find(KeyOf(Text, Culture), Index) then
    begin
      Item := TTranslationRepositoryItem(Items.Objects[Index]);
      Result := True;
    end;
  end;
end;

function TTranslationRepository.FindOrCreate(const Text: String;
  Culture: TCultureInfo): TTranslationRepositoryItem;
begin
  Result := FindOrCreate(KeyOf(Text, Culture), Text, Culture);
end;

function TTranslationRepository.FindOrCreate(const Key, Text: String;
  Culture: TCultureInfo): TTranslationRepositoryItem;
var
  Index: Integer;
  NormalizedText: String;
begin
  NormalizedText := TrimTrivials(Text);
  if Items.Find(Key, Index) then
    repeat
      Result := TTranslationRepositoryItem(Items.Objects[Index]);
      if Result.Text = NormalizedText then
        Exit;
      Inc(Index);
    until (Index >= Items.Count) or (Items[Index] <> Key);
  Result := TTranslationRepositoryItem.Create;
  Result.Key := Key;
  Result.Culture := Culture;
  Result.Text := NormalizedText;
  Items.AddObject(Key, Result);
  fModified := True;
end;

procedure TTranslationRepository.Relate(Item1, Item2: TTranslationRepositoryItem);
var
  Relation: TStringList;
  I: Integer;
begin
  if Item1.Relatives <> Item2.Relatives then
  begin
    if Item1.Relatives = nil then
    begin
      Item1.Relatives := Item2.Relatives;
      Item1.Relatives.AddObject(Item1.Culture.Language2, Item1);
    end
    else if Item2.Relatives = nil then
    begin
      Item2.Relatives := Item1.Relatives;
      Item2.Relatives.AddObject(Item2.Culture.Language2, Item2);
    end
    else
    begin
      Relation := Item2.Relatives;
      for I := Relation.Count - 1 downto 0 do
        if Item1.Relatives.IndexOfObject(Relation.Objects[I]) < 0 then
          Item1.Relatives.AddObject(Relation.Strings[I], Relation.Objects[I]);
      for I := Items.Count - 1 downto 0 do
        if TTranslationRepositoryItem(Items.Objects[I]).Relatives = Relation then
          TTranslationRepositoryItem(Items.Objects[I]).Relatives := Item1.Relatives;
      Relations.Remove(Relation);
    end;
    fModified := True;
  end
  else if (Item1.Relatives = nil) and (Item1 <> Item2) then
  begin
    Relation := CreateRelationship;
    Relation.AddObject(Item1.Culture.Language2, Item1);
    Item1.Relatives := Relation;
    Relation.AddObject(Item2.Culture.Language2, Item2);
    Item2.Relatives := Relation;
    fModified := True;
  end;
end;

function TTranslationRepository.Remove(const Text: String;
  Culture: TCultureInfo): Boolean;
var
  Item: TTranslationRepositoryItem;
  Relatives: TStringList;
  Index: Integer;
  Key, NormalizedText: String;
begin
  Result := False;
  if Culture <> nil then
  begin
    Key := KeyOf(Text, Culture);
    if Items.Find(Key, Index) then
    begin
      NormalizedText := TrimTrivials(Text);
      repeat
        Item := TTranslationRepositoryItem(Items.Objects[Index]);
        if Item.Text = NormalizedText then
        begin
          Relatives := Item.Relatives;
          Items.Delete(Index);
          Index := Relatives.IndexOfObject(Item);
          if Index >= 0 then
            Relatives.Delete(Index);
          if Relatives.Count = 1 then
          begin
            Item := TTranslationRepositoryItem(Relatives.Objects[0]);
            Index := Items.IndexOfObject(Item);
            if Index >= 0 then
              Items.Delete(Index);
            Relatives.Clear;
          end;
          if Relatives.Count = 0 then
            Relations.Remove(Relatives);
          Result := True;
          fModified := True;
          Exit;
        end;
        Inc(Index);
      until (Index >= Items.Count) or (Items[Index] <> Key);
    end
  end;
end;

function TTranslationRepository.DefineSynonym(const Text1: String;
  Culture1: TCultureInfo; const Text2: String; Culture2: TCultureInfo): Boolean;
var
  Item1, Item2: TTranslationRepositoryItem;
begin
  Result := False;
  if (Text1 <> '') and (Text2 <> '') and
     (Culture1 <> nil) and (Culture2 <> nil) and
     ((Culture1 <> Culture2) or (Text1 <> Text2)) then
  begin
    Item1 := FindOrCreate(Text1, Culture1);
    Inc(Item1.Hits);
    Item2 := FindOrCreate(Text2, Culture2);
    Inc(Item2.Hits);
    Relate(Item1, Item2);
    Result := True;
  end;
end;

function TTranslationRepository.HasSynonym(const SourceText: String;
  SourceCulture, TargetCulture: TCultureInfo): Boolean;
var
  Item: TTranslationRepositoryItem;
begin
  Result := (SourceCulture <> nil) and (TargetCulture <> nil)
        and (SourceCulture.IsDialectOf(TargetCulture) or
            (Find(SourceText, SourceCulture, Item) and
            (Item.Relatives.IndexOf(TargetCulture.Language2) >= 0)));
end;

function TTranslationRepository.FindSynonym(const SourceText: String;
  SourceCulture, TargetCulture: TCultureInfo; out Synonym: String): Boolean;
var
  Item, PreferedItem: TTranslationRepositoryItem;
  Relatives: TStringList;
  I: Integer;
begin
  if SourceCulture = TargetCulture then
  begin
    Synonym := SourceText;
    Result := True;
  end
  else
  begin
    Result := False;
    if (SourceCulture <> nil) and (TargetCulture <> nil) then
    begin
      if Find(SourceText, SourceCulture, Item) and
         Item.Relatives.Find(TargetCulture.Language2, I) then
      begin
        Relatives := Item.Relatives;
        PreferedItem := TTranslationRepositoryItem(Relatives.Objects[I]);
        for I := I + 1 to Relatives.Count - 1 do
        begin
          Item := TTranslationRepositoryItem(Relatives.Objects[I]);
          if not Item.Culture.IsDialectOf(TargetCulture) then
            Break
          else if Item.Hits > PreferedItem.Hits then
            PreferedItem := Item;
        end;
        Synonym := ModelTrivials(PreferedItem.Text, SourceText);
        Result := True;
      end
      else if SourceCulture.IsDialectOf(TargetCulture) then
      begin
        Synonym := SourceText;
        Result := True;
      end
      else if TrimTrivials(SourceText) = '' then
      begin
        Synonym := SourceText;
        Result := True;
      end;
    end;
  end;
end;

function TTranslationRepository.FindAllSynonyms(const SourceText: String;
  SourceCulture, TargetCulture: TCultureInfo; Synonyms: TStrings): Integer;
var
  Item: TTranslationRepositoryItem;
  Relatives: TStringList;
  Synonym, EscapedSource: String;
  I: Integer;
begin
  Result := 0;
  if (SourceCulture <> nil) and (TargetCulture <> nil) then
  begin
    Synonyms.BeginUpdate;
    try
      if Find(SourceText, SourceCulture, Item) and
         Item.Relatives.Find(TargetCulture.Language2, I) then
      begin
        Relatives := Item.Relatives;
        for I := I to Relatives.Count - 1 do
        begin
          Item := TTranslationRepositoryItem(Relatives.Objects[I]);
          if not Item.Culture.IsDialectOf(TargetCulture) then
            Break;
          Synonym := ModelTrivials(Item.Text, SourceText);
          Synonyms.AddObject(EscapeString(Synonym), TObject(Item.Hits));
          Inc(Result);
        end;
      end
      else if TrimTrivials(SourceText) = '' then
      begin
        Synonyms.Add(EscapeString(SourceText));
        Inc(Result);
      end;
      if SourceCulture.IsDialectOf(TargetCulture) then
      begin
        EscapedSource := EscapeString(SourceText);
        I := Synonyms.IndexOf(EscapedSource);
        if SourceCulture = TargetCulture then
        begin
          if I < 0 then
          begin
            Synonyms.AddObject(EscapedSource, TObject(MaxInt));
            Inc(Result);
          end
          else
            Synonyms.Objects[I] := TObject(MaxInt);
        end
        else if I < 0 then
        begin
          Synonyms.Add(EscapedSource);
          Inc(Result);
        end;
      end;
    finally
      Synonyms.EndUpdate;
    end;
  end;
end;

procedure TTranslationRepository.Learn(Catalog: TTranslationCatalog);

  procedure LearnFrom(Dictionary: TTextDictionary);
  var
    Definition: TTextDefinition;
    Translation: TTextTranslation;
    DefItem, TraItem: TTranslationRepositoryItem;
  begin
    Definition := Dictionary.First;
    while Definition <> nil do
    begin
      DefItem := nil;
      Translation := Definition.Translations.First;
      while Translation <> nil do
      begin
        if Translation.IsApproved then
        begin
          if DefItem = nil then
          begin
            DefItem := FindOrCreate(Definition.Value, Catalog.NativeCulture);
            Inc(DefItem.Hits);
          end;
          TraItem := FindOrCreate(Translation.Value, CultureOf(Translation.Locale));
          Inc(TraItem.Hits);
          Relate(DefItem, TraItem);
        end;
        Translation := Translation.Next;
      end;
      Definition := Definition.Next;
    end;
  end;

var
  TextDomain: TTextDomain;
begin
  if not Catalog.Empty and (Catalog.NativeCulture <> nil) then
  begin
    TextDomain := Catalog.TextDomains.First;
    while TextDomain <> nil do
    begin
      LearnFrom(TextDomain.Properties);
      LearnFrom(TextDomain.Literals);
      TextDomain := TextDomain.Next;
    end;
  end;
end;

procedure TTranslationRepository.Merge(Repository: TTranslationRepository);
var
  Synonyms: TStringList;
  Base, This: TTranslationRepositoryItem;
  I, J: Integer;
begin
  for I := 0 to Repository.Relations.Count - 1 do
  begin
    Base := nil;
    Synonyms := TStringList(Repository.Relations[I]);
    for J := 0 to Synonyms.Count - 1 do
    begin
      with TTranslationRepositoryItem(Synonyms.Objects[J]) do
      begin
        This := FindOrCreate(Key, Text, Culture);
        Inc(This.Hits, Hits);
      end;
      if Base <> nil then
        Relate(Base, This);
      Base := This;
    end;
  end;
end;

procedure TTranslationRepository.SaveToXML(const Document: IXMLDocument);
var
  Relatives: TStringList;
  Item: TTranslationRepositoryItem;
  I, J: Integer;
begin
  Document.DocumentElement := Document.CreateElement('i18nRepository', '');
  with Document.DocumentElement do
  begin
    Attributes['Version'] := '1.0';
    for I := 0 to Relations.Count - 1 do
    begin
      Relatives := TStringList(Relations[I]);
      if Relatives.Count <> 0 then
        with AddChild('Synonyms') do
        begin
          for J := 0 to Relatives.Count - 1 do
          begin
            Item := TTranslationRepositoryItem(Relatives.Objects[J]);
            with AddChild('Text') do
            begin
              Attributes['Locale'] := Item.Culture.Locale;
              if Item.Hits <> 0 then
                Attributes['Hits'] := Item.Hits;
              Text := EscapeString(Item.Text);
            end;
          end;
        end;
    end;
  end;
  fModified := False;
end;

procedure TTranslationRepository.SaveToStream(Stream: TStream);
var
  Document: IXMLDocument;
begin
  Document := NewXMLDocument();
  try
    Document.Options := [doNodeAutoIndent];
    Document.StandAlone := 'yes';
    Document.Encoding := 'utf-8';
    SaveToXML(Document);
    Document.SaveToStream(Stream);
  finally
    Document := nil;
  end;
end;

procedure TTranslationRepository.SaveToFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(NormalizePath(FileName), fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTranslationRepository.LoadFromXML(const Document: IXMLDocument);
var
  SynNode, ItemNode: IXMLNode;
  Relatives: TStringList;
  Item: TTranslationRepositoryItem;
begin
  Clear;
  if (Document.DocumentElement = nil) or
     not SameText(Document.DocumentElement.NodeName, 'i18nRepository')
  then
    raise ETranslationRepositoryError.CreateRes(@SRepositoryLoadError1);
  if Document.DocumentElement.HasChildNodes then
  begin
    try
      SynNode := Document.DocumentElement.ChildNodes.First;
      while SynNode <> nil do
      begin
        if SameText(SynNode.NodeName, 'Synonyms') and SynNode.HasChildNodes then
        begin
          Relatives := CreateRelationship;
          ItemNode := SynNode.ChildNodes.First;
          while ItemNode <> nil do
          begin
            if SameText(ItemNode.NodeName, 'Text') then
            begin
              Item := TTranslationRepositoryItem.Create;
              try
                Item.Relatives := Relatives;
                Item.Culture := CultureOf(String(ItemNode.Attributes['Locale']));
                Item.Text := UnescapeString(ItemNode.Text);
                if ItemNode.HasAttribute('Key') then
                  Item.Key := ItemNode.Attributes['Key']
                else
                  Item.Key := KeyOf(Item.Text, Item.Culture);
                if ItemNode.HasAttribute('Hits') then
                  Item.Hits := ItemNode.Attributes['Hits'];
                Relatives.AddObject(Item.Culture.Language2, Item);
                Items.AddObject(Item.Key, Item);
              except
                Item.Free;
              end;
            end;
            ItemNode := ItemNode.NextSibling;
          end;
        end;
        SynNode := SynNode.NextSibling;
      end;
    except
      on E: Exception do
      begin
        Clear;
        raise ETranslationRepositoryError.CreateResFmt(@SRepositoryLoadError2, [E.Message]);
      end;
    end;
  end;
end;

procedure TTranslationRepository.LoadFromStream(Stream: TStream);
var
  Document: IXMLDocument;
begin
  Document := TXMLDocument.Create(nil);
  try
    try
      Document.LoadFromStream(Stream);
    except
      on E: ETranslationRepositoryError do
        raise;
      on E: Exception do
      begin
        Clear;
        raise ETranslationRepositoryError.CreateResFmt(@SRepositoryLoadError2, [E.Message]);
      end;
    end;
    LoadFromXML(Document);
  finally
    Document := nil;
  end;
end;

procedure TTranslationRepository.LoadFromFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(NormalizePath(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.
