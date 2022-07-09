{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements classes to retrieve constant and literal strings from a
/// source code in Delphi language.
unit i18nParser;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, Classes, Contnrs, i18nCatalog, i18nLocalizer;

type

  {$region 'xmldoc'}
  /// <summary>
  /// EPascalParserError is the exception class for problems that occur while parsing
  /// a Pascal source code.</summary>
  /// <remarks>
  /// EPascalParserError exception occurs when Pascal parser encounters an error
  /// in the source code being parsed.</remarks>
  /// <seealso cref="TPascalStringCollector"/>
  {$endregion}
  EPascalParserError = class(Exception)
  private
    fOffset: Integer;
    fLineNo: Integer;
    fColNo: Integer;
    fToken: String;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Sets the additional information about the error.</summary>
    /// <param name="AOffset">
    /// the character index from the beginning of the source code where the the error
    /// occurred.</param>
    /// <param name="ALineNo">
    /// The line number of the source code where the error occurred.</param>
    /// <param name="AColNo">
    /// The character index from beginning of the line where the error occurred.</param>
    /// <param name="AToken">
    /// The last token that the parser was processing when the error occured.</param>
    {$endregion}
    procedure SetErrorDetails(AOffset, ALineNo, AColNo: Integer; const AToken: String);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the character index from the beginning of the source code where the
    /// the error occurred.</summary>
    {$endregion}
    property Offset: Integer read fOffset;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the line number of the source code where the error occurred.</summary>
    {$endregion}
    property LineNo: Integer read fLineNo;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the character index from beginning of the line where the error occurred.</summary>
    {$endregion}
    property ColNo: Integer read fColNo;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the last token that the parser was processing when the error occured.</summary>
    {$endregion}
    property Token: String read fToken;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the different types of Pascal tokens.</summary>
  {$endregion}
  TPascalToken = (
    {$region 'xmldoc'}
    /// Indicates that the token is a Pascal's reserved word.
    {$endregion}
    T_ReservedWord,
    {$region 'xmldoc'}
    /// Indicates that the token is an identifier.
    {$endregion}
    T_Identifier,
    {$region 'xmldoc'}
    /// Indicates that the token is a symbol (e.g. operators, and etc.)
    {$endregion}
    T_Symbol,
    {$region 'xmldoc'}
    /// Indicates that the token is a string literal.
    {$endregion}
    T_String,
    {$region 'xmldoc'}
    /// Indicates that the token is an integer literal.
    {$endregion}
    T_Integer,
    {$region 'xmldoc'}
    /// Indicates that the token is a floating point literal.
    {$endregion}
    T_Float,
    {$region 'xmldoc'}
    /// Indicates that the token is an integer literal in hexadecimal notation.
    {$endregion}
    T_Hex,
    {$region 'xmldoc'}
    /// Indicates that the token is a character literal.
    {$endregion}
    T_Character,
    {$region 'xmldoc'}
    /// Indicates that the token is a block of assembly code.
    {$endregion}
    T_Assembler,
    {$region 'xmldoc'}
    /// Indicates that the token is a comment.
    {$endregion}
    T_Comment,
    {$region 'xmldoc'}
    /// Indicates that the token is a series of whitespace characters.
    {$endregion}
    T_WhiteSpace,
    {$region 'xmldoc'}
    /// Indicates that end of Pascal code is reached.
    {$endregion}
    T_EOF
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the erros that may occur while parsing
  /// a Pascal code.</summary>
  {$endregion}
  TPascalParseError = (
    {$region 'xmldoc'}
    /// No error
    {$endregion}
    peNone,
    {$region 'xmldoc'}
    /// An unterminated comment found
    {$endregion}
    peUnterminatedComment,
    {$region 'xmldoc'}
    /// An unterminated string literal found
    {$endregion}
    peUnterminatedString,
    {$region 'xmldoc'}
    /// A syntax error found
    {$endregion}
    peSyntaxError
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TPascalTokenizer extracts special symbols, reserved words, identifiers,
  /// numerals, and character strings from a Pascal code.</summary>
  {$endregion}
  TPascalTokenizer = class(TObject)
  private
    fSource: String;
    fToken: String;
    fTokenPtr: PChar;
    fTokenLen: Integer;
    fTokenID: TPascalToken;
    fError: TPascalParseError;
    fInsideAssembler: Boolean;
    function GetToken: String;
    function GetTokenAsSource: String;
    function GetTokenPos: Integer;
    function GetTokenLineNo: Integer;
    function GetTokenColNo: Integer;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the next Pascal token from a Pascal code.</summary>
    /// <param name="S">
    /// Pointer to the Pascal code. When the method exits, the pointer
    /// moves to the end of current token.</param>
    /// <param name="Error">
    /// Returns the parser error if any.</param>
    /// <returns>
    /// The <see cref="TPascalToken"/> value of the token.</returns>
    /// <seealso cref="Next"/>
    {$endregion}
    function GetNextToken(var S: PChar; out Err: TPascalParseError): TPascalToken;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for tokenizing a specified Pascal code.</summary>
    /// <param name="ASource">
    /// The Pascal code to parse.</param>
    /// <param name="APos">
    /// The offset that indicates the start position of parsing.</param>
    {$endregion}
    constructor Create(const ASource: String; APos: Integer = 1);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a token string is a Pascal reserved word.</summary>
    /// <param name="Token">
    /// The token string to examine.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the token string is a Pascal reserved
    /// word, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="IsDirective"/>
    /// <seealso cref="IsPropertyDirective"/>
    /// <seealso cref="IsVisibilityDirective"/>
    /// <seealso cref="IsPortabilityDirective"/>
    {$endregion}
    class function IsReservedWord(const Token: String): Boolean; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a token string is a Pascal directive.</summary>
    /// <param name="Token">
    /// The token string to examine.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the token string is a Pascal directive,
    /// otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="IsReservedWord"/>
    /// <seealso cref="IsPropertyDirective"/>
    /// <seealso cref="IsVisibilityDirective"/>
    /// <seealso cref="IsPortabilityDirective"/>
    {$endregion}
    class function IsDirective(const Token: String): Boolean; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a token string is a Pascal property directive.</summary>
    /// <param name="Token">
    /// The token string to examine.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the token string is a Pascal property
    /// directive, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="IsReservedWord"/>
    /// <seealso cref="IsDirective"/>
    /// <seealso cref="IsVisibilityDirective"/>
    /// <seealso cref="IsPortabilityDirective"/>
    {$endregion}
    class function IsPropertyDirective(const Token: String): Boolean; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a token string is a Pascal visibility directive.</summary>
    /// <param name="Token">
    /// The token string to examine.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the token string is a Pascal visibility
    /// directive, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="IsReservedWord"/>
    /// <seealso cref="IsDirective"/>
    /// <seealso cref="IsPropertyDirective"/>
    /// <seealso cref="IsPortabilityDirective"/>
    {$endregion}
    class function IsVisibilityDirective(const Token: String): Boolean; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a token string is a Pascal portability directive.</summary>
    /// <param name="Token">
    /// The token string to examine.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the token string is a Pascal portability
    /// directive, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="IsReservedWord"/>
    /// <seealso cref="IsDirective"/>
    /// <seealso cref="IsPropertyDirective"/>
    /// <seealso cref="IsVisibilityDirective"/>
    {$endregion}
    class function IsPortabilityDirective(const Token: String): Boolean; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the properties with the next token.</summary>
    /// <returns>
    /// Returns <see langword="true"/> while no error is occurred and end of Pascal
    /// code is not reached, otherwise returns <see langword="false"/>.</returns>
    {$endregion}
    function Next: Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Instructs the parser to start at a specified position of the Pascal code.</summary>
    /// <param name="Pos">
    /// The character index that parsing should start from.</param>
    {$endregion}
    procedure SetPosition(Pos: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// The Pascal code that is being parsed.</summary>
    {$endregion}
    property Source: String read fSource;
    {$region 'xmldoc'}
    /// <summary>
    /// The current token as a string in lower case letters.</summary>
    {$endregion}
    property Token: String read GetToken;
    {$region 'xmldoc'}
    /// <summary>
    /// The current token as it appeared in the code.</summary>
    {$endregion}
    property TokenAsSource: String read GetTokenAsSource;
    {$region 'xmldoc'}
    /// <summary>
    /// The pointer to the current token within the <see cref="Source"/> property.</summary>
    {$endregion}
    property TokenPtr: PChar read fTokenPtr;
    {$region 'xmldoc'}
    /// <summary>
    /// The character index of the current token from beginning of the <see cref="Source"/>
    /// property.</summary>
    {$endregion}
    property TokenPos: Integer read GetTokenPos;
    {$region 'xmldoc'}
    /// <summary>
    /// The length of current token in characters.</summary>
    {$endregion}
    property TokenLen: Integer read fTokenLen;
    {$region 'xmldoc'}
    /// <summary>
    /// The current token as a <see cref="TPascalToken"/> value.</summary>
    {$endregion}
    property TokenID: TPascalToken read fTokenID;
    {$region 'xmldoc'}
    /// <summary>
    /// The line number of the current token in the <see cref="Source"/> property.</summary>
    {$endregion}
    property TokenLineNo: Integer read GetTokenLineNo;
    {$region 'xmldoc'}
    /// <summary>
    /// The character index from beginning of the line the current token is placed.</summary>
    {$endregion}
    property TokenColNo: Integer read GetTokenColNo;
    {$region 'xmldoc'}
    /// <summary>
    /// The last parse error.</summary>
    {$endregion}
    property Error: TPascalParseError read fError;
  end;

  TStringValueReference = class;
  TCodeBlock = class;

  {$region 'xmldoc'}
  /// <summary>
  /// TStringValue stores information about a string constant or a string literal
  /// in a Pascal code.</summary>
  {$endregion}
  TStringValue = class abstract(TObject)
  private
    fValue: String;
    fReferences: TObjectList;
    function GetReferenceCount: Integer; inline;
    function GetReferences(Index: Integer): TStringValueReference; inline;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Computes a unique key for the string value.</summary>
    /// <returns>
    /// Returns the unique key of the string.</returns>
    {$endregion}
    function GetKey: String; virtual;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class and initializes it with a specfied
    /// string value.</summary>
    /// <param name="AValue">
    /// The string value that the object represents.</param>
    {$endregion}
    constructor Create(const AValue: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releaes its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a reference to the string value in the <see cref="References"/> array.</summary>
    /// <param name="Block">
    /// The <see cref="TCodeBlock"/> object that represents the block of code that
    /// the string is referenced in.</param>
    /// <returns>
    /// The <see cref="TStringValueReference"/> objects that stores details of the
    /// reference.</returns>
    /// <seealso cref="TStringValueReference"/>
    {$endregion}
    function AddReference(Block: TCodeBlock; Pos, Len: Integer): TStringValueReference;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether any reference of the string value can be translated.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if at least one of references of the string
    /// value is translatable, otherwise returns <see langword="false"/>.</returns>
    {$endregion}
    function HasAnyTranslatableReference: Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the unique key of the string.</summary>
    {$endregion}
    property Key: String read GetKey;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the string value.</summary>
    {$endregion}
    property Value: String read fValue;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of references of the string value.</summary>
    {$endregion}
    property ReferenceCount: Integer read GetReferenceCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the array of references of the string value.</summary>
    {$endregion}
    property References[Index: Integer]: TStringValueReference read GetReferences;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TStringLiteral stores information about a literal string in a Pascal code.</summary>
  {$endregion}
  TStringLiteral = class(TStringValue);

  {$region 'xmldoc'}
  /// <summary>
  /// TStringConstant stores information about a constant or resource string in
  /// a in a Pascal code.</summary>
  {$endregion}
  TStringConstant = class(TStringValue)
  private
    fName: String;
    fIsResource: Boolean;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class and initializes it with name and value
    /// of the string constant.</summary>
    /// <param name="AName">
    /// The name of string constant that this object represents.</param>
    /// <param name="AValue">
    /// The value of string constant that this object represents.</param>
    /// <param name="ResStr">
    /// Indicates whether the string constant is a resource string.</param>
    {$endregion}
    constructor Create(const AName, AValue: String; ResStr: Boolean);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets whether the string constant is a resource string.</summary>
    {$endregion}
    property IsResourceString: Boolean read fIsResource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the name of string constant.</summary>
    {$endregion}
    property Name: String read fName;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TStringPluralForms stores information about a string that lists plural forms
  /// of a phrase.</summary>
  {$endregion}
  TStringPlural = class(TStringValue)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Computes a unique key for the string value.</summary>
    /// <returns>
    /// Returns the unique key of the string.</returns>
    {$endregion}
    function GetKey: String; override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the differente states of a reference to a
  /// string constant or string literal.</summary>
  {$endregion}
  TStringReferenceState = (
    {$region 'xmldoc'}
    /// Not translatable
    {$endregion}
    rsUntranslatable,
    {$region 'xmldoc'}
    /// Not translated
    {$endregion}
    rsUntranslated,
    {$region 'xmldoc'}
    /// Translated using <see cref="TTrsnalator.GetText"/> method
    {$endregion}
    rsTranslatorGetText,
    {$region 'xmldoc'}
    /// Translated using <see cref="TTrsnalator.GetNText"/> method
    {$endregion}
    rsTranslatorGetNText,
    {$region 'xmldoc'}
    /// Translated using global <see cref="GetText"/> function
    {$endregion}
    rsGlobalGetText,
    {$region 'xmldoc'}
    /// Translated using global <see cref="GetNText"/> function
    {$endregion}
    rsGlobalGetNText
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TStringValueReference stores information about a reference to a string
  /// constant or a string literal in a Pascal code.</summary>
  {$endregion}
  TStringValueReference = class(TObject)
  private
    fStrValue: TStringValue;
    fBlock: TCodeBlock;
    fPos: Integer;
    fLen: Integer;
    fState: TStringReferenceState;
    fFuncPos: Integer;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for a specified <see cref="TStringValue"/>
    /// object.</summary>
    /// <param name="AStrValue">
    /// The <see cref="TStringValue"/> object referenced by this instance.</param>
    /// <param name="ABlock">
    /// The <see cref="TCodeBlock"/> object that represents the block of code that
    /// the string is referenced in.</param>
    /// <param name="APos">
    /// The start position of this reference of the string value from start of the
    /// Pascal source code.</param>
    /// <param name="ALen">
    /// The length of this reference of the string value, in characters.</param>
    {$endregion}
    constructor Create(AStrValue: TStringValue; ABlock: TCodeBlock; APos, ALen: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether this reference of the string value can be translated.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if this reference of the string can be translated.
    /// Otherwise, returns <see langword="false"/>.</returns>
    {$endregion}
    function IsTranslatable: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether this reference of the string value is already passed to
    /// a translation function.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if this reference of the string is passed to a
    /// translation function. Otherwise, returns <see langword="false"/>.</returns>
    {$endregion}
    function IsTranslated: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TStringValue"/> object that represents the referenced
    /// string value.</summary>
    {$endregion}
    property StrValue: TStringValue read fStrValue;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TCodeBlock"/> object that represents the block of code
    /// that the string is referenced in.</summary>
    {$endregion}
    property Block: TCodeBlock read fBlock;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the offset of this reference of the string value in the Pascal
    /// source code.</summary>
    {$endregion}
    property Pos: Integer read fPos;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the length of this reference of the string value.</summary>
    {$endregion}
    property Len: Integer read fLen;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the current translation state of this reference of the string
    /// value.</summary>
    {$endregion}
    property State: TStringReferenceState read fState write fState;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets the offset of translator function that this reference of the
    /// string value is passed to.</summary>
    {$endregion}
    property FuncPos: Integer read fFuncPos write fFuncPos;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCodeBlock represents body of a unit, procedure, function, or method in a
  /// Pascal code.</summary>
  {$endregion}
  TCodeBlock = class(TObject)
  private
    fName: String;
    fParent: TCodeBlock;
    fBlocks: TObjectList;
    fStringValues: TObjectList;
    function GetFullName: String;
    function GetBlockCount: Integer; inline;
    function GetBlocks(Index: Integer): TCodeBlock; inline;
    function GetStringCount: Integer; inline;
    function GetStrings(Index: Integer): TStringValue; inline;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.</summary>
    /// <param name="AParent">
    /// The block that this block is inside it.</param>
    /// <param name="AName">
    /// The name of unit, procedure, function, or method that its body is
    /// represented by this block.</param>
    {$endregion}
    constructor Create(AParent: TCodeBlock; const AName: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a specified block is the current block or one of its
    /// parent blocks.</summary>
    /// <param name="ABlockName">
    /// The name of the black to be checked for accessibility.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the block specified by its name is
    /// either the current block or one of its parent blocks, otherwise returns
    /// <see langword="false"/>.</returns>
    {$endregion}
    function HaveAccessTo(const ABlockName: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates a new block as the child block of the current one.</summary>
    /// <param name="ABlockName">
    /// The name of the child block.</param>
    /// <returns>
    /// The <see cref="TCodeBlock"/> object that represents the new child block.</returns>
    {$endregion}
    function AddBlock(const ABlockName: String): TCodeBlock;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a new string value to the list of current block's strings.</summary>
    /// <param name="StringValue">
    /// The new <see cref="TStringValue"/> object to add.</param>
    {$endregion}
    procedure AddString(StringValue: TStringValue);
    {$region 'xmldoc'}
    /// <summary>
    /// Searches the current block and its parent blocks to find a string constant
    /// that is identified by its name.</summary>
    /// <param name="AName">
    /// The name of string constant to find.</param>
    /// <returns>
    /// The found <see cref="TStringConstant"/> object or <see langword="nil"/> if
    /// a string constant with the specified name is not found.</returns>
    /// <seealso cref="StringByValue"/>
    {$endregion}
    function StringByName(const AName: String): TStringConstant;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches the current block and its parent blocks to find a string constant
    /// or a string literal that has a specified value.</summary>
    /// <param name="AValue">
    /// The string value to find.</param>
    /// <returns>
    /// The found <see cref="TStringValue"/> object or <see langword="nil"/> if
    /// the string value is not found.</returns>
    /// <seealso cref="StringByName"/>
    {$endregion}
    function StringByValue(const AValue: String): TStringValue;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the name that identifies the block.</summary>
    {$endregion}
    property Name: String read fName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the qualified name of the block.</summary>
    {$endregion}
    property FullName: String read GetFullName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the parent block of the current block.</summary>
    {$endregion}
    property Parent: TCodeBlock read fParent;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of direct child blocks of this block.</summary>
    {$endregion}
    property BlockCount: Integer read GetBlockCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the direct child blocks of this block.</summary>
    {$endregion}
    property Blocks[Index: Integer]: TCodeBlock read GetBlocks;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of string constants and string literals in this block.</summary>
    {$endregion}
    property StringCount: Integer read GetStringCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the string constants and the string literals in this block.</summary>
    {$endregion}
    property Strings[Index: Integer]: TStringValue read GetStrings;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TPascalStringCollector parse a Pascal code and extracts its string constants
  /// and string literals. TPascalStringCollector can also modify the Pascal code,
  /// so that make a selection of those extracted strings translatable.</summary>
  {$endregion}
  TPascalStringCollector = class(TObject)
  private
    fSource: String;
    fTranslator: TTranslator;
    fMainBlock: TCodeBlock;
    function ParseAndCollectStrings: TCodeBlock;
  private
    const SGetText = 'GetText';
    const SGetNText = 'GetNText';
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Modifies a specified Pascal source code to disallow translating of a
    /// specified string.</summary>
    /// <param name="ASource">
    /// The Pascal source code to modify.</param>
    /// <param name="StrRef">
    /// The information about the string.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the source code is modified to reflect
    /// the request. Otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="EnableTranslation"/>
    {$endregion}
    function DisableTranslation(var ASource: String; StrRef: TStringValueReference): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Modifies a specified Pascal source code to allow translating of a specified
    /// string.</summary>
    /// <param name="ASource">
    /// The Pascal source code to modify.</param>
    /// <param name="StrRef">
    /// The information about the string.</param>
    /// <returns>
    /// Returns <see langword="true"/> if the source code is modified to reflect
    /// the request. Otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="DisableTranslation"/>
    {$endregion}
    function EnableTranslation(var ASource: String; StrRef: TStringValueReference): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TCodeBlock"/> object that contains information about
    /// the parsed Pascal code.</summary>
    {$endregion}
    property MainBlock: TCodeBlock read fMainBlock;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.</summary>
    /// <param name="ASource">
    /// The Pascal code that its string values should be extracted or mark as
    /// translatable.</param>
    /// <param name="ATranslator">
    /// The <see cref="TTranslator"/> component instance that will
    /// be used for modifing the Pascal code, so that a selection of string values
    /// can be translated.</param>
    {$endregion}
    constructor Create(const ASource: String; ATranslator: TTranslator = nil);
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Extracts only the translatable string values from the Pascal code.</summary>
    /// <param name="Dest">
    /// The list of extracted strings.</param>
    /// <seealso cref="SetTranslatableStrings"/>
    {$endregion}
    procedure GetTranslatableStrings(Dest: TTextItems);
    {$region 'xmldoc'}
    /// <summary>
    /// Returns a modified version of the source code, where only a specified
    /// list of strings are marked as translatable.</summary>
    /// <param name="Strings">
    /// The list of strings that can be translated.
    /// </param>
    /// <returns>
    /// The modified source code.</returns>
    /// <seealso cref="GetTranslatableStrings"/>
    {$endregion}
    function SetTranslatableStrings(Strings: TTextItems): String;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the original source code.</summary>
    {$endregion}
    property Source: String read fSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="TTranslator"/> component instance that
    /// is used for for modifing the original source code, so that a selection of
    /// string values can be translated.</summary>
    {$endregion}
    property Translator: TTranslator read fTranslator;
  end;

{$region 'xmldoc'}
/// <summary>
/// Indicates whether a specified name is a valid Pascal identifier</summary>
/// <param name="AName">
/// The name to check for validity.</param>
/// <returns>
/// Returns <see langword="true"/> if the specified name is a valid Pascal identifier.
/// Otherwise returns <see langword="false"/>.</returns>
{$endregion}
function IsValidIdentifier(const AName: String): Boolean;

implementation

uses
  RTLConsts, Types, StrUtils, i18nHashList, i18nMD5, i18nUtils, i18nZStrList;

resourcestring
  SSyntaxError                        = 'Syntax error';
  SUnterminatedCommentError           = 'Unterminated comment';
  SUnterminatedStringError            = 'Unterminated string';
  SUnexpectedEndOfFileError           = 'Unexpected end of file';
  SUnexpectedSomethingError           = 'Unexpected ''%s''';
  SSomethingExpectedButEndOfFileError = '''%s'' expected but end of file reached.';
  SSomethingExpectedError             = '''%s'' expected but ''%s'' found';
  SIdentifierExpectedError            = 'Identifier expected but ''%s'' found';
  SStringConstantExpectedError        = 'String constant expected but ''%s'' found';


{ Helper Functions }

function IsValidIdentifier(const AName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Length(AName) > 0) and CharInSet(AName[1], ['A'..'Z', 'a'..'z', '_']) then
  begin
    for I := 2 to Length(AName) do
      if not CharInSet(AName[I], ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
        Exit;
    Result := not TPascalTokenizer.IsReservedWord(AName);
  end;
end;

{ EPascalParserError }

procedure EPascalParserError.SetErrorDetails(AOffset, ALineNo, AColNo: Integer;
  const AToken: String);
begin
  fOffset := AOffset;
  fLineNo := ALineNo;
  fColNo := AColNo;
end;

{ TPascalTokenizer }

constructor TPascalTokenizer.Create(const ASource: String; APos: Integer);
begin
  fSource := ASource;
  SetPosition(APos);
end;

function TPascalTokenizer.GetToken: String;
begin
  if fTokenID in [T_ReservedWord, T_Identifier] then
    Result := fToken
  else if fTokenID <> T_EOF then
    SetString(Result, fTokenPtr, fTokenLen)
  else
    Result := '';
end;

function TPascalTokenizer.GetTokenAsSource: String;
begin
  if fTokenID <> T_EOF then
    SetString(Result, fTokenPtr, fTokenLen)
  else
    Result := '';
end;

function TPascalTokenizer.GetTokenPos: Integer;
begin
  Result := fTokenPtr - PChar(fSource) + 1;
end;

function TPascalTokenizer.GetTokenLineNo: Integer;
var
  P: PChar;
begin
  Result := 1;
  P := PChar(fSource);
  while P <= fTokenPtr do
  begin
    if CharInSet(P^, [#10, #13]) then
    begin
      Inc(Result);
      if (P^ <> (P + 1)^) and CharInSet((P + 1)^, [#10, #13]) then
        Inc(P);
    end;
    Inc(P);
  end;
end;

function TPascalTokenizer.GetTokenColNo: Integer;
var
  P: PChar;
begin
  P := fTokenPtr;
  while (P >= PChar(fSource)) and not CharInSet(P^, [#10, #13]) do
    Dec(P);
  Result := fTokenPtr - P;
end;

function TPascalTokenizer.GetNextToken(var S: PChar;
  out Err: TPascalParseError): TPascalToken;
var
  Identifier: String;
  P: Char;
begin
  Result := T_EOF;
  Err := peNone;
  case S^ of
    #0:                                            // end of file or white space?
    begin
      if fTokenPtr - PChar(fSource) < Length(Source) then
      begin
        Inc(S);
        Result := T_WhiteSpace;
      end;
    end;
    '&':                                           // reserved word as identifier
    begin
      Inc(S);
      if CharInSet(S^, ['A'..'Z', 'a'..'z', '_']) then
      begin
        repeat
          Inc(S);
        until not CharInSet(S^, ['A'..'Z', 'a'..'z', '0'..'9', '_']);
        Result := T_Identifier;
      end
      else
        Err := peSyntaxError;
    end;
    'A'..'Z', 'a'..'z', '_':                       // reserved word, identifier
    begin
      repeat
        Inc(S);
      until not CharInSet(S^, ['A'..'Z', 'a'..'z', '0'..'9', '_']);
      SetString(Identifier, fTokenPtr, S - fTokenPtr);
      if IsReservedWord(Identifier) then
        Result := T_ReservedWord
      else
        Result := T_Identifier;
    end;
    #1..#32:                                       // white space
    begin
      repeat
        Inc(S);
      until not CharInSet(S^, [#1..#32]);
      Result := T_WhiteSpace;
    end;
    '0'..'9':                                      // number, float
    begin
      Result := T_Integer;
      repeat
        Inc(S);
      until not CharInSet(S^, ['0'..'9']);
      if S^ = '.' then
      begin
        Result := T_Float;
        repeat
          Inc(S);
        until not CharInSet(S^, ['0'..'9']);
      end;
      if CharInSet(S^, ['E', 'e']) then
      begin
        Result := T_Float;
        Inc(S);
        if CharInSet(S^, ['+', '-']) then
          Inc(S);
        if CharInSet(S^, ['0'..'9']) then
          repeat
            Inc(S);
          until not CharInSet(S^, ['0'..'9'])
        else
        begin
          Err := peSyntaxError;
          Result := T_EOF;
        end;
      end;
    end;
    '$':                                           // hex
    begin
      repeat
        Inc(S);
      until not CharInSet(S^, ['0'..'9', 'a'..'f', 'A'..'F']);
      if (S - 1)^ <> '$' then
        Result := T_Hex
      else
        Err := peSyntaxError;
    end;
    '#':                                           // character
    begin
      Inc(S);
      if S^ = '$' then
      begin
        repeat
          Inc(S);
        until not CharInSet(S^, ['0'..'9', 'a'..'f', 'A'..'F']);
        if (S - 1)^ <> '$' then
          Result := T_Character;
      end
      else
      begin
        while CharInSet(S^, ['0'..'9']) do
          Inc(S);
        if (S - 1)^ <> '#' then
          Result := T_Character
        else
          Err := peSyntaxError;
      end;
    end;
    '''':                                          // string
    begin
      Inc(S);
      while ((S^ <> '''') or ((S + 1)^ = '''')) and (S^ <> #0) do
      begin
        if (S^ = '''') and ((S + 1)^ = '''') then
          Inc(S);
        Inc(S);
      end;
      if S^ <> #0 then
      begin
        Inc(S);
        Result := T_String;
      end
      else
        Err := peUnterminatedString;
    end;
    '{':                                           // comment { }
    begin
      repeat
        Inc(S)
      until (S^ = '}') or (S^ = #0);
      if S^ <> #0 then
      begin
        Inc(S);
        Result := T_Comment;
      end
      else
        Err := peUnterminatedComment;
    end
  else
    if (S^ = '(') and ((S + 1)^ = '*') then        // comment (* *)
    begin
      Inc(S, 2);
      while not ((S^ = '*') and ((S + 1)^ = ')')) and (S^ <> #0) do
        Inc(S);
      if S^ <> #0 then
      begin
        Inc(S, 2);
        Result := T_Comment;
      end
      else
        Err := peUnterminatedComment;
    end
    else if (S^ = '/') and ((S + 1)^ = '/') then   // inline comment
    begin
      Inc(S, 2);
      while not CharInSet(S^, [#0, #10, #13]) do
        Inc(S);
      Result := T_Comment;
    end
    else                                           // symbol
    begin
      P := S^;
      Inc(S);
      if ((P = ':') and (S^ = '=')) or
         ((P = '.') and (S^ = '.')) or
         ((P = '>') and (S^ = '=')) or
         ((P = '<') and ((S^ = '=') or (S^ = '>')))
      then
        Inc(S);
      Result := T_Symbol;
    end;
  end;
end;

function TPascalTokenizer.Next: Boolean;
const
  BeginASM = 'asm';
  EndASM = 'end';
var
  S, P: PChar;
begin
  repeat
    Inc(fTokenPtr, fTokenLen);
    S := fTokenPtr;
    if not fInsideAssembler then
    begin
      fTokenID := GetNextToken(S, fError);
      fTokenLen := S - fTokenPtr;
      if fTokenID in [T_Identifier, T_ReservedWord] then
      begin
        SetString(fToken, fTokenPtr, fTokenLen);
        fToken := LowerCase(fToken);
        if (fTokenID = T_ReservedWord) and (fToken = BeginASM) then
          fInsideAssembler := True;
      end;
    end
    else
    begin
      repeat
        repeat
          P := S;
          fTokenID := GetNextToken(S, fError);
        until fTokenID in [T_Identifier, T_EOF];
        SetString(fToken, P, S - P);
      until (fTokenID = T_EOF) or (LowerCase(fToken) = EndASM);
      if fTokenID <> T_EOF then
        fTokenID := T_Assembler;
      fTokenLen := P - fTokenPtr;
      fInsideAssembler := False;
    end;
  until not (TokenID in [T_WhiteSpace, T_Comment]);
  Result := (TokenID <> T_EOF);
end;

procedure TPascalTokenizer.SetPosition(Pos: Integer);
begin
  if Pos < 1 then
    Pos := 1;
  if Pos > Length(fSource) then
    Pos := Length(Source);
  fInsideAssembler := False;
  fTokenPtr := PChar(fSource);
  fTokenLen := Pos - 1;
  Next;
end;

class function TPascalTokenizer.IsReservedWord(const Token: String): Boolean;
const
  ReservedWords: array[1..65] of String = (
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const',
    'constructor', 'destructor', 'dispinterface', 'div', 'do', 'downto',
    'else', 'end', 'except', 'exports', 'file', 'finalization', 'finally',
    'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library',
    'mod', 'nil', 'not', 'object', 'of', 'or', 'out', 'packed', 'procedure',
    'program', 'property', 'raise', 'record', 'repeat', 'resourcestring',
    'set', 'shl', 'shr', 'string', 'then', 'threadvar', 'to', 'try', 'type',
    'unit', 'until', 'uses', 'var', 'while', 'with', 'xor');
var
  I: Integer;
  LowerToken: String;
begin
  Result := False;
  LowerToken := LowerCase(Token);
  for I := Low(ReservedWords) to High(ReservedWords) do
    if ReservedWords[I] = LowerToken then
    begin
      Result := True;
      Exit;
    end;
end;

class function TPascalTokenizer.IsDirective(const Token: String): Boolean;
const
  Directives: array[1..32] of String = (
    'abstract', 'assembler', 'cdecl', 'delayed', 'deprecated', 'dispid',
    'dynamic', 'experimental', 'export', 'external', 'far', 'final',
    'forward', 'helper', 'inline', 'library', 'local', 'message', 'near',
    'overload', 'override', 'pascal', 'platform', 'register', 'reintroduce',
    'safecall', 'sealed', 'static', 'stdcall', 'varargs', 'virtual', 'winapi');
var
  I: Integer;
  LowerToken: String;
begin
  Result := False;
  LowerToken := LowerCase(Token);
  for I := Low(Directives) to High(Directives) do
    if Directives[I] = LowerToken then
    begin
      Result := True;
      Exit;
    end;
end;

class function TPascalTokenizer.IsPropertyDirective(const Token: String): Boolean;
const
  PropertyDirectives: array[1..11] of String = (
    'default', 'dispid', 'implements', 'index', 'nodefault', 'read',
    'write', 'readonly', 'stored', 'write', 'writeonly');
var
  I: Integer;
  LowerToken: String;
begin
  Result := False;
  LowerToken := LowerCase(Token);
  for I := Low(PropertyDirectives) to High(PropertyDirectives) do
    if PropertyDirectives[I] = LowerToken then
    begin
      Result := True;
      Exit;
    end;
end;

class function TPascalTokenizer.IsVisibilityDirective(const Token: String): Boolean;
const
  VisibilityDirectives: array[1..5] of String = (
    'strict', 'private', 'protected', 'public', 'published');
var
  I: Integer;
  LowerToken: String;
begin
  Result := False;
  LowerToken := LowerCase(Token);
  for I := Low(VisibilityDirectives) to High(VisibilityDirectives) do
    if VisibilityDirectives[I] = LowerToken then
    begin
      Result := True;
      Exit;
    end;
end;

class function TPascalTokenizer.IsPortabilityDirective(const Token: String): Boolean;
const
  PortabilityDirectives: array[1..4] of String = (
    'deprecated', 'experimental', 'library', 'platform');
var
  I: Integer;
  LowerToken: String;
begin
  Result := False;
  LowerToken := LowerCase(Token);
  for I := Low(PortabilityDirectives) to High(PortabilityDirectives) do
    if PortabilityDirectives[I] = LowerToken then
    begin
      Result := True;
      Exit;
    end;
end;

{ TStringValue }

constructor TStringValue.Create(const AValue: String);
begin
  fValue := AValue;
  fReferences := TObjectList.Create(True);
end;

destructor TStringValue.Destroy;
begin
  fReferences.Free;
  inherited Destroy;
end;

function TStringValue.AddReference(Block: TCodeBlock; Pos, Len: Integer): TStringValueReference;
begin
  Result := TStringValueReference.Create(Self, Block, Pos, Len);
  fReferences.Add(Result);
end;

function TStringValue.HasAnyTranslatableReference: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ReferenceCount - 1 do
    if References[I].IsTranslatable then
    begin
      Result := True;
      Exit;
    end;
end;

function TStringValue.GetKey: String;
begin
  Result := MD5(Value);
end;

function TStringValue.GetReferenceCount: Integer;
begin
  Result := fReferences.Count;
end;

function TStringValue.GetReferences(Index: Integer): TStringValueReference;
begin
  Result := TStringValueReference(fReferences[Index]);
end;

{ TStringConstant }

constructor TStringConstant.Create(const AName, AValue: String; ResStr: Boolean);
begin
  inherited Create(AValue);
  fName := AName;
  fIsResource := ResStr;
end;

{ TStringPlural }

function TStringPlural.GetKey: String;
begin
  Result := '#' + inherited GetKey;
end;

{ TStringValueReference }

constructor TStringValueReference.Create(AStrValue: TStringValue;
  ABlock: TCodeBlock; APos, ALen: Integer);
begin
  fStrValue := AStrValue;
  fBlock := ABlock;
  fPos := APos;
  fLen := ALen;
end;

function TStringValueReference.IsTranslatable: Boolean;
begin
  Result := (State <> rsUntranslatable);
end;

function TStringValueReference.IsTranslated: Boolean;
begin
  Result := not (State in [rsUntranslatable, rsUntranslated]);
end;

{ TCodeBlock }

constructor TCodeBlock.Create(AParent: TCodeBlock; const AName: String);
begin
  fName := AName;
  fParent := AParent;
  fBlocks := TObjectList.Create(True);
  fStringValues := TObjectList.Create(True);
end;

destructor TCodeBlock.Destroy;
begin
  fBlocks.Free;
  fStringValues.Free;
  inherited Destroy;
end;

function TCodeBlock.GetFullName: String;
begin
  if Assigned(Parent) then
    Result := Parent.FullName + '.' + Name
  else
    Result := Name;
end;

function TCodeBlock.GetBlockCount: Integer;
begin
  Result := fBlocks.Count;
end;

function TCodeBlock.GetBlocks(Index: Integer): TCodeBlock;
begin
  Result := TCodeBlock(fBlocks[Index]);
end;

function TCodeBlock.GetStringCount: Integer;
begin
  Result := fStringValues.Count;
end;

function TCodeBlock.GetStrings(Index: Integer): TStringValue;
begin
  Result := TStringValue(fStringValues[Index]);
end;

function TCodeBlock.HaveAccessTo(const ABlockName: String): Boolean;
var
  Block: TCodeBlock;
begin
  Result := False;
  Block := Self;
  repeat
    if AnsiStartsText(ABlockName, Block.Name) and
       ((Length(Block.Name) = Length(ABlockName)) or
        (Block.Name[Length(ABlockName) + 1] = '.')) then
    begin
      Result := True;
      Exit;
    end;
    Block := Block.Parent;
  until not Assigned(Block);
end;

function TCodeBlock.AddBlock(const ABlockName: String): TCodeBlock;
begin
  Result := TCodeBlock.Create(Self, ABlockName);
  fBlocks.Add(Result);
end;

procedure TCodeBlock.AddString(StringValue: TStringValue);
begin
  fStringValues.Add(StringValue);
end;

function TCodeBlock.StringByName(const AName: String): TStringConstant;
var
  I: Integer;
begin
  for I := 0 to StringCount - 1 do
    if Strings[I] Is TStringConstant then
    begin
      Result := TStringConstant(Strings[I]);
      if SameText(Result.Name, AName) then
        Exit;
    end;
  if Assigned(Parent) then
    Result := Parent.StringByName(AName)
  else
    Result := nil;
end;

function TCodeBlock.StringByValue(const AValue: String): TStringValue;
var
  I: Integer;
begin
  for I := 0 to StringCount - 1 do
  begin
    Result := Strings[I];
    if Result.Value = AValue then
      Exit;
  end;
  if Assigned(Parent) then
    Result := Parent.StringByValue(AValue)
  else
    Result := nil;
end;

{ TPascalStringCollector }

constructor TPascalStringCollector.Create(const ASource: String;
  ATranslator: TTranslator);
begin
  fSource := ASource;
  fTranslator := ATranslator;
  fMainBlock := ParseAndCollectStrings;
end;

destructor TPascalStringCollector.Destroy;
begin
  if Assigned(fMainBlock) then
    fMainBlock.Free;
  inherited Destroy;
end;

function TPascalStringCollector.ParseAndCollectStrings: TCodeBlock;

  var T: TPascalTokenizer;

  type TDeclType = (dtMain, dtInterface, dtImplementation, dtFunction, dtClass);

  procedure ProcessDecl(Block: TCodeBlock; DeclType: TDeclType); forward;

  procedure Error(const ErrorText: String); overload;
  var
    E: EPascalParserError;
  begin
    E := EPascalParserError.CreateFmt('[%d:%d] %s', [T.TokenLineNo, T.TokenColNo, ErrorText]);
    E.SetErrorDetails(T.TokenPos, T.TokenLineNo, T.TokenColNo, T.TokenAsSource);
    raise E;
  end;

  procedure Error(const ErrorText: String; const Args: array of const); overload;
  begin
    Error(Format(ErrorText, Args));
  end;

  function NextToken: Boolean;
  begin
    Result := True;
    if not T.Next then
      case T.Error of
        peUnterminatedComment: Error(SUnterminatedCommentError);
        peUnterminatedString: Error(SUnterminatedStringError);
        peSyntaxError: Error(SSyntaxError);
      else
        Result := False;
      end;
  end;

  procedure NextTokenNeeded;
  begin
    if not NextToken then
      Error(SUnexpectedEndOfFileError)
  end;

  procedure SkipToSymbol(const Symbol: String);
  begin
    while (T.TokenID <> T_Symbol) or (T.Token <> Symbol) do
      if not NextToken then
        Error(SSomethingExpectedButEndOfFileError, [Symbol]);
  end;

  procedure SkipAfterSymbol(const Symbol: String);
  begin
    SkipToSymbol(Symbol);
    NextTokenNeeded;
  end;

  procedure SkipBlock(const BeginToken, EndToken: String);
  var
    Level: Integer;
  begin
    Level := 0;
    repeat
      if T.Token = BeginToken then
        Inc(Level)
      else if T.Token = EndToken then
        Dec(Level);
    until (Level = 0) or not NextToken;
    if Level <> 0 then
      Error(SSomethingExpectedButEndOfFileError, [EndToken]);
    NextTokenNeeded;
  end;

  procedure ProcessIdentifier(out Ident: String; out Pos: Integer;
    AllowGenerics: Boolean); overload;
  var
    ValidTokens: set of TPascalToken;
  begin
    Pos := T.TokenPos;
    ValidTokens := [T_Identifier];
    repeat
      if not (T.TokenID in ValidTokens) then
        Error(SIdentifierExpectedError, [T.Token]);
      Ident := Ident + T.Token;
      NextTokenNeeded;
      if AllowGenerics and (T.Token = '<') then
      begin
        repeat
          Ident := Ident + T.Token;
          NextTokenNeeded;
        until (T.Token = '>');
        Ident := Ident + T.Token;
        NextTokenNeeded;
      end;
      if T.Token <> '.' then
        Exit;
      Ident := Ident + T.Token;
      NextTokenNeeded;
      Include(ValidTokens, T_ReservedWord);
    until False;
  end;

  procedure ProcessIdentifier(out Ident: String; AllowGenerics: Boolean); overload;
  var
    Pos: Integer;
  begin
    ProcessIdentifier(Ident, Pos, AllowGenerics);
  end;

  procedure ProcessString(out Str: String; out Pos, Len: Integer);
  var
    SubStr: String;
  begin
    if not (T.TokenID in [T_String, T_Character]) then
      Error(SStringConstantExpectedError, [T.Token]);
    Str := '';
    Len := 0;
    Pos := T.TokenPos;
    repeat
      SubStr := T.Token;
      if T.TokenID = T_String then
        SubStr := AnsiDequotedStr(T.Token, '''')
      else
        SubStr := Char(StrToInt(Copy(SubStr, 2, Length(SubStr) - 1)));
      Str := Str + SubStr;
      Len := T.TokenPos - Pos + T.TokenLen;
      NextTokenNeeded;
      if T.Token = '+' then
        NextTokenNeeded;
    until not (T.TokenID in [T_String, T_Character]);
  end;

  procedure ProcessConsts(Block: TCodeBlock; IsResource, CanCollect: Boolean);
  var
    Indetifier, Value: String;
    ValuePos, ValueLen: Integer;
    StrConst: TStringConstant;
  begin
    if T.Token = '[' then
      SkipBlock('[', ']');
    while T.TokenID = T_Identifier do
    begin
      Indetifier := T.TokenAsSource;
      SkipAfterSymbol('=');
      if (T.TokenID in [T_ReservedWord, T_Identifier]) and
         ((T.Token = 'string') or (T.Token = 'ansistring') or
          (T.Token = 'widestring') or (T.Token = 'utf8string') or
          (T.Token = 'unicodestring') or (T.Token = 'rawbytestring') or
          (T.Token = 'shortstring')) then
      begin
        NextTokenNeeded;
        if T.Token <> '(' then
          Error(SSomethingExpectedError, ['(', T.Token]);
        NextTokenNeeded;
      end;
      if T.TokenID in [T_String, T_Character] then
      begin
        ProcessString(Value, ValuePos, ValueLen);
        if CanCollect and IsStringTranslatable(Value) then
        begin
          StrConst := TStringConstant.Create(Indetifier, Value, IsResource);
          Block.AddString(StrConst);
        end;
      end
      else if T.TokenID = T_Symbol then
      begin
        if T.Token = '(' then
          SkipBlock('(', ')')
        else if T.Token = '[' then
          SkipBlock('[', ']');
      end;
      SkipAfterSymbol(';');
      if T.Token = '[' then
        SkipBlock('[', ']');
    end;
  end;

  procedure ProcessFuncDecl(Block: TCodeBlock; out FuncName: String;
    out Prototype: Boolean);
  begin
    if T.TokenID = T_Identifier then
      ProcessIdentifier(FuncName, True)
    else
      FuncName := '';
    if T.Token = '(' then
      SkipBlock('(', ')');
    SkipAfterSymbol(';');
    Prototype := False;
    while (T.TokenID = T_Identifier) and T.IsDirective(T.Token) do
    begin
      if (T.Token = 'forward') or (T.Token = 'external') then
        Prototype := True;
      SkipAfterSymbol(';');
    end;
  end;

  procedure SkipFuncDecl(Block: TCodeBlock);
  var
    FuncName: String;
    Prototype: Boolean;
  begin
    ProcessFuncDecl(Block, FuncName, Prototype);
  end;

  procedure SkipTypeDecl(Block: TCodeBlock);
  var
    AlreadyEndOfDecl: Boolean;
  begin
    if T.Token = '[' then
      SkipBlock('[', ']');
    while T.TokenID = T_Identifier do
    begin
      if T.Token = '<' then
        SkipBlock('<', '>');
      SkipAfterSymbol('=');
      if T.Token = 'packed' then
        NextTokenNeeded;
      AlreadyEndOfDecl := False;
      if T.Token = 'record' then
        SkipBlock('record', 'end')
      else if (T.Token = 'function') or (T.Token = 'procedure') or
              (T.Token = 'constructor') or (T.Token = 'destructor') then
      begin
        NextTokenNeeded;
        SkipFuncDecl(Block);
        AlreadyEndOfDecl := True;
      end
      else if (T.Token = 'class') or (T.Token = 'interface') or (T.Token = 'dispinterface') then
      begin
        repeat
          NextToken;
        until (T.TokenID <> T_Identifier) or not T.IsDirective(T.Token);
        if T.Token <> 'of' then
        begin
          if T.Token = '(' then
          begin
            SkipBlock('(', ')');
            while (T.TokenID = T_Identifier) and T.IsPortabilityDirective(T.Token) do
              NextTokenNeeded;
          end;
          if T.Token = 'for' then
          begin
            NextTokenNeeded;
            if T.TokenID <> T_Identifier then
              Error(SIdentifierExpectedError, [T.Token]);
            NextTokenNeeded;
          end;
          if T.Token <> ';' then
          begin
            ProcessDecl(Block, dtClass);
            if T.Token <> 'end' then
              Error(SSomethingExpectedError, ['end', T.Token]);
          end;
        end;
      end;
      if not AlreadyEndOfDecl then
        SkipAfterSymbol(';');
      if (T.TokenID = T_Identifier) and T.IsVisibilityDirective(T.Token) then
        Exit;
      if T.Token = '[' then
        SkipBlock('[', ']');
    end;
  end;

  procedure SkipVarDecl(Block: TCodeBlock);
  var
    AlreadyEndOfDecl: Boolean;
  begin
    if T.Token = '[' then
      SkipBlock('[', ']');
    while T.TokenID = T_Identifier do
    begin
      SkipAfterSymbol(':');
      if T.Token = 'packed' then
        NextTokenNeeded;
      AlreadyEndOfDecl := False;
      if T.Token = 'record' then
        SkipBlock('record', 'end')
      else if (T.Token = 'function') or (T.Token = 'procedure') then
      begin
        NextTokenNeeded;
        SkipFuncDecl(Block);
        AlreadyEndOfDecl := True;
      end;
      if not AlreadyEndOfDecl then
        SkipAfterSymbol(';');
      if (T.TokenID = T_Identifier) and T.IsVisibilityDirective(T.Token) then
        Exit;
      if T.Token = '[' then
        SkipBlock('[', ']');
    end;
  end;

  procedure ProcessBody(Block: TCodeBlock; CanCollect: Boolean);
  var
    Level: Integer;
    StrValue: TStringValue;
    Str, Ident, Plural: String;
    IdentPos, PluralPos, FuncPos: Integer;
    StrPos, StrLen: Integer;
    AlreadyNextToken, InPlural: Boolean;
    TranslationState: TStringReferenceState;
    OpenBraces: Integer;

    procedure PrepareReference(StrRef: TStringValueReference);
    begin
      if TranslationState in [rsTranslatorGetNText, rsGlobalGetNText] then
      begin
        StrRef.State := rsUntranslatable;
        if InPlural then
          ZStrings.Add(Plural, StrRef.StrValue.Value);
      end
      else if not CanCollect or ((FuncPos = 0) and (TranslationState <> rsUntranslated)) then
        StrRef.State := rsUntranslatable
      else
      begin
        StrRef.State := TranslationState;
        StrRef.FuncPos := FuncPos;
        FuncPos := 0;
      end;
    end;

  begin
    if CanCollect then
      Level := 0
    else
      Level := 1;
    FuncPos := 0;
    PluralPos := 0;
    OpenBraces := 0;
    InPlural := False;
    TranslationState := rsUntranslated;
    repeat
      AlreadyNextToken := False;
      case T.TokenID of
        T_ReservedWord:
        begin
          if (T.Token = 'begin') or (T.Token = 'asm') or
             (T.Token = 'case') or (T.Token = 'try')
          then
            Inc(Level)
          else if T.Token = 'end' then
          begin
            Dec(Level);
            if Level <= 0 then
            begin
              if (Level = 0) and T.Next then
              begin
                if not Assigned(Block.Parent) then
                begin
                  if T.Token <> '.' then
                    Error(SSomethingExpectedError, ['.', T.Token]);
                end
                else
                begin
                  if T.Token <> ';' then
                    Error(SSomethingExpectedError, [';', T.Token]);
                end;
              end;
              Exit;
            end
          end;
        end;
        T_Identifier:
        begin
          ProcessIdentifier(Ident, IdentPos, False);
          if T.Token = '(' then
          begin
            if TranslationState = rsUntranslated then
            begin
              if SameText(Ident, SGetText) then
                TranslationState := rsGlobalGetText
              else if SameText(Ident, SGetNText) then
                TranslationState := rsGlobalGetNText
              else if Assigned(Translator) then
              begin
                if SameText(Ident, Translator.Name + '.' + SGetText) then
                  TranslationState := rsTranslatorGetText
                else if SameText(Ident, Translator.Name + '.' + SGetNText) then
                  TranslationState := rsTranslatorGetNText
              end;
              FuncPos := IdentPos;
              OpenBraces := 0;
            end;
          end
          else if T.Token <> '[' then
          begin
            StrValue := Block.StringByName(Ident);
            if Assigned(StrValue) then
              PrepareReference(StrValue.AddReference(Block, IdentPos, Length(Ident)));
          end;
          AlreadyNextToken := True;
        end;
        T_String, T_Character:
        begin
          ProcessString(Str, StrPos, StrLen);
          if IsStringTranslatable(Str) then
          begin
            StrValue := Block.StringByValue(Str);
            if not Assigned(StrValue) then
            begin
              StrValue := TStringLiteral.Create(Str);
              Block.AddString(StrValue);
            end;
            PrepareReference(StrValue.AddReference(Block, StrPos, StrLen));
          end;
          AlreadyNextToken := True;
        end;
        T_Symbol:
        begin
          if TranslationState <> rsUntranslated then
          begin
            if T.Token = ')' then
            begin
              Dec(OpenBraces);
              if OpenBraces = 0 then
              begin
                TranslationState := rsUntranslated;
                FuncPos := 0;
              end;
            end
            else if T.Token = '(' then
              Inc(OpenBraces)
            else if TranslationState in [rsTranslatorGetNText, rsGlobalGetNText] then
            begin
              if T.Token = '[' then
              begin
                PluralPos := T.TokenPos;
                InPlural := True;
                Plural := '';
              end
              else if T.Token = ']' then
              begin
                InPlural := False;
                StrValue := Block.StringByValue(Plural);
                if not Assigned(StrValue) then
                begin
                  StrValue := TStringPlural.Create(Plural);
                  Block.AddString(StrValue);
                end;
                StrValue.AddReference(Block, PluralPos, T.TokenPos - PluralPos).State := TranslationState;
              end;
            end;
          end;
        end;
      end;
      if not AlreadyNextToken then
        NextTokenNeeded;
    until False;
  end;

  procedure ProcessDecl(Block: TCodeBlock; DeclType: TDeclType);
  var
    SubBlock: TCodeBlock;
    FuncName: String;
    FuncPrototype: Boolean;
    AlreadyNextToken: Boolean;
  begin
    repeat
      AlreadyNextToken := False;
      if T.TokenID = T_ReservedWord then
      begin
        if T.Token = 'class' then
          NextTokenNeeded;
        if (T.Token = 'function') or (T.Token = 'procedure') or
           (T.Token = 'constructor') or (T.Token = 'destructor') or
           (T.Token = 'operator') then
        begin
          NextTokenNeeded;
          ProcessFuncDecl(Block, FuncName, FuncPrototype);
          if not FuncPrototype and (DeclType in [dtMain, dtImplementation, dtFunction]) then
          begin
            SubBlock := Block.AddBlock(FuncName);
            ProcessDecl(SubBlock, dtFunction);
            ProcessBody(SubBlock, True);
          end
          else
            AlreadyNextToken := True;
        end
        else if T.Token = 'resourcestring' then
        begin
          NextTokenNeeded;
          ProcessConsts(Block, True, DeclType <> dtClass);
          AlreadyNextToken := True;
        end
        else if T.Token = 'const' then
        begin
          NextTokenNeeded;
          ProcessConsts(Block, False, DeclType <> dtClass);
          AlreadyNextToken := True;
        end
        else if T.Token = 'type' then
        begin
          NextTokenNeeded;
          SkipTypeDecl(Block);
          AlreadyNextToken := True;
        end
        else if (T.Token = 'var') or (T.Token = 'threadvar') then
        begin
          NextTokenNeeded;
          SkipVarDecl(Block);
          AlreadyNextToken := True;
        end
        else if T.Token = 'asm' then
        begin
          if DeclType <> dtFunction then
            Error(SUnexpectedSomethingError, [T.Token]);
          Exit;
        end
        else if T.Token = 'begin' then
        begin
          if not (DeclType in [dtMain, dtImplementation, dtFunction]) then
            Error(SUnexpectedSomethingError, [T.Token]);
          Exit;
        end
        else if T.Token = 'end' then
        begin
          if not (DeclType in [dtMain, dtImplementation, dtClass]) then
            Error(SUnexpectedSomethingError, [T.Token]);
          Exit;
        end
        else if T.Token = 'implementation' then
        begin
          if DeclType <> dtInterface then
            Error(SUnexpectedSomethingError, [T.Token]);
          Exit;
        end
        else if (T.Token = 'initialization') or (T.Token = 'finalization') then
        begin
          if DeclType <> dtImplementation then
            Error(SUnexpectedSomethingError, [T.Token]);
          Exit;
        end
      end
      else if T.TokenID = T_Symbol then
      begin
         if T.Token = '[' then
         begin
           SkipBlock('[', ']');
           AlreadyNextToken := True;
         end;
      end
      else if (DeclType = dtClass) and (T.TokenID = T_Identifier) then
      begin
        if T.IsVisibilityDirective(T.Token) then
        begin
          repeat
            NextTokenNeeded;
          until (T.TokenID <> T_Identifier) or not T.IsVisibilityDirective(T.Token);
          AlreadyNextToken := True;
        end
        else if T.IsPropertyDirective(T.Token) then
        begin
          repeat
            SkipAfterSymbol(';');
          until (T.TokenID <> T_Identifier) or not T.IsPropertyDirective(T.Token);
          AlreadyNextToken := True;
        end
        else
        begin
          SkipVarDecl(Block);
          AlreadyNextToken := True;
        end;
      end;
      if not AlreadyNextToken then
        SkipAfterSymbol(';');
    until False;
  end;

var
  MainType, MainName: String;
begin
  Result := nil;
  T := TPascalTokenizer.Create(Source);
  try
    if T.TokenID = T_EOF then
      NextTokenNeeded;
    MainType := T.Token;
    NextTokenNeeded;
    ProcessIdentifier(MainName, False);
    SkipAfterSymbol(';');
    Result := TCodeBlock.Create(nil, MainName);
    try
      if (MainType = 'program') or (MainType = 'library') or (MainType = 'package') then
      begin
        ProcessDecl(Result, dtMain);
        ProcessBody(Result, T.Token = 'begin');
      end
      else if MainType = 'unit' then
      begin
        if T.Token <> 'interface' then
          Error(SSomethingExpectedError, ['interface', T.Token]);
        NextTokenNeeded;
        ProcessDecl(Result, dtInterface);
        if T.Token <> 'implementation' then
          Error(SSomethingExpectedError, ['implementation', T.Token]);
        NextTokenNeeded;
        ProcessDecl(Result, dtImplementation);
        if (T.Token <> 'begin') or NextToken then
          ProcessBody(Result, False);
      end;
    except
     if Assigned(Result) then
       FreeAndNil(Result);
     raise;
    end;
  finally
    T.Free;
  end;
end;

function TPascalStringCollector.DisableTranslation(var ASource: String;
  StrRef: TStringValueReference): Boolean;
var
  T: TPascalTokenizer;
begin
  Result := False;
  if (StrRef.FuncPos = 0) or not(StrRef.State in [rsTranslatorGetText, rsGlobalGetText]) then
    Exit;
  T := TPascalTokenizer.Create(ASource, StrRef.Pos + StrRef.Len);
  try
    if ((T.Token = ',') and not (T.Next and (T.TokenID in [T_Identifier, T_String]) and T.Next)) or (T.Token <> ')') then
      Exit;
    Delete(ASource, StrRef.Pos + StrRef.Len, T.TokenPos - (StrRef.Pos + StrRef.Len) + 1);
    Delete(ASource, StrRef.FuncPos, StrRef.Pos - StrRef.FuncPos);
  finally
    T.Free;
  end;
  Result := True;
end;

function TPascalStringCollector.EnableTranslation(var ASource: String;
  StrRef: TStringValueReference): Boolean;
var
  OldCode, NewCode: String;
begin
  OldCode := Copy(ASource, StrRef.Pos, StrRef.Len);
  if Assigned(Translator) and (Translator.Owner <> nil) and (Translator.Name <> '') and
    StrRef.Block.HaveAccessTo(Translator.Owner.ClassName)
  then
    NewCode := Format('%s.%s(%s)', [Translator.Name, SGetText, OldCode])
  else
    NewCode := Format('%s(%s, ''*'')', [SGetText, OldCode]);
  Delete(ASource, StrRef.Pos, StrRef.Len);
  Insert(NewCode, ASource, StrRef.Pos);
  Result := True;
end;

procedure TPascalStringCollector.GetTranslatableStrings(Dest: TTextItems);

  procedure GetTranslatableStringsOf(Block: TCodeBlock);
  var
    I: Integer;
    StrVal: TStringValue;
  begin
    for I := 0 to Block.StringCount - 1 do
    begin
      StrVal := Block.Strings[I];
      if StrVal.HasAnyTranslatableReference then
        Dest.Add(StrVal.Key).OriginalValue := StrVal.Value;
    end;
    for I := 0 to Block.BlockCount - 1 do
      GetTranslatableStringsOf(Block.Blocks[I]);
  end;

begin
  if Assigned(MainBlock) then
    GetTranslatableStringsOf(MainBlock);
end;

function TPascalStringCollector.SetTranslatableStrings(Strings: TTextItems): String;

  procedure SortReferences(Block: TCodeBlock; SortedList: TList);
  var
    I, J, Index: Integer;
    Reference: TStringValueReference;
  begin
    for I := 0 to Block.StringCount - 1 do
      for J := 0 to Block.Strings[I].ReferenceCount - 1 do
      begin
        Reference := Block.Strings[I].References[J];
        Index := 0;
        while Index < SortedList.Count do
        begin
          if TStringValueReference(SortedList[Index]).Pos < Reference.Pos then
            Break;
          Inc(Index);
        end;
        SortedList.Insert(Index, Reference);
      end;
    for I := 0 to Block.BlockCount - 1 do
      SortReferences(Block.Blocks[I], SortedList);
  end;

var
  I: Integer;
  SortedList: TList;
  Item: TTextItem;
  StrRef: TStringValueReference;
begin
  Result := Source;
  if Assigned(MainBlock) then
  begin
    SortedList := TList.Create;
    try
      SortReferences(MainBlock, SortedList);
      for I := 0 to SortedList.Count - 1 do
      begin
        StrRef := TStringValueReference(SortedList[I]);
        if StrRef.IsTranslatable then
        begin
          Item := Strings.Find(StrRef.StrValue.Key);
          if not StrRef.IsTranslated and Assigned(Item) then
            EnableTranslation(Result, StrRef)
          else if StrRef.IsTranslated and not Assigned(Item) then
            DisableTranslation(Result, StrRef);
        end;
      end;
    finally
      SortedList.Free;
    end;
  end;
end;

end.
