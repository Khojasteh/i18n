{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit implements a class to evaluate expressions in C language syntax.
unit i18nCExpr;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, Classes, Types;

type

  {$region 'xmldoc'}
  /// <summary>
  /// ECExpressionError is the exception class for problems that occur while parsing
  /// a expression in C language syntax.</summary>
  /// <remarks>
  /// ECExpressionError exception occurs when an expression in C language syntax has
  /// a syntax error.</remarks>
  /// <seealso cref="TCExpression"/>
  {$endregion}
  ECExpressionError = class(Exception)
  private
    fExpression: String;
    fOffset: Integer;
    fToken: String;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Sets the additional information about the error.</summary>
    /// <param name="AExpression">
    /// The expression that caused the exception to occur.</param>
    /// <param name="AOffset">
    /// The offset from the start of the expression where the parser encountered
    /// the problem.</param>
    /// <param name="AToken">
    /// The last token that the parser was processing when the problem occured.</param>
    {$endregion}
    procedure SetErrorDetails(const AExpression: String; AOffset: Integer; const AToken: String);
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the expression that caused the execption to occur.</summary>
    {$endregion}
    property Expression: String read fExpression;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the offset from the start of the expression where the parser encountered
    /// the problem.</summary>
    {$endregion}
    property Offset: Integer read fOffset;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the last token that the parser was processing when the problem occured.</summary>
    {$endregion}
    property Token: String read fToken;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCExprVar represents an integer variable in an expression.</summary>
  /// <seealso cref="TCExpression"/>
  {$endregion}
  TCExprVar = class(TObject)
  private
    fName: String;
    fValue: Integer;
    fRefCount: Integer;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Increments the reference count of the variable.</summary>
    /// <returns>
    /// The object itself.</returns>
    /// <seealso cref="RefCount"/>
    {$endregion}
    function AddRef: TCExprVar; inline;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for a specified variable name.</summary>
    /// <param name="AName">
    /// The name of the variable.</param>
    {$endregion}
    constructor Create(const AName: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the name of the variable.</summary>
    {$endregion}
    property Name: String read fName;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of times that the variable is referenced in the expression.</summary>
    {$endregion}
    property RefCount: Integer read fRefCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets or sets value of the variable.</summary>
    {$endregion}
    property Value: Integer read fValue write fValue;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCExprNode is the base class for sub-expressions of an expression.</summary>
  {$endregion}
  TCExprNode = class abstract(TObject)
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Evaluates the value of the sub-expression.</summary>
    /// <returns>
    /// The value of the sub-expression after evaluation.</returns>
    /// <seealso cref="Value"/>
    {$endregion}
    function GetValue: Integer; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the sub-expression as a string.</summary>
    /// <returns>
    /// The sub-expression as string.</returns>
    /// <seealso cref="AsString"/>
    {$endregion}
    function GetAsString: String; virtual; abstract;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the value of sub-expression depends on the value of one
    /// or more variables.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the value of sub-expression depends on
    /// variable. Otherwise, returns <see langword="false"/>.</returns>
    {$endregion}
    function IsVariable: Boolean; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the sub-expression as a string.</summary>
    {$endregion}
    property AsString: String read GetAsString;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets The value of the sub-expression after evaluation.</summary>
    {$endregion}
    property Value: Integer read GetValue;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCExprVarNode represents a reference to an integer variable in an expression.</summary>
  {$endregion}
  TCExprVarNode = class(TCExprNode)
  private
    fVariable: TCExprVar;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Evaluates the value of the sub-expression.</summary>
    /// <returns>
    /// The value of the variable.</returns>
    /// <seealso cref="Value"/>
    {$endregion}
    function GetValue: Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the sub-expression as a string.</summary>
    /// <returns>
    /// The name of the variable.</returns>
    /// <seealso cref="AsString"/>
    {$endregion}
    function GetAsString: String; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for a specified variable.</summary>
    /// <param name="AVariable">
    /// The variable that this instance refrences it.</param>
    {$endregion}
    constructor Create(const AVariable: TCExprVar);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the value of sub-expression depends on the value of one
    /// or more variables.</summary>
    /// <returns>
    /// Returns always <see langword="true"/>.</returns>
    {$endregion}
    function IsVariable: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the variable which this sub-expression is a reference to it.</summary>
    {$endregion}
    property Variable: TCExprVar read fVariable;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCExprVarNode represents an integer constant in an expression.</summary>
  {$endregion}
  TCExprConstNode = class(TCExprNode)
  private
    fValue: Integer;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Evaluates the value of the sub-expression.</summary>
    /// <returns>
    /// The value of the constant.</returns>
    /// <seealso cref="Value"/>
    {$endregion}
    function GetValue: Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the sub-expression as a string.</summary>
    /// <returns>
    /// The value of constant as a string.</returns>
    /// <seealso cref="AsString"/>
    {$endregion}
    function GetAsString: String; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class for a specified integer value.</summary>
    /// <param name="AValue">
    /// The value of constant that this instance represents it.</param>
    {$endregion}
    constructor Create(AValue: Integer);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the value of sub-expression depends on the value of one
    /// or more variables.</summary>
    /// <returns>
    /// Returns always <see langword="false"/>.</returns>
    {$endregion}
    function IsVariable: Boolean; override;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the unary operators in a C language expression.</summary>
  {$endregion}
  TCUnaryOp = (
    {$region 'xmldoc'}
    /// unary plus (+)
    {$endregion}
    opNone,
    {$region 'xmldoc'}
    /// unary minus (-)
    {$endregion}
    opNegate,
    {$region 'xmldoc'}
    /// logical not (!)
    {$endregion}
    opLogNot,
    {$region 'xmldoc'}
    /// bitwise not (~)
    {$endregion}
    opBitNot
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TCExprUnaryNode represents a unary operation in an expression.</summary>
  {$endregion}
  TCExprUnaryNode = class(TCExprNode)
  private
    fOp: TCUnaryOp;
    fOperand: TCExprNode;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Evaluates the value of the sub-expression.</summary>
    /// <returns>
    /// The value of the sub-expression after evaluation.</returns>
    /// <seealso cref="Value"/>
    {$endregion}
    function GetValue: Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the sub-expression as a string.</summary>
    /// <returns>
    /// The sub-expression as string.</returns>
    /// <seealso cref="AsString"/>
    {$endregion}
    function GetAsString: String; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.</summary>
    /// <param name="AOp">
    /// The unary operator.</param>
    /// <param name="AOperand">
    /// The operand of the unary operation.</param>
    {$endregion}
    constructor Create(AOp: TCUnaryOp; AOperand: TCExprNode);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the value of sub-expression depends on the value of one
    /// or more variables.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the value of sub-expression depends on
    /// variable. Otherwise, returns <see langword="false"/>.</returns>
    {$endregion}
    function IsVariable: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the operator of the unary operation.</summary>
    {$endregion}
    property Op: TCUnaryOp read fOp;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the operand of the unary operation.</summary>
    {$endregion}
    property Operand: TCExprNode read fOperand;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the binary operators in a C language expression.</summary>
  {$endregion}
  TCBinaryOp = (
    {$region 'xmldoc'}
    /// addition (+)
    {$endregion}
    opAdd,
    {$region 'xmldoc'}
    /// subtraction (-)
    {$endregion}
    opSubtract,
    {$region 'xmldoc'}
    /// multiplication (*)
    {$endregion}
    opMultiply,
    {$region 'xmldoc'}
    /// integer division (/)
    {$endregion}
    opDivide,
    {$region 'xmldoc'}
    /// reminder of integer devision (%)
    {$endregion}
    opModulus,
    {$region 'xmldoc'}
    /// bitwise and (&amp;)
    {$endregion}
    opBitAnd,
    {$region 'xmldoc'}
    /// bitwise or (|)
    {$endregion}
    opBitOr,
    {$region 'xmldoc'}
    /// bitwise xor (^)
    {$endregion}
    opBitXor,
    {$region 'xmldoc'}
    /// shift left (&lt;&lt;)
    {$endregion}
    opShiftLeft,
    {$region 'xmldoc'}
    /// shift right (&gt;&gt;)
    {$endregion}
    opShiftRight,
    {$region 'xmldoc'}
    /// equality (==)
    {$endregion}
    opEqual,
    {$region 'xmldoc'}
    /// not eqaul (!=)
    {$endregion}
    opNotEqual,
    {$region 'xmldoc'}
    /// greater than (&gt;)
    {$endregion}
    opGreaterThan,
    {$region 'xmldoc'}
    /// greater than or equal (&gt;=)
    {$endregion}
    opGreaterThanOrEqual,
    {$region 'xmldoc'}
    /// less than (&lt;)
    {$endregion}
    opLessThan,
    {$region 'xmldoc'}
    /// less than or equal (&lt;=)
    {$endregion}
    opLessThanOrEqual,
    {$region 'xmldoc'}
    /// logical and (&amp;&amp;)
    {$endregion}
    opLogAnd,
    {$region 'xmldoc'}
    /// logical or (||)
    {$endregion}
    opLogOr
  );

  {$region 'xmldoc'}
  /// <summary>
  /// TCExprBinaryNode represents a binary operation in an expression.</summary>
  {$endregion}
  TCExprBinaryNode = class(TCExprNode)
  private
    fOp: TCBinaryOp;
    fOperandLeft: TCExprNode;
    fOperandRight: TCExprNode;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Evaluates the value of the sub-expression.</summary>
    /// <returns>
    /// The value of the sub-expression after evaluation.</returns>
    /// <seealso cref="Value"/>
    {$endregion}
    function GetValue: Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the sub-expression as a string.</summary>
    /// <returns>
    /// The sub-expression as string.</returns>
    /// <seealso cref="AsString"/>
    {$endregion}
    function GetAsString: String; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.</summary>
    /// <param name="AOp">
    /// The binary operator.</param>
    /// <param name="AOperandLeft">
    /// The left operand of the binary operation.</param>
    /// <param name="AOperandRight">
    /// The right operand of the binary operation.</param>
    {$endregion}
    constructor Create(AOp: TCBinaryOp; AOperandLeft, AOperandRight: TCExprNode);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the value of sub-expression depends on the value of one
    /// or more variables.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the value of sub-expression depends on
    /// variable. Otherwise, returns <see langword="false"/>.</returns>
    {$endregion}
    function IsVariable: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the operator of the binary operation.</summary>
    {$endregion}
    property Op: TCBinaryOp read fOp;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the left operand of the binary operation.</summary>
    {$endregion}
    property OperandLeft: TCExprNode read fOperandLeft;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the right operand of the binary operation.</summary>
    {$endregion}
    property OperandRight: TCExprNode read fOperandRight;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCExprBinaryNode represents a ternary conditional operation in an expression.</summary>
  {$endregion}
  TCExprTernaryNode = class(TCExprNode)
  private
    fCondition: TCExprNode;
    fOperandTrue: TCExprNode;
    fOperandFalse: TCExprNode;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Evaluates the value of the sub-expression.</summary>
    /// <returns>
    /// The value of the sub-expression after evaluation.</returns>
    /// <seealso cref="Value"/>
    {$endregion}
    function GetValue: Integer; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the sub-expression as a string.</summary>
    /// <returns>
    /// The sub-expression as string.</returns>
    /// <seealso cref="AsString"/>
    {$endregion}
    function GetAsString: String; override;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.</summary>
    /// <param name="ACondition">
    /// The condition operand.</param>
    /// <param name="AOperandTrue">
    /// The operand that evaluates when the condition evaluates to true.</param>
    /// <param name="AOperandFalse">
    /// The operand that evaluates when the condition evaluates to false.</param>
    {$endregion}
    constructor Create(ACondition, AOperandTrue, AOperandFalse: TCExprNode);
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the value of sub-expression depends on the value of one
    /// or more variables.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the value of sub-expression depends on
    /// variable. Otherwise, returns <see langword="false"/>.</returns>
    {$endregion}
    function IsVariable: Boolean; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the condition operand of the ternary operation.</summary>
    {$endregion}
    property Condition: TCExprNode read fCondition;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the true phrase operand of the ternary operation.</summary>
    {$endregion}
    property OperandTrue: TCExprNode read fOperandTrue;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the false phrase operand of the ternary operation.</summary>
    {$endregion}
    property OperandFalse: TCExprNode read fOperandFalse;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// TCExpression is parser and evaluator for expressions in C language syntax.</summary>
  /// <remarks>
  /// TCExpression parses and evaluates an expression (specified as a string) which is using
  /// the C language syntax. Exceptions are that numbers must be decimal and no assignment,
  /// incerement/decrement, or comma operators are allowed. The expression can use user-defined
  /// variables.
  ///
  /// TCExpression supports comparison and logical operations as well as arithmetic
  /// and bitwaise ones. The comparison and logical operations always evaluate to
  /// either zero (false) or one (true).
  ///
  /// TCExpression parses the expression once to build the parse tree. Then, each time
  /// you call the <see cref="Evaluate"/> method, it uses the parse tree and the value
  /// of the variables listed by the <see cref="Vars"/> property to calculate the result
  /// of the expression. For this reason, TCExpression is very usful and suitable when
  /// you have one expression that will be evaluated many times with different variable
  /// values.
  ///
  /// The following table lists C operators that are supported by TCExpression. The
  /// operators are listed in order of precedence (highest to lowest). Their associativity
  /// indicates in what order operators of equal precedence in an expression are applied.
  ///
  /// <list type="table">
  ///   <listheader>
  ///     <term>Operator</term>
  ///     <description>Description</description>
  ///     <description>Associativity</description>
  ///   </listheader>
  ///   <item>
  ///     <term>+ - ! ~</term>
  ///     <description>Unary plus &bull; Unary minus &bull; Logical negation &bull;
  ///     Bitwise complement</description>
  ///     <description>right-to-left</description>
  ///   </item>
  ///   <item>
  ///     <term>* / %</term>
  ///     <description>Multiplication &bull; Division &bull; Modulus</description>
  ///     <description>left-to-right</description>
  ///   </item>
  ///   <item>
  ///     <term>+ -</term>
  ///     <description>Addition &bull; Subtraction</description>
  ///     <description>left-to-right</description>
  ///   </item>
  ///   <item>
  ///     <term>&lt;&lt; &gt;&gt;</term>
  ///     <description>Bitwise shift left &bull; Bitwise shift right</description>
  ///     <description>left-to-right</description>
  ///   </item>
  ///   <item>
  ///     <term>&lt; &lt;= &gt; &gt;=</term>
  ///     <description>Relational less than &bull; less than or equal to &bull;
  ///     greater than &bull; greater than or equal to</description>
  ///     <description>left-to-right</description>
  ///   </item>
  ///   <item>
  ///     <term>== !=</term>
  ///     <description>Relational is equal to &bull; is not equal to</description>
  ///     <description>left-to-right</description>
  ///   </item>
  ///   <item>
  ///     <term>&amp;</term>
  ///     <description>Bitwise AND</description>
  ///     <description>left-to-right</description>
  ///   </item>
  ///   <item>
  ///     <term>^</term>
  ///     <description>Bitwise exclusive OR</description>
  ///     <description>left-to-right</description>
  ///   </item>
  ///   <item>
  ///     <term>|</term>
  ///     <description>Bitwise inclusive OR</description>
  ///     <description>left-to-right</description>
  ///   </item>
  ///   <item>
  ///     <term>&amp;&amp;</term>
  ///     <description>Logical AND</description>
  ///     <description>left-to-right</description>
  ///   </item>
  ///   <item>
  ///     <term>||</term>
  ///     <description>Logical OR</description>
  ///     <description>left-to-right</description>
  ///   </item>
  ///   <item>
  ///     <term>?:</term>
  ///     <description>Ternary conditional</description>
  ///     <description>left-to-right</description>
  ///   </item>
  /// </list>
  ///
  /// Parenthesis are also used to group sub-expressions to force a different
  /// precedence; such parenthetical expressions can be nested and are evaluated
  /// from inner to outer.</remarks>
  {$endregion}
  TCExpression = class(TObject)
  private
    fSource: String;
    fVars: TStringList;
    Expr: TCExprNode;
    function GetFormattedSource: String; inline;
    function GetVarCount: Integer; inline;
    function GetVarNames(Index: Integer): String; inline;
    function GetVars(const Name: String): TCExprVar;
    function Parse(const Expression: String): TCExprNode;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the expression evaluator for a specified expression in
    /// C language syntax.</summary>
    /// <param name="Expression">
    /// The expression to parse and evaluate.</param>
    /// <exception cref="ECExpressionError">
    /// Occurs when the expression has a syntax error.</exception>
    {$endregion}
    constructor Create(const Expression: String);
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the instance and releases its allocated memory.</summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Evaluates the expression for the variables specified by the <see cref="Vars"/>
    /// property.</summary>
    /// <returns>
    /// Returns the result of the expression.</returns>
    {$endregion}
    function Evaluate: Integer; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether the value of expression depends on value of any variables.</summary>
    /// <returns>
    /// Returns <see langword="true"/> if the value of the expression depends on
    /// one or more variables. Otherwise, returns <see langword="false"/>.</returns>
    {$endregion}
    function IsVariable: Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the source expression.</summary>
    /// <seealso cref="FormattedSource"/>
    {$endregion}
    property Source: String read fSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the source expression as formatted.</summary>
    /// <seealso cref="Source"/>
    {$endregion}
    property FormattedSource: String read GetFormattedSource;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the numnber of variables in the expression.</summary>
    {$endregion}
    property VarCount: Integer read GetVarCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the name of variables in the expression.</summary>
    {$endregion}
    property VarNames[Index: Integer]: String read GetVarNames;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the variables in the expression.</summary>
    /// <remarks>
    /// Use the Vars property to get or set values of variables in the expression.
    ///
    /// TCExpression does not prevent you to set value for variables that are not used by
    /// the expression. This helps you to manage the variables within your code independent
    /// of the expression being used. You can examine the value of <see cref="TCExprVar.RefCount"/>
    /// property to discover whether a veriable is referenced in the expression or not.</remarks>
    /// <seealso cref="TCExprVar"/>
    {$endregion}
    property Vars[const Name: String]: TCExprVar read GetVars;
  end;

implementation

resourcestring
  SUnexpectedEndOfExpressionError = 'Unexpected end of expression';
  SUnexpectedSymbolError = 'Unexpected symbol ''%s''';
  SMissingExpectedSymbolError = 'Missing symbol ''%s''';

const
  UnaryOpSymbols: array[TCUnaryOp] of String = ('+', '-', '!', '~');
  BinaryOpSymbols: array[TCBinaryOp] of String = ('+', '-', '*', '/', '%',
    '&', '|', '^', '<<', '>>', '==', '!=', '>', '>=', '<', '<=', '&&', '||');

{ ECExpressionError }

procedure ECExpressionError.SetErrorDetails(const AExpression: String;
  AOffset: Integer; const AToken: String);
begin
  fExpression := AExpression;
  fOffset := AOffset;
  fToken := AToken;
end;

{ TCExprVar }

constructor TCExprVar.Create(const AName: String);
begin
  fName := AName;
end;

function TCExprVar.AddRef: TCExprVar;
begin
  Inc(fRefCount);
  Result := Self;
end;

{ TCExprVarNode }

constructor TCExprVarNode.Create(const AVariable: TCExprVar);
begin
  fVariable := AVariable.AddRef;
end;

function TCExprVarNode.GetAsString: String;
begin
  Result := Variable.Name;
end;

function TCExprVarNode.GetValue: Integer;
begin
  Result := Variable.Value;
end;

function TCExprVarNode.IsVariable: Boolean;
begin
  Result := True;
end;

{ TCExprConstNode }

constructor TCExprConstNode.Create(AValue: Integer);
begin
  fValue := AValue;
end;

function TCExprConstNode.GetAsString: String;
begin
  Result := IntToStr(Value)
end;

function TCExprConstNode.GetValue: Integer;
begin
  Result := fValue;
end;

function TCExprConstNode.IsVariable: Boolean;
begin
  Result := False;
end;

{ TCExprUnaryNode }

constructor TCExprUnaryNode.Create(AOp: TCUnaryOp; AOperand: TCExprNode);
begin
  fOp := AOp;
  fOperand := AOperand;
end;

destructor TCExprUnaryNode.Destroy;
begin
  fOperand.Free;
  inherited Destroy;
end;

function TCExprUnaryNode.GetAsString: String;

  function ToString(Operand: TCExprNode): String;
  begin
    Result := Operand.AsString;
    if (Operand is TCExprUnaryNode) or
       (Operand is TCExprBinaryNode) or
       (Operand is TCExprTernaryNode)
    then
      Result := '(' + Result + ')';
  end;

begin
  Result := UnaryOpSymbols[Op] + ToString(Operand);
end;

function TCExprUnaryNode.GetValue: Integer;
begin
  case Op of
    opNegate:
      Result := -Operand.Value;
    opBitNot:
      Result := not Operand.Value;
    opLogNot:
      Result := Ord(Operand.Value = 0);
  else
    Result := Operand.Value
  end;
end;

function TCExprUnaryNode.IsVariable: Boolean;
begin
  Result := Operand.IsVariable;
end;

{ TCExprBinaryNode }

constructor TCExprBinaryNode.Create(AOp: TCBinaryOp; AOperandLeft,
  AOperandRight: TCExprNode);
begin
  fOp := AOp;
  fOperandLeft := AOperandLeft;
  fOperandRight := AOperandRight;
end;

destructor TCExprBinaryNode.Destroy;
begin
  fOperandLeft.Free;
  fOperandRight.Free;
  inherited Destroy;
end;

function TCExprBinaryNode.GetAsString: String;

  function ToString(Operand: TCExprNode): String;
  begin
    Result := Operand.AsString;
    if (Operand is TCExprTernaryNode) or ((Operand is TCExprBinaryNode) and
      ((TCExprBinaryNode(Operand).Op <> Op) or not (Op in [opMultiply, opAdd, opSubtract, opBitAnd, opBitOr, opBitXor, opLogAnd, opLogOr])) and
      (not (TCExprBinaryNode(Operand).Op in [opAdd, opSubtract]) or not (Op in [opAdd, opSubtract])))
    then
      Result := '(' + Result + ')';
  end;

begin
  Result := ToString(OperandLeft) + ' ' + BinaryOpSymbols[Op] + ' ' + ToString(OperandRight);
end;

function TCExprBinaryNode.GetValue: Integer;
begin
  case Op of
    opAdd:
      Result := OperandLeft.Value + OperandRight.Value;
    opSubtract:
      Result := OperandLeft.Value - OperandRight.Value;
    opMultiply:
      Result := OperandLeft.Value * OperandRight.Value;
    opDivide:
      Result := OperandLeft.Value div OperandRight.Value;
    opModulus:
      Result := OperandLeft.Value mod OperandRight.Value;
    opBitAnd:
      Result := OperandLeft.Value and OperandRight.Value;
    opBitOr:
      Result := OperandLeft.Value or OperandRight.Value;
    opBitXor:
      Result := OperandLeft.Value xor OperandRight.Value;
    opShiftLeft:
      Result := OperandLeft.Value shl OperandRight.Value;
    opShiftRight:
      Result := OperandLeft.Value shr OperandRight.Value;
    opEqual:
      Result := Ord(OperandLeft.Value = OperandRight.Value);
    opNotEqual:
      Result := Ord(OperandLeft.Value <> OperandRight.Value);
    opGreaterThan:
      Result := Ord(OperandLeft.Value > OperandRight.Value);
    opGreaterThanOrEqual:
      Result := Ord(OperandLeft.Value >= OperandRight.Value);
    opLessThan:
      Result := Ord(OperandLeft.Value < OperandRight.Value);
    opLessThanOrEqual:
      Result := Ord(OperandLeft.Value <= OperandRight.Value);
    opLogAnd:
    begin
      Result := Ord(OperandLeft.Value <> 0);
      if Result <> 0 then
        Result := Ord(OperandRight.Value <> 0);
    end;
    opLogOr:
    begin
      Result := Ord(OperandLeft.Value <> 0);
      if Result = 0 then
        Result := Ord(OperandRight.Value <> 0);
    end
  else
    Result := 0; // To prevent compiler warning; code never reaches here.
  end;
end;

function TCExprBinaryNode.IsVariable: Boolean;
begin
  Result := OperandLeft.IsVariable or OperandRight.IsVariable;
end;

{ TCExprTernaryNode }

constructor TCExprTernaryNode.Create(ACondition, AOperandTrue, AOperandFalse: TCExprNode);
begin
  fCondition := ACondition;
  fOperandTrue := AOperandTrue;
  fOperandFalse := AOperandFalse;
end;

destructor TCExprTernaryNode.Destroy;
begin
  fCondition.Free;
  fOperandTrue.Free;
  fOperandFalse.Free;
  inherited Destroy;
end;

function TCExprTernaryNode.GetAsString: String;

  function ToString(Operand: TCExprNode): String;
  begin
    Result := Operand.AsString;
    if (Operand is TCExprBinaryNode) or (Operand is TCExprTernaryNode) then
      Result := '(' + Result + ')';
  end;

begin
  Result := ToString(Condition) + ' ? ' + ToString(OperandTrue) + ' : ' + ToString(OperandFalse);
end;

function TCExprTernaryNode.GetValue: Integer;
begin
  if Condition.Value <> 0 then
    Result := OperandTrue.Value
  else
    Result := OperandFalse.Value;
end;

function TCExprTernaryNode.IsVariable: Boolean;
begin
  if not Condition.IsVariable then
    if Condition.Value <> 0 then
      Result := OperandTrue.IsVariable
    else
      Result := OperandFalse.IsVariable
  else
    Result := True;
end;

{ TCExpression }

constructor TCExpression.Create(const Expression: String);
begin
  fSource := Expression;
  fVars := TStringList.Create;
  fVars.CaseSensitive := False;
  fVars.OwnsObjects := True;
  fVars.Sorted := True;
  fVars.Duplicates := dupIgnore;
  Expr := Parse(Expression);
end;

destructor TCExpression.Destroy;
begin
  if Assigned(Expr) then
    Expr.Free;
  fVars.Free;
  inherited Destroy;
end;

function TCExpression.GetFormattedSource: String;
begin
  Result := Expr.AsString;
end;

function TCExpression.GetVarCount: Integer;
begin
  Result := fVars.Count;
end;

function TCExpression.GetVarNames(Index: Integer): String;
begin
  Result := fVars[Index];
end;

function TCExpression.GetVars(const Name: String): TCExprVar;
var
  Index: Integer;
begin
  Index := fVars.Add(Name);
  Result := TCExprVar(fVars.Objects[Index]);
  if Result = nil then
  begin
    Result := TCExprVar.Create(Name);
    fVars.Objects[Index] := Result;
  end;
end;

function TCExpression.Evaluate: Integer;
begin
  Result := Expr.Value;
end;

function TCExpression.IsVariable: Boolean;
begin
  Result := Expr.IsVariable;
end;

function TCExpression.Parse(const Expression: String): TCExprNode;

  type
    TCExprToken = (ceNone, ceIdentifier, ceNumber, ceSymbol);
    TBinaryOperations = set of TCBinaryOp;

  const
    BinaryOpPrecedence: array[1..10] of TBinaryOperations = (
      [opMultiply, opDivide, opModulus],
      [opAdd, opSubtract],
      [opShiftLeft, opShiftRight],
      [opGreaterThan, opGreaterThanOrEqual, opLessThan, opLessThanOrEqual],
      [opEqual, opNotEqual],
      [opBitAnd],
      [opBitXor],
      [opBitOr],
      [opLogAnd],
      [opLogOr]);

  var
    S: PChar;
    Token: String;
    TokenID: TCExprToken;

  procedure NextToken;
  var
    P: PChar;
  begin
    while CharInSet(S^, [' ', ^I, #10, #13])  do
      Inc(S);
    P := S;
    case S^ of
      #0:
      begin
        Token := '';
        TokenID := ceNone;
      end;
      '0'..'9':
      begin
        repeat Inc(S) until not CharInSet(S^, ['0'..'9']);
        SetString(Token, P, S - P);
        TokenID := ceNumber;
      end;
      '_', 'A'..'Z', 'a'..'z':
      begin
        repeat Inc(S) until not CharInSet(S^, ['_', 'A'..'Z', 'a'..'z', '0'..'9']);
        SetString(Token, P, S - P);
        TokenID := ceIdentifier;
      end;
    else
      Inc(S);
      case P^ of
        '<': if CharInSet(S^, ['<', '=']) then Inc(S);
        '>': if CharInSet(S^, ['>', '=']) then Inc(S);
        '=': if S^ = '=' then Inc(S);
        '!': if S^ = '=' then Inc(S);
        '&': if S^ = '&' then Inc(S);
        '|': if S^ = '|' then Inc(S);
      end;
      SetString(Token, P, S - P);
      TokenID := ceSymbol;
    end;
  end;

  function IsTokenUnaryOp(out UnaryOp: TCUnaryOp): Boolean;
  var
    Op: TCUnaryOp;
  begin
    if TokenID = ceSymbol then
      for Op := Low(UnaryOpSymbols) to High(UnaryOpSymbols) do
        if UnaryOpSymbols[Op] = Token then
        begin
          UnaryOp := Op;
          Result := True;
          Exit;
        end;
    Result := False;
  end;

  function IsTokenBinaryOp(out BinaryOp: TCBinaryOp): Boolean;
  var
    Op: TCBinaryOp;
  begin
    if TokenID = ceSymbol then
      for Op := Low(BinaryOpSymbols) to High(BinaryOpSymbols) do
        if BinaryOpSymbols[Op] = Token then
        begin
          BinaryOp := Op;
          Result := True;
          Exit;
        end;
    Result := False;
  end;

  function ParseExpression: TCExprNode; forward;

  function ParseFactor: TCExprNode;
  var
    UnaryOp, Temp: TCUnaryOp;
    Value: Integer;
  begin
    if IsTokenUnaryOp(UnaryOp) then
      NextToken
    else
      UnaryOp := opNone;
    case TokenID of
      ceIdentifier:
      begin
        Result := TCExprVarNode.Create(Vars[Token]);
        NextToken;
      end;
      ceNumber:
      begin
        Result := TCExprConstNode.Create(StrToInt(Token));
        NextToken;
      end;
      ceSymbol:
      begin
        if Token = '(' then
        begin
          NextToken;
          Result := ParseExpression;
          if Token <> ')' then
          begin
            Result.Free;
            raise ECExpressionError.CreateResFmt(@SMissingExpectedSymbolError, [')']);
          end;
          NextToken;
        end
        else if IsTokenUnaryOp(Temp) then
          Result := ParseFactor
        else
          raise ECExpressionError.CreateResFmt(@SUnexpectedSymbolError, [Token]);
      end;
    else
      raise ECExpressionError.CreateRes(@SUnexpectedEndOfExpressionError);
    end;
    if UnaryOp <> opNone then
    begin
      Result := TCExprUnaryNode.Create(UnaryOp, Result);
      if not Result.IsVariable then
      begin
        try
          Value := Result.Value;
        finally
          Result.Free;
        end;
        Result := TCExprConstNode.Create(Value);
      end;
    end;
  end;

  function ParseTerm(Level: Integer = High(BinaryOpPrecedence)): TCExprNode;
  var
    NextOperand: TCExprNode;
    BinaryOp: TCBinaryOp;
    Value: Integer;
  begin
    if Level = Low(BinaryOpPrecedence) then
      Result := ParseFactor
    else
      Result := ParseTerm(Level - 1);
    while IsTokenBinaryOp(BinaryOp) and (BinaryOp in BinaryOpPrecedence[Level]) do
    begin
      NextToken;
      try
        if Level = Low(BinaryOpPrecedence) then
          NextOperand := ParseFactor
        else
          NextOperand := ParseTerm(Level - 1);
      except
        Result.Free;
        raise;
      end;
      Result := TCExprBinaryNode.Create(BinaryOp, Result, NextOperand);
      if not Result.IsVariable then
      begin
        try
          Value := Result.Value;
        finally
          Result.Free;
        end;
        Result := TCExprConstNode.Create(Value);
      end;
    end;
  end;

  function ParseExpression: TCExprNode;
  var
    OperandTrue, OperandFalse: TCExprNode;
  begin
    Result := ParseTerm;
    if Token = '?' then
    begin
      NextToken;
      try
        OperandTrue := ParseExpression;
      except;
        Result.Free;
        raise;
      end;
      if Token <> ':' then
      begin
        Result.Free;
        raise ECExpressionError.CreateResFmt(@SMissingExpectedSymbolError, [':']);
      end;
      NextToken;
      try
        OperandFalse := ParseExpression;
      except
        Result.Free;
        OperandTrue.Free;
        raise;
      end;
      if Result.IsVariable then
        Result := TCExprTernaryNode.Create(Result, OperandTrue, OperandFalse)
      else
      begin
        if Result.Value <> 0 then
        begin
          Result.Free;
          Result := OperandTrue;
          OperandFalse.Free;
        end
        else
        begin
          Result.Free;
          Result := OperandFalse;
          OperandTrue.Free;
        end;
      end;
    end;
  end;

var
  Error: ECExpressionError;
begin
  S := PChar(Expression);
  NextToken;
  try
    Result := ParseExpression;
    if TokenID <> ceNone then
    begin
      Result.Free;
      raise ECExpressionError.CreateResFmt(@SUnexpectedSymbolError, [Token]);
    end;
  except
    on E: ECExpressionError do
    begin
      E.SetErrorDetails(Expression, S - PChar(Expression), Token);
      raise;
    end;
    on E: Exception do
    begin
      Error := ECExpressionError.Create(E.Message);
      Error.SetErrorDetails(Expression, S - PChar(Expression), Token);
      raise Error;
    end;
  end;
end;

end.

