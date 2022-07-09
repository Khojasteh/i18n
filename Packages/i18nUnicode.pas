{------------------------------------------------------------------------------}
{                                                                              }
{ i18n Package                                                                 }
{ by Kambiz R. Khojasteh                                                       }
{                                                                              }
{ kambiz@delphiarea.com                                                        }
{ http://www.delphiarea.com                                                    }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit defines some functions, data types, and constants regarding the
/// Unicode characters.
unit i18nUnicode;

{$I DELPHIAREA.INC}

interface

const

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Left-to-right mark (LMR)</summary>
  /// <remarks>
  /// Acts exactly like left-to-right characters, except that it is not displayed.
  /// LRM does not have any other semantic effect.</remarks>
  {$endregion}
  UCC_LMR = #$200E;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Right-to-left mark (RLM)</summary>
  /// <remarks>
  /// Acts exactly like right-to-left characters, except that it is not displayed.
  /// RLM does not have any other semantic effect.</remarks>
  {$endregion}
  UCC_RLM = #$200F;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Zero width joiner (ZWJ)</summary>
  /// <remarks>
  /// Prevents the joining of consecutive characters on output.</remarks>
  {$endregion}
  UCC_ZWJ = #$200D;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Zero width non-joiner (ZWNJ)</summary>
  /// <remarks>
  /// Adds a non-joiner between two characters to prevent them from forming a
  /// cursive connection with each other when rendered.</remarks>
  {$endregion}
  UCC_ZWNJ = #$200C;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Start of left-to-right embedding (LRE)</summary>
  /// <remarks>
  /// Signals that a piece of text is to be treated as embedded left-to-right.
  /// For example, an English quotation in the middle of an Arabic sentence could
  /// be marked as being embedded left-to-right text.
  /// NOTE: LRE affects word order, not character order.</remarks>
  {$endregion}
  UCC_LRE = #$202A;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Start of right-to-left embedding (RLE)</summary>
  /// <remarks>
  /// Signals that a piece of text is to be treated as embedded right-to-left.
  /// For example, a Hebrew phrase in the middle of an English quotation could be
  /// marked as being embedded right-to-left.
  /// NOTE: RLE affects word order, not character order.</remarks>
  {$endregion}
  UCC_RLE = #$202B;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Start of left-to-right override (LRO)</summary>
  /// <remarks>
  /// Overrides bidirectional character types when required for special cases, such
  /// as for part numbers. LRO forces characters to be treated as strong left-to-right
  /// characters.</remarks>
  {$endregion}
  UCC_LRO = #$202D;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Start of right-to-left override (RLO)</summary>
  /// <remarks>
  /// Overrides bidirectional character types when required for special cases, such
  /// as for part numbers. RLO forces characters to be treated as strong right-to-left
  /// characters.</remarks>
  {$endregion}
  UCC_RLO = #$202E;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Pop directional formatting (PDF)</summary>
  /// <remarks>
  /// Terminates the effects of the last explicit code (either embedding or override)
  /// and restores the bidirectional state to what it was before the last LRE, RLE,
  /// RLO, or LRO control characters.</remarks>
  {$endregion}
  UCC_PDF = #$202C;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: National digit shapes substitution (NADS)</summary>
  /// <remarks>
  /// Uses national digit shapes to display the U+0030-U+0039 (ASCII numerals). The
  /// national digit shapes are determined by the current user locale.</remarks>
  {$endregion}
  UCC_NADS = #$206E;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Nominal (European) digit shapes (NODS)</summary>
  /// <remarks>
  /// Uses the nominal digit shapes for the digits U+0030-U+0039 (ASCII digits).
  /// Nominal digit shapes are Western digits.</remarks>
  {$endregion}
  UCC_NODS = #$206F;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Activate symmetric swapping (ASS)</summary>
  /// <remarks>
  /// Indicates whether the term LEFT or RIGHT in pairs of character names, such
  /// as parentheses, should be interpreted as meaning opening or closing, respectively.
  /// The default state is activated.</remarks>
  {$endregion}
  UCC_ASS = #$206B;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Inhibit symmetric swapping (ISS)</summary>
  /// <remarks>
  /// Turns off the symmetric swapping of characters such as parenthesis, so that left
  /// and right continue to mean left and right facing, as opposed to opening and closing,
  /// when symmetric swapping is on.</remarks>
  {$endregion}
  UCC_ISS = #$206A;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Activate Arabic form shaping (AAFS)</summary>
  /// <remarks>
  /// Controls the shaping behavior of the Arabic compatibility characters. During the
  /// presentation process, certain letterforms may be joined together in cursive
  /// connection or ligatures. The shaping selector codes indicate that the character
  /// shape determination (glyph selection) process used to achieve this presentation
  /// effect is to be either activated or inhibited. The default state is inhibited.</remarks>
  {$endregion}
  UCC_AAFS = #$206D;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Inhibit Arabic form shaping (IAFS)</summary>
  /// <remarks>
  /// Inhibits the character shaping determination process, so that characters are not
  /// shaped based on position.</remarks>
  {$endregion}
  UCC_IAFS = #$206C;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Record Separator (Block separator) (RS)</summary>
  /// <remarks>
  /// Begins a new line after each line separator.</remarks>
  {$endregion}
  UCC_RS = #$241E;

  {$region 'xmldoc'}
  /// <summary>
  /// Unicode Control Character: Unit Separator (Segment separator) (US)</summary>
  /// <remarks>
  /// Unicode Control Character: Begins a new paragraph after each paragraph separator.</remarks>
  {$endregion}
  UCC_US = #$241F;

type

  {$region 'xmldoc'}
  /// <summary>
  /// This enumeration type identifies the different type of scripts.</summary>
  {$endregion}
  TUnicodeScript = (
    {$region 'xmldoc'}
    /// Arabic
    {$endregion}
    usArabic,
    {$region 'xmldoc'}
    /// Imperial Aramaic
    {$endregion}
    usImperialAramaic,
    {$region 'xmldoc'}
    /// Armenian
    {$endregion}
    usArmenian,
    {$region 'xmldoc'}
    /// Avestan
    {$endregion}
    usAvestan,
    {$region 'xmldoc'}
    /// Balinese
    {$endregion}
    usBalinese,
    {$region 'xmldoc'}
    /// Bamum
    {$endregion}
    usBamum,
    {$region 'xmldoc'}
    /// Bassa Vah
    {$endregion}
    usBassaVah,
    {$region 'xmldoc'}
    /// Batak
    {$endregion}
    usBatak,
    {$region 'xmldoc'}
    /// Bengali
    {$endregion}
    usBengali,
    {$region 'xmldoc'}
    /// Blissymbols
    {$endregion}
    usBlissymbols,
    {$region 'xmldoc'}
    /// Bopomofo
    {$endregion}
    usBopomofo,
    {$region 'xmldoc'}
    /// Brahmi
    {$endregion}
    usBrahmi,
    {$region 'xmldoc'}
    /// Braille
    {$endregion}
    usBraille,
    {$region 'xmldoc'}
    /// Buginese
    {$endregion}
    usBuginese,
    {$region 'xmldoc'}
    /// Buhid
    {$endregion}
    usBuhid,
    {$region 'xmldoc'}
    /// Chakma
    {$endregion}
    usChakma,
    {$region 'xmldoc'}
    /// Unified Canadian Aboriginal Syllabics
    {$endregion}
    usUnifiedCanadianAboriginalSyllabics,
    {$region 'xmldoc'}
    /// Carian
    {$endregion}
    usCarian,
    {$region 'xmldoc'}
    /// Cham
    {$endregion}
    usCham,
    {$region 'xmldoc'}
    /// Cherokee
    {$endregion}
    usCherokee,
    {$region 'xmldoc'}
    /// Cirth
    {$endregion}
    usCirth,
    {$region 'xmldoc'}
    /// Coptic
    {$endregion}
    usCoptic,
    {$region 'xmldoc'}
    /// Cypriot
    {$endregion}
    usCypriot,
    {$region 'xmldoc'}
    /// Cyrillic
    {$endregion}
    usCyrillic,
    {$region 'xmldoc'}
    /// Cyrillic (Old Church Slavonic variant)
    {$endregion}
    usCyrillic_OldChurchSlavonic,
    {$region 'xmldoc'}
    /// Devanagari (Nagari)
    {$endregion}
    usDevanagari,
    {$region 'xmldoc'}
    /// Deseret (Mormon)
    {$endregion}
    usDeseret,
    {$region 'xmldoc'}
    /// Duployan shorthand, Duployan stenography
    {$endregion}
    usDuployan,
    {$region 'xmldoc'}
    /// Egyptian demotic
    {$endregion}
    usEgyptianDemotic,
    {$region 'xmldoc'}
    /// Egyptian hieratic
    {$endregion}
    usEgyptianHieratic,
    {$region 'xmldoc'}
    /// Egyptian hieroglyphs
    {$endregion}
    usEgyptianHieroglyphs,
    {$region 'xmldoc'}
    /// Elbasan
    {$endregion}
    usElbasan,
    {$region 'xmldoc'}
    /// Ethiopic
    {$endregion}
    usEthiopic,
    {$region 'xmldoc'}
    /// Khutsuri
    {$endregion}
    usKhutsuri,
    {$region 'xmldoc'}
    /// Georgian (Mkhedruli)
    {$endregion}
    usGeorgian,
    {$region 'xmldoc'}
    /// Glagolitic
    {$endregion}
    usGlagolitic,
    {$region 'xmldoc'}
    /// Gothic
    {$endregion}
    usGothic,
    {$region 'xmldoc'}
    /// Grantha
    {$endregion}
    usGrantha,
    {$region 'xmldoc'}
    /// Greek
    {$endregion}
    usGreek,
    {$region 'xmldoc'}
    /// Gujarati
    {$endregion}
    usGujarati,
    {$region 'xmldoc'}
    /// Gurmukhi
    {$endregion}
    usGurmukhi,
    {$region 'xmldoc'}
    /// Hangul (Hangŭl, Hangeul)
    {$endregion}
    usHangul,
    {$region 'xmldoc'}
    /// Han (Hanzi, Kanji, Hanja)
    {$endregion}
    usHan,
    {$region 'xmldoc'}
    /// Hanunoo (Hanunóo)
    {$endregion}
    usHanunoo,
    {$region 'xmldoc'}
    /// Han (Simplified variant)
    {$endregion}
    usHan_Simplified,
    {$region 'xmldoc'}
    /// Han (Traditional variant)
    {$endregion}
    usHan_Traditional,
    {$region 'xmldoc'}
    /// Hebrew
    {$endregion}
    usHebrew,
    {$region 'xmldoc'}
    /// Hiragana
    {$endregion}
    usHiragana,
    {$region 'xmldoc'}
    /// Pahawh Hmong
    {$endregion}
    usPahawhHmong,
    {$region 'xmldoc'}
    /// (alias for Hiragana + Katakana)
    {$endregion}
    usHiraganaKatakana,
    {$region 'xmldoc'}
    /// Old Hungarian
    {$endregion}
    usOldHungarian,
    {$region 'xmldoc'}
    /// Indus (Harappan)
    {$endregion}
    usIndus,
    {$region 'xmldoc'}
    /// Old Italic (Etruscan, Oscan, etc.)
    {$endregion}
    usOldItalic,
    {$region 'xmldoc'}
    /// Javanese
    {$endregion}
    usJavanese,
    {$region 'xmldoc'}
    /// Japanese (alias for Han + Hiragana + Katakana)
    {$endregion}
    usJapanese,
    {$region 'xmldoc'}
    /// Kayah Li
    {$endregion}
    usKayahLi,
    {$region 'xmldoc'}
    /// Katakana
    {$endregion}
    usKatakana,
    {$region 'xmldoc'}
    /// Kharoshthi
    {$endregion}
    usKharoshthi,
    {$region 'xmldoc'}
    /// Khmer
    {$endregion}
    usKhmer,
    {$region 'xmldoc'}
    /// Kannada
    {$endregion}
    usKannada,
    {$region 'xmldoc'}
    /// Korean (alias for Hangul + Han)
    {$endregion}
    usKorean,
    {$region 'xmldoc'}
    /// Kpelle
    {$endregion}
    usKpelle,
    {$region 'xmldoc'}
    /// Kaithi
    {$endregion}
    usKaithi,
    {$region 'xmldoc'}
    /// Tai Tham (Lanna)
    {$endregion}
    usTaiTham,
    {$region 'xmldoc'}
    /// Lao
    {$endregion}
    usLao,
    {$region 'xmldoc'}
    /// Latin (Fraktur variant)
    {$endregion}
    usLatin_Fraktur,
    {$region 'xmldoc'}
    /// Latin (Gaelic variant)
    {$endregion}
    usLatin_Gaelic,
    {$region 'xmldoc'}
    /// Latin
    {$endregion}
    usLatin,
    {$region 'xmldoc'}
    /// Lepcha (Róng)
    {$endregion}
    usLepcha,
    {$region 'xmldoc'}
    /// Limbu
    {$endregion}
    usLimbu,
    {$region 'xmldoc'}
    /// Linear A
    {$endregion}
    usLinearA,
    {$region 'xmldoc'}
    /// Linear B
    {$endregion}
    usLinearB,
    {$region 'xmldoc'}
    /// Lisu (Fraser)
    {$endregion}
    usLisu,
    {$region 'xmldoc'}
    /// Loma
    {$endregion}
    usLoma,
    {$region 'xmldoc'}
    /// Lycian
    {$endregion}
    usLycian,
    {$region 'xmldoc'}
    /// Lydian
    {$endregion}
    usLydian,
    {$region 'xmldoc'}
    /// Mandaic
    {$endregion}
    usMandaic,
    {$region 'xmldoc'}
    /// Manichaean
    {$endregion}
    usManichaean,
    {$region 'xmldoc'}
    /// Mayan hieroglyphs
    {$endregion}
    usMayanHieroglyphs,
    {$region 'xmldoc'}
    /// Mende
    {$endregion}
    usMende,
    {$region 'xmldoc'}
    /// Meroitic Cursive
    {$endregion}
    usMeroiticCursive,
    {$region 'xmldoc'}
    /// Meroitic Hieroglyphs
    {$endregion}
    usMeroiticHieroglyphs,
    {$region 'xmldoc'}
    /// Malayalam
    {$endregion}
    usMalayalam,
    {$region 'xmldoc'}
    /// Mongolian
    {$endregion}
    usMongolian,
    {$region 'xmldoc'}
    /// Moon (Moon code, Moon script, Moon type)
    {$endregion}
    usMoon,
    {$region 'xmldoc'}
    /// Meitei Mayek (Meithei, Meetei)
    {$endregion}
    usMeiteiMayek,
    {$region 'xmldoc'}
    /// Myanmar (Burmese)
    {$endregion}
    usMyanmar,
    {$region 'xmldoc'}
    /// Old North Arabian (Ancient North Arabian)
    {$endregion}
    usOldNorthArabian,
    {$region 'xmldoc'}
    /// Nabataean
    {$endregion}
    usNabataean,
    {$region 'xmldoc'}
    /// Nakhi Geba ('Na-'Khi ²Ggŏ-¹baw, Naxi Geba)
    {$endregion}
    usNakhiGeba,
    {$region 'xmldoc'}
    /// N’Ko
    {$endregion}
    usNKo,
    {$region 'xmldoc'}
    /// Ogham
    {$endregion}
    usOgham,
    {$region 'xmldoc'}
    /// Ol Chiki (Ol Cemet’, Ol, Santali)
    {$endregion}
    usOlChiki,
    {$region 'xmldoc'}
    /// Old Turkic, Orkhon Runic
    {$endregion}
    usOldTurkicOrkhonRunic,
    {$region 'xmldoc'}
    /// Oriya
    {$endregion}
    usOriya,
    {$region 'xmldoc'}
    /// Osmanya
    {$endregion}
    usOsmanya,
    {$region 'xmldoc'}
    /// Palmyrene
    {$endregion}
    usPalmyrene,
    {$region 'xmldoc'}
    /// Old Permic
    {$endregion}
    usOldPermic,
    {$region 'xmldoc'}
    /// Phags-pa
    {$endregion}
    usPhagsPa,
    {$region 'xmldoc'}
    /// Inscriptional Pahlavi
    {$endregion}
    usInscriptionalPahlavi,
    {$region 'xmldoc'}
    /// Psalter Pahlavi
    {$endregion}
    usPsalterPahlavi,
    {$region 'xmldoc'}
    /// Book Pahlavi
    {$endregion}
    usBookPahlavi,
    {$region 'xmldoc'}
    /// Phoenician
    {$endregion}
    usPhoenician,
    {$region 'xmldoc'}
    /// Miao (Pollard)
    {$endregion}
    usMiao,
    {$region 'xmldoc'}
    /// Inscriptional Parthian
    {$endregion}
    usInscriptionalParthian,
    {$region 'xmldoc'}
    /// Reserved for private use (start)
    {$endregion}
    usReservedStart,
    {$region 'xmldoc'}
    /// Reserved for private use (end)
    {$endregion}
    usReservedEnd,
    {$region 'xmldoc'}
    /// Rejang (Redjang, Kaganga)
    {$endregion}
    usRejang,
    {$region 'xmldoc'}
    /// Rongorongo
    {$endregion}
    usRongorongo,
    {$region 'xmldoc'}
    /// Runic
    {$endregion}
    usRunic,
    {$region 'xmldoc'}
    /// Samaritan
    {$endregion}
    usSamaritan,
    {$region 'xmldoc'}
    /// Sarati
    {$endregion}
    usSarati,
    {$region 'xmldoc'}
    /// Old South Arabian
    {$endregion}
    usOldSouthArabian,
    {$region 'xmldoc'}
    /// Saurashtra
    {$endregion}
    usSaurashtra,
    {$region 'xmldoc'}
    /// SignWriting
    {$endregion}
    usSignWriting,
    {$region 'xmldoc'}
    /// Shavian (Shaw)
    {$endregion}
    usShavian,
    {$region 'xmldoc'}
    /// Sindhi
    {$endregion}
    usSindhi,
    {$region 'xmldoc'}
    /// Sinhala
    {$endregion}
    usSinhala,
    {$region 'xmldoc'}
    /// Sundanese
    {$endregion}
    usSundanese,
    {$region 'xmldoc'}
    /// Syloti Nagri
    {$endregion}
    usSylotiNagri,
    {$region 'xmldoc'}
    /// Syriac
    {$endregion}
    usSyriac,
    {$region 'xmldoc'}
    /// Syriac (Estrangelo variant)
    {$endregion}
    usSyriac_Estrangelo,
    {$region 'xmldoc'}
    /// Syriac (Western variant)
    {$endregion}
    usSyriac_Western,
    {$region 'xmldoc'}
    /// Syriac (Eastern variant)
    {$endregion}
    usSyriac_Eastern,
    {$region 'xmldoc'}
    /// Tagbanwa
    {$endregion}
    usTagbanwa,
    {$region 'xmldoc'}
    /// Tai
    {$endregion}
    usTai,
    {$region 'xmldoc'}
    /// New Tai Lue
    {$endregion}
    usNewTaiLue,
    {$region 'xmldoc'}
    /// Tamil
    {$endregion}
    usTamil,
    {$region 'xmldoc'}
    /// Tai Viet
    {$endregion}
    usTaiViet,
    {$region 'xmldoc'}
    /// Telugu
    {$endregion}
    usTelugu,
    {$region 'xmldoc'}
    /// Tengwar
    {$endregion}
    usTengwar,
    {$region 'xmldoc'}
    /// Tifinagh (Berber)
    {$endregion}
    usTifinagh,
    {$region 'xmldoc'}
    /// Tagalog (Baybayin, Alibata)
    {$endregion}
    usTagalog,
    {$region 'xmldoc'}
    /// Thaana
    {$endregion}
    usThaana,
    {$region 'xmldoc'}
    /// Thai
    {$endregion}
    usThai,
    {$region 'xmldoc'}
    /// Tibetan
    {$endregion}
    usTibetan,
    {$region 'xmldoc'}
    /// Ugaritic
    {$endregion}
    usUgaritic,
    {$region 'xmldoc'}
    /// Vai
    {$endregion}
    usVai,
    {$region 'xmldoc'}
    /// Visible Speech
    {$endregion}
    usVisibleSpeech,
    {$region 'xmldoc'}
    /// Warang Citi (Varang Kshiti)
    {$endregion}
    usWarangCiti,
    {$region 'xmldoc'}
    /// Old Persian
    {$endregion}
    usOldPersian,
    {$region 'xmldoc'}
    /// Cuneiform, Sumero-Akkadian
    {$endregion}
    usCuneiformSumeroAkkadian,
    {$region 'xmldoc'}
    /// Yi
    {$endregion}
    usYi,
    {$region 'xmldoc'}
    /// Code for inherited script
    {$endregion}
    usInherited,
    {$region 'xmldoc'}
    /// Mathematical notation
    {$endregion}
    usMathematical,
    {$region 'xmldoc'}
    /// Symbols
    {$endregion}
    usSymbols,
    {$region 'xmldoc'}
    /// Unwritten documents
    {$endregion}
    usUnwritten,
    {$region 'xmldoc'}
    /// Undetermined script
    {$endregion}
    usUndetermined,
    {$region 'xmldoc'}
    /// Uncoded script
    {$endregion}
    usUncoded
  );

  {$region 'xmldoc'}
  /// <summary>
  /// This data type represents a set of <see cref="TUnicodeScript"/> values.</summary>
  {$endregion}
  TUnicodeScripts = set of TUnicodeScript;

{$region 'xmldoc'}
/// <summary>
/// Converts a semicolon separated list of the four characters ISO 15924 script
/// codes to a set of <see cref="TUnicodeScript"/> values.</summary>
/// <param name="Codes">
/// The semicolon separated list of the four-character codes.</param>
/// <returns>
/// The set of <see cref="TUnicodeScript"/> values.</returns>
/// <seealso cref="ScriptsToScriptCodes"/>
{$endregion}
function ScriptCodesToScripts(const Codes: String): TUnicodeScripts;

{$region 'xmldoc'}
/// <summary>
/// Converts a set of <see cref="TUnicodeScript"/> values to a semicolon separated
/// list of the four characters ISO 15924 script codes.</summary>
/// <param name="Scripts">
/// The set of <see cref="TUnicodeScript"/> values.</param>
/// <returns>
/// The semicolon separated list of the four-character codes.</returns>
/// <seealso cref="ScriptCodesToScripts"/>
{$endregion}
function ScriptsToScriptCodes(Scripts: TUnicodeScripts): String;

implementation

uses
  SysUtils, Types;

const
  // ISO 15924 script codes in alphabetical order
  ScriptCodes: array [TUnicodeScript] of PChar = (
    'Arab', 'Armi', 'Armn', 'Avst', 'Bali', 'Bamu', 'Bass', 'Batk', 'Beng',
    'Blis', 'Bopo', 'Brah', 'Brai', 'Bugi', 'Buhd', 'Cakm', 'Cans', 'Cari',
    'Cham', 'Cher', 'Cirt', 'Copt', 'Cprt', 'Cyrl', 'Cyrs', 'Deva', 'Dsrt',
    'Dupl', 'Egyd', 'Egyh', 'Egyp', 'Elba', 'Ethi', 'Geok', 'Geor', 'Glag',
    'Goth', 'Gran', 'Grek', 'Gujr', 'Guru', 'Hang', 'Hani', 'Hano', 'Hans',
    'Hant', 'Hebr', 'Hira', 'Hmng', 'Hrkt', 'Hung', 'Inds', 'Ital', 'Java',
    'Jpan', 'Kali', 'Kana', 'Khar', 'Khmr', 'Knda', 'Kore', 'Kpel', 'Kthi',
    'Lana', 'Laoo', 'Latf', 'Latg', 'Latn', 'Lepc', 'Limb', 'Lina', 'Linb',
    'Lisu', 'Loma', 'Lyci', 'Lydi', 'Mand', 'Mani', 'Maya', 'Mend', 'Merc',
    'Mero', 'Mlym', 'Mong', 'Moon', 'Mtei', 'Mymr', 'Narb', 'Nbat', 'Nkgb',
    'Nkoo', 'Ogam', 'Olck', 'Orkh', 'Orya', 'Osma', 'Palm', 'Perm', 'Phag',
    'Phli', 'Phlp', 'Phlv', 'Phnx', 'Plrd', 'Prti', 'Qaaa', 'Qabx', 'Rjng',
    'Roro', 'Runr', 'Samr', 'Sara', 'Sarb', 'Saur', 'Sgnw', 'Shaw', 'Sind',
    'Sinh', 'Sund', 'Sylo', 'Syrc', 'Syre', 'Syrj', 'Syrn', 'Tagb', 'Tale',
    'Talu', 'Taml', 'Tavt', 'Telu', 'Teng', 'Tfng', 'Tglg', 'Thaa', 'Thai',
    'Tibt', 'Ugar', 'Vaii', 'Visp', 'Wara', 'Xpeo', 'Xsux', 'Yiii', 'Zinh',
    'Zmth', 'Zsym', 'Zxxx', 'Zyyy', 'Zzzz');

function FindScript(pCode: PChar; out Script: TUnicodeScript): Boolean;
var
  L, H: TUnicodeScript;
  C: Integer;
begin
  L := Low(TUnicodeScript);
  H := High(TUnicodeScript);
  repeat
    Script := TUnicodeScript((Ord(L) + Ord(H)) shr 1);
    C := StrLIComp(ScriptCodes[Script], pCode, 4);
    if C < 0 then
      L := Succ(Script)
    else if C > 0 then
      H := Pred(Script)
    else
    begin
      Result := True;
      Exit;
    end;
  until L > H;
  Result := False;
end;

function ScriptCodesToScripts(const Codes: String): TUnicodeScripts;
var
  Script: TUnicodeScript;
  S, E: PChar;
begin
  Result := [];
  S := PChar(Codes);
  E := StrScan(S, ';');
  while E <> nil do
  begin
    if (E - S = 4) and FindScript(S, Script) then
      Include(Result, Script);
    S := E + 1;
    if S^ = #0 then
      Exit;
    E := StrScan(S, ';');
  end;
end;

function ScriptsToScriptCodes(Scripts: TUnicodeScripts): String;
var
  Script: TUnicodeScript;
begin
  Result := '';
  for Script := Low(TUnicodeScript) to High(TUnicodeScript) do
    if Script in Scripts then
      Result := Result + ScriptCodes[Script] + ';';
end;

end.


