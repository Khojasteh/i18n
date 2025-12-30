unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCore, Grids, ValEdit, StdCtrls, i18nCtrls, ImgList, ComCtrls,
  i18nCalendar, System.ImageList;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    FlagImageList1: TFlagImageList;
    CultureBox1: TCultureBox;
    ValueListEditor1: TValueListEditor;
    TerritoryBox1: TTerritoryBox;
    ValueListEditor2: TValueListEditor;
    Label1: TLabel;
    CultureListBox1: TCultureListBox;
    CurrencyBox1: TCurrencyBox;
    ValueListEditor3: TValueListEditor;
    Label2: TLabel;
    TerritoryListBox1: TTerritoryListBox;
    procedure FormCreate(Sender: TObject);
    procedure CultureBox1Select(Sender: TObject);
    procedure TerritoryBox1Select(Sender: TObject);
    procedure CurrencyBox1Select(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  CultureBox1.ItemSelected := GetUserDefaultUICulture;
  if CultureBox1.ItemSelected <> nil then
  begin
    CultureBox1Select(nil);
    CurrencyBox1.ItemSelected := CultureBox1.ItemSelected.Currency;
    if CurrencyBox1.ItemSelected <> nil then
      CurrencyBox1Select(nil);
  end;

  TerritoryBox1.ItemSelected := GetUserTerritory;
  if TerritoryBox1.ItemSelected <> nil then
    TerritoryBox1Select(nil);
end;

procedure TForm1.CultureBox1Select(Sender: TObject);
const
  MeasurementSystems: array[TMeasurementSystem] of String =
    ('S.I. system', 'U.S. system');
  ReadingLayouts: array[TReadingLayout] of String =
    ('Left to right', 'Right to left',
     'Top to bottom, columns right to left',
     'Top to bottom, columns left to right');
  DaysOfWeek: array[TDayOfWeek] of String =
     ('Monday', 'Tuesday', 'Wednesday', 'Thursday',
      'Friday', 'Saturday', 'Sunday');
begin
  with ValueListEditor1.Strings, CultureBox1.ItemSelected do
  begin
    Values['Locale ID'] := IntToHex(LocaleID, 4);
    Values['Locale Name'] := Locale;
    Values['ISO 639-1 Language Code'] := language2;
    Values['ISO 639-2/T Language Code'] := language3;
    Values['ISO 3166-2 Country Code'] := Country.Code2;
    Values['ISO 3166-3 Country Code'] := Country.Code3;
    Values['ISO 15924 Script Code(s)'] := ScriptCodes;
    Values['Native Display Name'] := NativeDisplayName;
    Values['Measurement System'] := MeasurementSystems[MeasurementSystem];
    Values['Reading Layout'] := ReadingLayouts[ReadingLayout];
    Values['Currency'] := Currency.EnglishName;
    Values['Digits'] := NativeDigits;
    Values['First Day of Week'] := DaysOfWeek[NativeCalendar.Settings.FirstDayOfWeek];
    Values['Calendar System'] := NativeCalendar.CalendarName;
    Values['Long Date Format'] := FormatDateTime('dddddd', Now);
    Values['Short Date Format'] := FormatDateTime('ddddd', Now);
    Values['Long Time Format'] := FormatDateTime('tt', Now);
    Values['Short Time Format'] := FormatDateTime('t', Now);
    Values['Number Format'] := FormatNumber('#,##0.00', 14937.16);
    Values['Positive Percent Format'] := FormatPercent(0.8725);
    Values['Negative Percent Format'] := FormatPercent(-0.8725);
    Values['Language''s Plural Rule'] := PluralRule;
  end;
end;

procedure TForm1.TerritoryBox1Select(Sender: TObject);
begin
  with ValueListEditor2.Strings, TerritoryBox1.ItemSelected do
  begin
    Values['Geo ID'] := IntToStr(GeoID);
    Values['ISO 3166-2 Code'] := Code2;
    Values['ISO 3166-3 Code'] := Code3;
    Values['Official Name'] := OfficialName;
    Values['Friendly Name'] := FriendlyName;
    Values['Native Name'] := NativeName;
    Values['Latitude'] := Format('%g°', [Latitude]);
    Values['Longitude'] := Format('%g°', [Longitude]);
  end;
  CultureListBox1.Items.Assign(TerritoryBox1.ItemSelected.Cultures);
end;

procedure TForm1.CurrencyBox1Select(Sender: TObject);
begin
  with ValueListEditor3.Strings, CurrencyBox1.ItemSelected do
  begin
    Values['International Symbol'] := IntlSymbol;
    Values['Local Symbol'] := LocalSymbol;
    Values['Native Name'] := NativeName;
  end;
  TerritoryListBox1.Items.Assign(CurrencyBox1.ItemSelected.Countries);
end;

end.
