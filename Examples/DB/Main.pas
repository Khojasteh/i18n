unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ExtCtrls, i18nLocalizer, i18nCore, i18nCalendar, ImgList, i18nCtrls, DB,
  DBClient, StdCtrls, i18nDBCtrls, i18nDateCtrls, i18nDBDateCtrls, DBCtrls,
  Grids, DBGrids;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1ID: TAutoIncField;
    ClientDataSet1Culture_LocaleID: TIntegerField;
    ClientDataSet1Culture_LocaleName: TStringField;
    ClientDataSet1Territory_GeoID: TIntegerField;
    ClientDataSet1Territory_Code2: TStringField;
    ClientDataSet1Territory_Code3: TStringField;
    ClientDataSet1Currency_IntlSymbol: TStringField;
    ClientDataSet1TheDate: TDateField;
    FlagImageList1: TFlagImageList;
    DBNavigator1: TDBNavigator;
    GroupBox1: TGroupBox;
    DBIntlDateTimeLabel1: TDBIntlDateTimeLabel;
    DBIntlDatePicker1: TDBIntlDatePicker;
    DBText1: TDBText;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    DBText2: TDBText;
    Label2: TLabel;
    GroupBox3: TGroupBox;
    DBText3: TDBText;
    Label3: TLabel;
    GroupBox4: TGroupBox;
    DBText4: TDBText;
    Label4: TLabel;
    GroupBox5: TGroupBox;
    DBText5: TDBText;
    Label5: TLabel;
    GroupBox6: TGroupBox;
    DBText6: TDBText;
    Label6: TLabel;
    GroupBox7: TGroupBox;
    DBText7: TDBText;
    Label7: TLabel;
    DBCultureBox1: TDBCultureBox;
    DBCultureBox2: TDBCultureBox;
    DBCultureListBox1: TDBCultureListBox;
    DBCultureListBox2: TDBCultureListBox;
    DBCurrencyListBox1: TDBCurrencyListBox;
    DBCurrencyBox1: TDBCurrencyBox;
    DBTerritoryBox1: TDBTerritoryBox;
    DBTerritoryListBox1: TDBTerritoryListBox;
    DBTerritoryBox2: TDBTerritoryBox;
    DBTerritoryListBox2: TDBTerritoryListBox;
    DBTerritoryBox3: TDBTerritoryBox;
    DBTerritoryListBox3: TDBTerritoryListBox;
    DBTerritoryLabel1: TDBTerritoryLabel;
    DBTerritoryLabel2: TDBTerritoryLabel;
    DBTerritoryLabel3: TDBTerritoryLabel;
    DBCultureLabel1: TDBCultureLabel;
    DBCultureLabel2: TDBCultureLabel;
    DBCurrencyLabel1: TDBCurrencyLabel;
    ReadonlyDataSet: TCheckBox;
    CalendarSystems: TComboBox;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReadonlyDataSetClick(Sender: TObject);
    procedure CalendarSystemsSelect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ClientDataSet1.SaveToFile;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // Open data set
  ClientDataSet1.Active := False;
  if FileExists(ClientDataSet1.FileName) then
    ClientDataSet1.LoadFromFile
  else
    ClientDataSet1.CreateDataSet;
  ClientDataSet1.Active := True;
  // Prepare list of calendar types
  CalendarSystems.Items.BeginUpdate;
  CalendarSystems.Items.Clear;
  for I := 0 to CalendarTypes.Count - 1 do
    CalendarSystems.Items.Add(CalendarTypes.ByIndex(I).CalendarName);
  CalendarSystems.Items.EndUpdate;
  // Synchronize controls
  CalendarSystems.ItemIndex := CalendarSystems.Items.IndexOf(DefaultCalendar.CalendarName);
  ReadonlyDataSet.Checked := ClientDataSet1.ReadOnly;
end;

procedure TForm1.ReadonlyDataSetClick(Sender: TObject);
begin
  ClientDataSet1.ReadOnly := ReadonlyDataSet.Checked;
end;

procedure TForm1.CalendarSystemsSelect(Sender: TObject);
var
  CalendarName: String;
begin
  CalendarName := CalendarSystems.Items[CalendarSystems.ItemIndex];
  DBIntlDatePicker1.CalendarType := CalendarTypes.ByName(CalendarName);
  DBIntlDateTimeLabel1.CalendarType := DBIntlDatePicker1.CalendarType;
end;

end.
