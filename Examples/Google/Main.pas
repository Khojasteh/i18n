unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nUtils, StdCtrls, i18nCtrls, i18nGoogle, ImgList, i18nCore,
  ExtCtrls, i18nHTTP;

type
  TMainForm = class(TForm)
    SourceText: TMemo;
    TranslatedText: TMemo;
    btnTranslate: TButton;
    GoogleTranslator: TGoogleTranslator;
    lblSourceLanguage: TLabel;
    lblTargetLanguage: TLabel;
    SourceLanguage: TCultureBox;
    TargetLanguage: TCultureBox;
    lblSourceText: TLabel;
    lblTranslatedText: TLabel;
    Flags: TFlagImageList;
    rgTextFormat: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure SourceLanguageSelect(Sender: TObject);
    procedure TargetLanguageSelect(Sender: TObject);
    procedure btnTranslateClick(Sender: TObject);
    procedure rgTextFormatClick(Sender: TObject);
    procedure SourceLanguageKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TargetLanguage.ItemSelected := GetUserDefaultUICulture;
  SourceLanguageSelect(nil);
  TargetLanguageSelect(nil);
end;

procedure TMainForm.SourceLanguageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_BACK) and (Shift = []) then
  begin
    SourceLanguage.ItemSelected := nil;
    Key := 0;
  end;
end;

procedure TMainForm.SourceLanguageSelect(Sender: TObject);
begin
  if SourceLanguage.ItemIndex >= 0 then
  begin
    GoogleTranslator.SourceLang := CultureToGoogleLang(SourceLanguage.ItemSelected);
    SourceText.BiDiMode := SourceLanguage.ItemSelected.BiDiMode;
  end
  else
  begin
    GoogleTranslator.SourceLang := '';
    SourceText.BiDiMode := Application.BiDiMode;
  end;
end;

procedure TMainForm.TargetLanguageSelect(Sender: TObject);
begin
  if TargetLanguage.ItemIndex >= 0 then
  begin
    GoogleTranslator.TargetLang := CultureToGoogleLang(TargetLanguage.ItemSelected);
    TranslatedText.BiDiMode := TargetLanguage.ItemSelected.BiDiMode;
  end
  else
  begin
    GoogleTranslator.TargetLang := '';
    TranslatedText.BiDiMode := Application.BiDiMode;
  end;
  TranslatedText.Clear;
end;

procedure TMainForm.rgTextFormatClick(Sender: TObject);
begin
  GoogleTranslator.TextFormat := TTextFormat(rgTextFormat.ItemIndex);
end;

procedure TMainForm.btnTranslateClick(Sender: TObject);
begin
  TranslatedText.Text := GoogleTranslator.Translate(SourceText.Text);
  // if the source language was not specified
  if SourceLanguage.ItemIndex < 0 then
  begin
    // update the related control by the detected language
    SourceLanguage.ItemSelected := GoogleLangToCulture(GoogleTranslator.DetectedSourceLang);
    SourceLanguageSelect(nil);
  end;
end;

end.
