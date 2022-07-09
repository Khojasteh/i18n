{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, pngimage, i18nCore, i18nLocalizer, i18nCtrls;

type
  TAboutDialog = class(TForm)
    Translator: TTranslator;
    Copyright: TLabel;
    Logo: TImage;
    Background: TShape;
    Disclaimer: TLabel;
    Link: TLabel;
    Spacer: TBevel;
    BuildInfo: TLabel;
    TranslatorInfoPanel: TPanel;
    TranslatorName: TLabel;
    TranslatorLink: TLabel;
    TranslatorLanguage: TCultureLabel;
    Separator: TBevel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure LinkClick(Sender: TObject);
    procedure TranslatorLinkClick(Sender: TObject);
  public
    class procedure Execute;
  end;

implementation

{$R *.dfm}

uses
  ShellAPI, DataModule, i18nUtils;

{ Helper Functions }

function GetVersionInfo(const FileName, ValueName: String): String;
var
  Trans: PCardinal;
  VerInfoSize, ValSize, Dummy: Cardinal;
  VerInfo, Value: Pointer;
  TransStr, InfoStr: String;
begin
  Result := '';
  VerInfoSize := GetFileVersioninfoSize(PChar(FileName), Dummy);
  if VerInfoSize <> 0 then
  begin
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Dummy, VerInfoSize, VerInfo) then
      begin
        VerQueryValue(VerInfo, '\VarFileInfo\Translation', Pointer(Trans), valSize);
        TransStr := IntToHex(LoWord(Trans^), 4) + IntToHex(HiWord(Trans^), 4);
        if not SameText(ValueName, 'Language') then
        begin
          InfoStr := '\StringFileInfo\' + TransStr + '\' + ValueName;
          if VerQueryValue(VerInfo, PChar(InfoStr), Value, ValSize) then
            Result := PChar(Value);
        end
        else
        begin
          SetLength(Result, 256);
          VerLanguageName(LoWord(Trans^), PChar(Result), Length(Result));
          SetLength(Result, StrLen(PChar(Result)));
        end;
      end;
    finally
      FreeMem(VerInfo);
    end;
  end;
end;

{ TAboutDialog }

class procedure TAboutDialog.Execute;
begin
  with Create(Application) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TAboutDialog.LinkClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(Link.Caption), nil, nil, SW_NORMAL);
end;

procedure TAboutDialog.TranslatorLinkClick(Sender: TObject);
var
  URI: String;
begin
  URI := TranslatorLink.Caption;
  if Pos('@', URI) <> 0 then
    URI := 'mailto:' + URI;
  ShellExecute(Handle, 'open', PChar(URI), nil, nil, SW_NORMAL);
end;

procedure TAboutDialog.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_ESCAPE then
  begin
    Key := #0;
    Close;
  end;
end;

procedure TAboutDialog.FormShow(Sender: TObject);
begin
  BuildInfo.Caption := FormatCS(BuildInfo.Caption,
    [GetVersionInfo(ParamStr(0), 'FileVersion')]);
  if TranslatorLanguage.Culture <> nil then
  begin
    TranslatorLanguage.Caption :=
      DM.Localizer.FormatCS(TranslatorLanguage.Caption,
      [TranslatorLanguage.Culture.NativeLanguageName]);
    TranslatorName.Visible := (TranslatorName.Caption <> '')
      and not SameText(TranslatorName.Caption, '[YOUR_NAME]');
    TranslatorLink.Visible := (TranslatorLink.Caption <> '')
      and not SameText(TranslatorLink.Caption, '[YOUR_LINK]');
    TranslatorInfoPanel.Visible := TranslatorName.Visible or TranslatorLink.Visible;
  end;
  Background.BoundsRect := ClientRect;
end;

end.
