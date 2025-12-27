{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz Khojasteh                                                         }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit DataModule;

interface

uses
  Windows, SysUtils, Classes, Graphics, ImgList, MRU, i18nCore, ActnList,
  i18nLocalizer, i18nCtrls, i18nDialogs, i18nCatalog, Controls, System.Actions;

const
  AppRegRootKey = '\Software\DELPHI AREA\i18n';

type
  TDM = class(TDataModule)
    MsgDlg: TMessageDialog;
    Flags: TFlagImageList;
    Localizer: TLocalizer;
    ReopenActions: TActionList;
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    Action5: TAction;
    Action6: TAction;
    Action7: TAction;
    Action8: TAction;
    Action9: TAction;
    Action10: TAction;
    Translator: TTranslator;
    MsgImages: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    procedure MRUListChanged(Sender: TObject);
  public
    RepositoryFileName: String;
    Repository: TTranslationRepository;
    RecentFiles: TRecentList;
    RecentFindPhrases: TRecentList;
    RecentReplacePhrases: TRecentList;
    RecentExportNames: TRecentList;
    SortColumn: Integer;
    SortAscending: Boolean;
    SortGrouping: Boolean;
    SortImmediately: Boolean;
    BulkActions: Integer;
    procedure LoadPersistentUserChoices;
    procedure SavePersistentUserChoices;
    procedure CopyFlagTo(Culture: TCultureInfo; dstImages: TCustomImageList;
      dstIndex: Integer; xOffset: Integer = 0; yOffset: Integer = 0);
  end;

function OpenFileLocation(const FileName: String): Boolean;

procedure AddPluralChoices(Choices: TStrings; NumOfPlurals: Integer);

var
  DM: TDM;

implementation

{$R *.dfm}

uses
  Registry, ShellAPI, i18nUtils, dlgLanguageSelector;

const
  SSelectUILanguage     = 'Please select the user interface language from the given list.';
  SRepositoryReadError  = 'Error on loading translation repository from {0}.';
  SRepositoryWriteError = 'Error on saving translation repository to {0}.';

const
  DefaultRepositoryFile = '%ALLUSERSPROFILE%\DELPHI AREA\i18n\Repository' + i18nRepositoryFileExt;

{ Helper Functions }

function OpenFileLocation(const FileName: String): Boolean;
var
  Param: String;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    Param := Format('/n,/select,"%s"', [FileName]);
    ShellExecute(0, 'open', 'explorer.exe', PChar(Param), nil, SW_NORMAL);
    Result := True;
  end;
end;

procedure AddPluralChoices(Choices: TStrings; NumOfPlurals: Integer);
var
  I: Integer;
begin
  Choices.BeginUpdate;
  try
    for I := Choices.Count to NumOfPlurals - 1 do
      Choices.Add(Format(' plural=%d ', [I]));
    for I := Choices.Count - 1 downto NumOfPlurals do
      Choices.Delete(I);
  finally
    Choices.EndUpdate;
  end;
end;

{ TDM }

procedure TDM.DataModuleCreate(Sender: TObject);
var
  UserCulture: TCultureInfo;
begin
  Repository := TTranslationRepository.Create;
  RecentFiles := TRecentList.Create;
  RecentFiles.MaxCount := ReopenActions.ActionCount;
  RecentFiles.OnChange := MRUListChanged;
  RecentFindPhrases := TRecentList.Create;
  RecentReplacePhrases := TRecentList.Create;
  RecentExportNames := TRecentList.Create;
  SortColumn := -1;
  SortAscending := True;
  SortGrouping := True;
  SortImmediately := False;
  BulkActions := -1;
  LoadPersistentUserChoices;
  if FileExists(RepositoryFileName) then
    try
      Repository.LoadFromFile(RepositoryFileName);
    except
      MsgDlg.ShowError(Localizer.FormatCS(Translator.GetText(SRepositoryReadError), [RepositoryFileName]));
    end;
  if (Localizer.Culture = nil) and (Localizer.Cultures.Count <> 0) then
  begin
    if Localizer.Cultures.Count = 1 then
      Localizer.Culture := Localizer.Cultures[0]
    else
    begin
      UserCulture := GetUserDefaultUICulture;
      if TSelectLanguageDialog.Execute(Translator.GetText(SSelectUILanguage), UserCulture, Localizer.Cultures) then
        Localizer.Culture := UserCulture;
    end;
  end;
end;

procedure TDM.DataModuleDestroy(Sender: TObject);
begin
  SavePersistentUserChoices;
  if Repository.Modified then
    try
      if not FileExists(RepositoryFileName) then
        ForceDirectories(ExtractFilePath(RepositoryFileName));
      Repository.SaveToFile(RepositoryFileName);
    except
      MsgDlg.ShowError(Localizer.FormatCS(Translator.GetText(SRepositoryWriteError), [RepositoryFileName]));
    end;
  Repository.Free;
  RecentFiles.Free;
  RecentFindPhrases.Free;
  RecentReplacePhrases.Free;
  RecentExportNames.Free;
end;

procedure TDM.MRUListChanged(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ReopenActions.ActionCount - 1 do
    with TAction(ReopenActions.Actions[I]) do
    begin
      if I < RecentFiles.Count then
      begin
        Tag := I;
        Caption := RecentFiles.Captions[I];
        Visible := True;
      end
      else
        Visible := False;
    end;
end;

procedure TDM.LoadPersistentUserChoices;
var
  R: TRegistry;
begin
  R := TRegistry.Create(KEY_QUERY_VALUE);
  try
    if R.OpenKeyReadOnly(AppRegRootKey) then
    begin
      if R.ValueExists('Repository') then
        RepositoryFileName := R.ReadString('Repository');
      if R.ValueExists('UserLocale') then
        Localizer.Culture := CultureOf(R.ReadString('UserLocale'));
      RecentFiles.LoadFromRegistry(R, 'RecentFile');
      RecentFindPhrases.LoadFromRegistry(R, 'Search.Find');
      RecentReplacePhrases.LoadFromRegistry(R, 'Search.Replace');
      RecentExportNames.LoadFromRegistry(R, 'Import.BaseName');
      if R.ValueExists('SortColumn') then
        SortColumn := R.ReadInteger('SortColumn');
      if R.ValueExists('SortAscending') then
        SortAscending := R.ReadBool('SortAscending');
      if R.ValueExists('SortGrouping') then
        SortGrouping := R.ReadBool('SortGrouping');
      if R.ValueExists('BulkActions') then
        BulkActions := R.ReadInteger('BulkActions');
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
  if RepositoryFileName = '' then
    RepositoryFileName := ExpandEnvStr(DefaultRepositoryFile);
end;

procedure TDM.SavePersistentUserChoices;
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    if R.OpenKey(AppRegRootKey, True) then
    begin
      if Assigned(Localizer.Culture) then
        R.WriteString('UserLocale', Localizer.Culture.Locale)
      else if R.ValueExists('UserLocale') then
        R.DeleteValue('UserLocale');
      if RecentFiles.Modified then
        RecentFiles.SaveToRegistry(R, 'RecentFile');
      if RecentFindPhrases.Modified then
        RecentFindPhrases.SaveToRegistry(R, 'Search.Find');
      if RecentReplacePhrases.Modified then
        RecentReplacePhrases.SaveToRegistry(R, 'Search.Replace');
      if RecentExportNames.Modified then
        RecentExportNames.SaveToRegistry(R, 'Import.BaseName');
      R.WriteInteger('SortColumn', SortColumn);
      R.WriteBool('SortAscending', SortAscending);
      R.WriteBool('SortGrouping', SortGrouping);
      R.WriteBool('SortImmediately', SortImmediately);
      R.WriteInteger('BulkActions', BulkActions);
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
end;

procedure TDM.CopyFlagTo(Culture: TCultureInfo; dstImages: TCustomImageList;
  dstIndex: Integer; xOffset, yOffset: Integer);
var
  Flag: TIcon;
  Image: TBitmap;
begin
  Flag := TIcon.Create;
  try
    Flags.GetIcon(Flags.ImageIndexOf(Culture), Flag);
    Image := TBitmap.Create;
    try
      Image.Width := dstImages.Width;
      Image.Height := dstImages.Height;
      Image.Canvas.Brush.Color := clFuchsia;
      Image.Canvas.FillRect(Image.Canvas.ClipRect);
      Image.Canvas.Draw(
        xOffset + (Image.Width - Flag.Width) div 2,
        yOffset + (Image.Height - Flag.Height) div 2, Flag);
      if dstIndex < dstImages.Count then
        dstImages.Delete(dstIndex);
      dstImages.InsertMasked(dstIndex, Image, clFuchsia);
    finally
      Image.Free;
    end;
  finally
    Flag.Free;
  end;
end;

end.
