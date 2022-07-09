{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Editor                                                                 }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

unit dlgOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, i18nCore, ComCtrls, i18nLocalizer, StdCtrls, ExtCtrls;

type
  TOptionsDialog = class(TForm)
    ButtonsPanel: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    Translator: TTranslator;
    PageControl: TPageControl;
    tabGeneral: TTabSheet;
    cbSortImmediately: TCheckBox;
    cbAskConfirmations: TCheckBox;
    btnAssociate: TButton;
    procedure btnAssociateClick(Sender: TObject);
  public
    class function Execute: Boolean;
  end;

implementation

{$R *.dfm}
{$R *.res}

uses
  DataModule, Registry, i18nCatalog;

{ Helper Functions }

function IsAssicoated(const FileExt, Application: String; AllUsers: Boolean): Boolean;
var
  R: TRegistry;
  FileType, Association: String;
begin
  Result := False;
  R := TRegistry.Create(KEY_QUERY_VALUE);
  try
    if AllUsers then
      R.RootKey := HKEY_LOCAL_MACHINE
    else
      R.RootKey := HKEY_CURRENT_USER;
    try
      if R.OpenKeyReadOnly('\Software\Classes\' + FileExt) then
      begin
        FileType := R.ReadString('');
        R.CloseKey;
        if R.OpenKeyReadOnly('\Software\Classes\' + FileType + '\shell\open\command') then
        begin
          Association := R.ReadString('');
          R.CloseKey;
          if SameText(Association, Format('"%s" "%%1"', [Application])) then
            Result := True;
        end;
      end;
    except
      // ignore exceptions
    end;
  finally
    R.Free;
  end;
end;

function Assicoate(const FileExt, FileType, FileDesc: String;
  const Application: String; IconIndex: Integer; AllUsers: Boolean): Boolean;
var
  R: TRegistry;
begin
  Result := False;
  R := TRegistry.Create;
  try
    if AllUsers then
      R.RootKey := HKEY_LOCAL_MACHINE
    else
      R.RootKey := HKEY_CURRENT_USER;
    try
      if R.OpenKey('\Software\Classes\' + FileExt, True) then
      begin
        R.WriteString('', FileType);
        R.CloseKey;
        if R.OpenKey('\Software\Classes\' + FileType, True) then
        begin
          R.WriteString('', FileDesc);
          R.CloseKey;
          if R.OpenKey('\Software\Classes\' + FileType + '\shell\open\command', True) then
          begin
            R.WriteString('', Format('"%s" "%%1"', [Application]));
            R.CloseKey;
            if R.OpenKey('\Software\Classes\' + FileType + '\DefaultIcon', True) then
            begin
              R.WriteString('', Format('"%s",%d', [Application, IconIndex]));
              R.CloseKey;
            end;
            Result := True;
          end;
        end;
      end;
    except
      // ignore exceptions
    end;
  finally
    R.Free;
  end;
end;

{ TOptionsDialog }

class function TOptionsDialog.Execute: Boolean;
begin
  Result := False;
  with Create(Application) do
    try
      btnAssociate.Enabled := not IsAssicoated(i18nCatalogFileExt, Application.ExeName, False);
      cbSortImmediately.Checked := DM.SortImmediately;
      cbAskConfirmations.Checked := (DM.BulkActions <> 0);
      cbAskConfirmations.Enabled := (DM.BulkActions >= 0);
      if ShowModal = mrOK then
      begin
        DM.SortImmediately := cbSortImmediately.Checked;
        if cbAskConfirmations.Enabled then
          DM.BulkActions := Ord(cbAskConfirmations.Checked);
        Result := True;
      end;
    finally
      Free;
    end;
end;

procedure TOptionsDialog.btnAssociateClick(Sender: TObject);
begin
  btnAssociate.Enabled := not Assicoate(i18nCatalogFileExt,
    'i18n', 'i18n Catalog', Application.ExeName, 1, False);
end;

end.
