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
/// This unit implements functions to get or set the text of an active Delphi
/// IDE's editor.
/// </summary>
/// <remarks>
/// This unit cannot be referenced in the runtime packages.
/// </remarks>
unit i18nIDE;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, Classes, Types, ToolsAPI;

{$region 'xmldoc'}
/// <summary>
/// Gets the text inside the active Delphi IDE's code editor.
/// </summary>
/// <param name="Text">
/// Receives the text being edited in the active code editor.
/// </param>
/// <returns>
/// Returns <see langword="true"/> if there is an open code editor in Delphi
/// IDE, otherwise returns <see langword="false"/>.
/// </returns>
/// <seealso cref="ideSetActiveEditorText"/>
{$endregion}
function ideGetActiveEditorText(out Text: String): Boolean;
{$region 'xmldoc'}
/// <summary>
/// Replaces the text inside the active Delphi IDE's code editor. The user can
/// undo changes made by this function.
/// </summary>
/// <param name="Text">
/// The new text of the active code editor.
/// </param>
/// <returns>
/// Returns <see langword="true"/> if there is an open code editor in Delphi
/// IDE and it is not read-only, otherwise returns <see langword="false"/>.
/// </returns>
/// <seealso cref="ideGetActiveEditorText"/>
{$endregion}
function ideSetActiveEditorText(const Text: String): Boolean;

implementation

{ Helper Functions }

function ideGetActiveSourceEditor: IOTASourceEditor;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  EditorServices: IOTAEditorServices;
  EditBuffer: IOTAEditBuffer;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
  FileName: String;
  I: Integer;
begin
  Result := nil;
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  EditorServices := (BorlandIDEServices as IOTAEditorServices);
  if (ModuleServices <> nil) and (EditorServices <> nil) then
  begin
    Module := ModuleServices.CurrentModule;
    EditBuffer := EditorServices.TopBuffer;
    if (Module <> nil) and (EditBuffer <> nil) then
    begin
      FileName := EditBuffer.FileName;
      for I := 0 to Module.GetModuleFileCount - 1 do
      begin
        Editor := Module.GetModuleFileEditor(I);
        if Supports(Editor, IOTASourceEditor, SourceEditor) and (SourceEditor <> nil) and
          ((FileName = '') or SameFileName(SourceEditor.FileName, FileName)) then
        begin
          Result := SourceEditor;
          Exit;
        end;
      end;
    end;
  end;
end;

function ideGetEditorText(Reader: IOTAEditReader): String;
const
  BufferSize = 10 * 1024;
var
  Stream: TStringStream;
  Buffer: PAnsiChar;
  TotalBytesRead: Integer;
  BytesRead: Integer;
begin
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    GetMem(Buffer, BufferSize);
    try
      TotalBytesRead := 0;
      BytesRead := Reader.GetText(TotalBytesRead, Buffer, BufferSize);
      while BytesRead <> 0 do
      begin
        Stream.Write(Buffer^, BytesRead);
        Inc(TotalBytesRead, BytesRead);
        BytesRead := Reader.GetText(TotalBytesRead, Buffer, BufferSize);
      end;
    finally
      FreeMem(Buffer);
    end;
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure ideSetEditorText(Writer: IOTAEditWriter; const Text: String);
var
  Buffer: AnsiString;
begin
  Buffer := UTF8Encode(Text);
  Writer.CopyTo(0);
  Writer.DeleteTo(MaxInt);
  Writer.Insert(PAnsiChar(Buffer));
end;

function ideGetActiveEditorText(out Text: String): Boolean;
var
  SourceEditor: IOTASourceEditor;
  EditReader: IOTAEditReader;
begin
  Result := False;
  SourceEditor := ideGetActiveSourceEditor;
  if (SourceEditor <> nil) and (SourceEditor.EditViewCount > 0) then
  begin
    EditReader := SourceEditor.CreateReader;
    Text := ideGetEditorText(EditReader);
    Result := True;
  end;
end;

function ideSetActiveEditorText(const Text: String): Boolean;
var
  SourceEditor: IOTASourceEditor;
  EditWriter: IOTAEditWriter;
begin
  Result := False;
  SourceEditor := ideGetActiveSourceEditor;
  if (SourceEditor <> nil) and (SourceEditor.EditViewCount > 0) then
  begin
    EditWriter := SourceEditor.CreateUndoableWriter;
    ideSetEditorText(EditWriter, Text);
    Result := True;
  end;
end;

end.
