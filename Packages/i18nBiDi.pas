{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  Internationalization and Localization for Delphi                            }
{                                                                              }
{  Copyright (c) Kambiz Khojasteh                                              }
{  https://github.com/khojasteh/i18n                                           }
{                                                                              }
{------------------------------------------------------------------------------}

/// This unit contains code for flipping controls and updating properties based
/// on the direction of the user interface language.
unit i18nBiDi;

{$I DELPHIAREA.INC}

interface

uses
  Windows, Messages, SysUtils, TypInfo, Classes, Controls, i18nHashList;

type

  {$region 'xmldoc'}
  /// <summary>
  /// BiDi provides a set of class methods for horizontal flipping of the
  /// bi-directional controls.
  /// </summary>
  {$endregion}
  BiDi = class
  public
    type
      {$region 'xmldoc'}
      /// <summary>
      /// Represents a callback function that receives the runtime type information of
      /// the property that should be updated when its owner object is flipped.
      /// </summary>
      /// <param name="Instance">
      /// The object whose properties are examined.
      /// </param>
      /// <param name="Prop">
      /// The runtime type information of the property to update.
      /// </param>
      /// <seealso cref="RegisterProperty"/>
      /// <seealso cref="FlipProperties"/>
      /// <seealso cref="FlipChildren"/>
      {$endregion}
      TPropertyFlipFunc = procedure(Instance: TObject; Prop: PPropInfo);
  private
    class var PropFlipFuncs: TKeyLookup<PTypeInfo,TPropertyFlipFunc>;
    class procedure Cleanup;  static;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Performs special actions on a specified common control to toggle its
    /// direction.
    /// </summary>
    /// <param name="Control">
    /// The target common control.
    /// </param>
    /// <seealso cref="FlipChildren"/>
    {$endregion}
    class procedure FlipCommonControl(Control: TWinControl); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Applies the registered property flip functions to the properties of a
    /// specified object.
    /// </summary>
    /// <param name="Instance">
    /// The target object.
    /// </param>
    /// <seealso cref="RegisterProperty"/>
    {$endregion}
    class procedure FlipProperties(Instance: TObject); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Updates the ExStyle of a specified window handle to mirror its layout
    /// according to a specified <see cref="TBiDiMode"/> value.
    /// </summary>
    /// <param name="hWnd">
    /// The window handle of the control.
    /// </param>
    /// <param name="BiDiMode">
    /// The <see cref="TBiDiMode"/> that determines the requested direction of
    /// the control's layout.
    /// </param>
    {$endregion}
    class procedure UpdateLayout(hWnd: HWND; BiDiMode: TBiDiMode); static;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Registers a callback function that will be responsible for updating a particular
    /// property type when the owner object is flipped.
    /// </summary>
    /// <param name="PropType">
    /// The type information of the property that the handler is registered for.
    /// </param>
    /// <param name="FlipFunc">
    /// The callback function that handles flipping of the property.
    /// </param>
    /// <seealso cref="FlipChildren"/>
    /// <seealso cref="UnregisterProperty"/>
    {$endregion}
    class procedure RegisterProperty(PropType: PTypeInfo; FlipFunc: TPropertyFlipFunc); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Unregisters a callback function that was responsible for updating a particular
    /// property type when the owner object was flipped.
    /// </summary>
    /// <param name="PropType">
    /// The type information of the property that the handler was registered for.
    /// </param>
    /// <seealso cref="RegisterProperty"/>
    {$endregion}
    class procedure UnregisterProperty(PropType: PTypeInfo); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Horizontally flips position of child controls of a specified control.
    /// </summary>
    /// <remarks>
    /// When <see cref="ParentBidiMode"/> property of a child control is <see langword="true"/>,
    /// this function also flips the content of the child control and updates its direction-dependent
    /// properties.
    /// </remarks>
    /// <param name="Control">
    /// The control to flip its children.
    /// </param>
    /// <seealso cref="RegisterProperty"/>
    {$endregion}
    class procedure FlipChildren(Control: TWinControl); static;
    {$region 'xmldoc'}
    /// <summary>
    /// Swaps Left and Right properties of a specified <see cref="TMargins"/> object.
    /// </summary>
    /// <param name="Margins">
    /// The <see cref="TMargins"/> object that should be flipped.
    /// </param>
    {$endregion}
    class procedure FlipMargins(Margins: TMargins); inline; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Toggles akLeft and akRight members of a specified <see cref="TAnchors"/>
    /// value if it has one and only one of these members.
    /// </summary>
    /// <param name="Anchors">
    /// The <see cref="TAnchors"/> value that should be flipped.
    /// </param>
    /// <returns>
    /// The new <see cref="TAnchors"/> value.
    /// </returns>
    {$endregion}
    class function FlipAnchors(Anchors: TAnchors): TAnchors; inline; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Changes alLeft and alRight values of a specified <see cref="TAlign"/> value
    /// respectively to alRight and alLeft if the <see cref="TAlign"/> value is one
    /// of these two values.
    /// </summary>
    /// <param name="Align">
    /// The <see cref="TAlign"/> value that should be flipped.
    /// </param>
    /// <returns>
    /// The new <see cref="TAlign"/> value.
    /// </returns>
    {$endregion}
    class function FlipAlign(Align: TAlign): TAlign; inline; static;
    {$region 'xmldoc'}
    /// <summary>
    /// Changes taLeftJustify and taRightJustify values of a specified
    /// <see cref="TAlignment"/> value respectively to taRightJustify and
    /// taLeftJustify if the <see cref="TAlignment"/> value is one of these two
    /// values.
    /// </summary>
    /// <param name="Alignment">
    /// The <see cref="TAlignment"/> value that should be flipped.
    /// </param>
    /// <returns>
    /// The new <see cref="TAlignment"/> value.
    /// </returns>
    {$endregion}
    class function FlipAlignment(Alignment: TAlignment): TAlignment; inline; static;
  end;

implementation

uses
  Types, ComCtrls, CommCtrl, Buttons;

type
  TControlHack = class(TControl);
  TWinControlHack = class(TWinControl);

{ BiDi }

class procedure BiDi.FlipChildren(Control: TWinControl);
var
  ChildControl: TControl;
  I, ParentWidth: Integer;
begin
  Control.DisableAlign;
  try
    ParentWidth := Control.ClientWidth;
    for I := 0 to Control.ControlCount - 1 do
    begin
      ChildControl := Control.Controls[I];
      with TControlHack(ChildControl) do
      begin
        Left := ParentWidth - Width - Left;
        if ParentBiDiMode then
        begin
          FlipProperties(ChildControl);
          if ChildControl is TWinControl then
          begin
            FlipCommonControl(TWinControl(ChildControl));
            if TWinControl(ChildControl).ControlCount <> 0 then
              FlipChildren(TWinControl(ChildControl));
          end;
        end;
        Perform(CM_ALLCHILDRENFLIPPED, 0, 0);
        FlipMargins(Margins);
        Align := FlipAlign(Align);
        Anchors := FlipAnchors(Anchors);
      end;
    end;
  finally
    Control.EnableAlign;
  end;
end;

class procedure BiDi.FlipCommonControl(Control: TWinControl);
var
  I: Integer;
  hHeader: HWND;
begin
  if Control is TCustomTabControl then
  begin
    {$IFDEF COMPILER2010_UP}
    // Just adjust ExStyle of the window to mirror its layout.
    UpdateLayout(Control.Handle, Control.BiDiMode);
    {$ENDIF}
  end
  else if Control is TCustomTreeView then
  begin
    // Just adjust ExStyle of the window to mirror its layout.
    UpdateLayout(Control.Handle, Control.BiDiMode);
  end
  else if Control is TCustomListView then
  begin
    // Adjust ExStyle of the window to mirror its layout.
    UpdateLayout(Control.Handle, Control.BiDiMode);
    // If the ListView has a header, update its ExStyle too.
    hHeader := ListView_GetHeader(Control.Handle);
    if hHeader <> 0 then
      UpdateLayout(hHeader, Control.BiDiMode);
    // Because the layout is mirrored, there is no need to flip alignment of
    // list columns. So, flip them back to original.
    with TListView(Control) do
    begin
      Columns.BeginUpdate;
      for I := 0 to Columns.Count - 1 do
        Columns[I].Alignment := FlipAlignment(Columns[I].Alignment);
      Columns.EndUpdate;
    end;
  end
  else if Control is TCustomStatusBar then
  begin
    if not Control.HandleAllocated and (Control.BidiMode <> bdLeftToRight) then
    begin
      // If the control's BiDiMode is bdRightToLeft and the control's handle is
      // not allocated yet, caption of the panels render in opposite direction.
      // This needs a workaround!
    end;
    // Adjust ExStyle of the window to mirror its layout.
    UpdateLayout(Control.Handle, Control.BiDiMode);
    // Because the layout is mirrored, there is no need to flip alignment of
    // status panels. So, flip them back to original.
    with TCustomStatusBar(Control) do
    begin
      Panels.BeginUpdate;
      for I := 0 to Panels.Count - 1 do
        Panels[I].Alignment := FlipAlignment(Panels[I].Alignment);
      Panels.EndUpdate;
    end;
  end
  else if Control is TCustomHeaderControl then
  begin
    if not Control.HandleAllocated and (Control.BidiMode <> bdLeftToRight) then
    begin
      // If the control's BiDiMode is bdRightToLeft and the control's handle is
      // not allocated yet, caption of the sections render in opposite direction.
      // This is a workaround!
      Control.BidiMode := bdLeftToRight;
      Control.HandleNeeded;
      TControlHack(Control).ParentBiDiMode := True;
    end;
    // Adjust ExStyle of the window to mirror its layout.
    UpdateLayout(Control.Handle, Control.BiDiMode);
    // Because the layout is mirrored, there is no need to flip alignment of
    // header sections. So, flip them back to original.
    with TCustomHeaderControl(Control) do
    begin
      Sections.BeginUpdate;
      for I := 0 to Sections.Count - 1 do
        Sections[I].Alignment := FlipAlignment(Sections[I].Alignment);
      Sections.EndUpdate;
    end;
  end;
end;

class procedure BiDi.UpdateLayout(hWnd: HWND; BiDiMode: TBiDiMode);
const
  RTLOffBits = WS_EX_RIGHT or WS_EX_LEFTSCROLLBAR or WS_EX_RTLREADING;
  RTLOnBits = WS_EX_LAYOUTRTL or WS_EX_NOINHERITLAYOUT;
var
  ExStyle: Integer;
begin
  ExStyle := GetWindowLong(hWnd, GWL_EXSTYLE);
  if BiDiMode = bdRightToLeft then
    ExStyle := (ExStyle and not RTLOffBits) or RTLOnBits
  else
    ExStyle := ExStyle and not RTLOnBits;
  SetWindowLong(hWnd, GWL_EXSTYLE, ExStyle);
end;

class procedure BiDi.FlipMargins(Margins: TMargins);
var
  T: Integer;
begin
  T := Margins.Left;
  Margins.Left := Margins.Right;
  Margins.Right := T;
end;

class function BiDi.FlipAnchors(Anchors: TAnchors): TAnchors;
begin
  if (akLeft in Anchors) xor (akRight in Anchors) then
    Anchors := (Anchors - [akLeft, akRight]) + ([akLeft, akRight] - Anchors);
  Result := Anchors;
end;

class function BiDi.FlipAlign(Align: TAlign): TAlign;
begin
  if Align = alLeft then
    Align := alRight
  else if Align = alRight then
    Align := alLeft;
  Result := Align;
end;

class function BiDi.FlipAlignment(Alignment: TAlignment): TAlignment;
begin
  if Alignment = taLeftJustify then
    Alignment := taRightJustify
  else if Alignment = taRightJustify then
    Alignment := taLeftJustify;
  Result := Alignment;
end;

class procedure BiDi.RegisterProperty(PropType: PTypeInfo; FlipFunc: TPropertyFlipFunc);
begin
  if Assigned(FlipFunc) then
  begin
    if not Assigned(PropFlipFuncs) then
      PropFlipFuncs := TKeyLookup<PTypeInfo,TPropertyFlipFunc>.Create(8);
    PropFlipFuncs.AddOrUpdate(PropType, @FlipFunc);
  end;
end;

class procedure BiDi.UnregisterProperty(PropType: PTypeInfo);
begin
  if Assigned(PropFlipFuncs) then
    PropFlipFuncs.Remove(PropType);
end;

class procedure BiDi.FlipProperties(Instance: TObject);
var
  Count, I: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  FlipFunc: TPropertyFlipFunc;
begin
  if Assigned(PropFlipFuncs) then
  begin
    Count := GetPropList(Instance, PropList);
    if Count <> 0 then
      try
        for I := 0 to Count - 1 do
        begin
          PropInfo := PropList[I];
          if PropFlipFuncs.Retrieve(PropInfo^.PropType^, FlipFunc) then
            FlipFunc(Instance, PropInfo);
        end;
      finally
        FreeMem(PropList);
      end;
  end;
end;

class procedure BiDi.Cleanup;
begin
  if Assigned(PropFlipFuncs) then
    FreeAndNil(PropFlipFuncs);
end;

{ Custom Property Flip Functions }

procedure PropFlipper_ButtonLayout(Instance: TObject; Prop: PPropInfo);
var
  EnumName: String;
begin
  EnumName := GetEnumProp(Instance, Prop);
  if EnumName = 'blGlyphLeft' then
    SetEnumProp(Instance, Prop, 'blGlyphRight')
  else if EnumName = 'blGlyphRight' then
    SetEnumProp(Instance, Prop, 'blGlyphLeft');
end;

procedure PropFlipper_Padding(Instance: TObject; Prop: PPropInfo);
begin
  BiDi.FlipMargins(TPadding(GetObjectProp(Instance, Prop)));
end;

initialization
  BiDi.RegisterProperty(TypeInfo(TButtonLayout), @PropFlipper_ButtonLayout);
  BiDi.RegisterProperty(TypeInfo(TPadding), @PropFlipper_Padding);
finalization
  BiDi.Cleanup;
end.
