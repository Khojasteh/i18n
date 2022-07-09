object RenameDialog: TRenameDialog
  Left = 0
  Top = 0
  AutoSize = True
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  BorderWidth = 8
  ClientHeight = 146
  ClientWidth = 270
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  ParentBiDiMode = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object CurNameLabel: TLabel
    AlignWithMargins = True
    Left = 0
    Top = 4
    Width = 270
    Height = 13
    Margins.Left = 0
    Margins.Top = 4
    Margins.Right = 0
    Align = alTop
    Caption = 'Current Name:'
    FocusControl = CurName
    ExplicitWidth = 71
  end
  object NewNameLabel: TLabel
    AlignWithMargins = True
    Left = 0
    Top = 49
    Width = 270
    Height = 13
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 0
    Align = alTop
    Caption = 'New Name:'
    FocusControl = NewName
    ExplicitWidth = 55
  end
  object ErrorMsg: TImageLabel
    AlignWithMargins = True
    Left = 0
    Top = 94
    Width = 270
    Height = 16
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    BorderColor = 9816831
    BorderWidth = 1
    Color = 15334142
    ImageIndex = 0
    Images = DM.MsgImages
    Padding.Left = 4
    Padding.Top = 2
    Padding.Right = 4
    Padding.Bottom = 2
    ParentColor = False
    ShowAccelChar = False
    Transparent = False
    Visible = False
    WordWrap = True
    ExplicitWidth = 20
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 110
    Width = 270
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      270
      36)
    object btnCancel: TButton
      Left = 175
      Top = 11
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 74
      Top = 11
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object CurName: TEdit
    Left = 0
    Top = 20
    Width = 270
    Height = 21
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    TabStop = False
    Align = alTop
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
    OnChange = CurNameChange
  end
  object NewName: TEdit
    Left = 0
    Top = 65
    Width = 270
    Height = 21
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    TabOrder = 2
    OnChange = NewNameChange
  end
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      'btnCancel.Caption'
      'btnOK.Caption'
      'CurNameLabel.Caption'
      'NewNameLabel.Caption')
    Translatables.Literals = (
      '5E8124E7537A178107438DF9AC1D6A49'
      'BEC14B9F2C3E6EB0E1C763E7C486009C'
      '430E3F6DD0BAD3C10D6F8781CF740F2E')
    Left = 1
    Top = 117
  end
end
