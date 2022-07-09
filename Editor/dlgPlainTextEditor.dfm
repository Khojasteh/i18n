object PlainTextEditorDialog: TPlainTextEditorDialog
  Left = 0
  Top = 0
  ActiveControl = Memo
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  BorderWidth = 8
  ClientHeight = 283
  ClientWidth = 462
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  ParentBiDiMode = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonsPanel: TPanel
    Left = 0
    Top = 250
    Width = 462
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      462
      33)
    object btnCancel: TButton
      Left = 367
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 266
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 462
    Height = 250
    Align = alClient
    BiDiMode = bdLeftToRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBiDiMode = False
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      'btnCancel.Caption'
      'btnOK.Caption')
    Left = 2
    Top = 254
  end
end
