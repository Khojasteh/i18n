object ReplaceDialog: TReplaceDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Replace'
  ClientHeight = 236
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    422
    236)
  PixelsPerInch = 96
  TextHeight = 13
  object SearchPhraseLabel: TLabel
    Left = 16
    Top = 8
    Width = 62
    Height = 13
    Caption = 'Text to Find:'
    FocusControl = SearchPhrase
  end
  object ReplacePhraseLabel: TLabel
    Left = 16
    Top = 56
    Width = 73
    Height = 13
    Caption = 'Replace Width:'
    FocusControl = ReplacePhrase
  end
  object SearchPhrase: TEdit
    Left = 16
    Top = 23
    Width = 390
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = SearchPhraseChange
    ExplicitWidth = 380
  end
  object btnOK: TButton
    Left = 250
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 5
    ExplicitTop = 201
  end
  object btnCancel: TButton
    Left = 331
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
    ExplicitTop = 201
  end
  object gbOptions: TGroupBox
    Left = 16
    Top = 105
    Width = 117
    Height = 81
    Caption = ' Options '
    TabOrder = 2
    DesignSize = (
      117
      81)
    object cbOptionMatchCase: TCheckBox
      Left = 16
      Top = 24
      Width = 85
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Match Case'
      TabOrder = 0
    end
    object cbOptionWholeWord: TCheckBox
      Left = 16
      Top = 48
      Width = 85
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Whole World'
      TabOrder = 1
    end
  end
  object gbDirection: TGroupBox
    Left = 289
    Top = 105
    Width = 117
    Height = 81
    Anchors = [akTop, akRight]
    Caption = ' Direction '
    TabOrder = 4
    ExplicitLeft = 279
    DesignSize = (
      117
      81)
    object rbDirectionUp: TRadioButton
      Left = 16
      Top = 24
      Width = 85
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Up'
      TabOrder = 0
    end
    object rbDirectionDown: TRadioButton
      Left = 16
      Top = 48
      Width = 85
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Down'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
  object gbOrigin: TGroupBox
    Left = 153
    Top = 105
    Width = 117
    Height = 81
    Anchors = [akTop]
    Caption = ' Origin '
    TabOrder = 3
    ExplicitLeft = 148
    DesignSize = (
      117
      81)
    object rbOriginBeginning: TRadioButton
      Left = 16
      Top = 24
      Width = 85
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'From Begining'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbOriginCurrent: TRadioButton
      Left = 16
      Top = 48
      Width = 85
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'From Current'
      TabOrder = 1
    end
  end
  object ReplacePhrase: TEdit
    Left = 16
    Top = 71
    Width = 390
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    ExplicitWidth = 380
  end
  object Translator: TTranslator
    Localizer = MainForm.Localizer
    Left = 16
    Top = 195
    Translatables.Properties = (
      '.Caption'
      'btnCancel.Caption'
      'btnOK.Caption'
      'cbOptionMatchCase.Caption'
      'cbOptionWholeWord.Caption'
      'gbDirection.Caption'
      'gbOptions.Caption'
      'gbOrigin.Caption'
      'rbDirectionDown.Caption'
      'rbDirectionUp.Caption'
      'rbOriginBeginning.Caption'
      'rbOriginCurrent.Caption'
      'ReplacePhraseLabel.Caption'
      'SearchPhraseLabel.Caption')
  end
end
