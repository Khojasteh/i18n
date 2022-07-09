object SearchDialog: TSearchDialog
  Left = 0
  Top = 0
  ActiveControl = TargetPhrase
  AutoSize = True
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Search'
  ClientHeight = 367
  ClientWidth = 492
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  ParentBiDiMode = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl: TTabControl
    Left = 0
    Top = 0
    Width = 492
    Height = 334
    Align = alTop
    TabOrder = 1
    Tabs.Strings = (
      'Find'
      'Replace')
    TabIndex = 0
    OnChange = TabControlChange
    object ClientPanel: TPanel
      AlignWithMargins = True
      Left = 14
      Top = 32
      Width = 462
      Height = 285
      Margins.Left = 10
      Margins.Top = 8
      Margins.Right = 12
      Margins.Bottom = 12
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      ParentColor = True
      ShowCaption = False
      TabOrder = 0
      object TargetPhraseLabel: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 462
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Text to Find:'
        FocusControl = TargetPhrase
        ExplicitWidth = 62
      end
      object ReplacePhraseLabel: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 45
        Width = 462
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Replace With:'
        FocusControl = ReplacePhrase
        ExplicitWidth = 67
      end
      object TargetPhrase: TComboBox
        AlignWithMargins = True
        Left = 0
        Top = 16
        Width = 462
        Height = 21
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 8
        Align = alTop
        TabOrder = 0
        OnChange = TargetPhraseChange
      end
      object gbWhere: TGroupBox
        AlignWithMargins = True
        Left = 0
        Top = 179
        Width = 462
        Height = 106
        Margins.Left = 0
        Margins.Top = 8
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = ' Where '
        TabOrder = 3
        DesignSize = (
          462
          106)
        object cbWhereOriginal: TCheckBox
          Left = 16
          Top = 17
          Width = 211
          Height = 28
          Caption = 'Original Text'
          TabOrder = 0
        end
        object cbWhereTranslated: TCheckBox
          Tag = 1
          Left = 16
          Top = 44
          Width = 211
          Height = 28
          Caption = 'Translated Text'
          TabOrder = 1
        end
        object cbWhereComment: TCheckBox
          Tag = 3
          Left = 233
          Top = 17
          Width = 211
          Height = 28
          Anchors = [akTop, akRight]
          Caption = 'Developer'#39's Comment'
          TabOrder = 3
        end
        object cbWhereNote: TCheckBox
          Tag = 4
          Left = 233
          Top = 44
          Width = 211
          Height = 28
          Anchors = [akTop, akRight]
          Caption = 'Translator'#39's Note'
          TabOrder = 4
        end
        object cbWhereName: TCheckBox
          Tag = 2
          Left = 16
          Top = 71
          Width = 211
          Height = 28
          Caption = 'Property Name'
          TabOrder = 2
        end
      end
      object ReplacePhrase: TComboBox
        AlignWithMargins = True
        Left = 0
        Top = 61
        Width = 462
        Height = 21
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 8
        Align = alTop
        TabOrder = 1
      end
      object FlowPanel: TPanel
        Left = 0
        Top = 90
        Width = 462
        Height = 81
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        ParentColor = True
        ShowCaption = False
        TabOrder = 2
        DesignSize = (
          462
          81)
        object gbTypes: TGroupBox
          Left = 0
          Top = 0
          Width = 146
          Height = 81
          Caption = ' Options '
          TabOrder = 0
          object cbTypeMatchCase: TCheckBox
            AlignWithMargins = True
            Left = 18
            Top = 17
            Width = 110
            Height = 28
            Margins.Left = 16
            Margins.Top = 2
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Match Case'
            TabOrder = 0
            WordWrap = True
          end
          object cbTypeWholeWord: TCheckBox
            AlignWithMargins = True
            Left = 18
            Top = 45
            Width = 110
            Height = 28
            Margins.Left = 16
            Margins.Top = 0
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Whole Word'
            TabOrder = 1
            WordWrap = True
          end
        end
        object gbOrigin: TGroupBox
          Left = 158
          Top = 0
          Width = 146
          Height = 81
          Anchors = [akTop]
          Caption = ' Origin '
          TabOrder = 1
          object rbOriginEntire: TRadioButton
            AlignWithMargins = True
            Left = 18
            Top = 45
            Width = 110
            Height = 28
            Margins.Left = 16
            Margins.Top = 0
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Entire Scope'
            TabOrder = 1
            WordWrap = True
          end
          object rbOriginCurrent: TRadioButton
            AlignWithMargins = True
            Left = 18
            Top = 17
            Width = 110
            Height = 28
            Margins.Left = 16
            Margins.Top = 2
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Current Item'
            Checked = True
            TabOrder = 0
            TabStop = True
            WordWrap = True
          end
        end
        object gbDirection: TGroupBox
          Left = 316
          Top = 0
          Width = 146
          Height = 81
          Anchors = [akTop, akRight]
          Caption = ' Direction '
          TabOrder = 2
          object rbDirBackward: TRadioButton
            AlignWithMargins = True
            Left = 18
            Top = 45
            Width = 110
            Height = 28
            Margins.Left = 16
            Margins.Top = 0
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Backward'
            TabOrder = 1
            WordWrap = True
          end
          object rbDirForward: TRadioButton
            AlignWithMargins = True
            Left = 18
            Top = 17
            Width = 110
            Height = 28
            Margins.Left = 16
            Margins.Top = 2
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Forward'
            Checked = True
            TabOrder = 0
            TabStop = True
            WordWrap = True
          end
        end
      end
    end
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 334
    Width = 492
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      492
      33)
    object btnCancel: TButton
      Left = 396
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
      Left = 294
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
    end
  end
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      '.Caption'
      'btnCancel.Caption'
      'btnOK.Caption'
      'cbTypeMatchCase.Caption'
      'cbTypeWholeWord.Caption'
      'cbWhereComment.Caption'
      'cbWhereName.Caption'
      'cbWhereNote.Caption'
      'cbWhereOriginal.Caption'
      'cbWhereTranslated.Caption'
      'gbDirection.Caption'
      'gbOrigin.Caption'
      'gbTypes.Caption'
      'gbWhere.Caption'
      'rbDirBackward.Caption'
      'rbDirForward.Caption'
      'rbOriginCurrent.Caption'
      'rbOriginEntire.Caption'
      'ReplacePhraseLabel.Caption'
      'TabControl.Tabs[0]'
      'TabControl.Tabs[1]'
      'TargetPhraseLabel.Caption')
    Left = 2
    Top = 337
  end
end
