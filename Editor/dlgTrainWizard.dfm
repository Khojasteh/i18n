object RepositoryTrainWizardDialog: TRepositoryTrainWizardDialog
  Left = 0
  Top = 0
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  Caption = 'Auto Translation Trainer'
  ClientHeight = 418
  ClientWidth = 458
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  ParentBiDiMode = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 458
    Height = 376
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    object Notebook: TNotebook
      Left = 0
      Top = 0
      Width = 458
      Height = 376
      Align = alClient
      TabOrder = 0
      OnPageChanged = NotebookPageChanged
      object TPage
        Left = 0
        Top = 0
        Caption = 'Training Sources'
        object Header1: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title1: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 442
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Choose Training Sources'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
            ExplicitWidth = 150
          end
          object Subtitle1: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 442
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Which type of files should be used for the training?'
            ShowAccelChar = False
            ExplicitWidth = 244
          end
        end
        object cb_i18n: TCheckBox
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 414
          Height = 17
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Translation Catalog Files (*.i18n)'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = SourceTypeClick
        end
        object cb_m17n: TCheckBox
          AlignWithMargins = True
          Left = 22
          Top = 96
          Width = 414
          Height = 17
          Margins.Left = 22
          Margins.Top = 8
          Margins.Right = 22
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Translation Repository Files (*.m17n)'
          TabOrder = 2
          OnClick = SourceTypeClick
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Search Path'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblFolder: TLabel
          AlignWithMargins = True
          Left = 22
          Top = 65
          Width = 414
          Height = 13
          Margins.Left = 22
          Margins.Top = 16
          Margins.Right = 22
          Align = alTop
          Caption = 'Search Folder:'
          FocusControl = Folders
          ExplicitWidth = 70
        end
        object Header2: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title2: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 115
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Select Search Path'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle2: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 187
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Where to look for the training sources?'
            ShowAccelChar = False
          end
        end
        object Folders: TShellTreeView
          AlignWithMargins = True
          Left = 22
          Top = 105
          Width = 414
          Height = 230
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 0
          ObjectTypes = [otFolders]
          Root = 'rfMyComputer'
          UseShellImages = True
          Align = alClient
          AutoRefresh = False
          HideSelection = False
          Indent = 19
          ParentColor = False
          RightClickSelect = True
          ShowRoot = False
          TabOrder = 2
          OnChange = FoldersChange
          OnEditing = FoldersEditing
        end
        object edFolder: TEdit
          AlignWithMargins = True
          Left = 22
          Top = 81
          Width = 414
          Height = 21
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Align = alTop
          BiDiMode = bdLeftToRight
          ParentBiDiMode = False
          TabOrder = 1
          OnChange = edFolderChange
          OnExit = edFolderExit
        end
        object cbIncludeSubfolders: TCheckBox
          AlignWithMargins = True
          Left = 22
          Top = 343
          Width = 414
          Height = 17
          Margins.Left = 22
          Margins.Top = 8
          Margins.Right = 22
          Margins.Bottom = 16
          Align = alBottom
          Caption = 'Including subfolders'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Before Process'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object LastNotice: TLabel
          AlignWithMargins = True
          Left = 22
          Top = 65
          Width = 403
          Height = 26
          Margins.Left = 22
          Margins.Top = 16
          Margins.Right = 22
          Margins.Bottom = 8
          Align = alTop
          Caption =
            'Click <Train> to continue with the training, or click <Back> if ' +
            'you want to review or change any settings.'
          ShowAccelChar = False
          WordWrap = True
        end
        object Header3: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title3: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 90
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Ready to Train'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle3: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 189
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Program is now ready to begin training.'
            ShowAccelChar = False
          end
        end
        object Summary: TRichEdit
          AlignWithMargins = True
          Left = 22
          Top = 99
          Width = 414
          Height = 255
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 22
          Align = alClient
          Font.Charset = ARABIC_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Processing'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object PleaseWait: TLabel
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 267
          Height = 13
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Please wait while the auto translation is being trained...'
          ShowAccelChar = False
          WordWrap = True
        end
        object Header4: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title4: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 49
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Training'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle4: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 171
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Auto translation is being trained...'
            ShowAccelChar = False
          end
        end
        object ProgressBar: TProgressBar
          AlignWithMargins = True
          Left = 22
          Top = 88
          Width = 414
          Height = 18
          Margins.Left = 22
          Margins.Top = 2
          Margins.Right = 22
          Margins.Bottom = 0
          Align = alTop
          Style = pbstMarquee
          TabOrder = 1
        end
        object SearchDetails: TPanel
          AlignWithMargins = True
          Left = 22
          Top = 316
          Width = 414
          Height = 28
          Margins.Left = 22
          Margins.Top = 32
          Margins.Right = 22
          Margins.Bottom = 32
          Align = alBottom
          AutoSize = True
          BevelOuter = bvNone
          Color = clWindow
          DoubleBuffered = True
          ParentBackground = False
          ParentDoubleBuffered = False
          ShowCaption = False
          TabOrder = 2
          object lblSearchingFolder: TLabel
            AlignWithMargins = True
            Left = 0
            Top = 0
            Width = 152
            Height = 13
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 2
            Align = alTop
            Caption = 'Searching for translation files...'
            ShowAccelChar = False
          end
          object SearchingFolder: TLabel
            Left = 0
            Top = 15
            Width = 414
            Height = 13
            Align = alTop
            AutoSize = False
            BiDiMode = bdLeftToRight
            EllipsisPosition = epPathEllipsis
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentBiDiMode = False
            ParentFont = False
            ShowAccelChar = False
            Transparent = True
            ExplicitTop = 13
            ExplicitWidth = 3
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'After Process'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object TotalPhraseCount: TLabel
          AlignWithMargins = True
          Left = 22
          Top = 347
          Width = 266
          Height = 13
          Margins.Left = 22
          Margins.Top = 8
          Margins.Right = 22
          Margins.Bottom = 16
          Align = alBottom
          Caption = 'Currently the translation repository contains {0} in {1}.'
          ShowAccelChar = False
          WordWrap = True
        end
        object Header5: TPanel
          Left = 0
          Top = 0
          Width = 458
          Height = 49
          Align = alTop
          AutoSize = True
          BevelEdges = [beBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
          object Title5: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 99
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Congratulations'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle5: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 213
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'The training process completed successfully.'
            ShowAccelChar = False
          end
        end
        object UsedFiles: TListView
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 414
          Height = 268
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 0
          Align = alClient
          BiDiMode = bdLeftToRight
          Columns = <
            item
              Caption = 'Processed File Name'
              Width = 265
            end
            item
              Alignment = taRightJustify
              Caption = '# of New Phrases'
              Width = 120
            end>
          ColumnClick = False
          ReadOnly = True
          RowSelect = True
          ParentBiDiMode = False
          ParentShowHint = False
          ShowHint = True
          SmallImages = ImageList
          TabOrder = 1
          ViewStyle = vsReport
          OnAdvancedCustomDraw = UsedFilesAdvancedCustomDraw
          OnDblClick = UsedFilesDblClick
          OnInfoTip = UsedFilesInfoTip
        end
      end
    end
  end
  object ButtonsPanel: TPanel
    Left = 0
    Top = 376
    Width = 458
    Height = 42
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      458
      40)
    object btnFinish: TButton
      Left = 354
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Finish'
      Default = True
      ModalResult = 1
      TabOrder = 4
      Visible = False
    end
    object btnCancel: TButton
      Left = 354
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
      OnClick = btnCancelClick
    end
    object btnStart: TButton
      Left = 253
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Train'
      Default = True
      TabOrder = 2
      Visible = False
      OnClick = btnStartClick
    end
    object btnBack: TButton
      Left = 152
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Back'
      TabOrder = 0
      Visible = False
      OnClick = btnBackClick
    end
    object btnNext: TButton
      Left = 253
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Next'
      Enabled = False
      TabOrder = 1
      OnClick = btnNextClick
    end
  end
  object Translator: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      '.Caption'
      'btnBack.Caption'
      'btnCancel.Caption'
      'btnFinish.Caption'
      'btnNext.Caption'
      'btnStart.Caption'
      'cbIncludeSubfolders.Caption'
      'cb_i18n.Caption'
      'cb_m17n.Caption'
      'LastNotice.Caption'
      'lblFolder.Caption'
      'lblSearchingFolder.Caption'
      'Notebook.Pages[0]'
      'Notebook.Pages[1]'
      'PleaseWait.Caption'
      'Subtitle1.Caption'
      'Subtitle2.Caption'
      'Subtitle3.Caption'
      'Subtitle4.Caption'
      'Subtitle5.Caption'
      'Title1.Caption'
      'Title2.Caption'
      'Title3.Caption'
      'Title4.Caption'
      'Title5.Caption'

        'TotalPhraseCount.Caption={0} = Number of Phrases (For example, 5' +
        ' phrases)'#13#10'{1} = Number of Groups (For example, one synonymical ' +
        'group)'
      'UsedFiles.Columns[0].Caption'
      'UsedFiles.Columns[1].Caption')
    Translatables.Literals = (
      '4027014469927D35A21B9B88D3113542'
      'EF8158DEA0801EC79B3BBAD3747BF83A'
      '4E2F027ADF000CAEBC05F1820DDCA978'
      '#A8B9E1F07AA3632B1AA07F6D10AE4E8D'
      '#F77E784B19DAEC317E926B7EEB0D76D1')
    OnLoaded = TranslatorLoaded
    Left = 367
    Top = 8
  end
  object ImageList: TImageList
    Height = 20
    Left = 399
    Top = 8
    Bitmap = {
      494C010102000800D40010001400FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001400000001002000000000000014
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B3B3B34C9595
      9584939393869393938693939386939393869393938693939386939393869393
      9386939393869393938695959584B3B3B34C0000000000000000000000000000
      00000000000000000000E6E6E631A2A2A2A88D8D8DBFD3D3D34A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000095959584F9F9
      F9F5FCFCFCFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFDF9F9F9F3959595840000000000000000FBFCFB05A8CC
      AB7172AB76F093AE7FFF8DB183FF63A16DFF7D9F7EFF9C9C9CB0FAFAFA08EAEA
      EA22000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEFEFE017E9885A2A0C7
      AAFF71AA7FFF5A9D6BFF72AB80FFA1C8AAFFD6E6DAFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFD9393938600000000FCFDFC03BDDDC15E7EC4
      94EBB5D5A1FFD3B57EFFC3B17AFF85BF91FF66A87AFF809981BEB2B5B2826969
      69F79D9D9D9B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C2DAC85D6CA279DC73AA
      7BFFA5B48AFFBAB58CFF90B991FF599D6BFF78AD84FFC0D9C7FFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFF9393938600000000ECF6ED1B95D0A2B57BC9
      90FF9FB76CFFD6A65CFFCBA155FF82A058FF79B58DFF5E9669FD8E9B8EFBD2D2
      D2FF949494FFC8C8C85C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D7E7DC3C76AD84D17EC299FFAECA
      8DFFCEAD6DFFD9AB67FFA1A466FF6CB889FF69A67EFF77AC83FFD4E4D9FFFAFA
      FAFFFAFAFAFFFAFAFAFFFCFCFCFF9393938600000000E2F3E53390D5A3E79CD3
      8AFFBFB064FFD9A653FFCF9547FFBA8E47FF85A56BFF79B790FF9AB39DFFD3D3
      D3FF838383FECFCFCF5D00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2C8AC8E77B78CFF73C47BFFA9B4
      69FFD5A457FFE1A855FFBB974CFF89A055FF7BB187FF5B9B6CFFA0C7AAFFFAFA
      FAFFF8F8F8FFF8F8F8FFFCFCFCFF9393938600000000E1F4E63B8DD99FF69DD9
      85FFCAB971FFD0A658FFBEB161FFB2AB5AFF759F55FF80BD96FF93B096FFBCBC
      BCFFBCBCBC850000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000072AC81D88CD1A0FFAFD582FFC8AD
      62FFDBAA56FFD3A04FFFBF8844FFBF904EFF82A265FF7AB995FF72AB81FFF9F9
      F9FFF9F9F9FFF8F8F8FFFCFCFCFF9393938600000000E1F4E63E8FDBA0FD93E1
      88FFD0E2ABFFC7B569FFA5D46DFF70D649FF5AB951FF87C29AFF98B59CFFC7C7
      C7FF828282EE7F7F7FDDC3C3C366000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005A9E6CFE8EDB9EFF94D978FFCAC5
      81FFCDA558FFBFBB6FFFABCF6BFF9BB854FF63A451FF8EC8A8FF5A9D6BFFF9F9
      F9FFF6F6F6FFF6F6F6FFFCFCFCFF9393938600000000E0F2E43F98DBABFFB3ED
      ABFFD0ECBDFFD4C682FFB7C069FF82D661FF79D876FF85C099FF9CB7A0FFC7C7
      C7FFCDCDCDFFC2C2C2FEBBBBBB7E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000073AC82D893D7A2FFA0E78BFFD5EA
      BCFFCCB369FFC4C876FF7CDB5AFF5EDA44FF60C25AFF87C19EFF72AA80FFF6F6
      F6FFF3F3F3FFF2F2F2FFFCFCFCFF9393938600000000E7F2E93F9CD5ABFEB5EE
      C2FFC3EFBEFFD6DFAEFFD8C786FFC6C07CFFAAE2AFFF76B586FCB3C0B4FFCACA
      CAFFD2D2D2FFCCCCCCFEC0C0C07E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A1C8AC8F84C29AFFBAF1AAFFC9EC
      B9FFD8D599FFD6BF79FFB6BB66FF92D670FF8ADF90FF6BAB7FFF9EC4A7FFF2F2
      F2FFEFEFEFFFEDEDEDFFFCFCFCFF9393938600000000F5F6F533B4D3B8E2A0DE
      B5FCD6F3E5FFD6E7C6FFDCDAAAFFD1CEA0FF8EC791FF93B798E9CECECEFFD4D4
      D4FFA5A5A5EEA7A7A7DDD5D5D566000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D7E7DC3C77AF86D2ADE2C1FFCDF2
      CFFFD4E4BDFFDBDBA2FFDAC283FFD1C28BFF99CAA1FF76AD84FFCDDDD2FFECEC
      ECFFEAEAEAFFE6E6E6FFFCFCFCFF939393860000000000000000FEFEFE02B4DE
      BA968DD59EFFA5DCADFFA5D49FFF86C78CFF8AB48FFFCFCFCFFFD0D0D0FFC7C7
      C7FFCBCBCB850000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C2DAC85D6EA67DDC90C3
      A2FFB8DABBFFD4DCB4FFB9CB9EFF83B281FF77AE85FFBAD4C1FFEBEBEBFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFF939393860000000000000000F5F5F53ED7D7
      D7DDE4E4E4FFDFDFDFFFE2E2E2FFE0E0E0FFDDDDDDFFDFDFDFFFD9D9D9FFD3D3
      D3FF8D8D8DFED2D2D25D00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEFEFE017E9885A2A0C8
      AAFF71AB80FF5A9E6CFF72AB80FF9EC4A8FFCFDFD3FFF0F0F0FFEAEAEAFFFCFC
      FCFFF6F6F6FFF4F4F4FF909090BDC7C7C7380000000000000000F6F6F63DE4E4
      E4DEF1F1F1FFDDDDDDFCCECECEF9DADADAFEDDDDDDFFBFBFBFFBC9C9C9F9E6E6
      E6FFC4C4C4FFDCDCDC5C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000093939386FBFB
      FBFDF4F4F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFC
      FCFFE7E7E7FF8F8F8FBDC7C7C738FDFDFD02000000000000000000000000EEEE
      EE74D8D8D8FDE0E0E0B3E7E7E780E0E0E0F9E5E5E5FFD2D2D2B3DDDDDD80BABA
      BAF7D0D0D09B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000095959584F8F8
      F8F0FBFBFBFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8
      F8FF8F8F8FBDC7C7C738FDFDFD02000000000000000000000000000000000000
      0000F9F9F929FDFDFD0BEAEAEA76E4E4E4F9EAEAEAFFD0D0D0B0FDFDFD08F6F6
      F622000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B4B4B44B9595
      9584939393869393938693939386939393869393938693939386939393869393
      9386C7C7C738FDFDFD0200000000000000000000000000000000000000000000
      00000000000000000000F7F7F731E2E2E2A8DBDBDBBFF0F0F04A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000140000000100010000000000A00000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      C000FC3F00000000C000C00F0000000080008007000000008000800300000000
      0000800300000000000080070000000000008001000000000000800100000000
      000080010000000000008001000000000000C007000000008000C00300000000
      8000C00300000000C000E00700000000C001F00F00000000C003FC3F00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
