object ExportWizardDialog: TExportWizardDialog
  Left = 0
  Top = 0
  BiDiMode = bdLeftToRight
  BorderStyle = bsDialog
  Caption = 'Export'
  ClientHeight = 418
  ClientWidth = 458
  Color = clBtnFace
  DoubleBuffered = True
  ParentFont = True
  OldCreateOrder = False
  ParentBiDiMode = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
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
        Caption = 'Text Domains'
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
            Caption = 'Select Text Domains'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
            ExplicitWidth = 124
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
            Caption = 'Which text domains are included in the export?'
            ShowAccelChar = False
            ExplicitWidth = 225
          end
        end
        object Domains: TListView
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 414
          Height = 283
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 22
          Align = alClient
          BiDiMode = bdLeftToRight
          Checkboxes = True
          Columns = <
            item
              Caption = 'Text Domain'
              Width = 185
            end
            item
              Alignment = taRightJustify
              Caption = '# of Properties'
              Width = 100
            end
            item
              Alignment = taRightJustify
              Caption = '# of Literals'
              Width = 100
            end>
          ColumnClick = False
          ReadOnly = True
          RowSelect = True
          ParentBiDiMode = False
          SmallImages = ImageList
          SortType = stText
          TabOrder = 1
          ViewStyle = vsReport
          OnItemChecked = DomainsItemChecked
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Languages'
        ExplicitWidth = 0
        ExplicitHeight = 0
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
            Width = 107
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Select Languages'
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
            Width = 266
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Which translation languages are included in the export?'
            ShowAccelChar = False
          end
        end
        object Languages: TListView
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 414
          Height = 283
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 22
          Align = alClient
          BiDiMode = bdLeftToRight
          Checkboxes = True
          Columns = <
            item
              Caption = 'Language'
              Width = 275
            end
            item
              Alignment = taRightJustify
              Caption = '% of Progress'
              Width = 110
            end>
          ColumnClick = False
          ReadOnly = True
          RowSelect = True
          ParentBiDiMode = False
          SmallImages = ImageList
          SortType = stText
          TabOrder = 1
          ViewStyle = vsReport
          OnItemChecked = LanguagesItemChecked
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Options'
        ExplicitWidth = 0
        ExplicitHeight = 0
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
            Width = 142
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Choose Export Options'
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
            Width = 200
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'How the translations should be exported?'
            ShowAccelChar = False
          end
        end
        object gbDetails: TGroupBox
          AlignWithMargins = True
          Left = 22
          Top = 65
          Width = 414
          Height = 112
          Margins.Left = 22
          Margins.Top = 16
          Margins.Right = 22
          Margins.Bottom = 0
          Align = alTop
          Caption = ' Filters '
          TabOrder = 1
          object cbIgnoreDeveloperComments: TCheckBox
            AlignWithMargins = True
            Left = 18
            Top = 19
            Width = 378
            Height = 28
            Margins.Left = 16
            Margins.Top = 4
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Do not export developer'#39's comments'
            TabOrder = 0
          end
          object cbIgnoreTranslatorNotes: TCheckBox
            AlignWithMargins = True
            Left = 18
            Top = 47
            Width = 378
            Height = 28
            Margins.Left = 16
            Margins.Top = 0
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Do not export translator'#39's notes'
            TabOrder = 1
          end
          object cbUsablesOnly: TCheckBox
            AlignWithMargins = True
            Left = 18
            Top = 75
            Width = 378
            Height = 28
            Margins.Left = 16
            Margins.Top = 0
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Export usable translations only'
            TabOrder = 2
          end
        end
        object gbOutput: TGroupBox
          AlignWithMargins = True
          Left = 22
          Top = 190
          Width = 414
          Height = 164
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 22
          Align = alBottom
          Caption = ' Output '
          TabOrder = 2
          object lblBaseName: TLabel
            AlignWithMargins = True
            Left = 18
            Top = 109
            Width = 3
            Height = 13
            Margins.Left = 16
            Margins.Top = 2
            Margins.Right = 16
            Align = alTop
            FocusControl = edBaseName
          end
          object cbGroupByDomain: TCheckBox
            AlignWithMargins = True
            Left = 18
            Top = 49
            Width = 378
            Height = 28
            Margins.Left = 16
            Margins.Top = 2
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Export each text domain in a separate file'
            TabOrder = 1
            OnClick = ExportByGroupClick
          end
          object cbGroupByLanguage: TCheckBox
            AlignWithMargins = True
            Left = 18
            Top = 19
            Width = 378
            Height = 28
            Margins.Left = 16
            Margins.Top = 4
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Export each language in a separate file'
            TabOrder = 0
            OnClick = ExportByGroupClick
          end
          object cbCompressed: TCheckBox
            AlignWithMargins = True
            Left = 18
            Top = 79
            Width = 378
            Height = 28
            Margins.Left = 16
            Margins.Top = 2
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Export in compressed binary format'
            TabOrder = 2
          end
          object edBaseName: TComboBox
            AlignWithMargins = True
            Left = 18
            Top = 125
            Width = 378
            Height = 21
            Margins.Left = 16
            Margins.Top = 0
            Margins.Right = 16
            Margins.Bottom = 0
            Align = alTop
            BiDiMode = bdLeftToRight
            ParentBiDiMode = False
            TabOrder = 3
            OnChange = edBaseNamChange
            OnExit = edBaseNameExit
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Destination'
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
          Caption = 'Destination Folder:'
          FocusControl = Folders
          ExplicitWidth = 91
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
            Width = 112
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Select Destination'
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
            Width = 196
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Where the export files should be stored?'
            ShowAccelChar = False
          end
        end
        object Folders: TShellTreeView
          AlignWithMargins = True
          Left = 22
          Top = 105
          Width = 414
          Height = 249
          Margins.Left = 22
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 22
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
          Width = 407
          Height = 26
          Margins.Left = 22
          Margins.Top = 16
          Margins.Right = 22
          Margins.Bottom = 8
          Align = alTop
          Caption = 
            'Click <Export> to continue with the export, or click <Back> if y' +
            'ou want to review or change any settings.'
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
            Width = 101
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Ready to Export'
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
            Width = 277
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'Program is now ready to begin exporting the translations.'
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
          Width = 266
          Height = 13
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 4
          Align = alTop
          Caption = 'Please wait while the translations are being exported...'
          ShowAccelChar = False
          WordWrap = True
        end
        object Header6: TPanel
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
          object Title6: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Width = 60
            Height = 14
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alTop
            Caption = 'Exporting'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
          end
          object Subtitle6: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 184
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'The translations are being exported...'
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
          Margins.Top = 0
          Margins.Right = 22
          Margins.Bottom = 0
          Align = alTop
          Step = 1
          TabOrder = 1
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'After Process'
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Header7: TPanel
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
          object Title7: TLabel
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
          object Subtitle7: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 26
            Width = 209
            Height = 13
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 8
            Align = alTop
            Caption = 'The export process completed successfully.'
            ShowAccelChar = False
          end
        end
        object OutputFiles: TListView
          AlignWithMargins = True
          Left = 22
          Top = 71
          Width = 414
          Height = 283
          Margins.Left = 22
          Margins.Top = 22
          Margins.Right = 22
          Margins.Bottom = 22
          Align = alClient
          BiDiMode = bdLeftToRight
          Columns = <
            item
              Caption = 'Export File Name'
              Width = 385
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
          OnAdvancedCustomDraw = OutputFilesAdvancedCustomDraw
          OnDblClick = OutputFilesDblClick
          OnInfoTip = OutputFilesInfoTip
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
    end
    object btnStart: TButton
      Left = 253
      Top = 8
      Width = 95
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Export'
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
      'cbCompressed.Caption'
      'cbGroupByDomain.Caption'
      'cbGroupByLanguage.Caption'
      'cbIgnoreDeveloperComments.Caption'
      'cbIgnoreTranslatorNotes.Caption'
      'cbUsablesOnly.Caption'
      'Domains.Columns[0].Caption'
      'Domains.Columns[1].Caption'
      'Domains.Columns[2].Caption'
      'gbDetails.Caption'
      'gbOutput.Caption'
      'Languages.Columns[0].Caption'
      'Languages.Columns[1].Caption'
      'LastNotice.Caption'
      'lblFolder.Caption'
      'Notebook.Pages[0]'
      'Notebook.Pages[1]'
      'Notebook.Pages[2]'
      'Notebook.Pages[3]'
      'OutputFiles.Columns[0].Caption'
      'PleaseWait.Caption'
      'Subtitle1.Caption'
      'Subtitle2.Caption'
      'Subtitle3.Caption'
      'Subtitle4.Caption'
      'Subtitle5.Caption'
      'Subtitle6.Caption'
      'Subtitle7.Caption'
      'Title1.Caption'
      'Title2.Caption'
      'Title3.Caption'
      'Title4.Caption'
      'Title5.Caption'
      'Title6.Caption'
      'Title7.Caption')
    Translatables.Literals = (
      '94E6ED5916959B615F92C1D260609ED5={0} = File Name'
      '081B32A26196A8624265BE0A64F0093C={0} = File Name'
      '708EA0E601087E73C4764D5086E3EA3C'
      '021F6075292FBF1A590AA85F35040CD5'
      
        '0426C850B00673C4B775A92E9DDE607E=This string becomes part of a f' +
        'ile name template to indicate where the domain name will be inse' +
        'rted.'
      
        '45D05B099FE86EAD376A8BB551CB45C2=This string becomes part of a f' +
        'ile name template to indicate where the locale code will be inse' +
        'rted.'
      'EECFECE6F1C078EAC52777DF0BD10A87')
    OnLoaded = TranslatorLoaded
    Left = 375
    Top = 8
  end
  object ImageList: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Height = 20
    Left = 406
    Top = 8
    Bitmap = {
      494C0101020008008C0110001400FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
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
      00000000000000000000000000000000000000000000000000003535354C4D4D
      4D844D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D
      4D864D4D4D864D4D4D864D4D4D843535354C000000005151515CC9C9C9FFC9C9
      C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9
      C9FFC9C9C9FFCCCCCCFF5151515C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004D4D4D84EFEF
      EFF5FAFAFAFDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFAFAFAFDEDEDEDF34D4D4D8400000000CCCCCCFFF9F9F9FFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFF9F9F9FFCCCCCCFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000014F6054A2A0C7
      AAFF70AA7EFF599D6AFF71AB80FFA1C8AAFFD6E6DAFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFAFAFAFD4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000464F495D5C8C67DC72AA
      7AFFA5B48AFFBAB58CFF90B991FF589D6AFF77AD84FFC0D9C7FFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFDEDEDEFFDEDEDEFFDEDEDEFFD5D5D5FFD5D5D5FFD5D5D5FFD5D5D5FFD5D5
      D5FFFBFBFBFFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003236333C608E6CD17DC299FFAECA
      8DFFCEAD6CFFD9AB66FFA1A465FF6BB889FF68A67DFF76AC83FFD4E4D9FFFAFA
      FAFFFAFAFAFFFAFAFAFFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005A6F5F8E76B78CFF72C47AFFA9B4
      68FFD5A456FFE1A854FFBB974BFF89A054FF7AB187FF5A9B6BFFA0C7AAFFFAFA
      FAFFF8F8F8FFF8F8F8FFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFD5D5D5FFD5D5D5FFCCCCCCFFCCCCCCFFC9C9C9FFC9C9C9FFC9C9C9FFC9C9
      C9FFF8F8F8FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000060916DD88CD1A0FFAFD582FFC8AD
      61FFDBAA55FFD3A04EFFBF8843FFBF904DFF82A264FF79B995FF71AB81FFF9F9
      F9FFF9F9F9FFF8F8F8FFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF9F9F9FFF9F9
      F9FFF8F8F8FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000599D6BFE8EDB9EFF94D977FFCAC5
      81FFCDA557FFBFBB6EFFABCF6AFF9BB853FF62A450FF8EC8A8FF599D6AFFF9F9
      F9FFF6F6F6FFF6F6F6FFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFCCCCCCFFC9C9C9FFC9C9C9FFC2C2C2FFC2C2C2FFBBBBBBFFBBBBBBFFBBBB
      BBFFF8F8F8FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000061916ED893D7A2FFA0E78BFFD5EA
      BCFFCCB368FFC4C875FF7BDB59FF5DDA43FF5FC259FF87C19EFF71AA80FFF6F6
      F6FFF3F3F3FFF2F2F2FFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF8F8F8FFF2F2
      F2FFF2F2F2FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005A70608F84C29AFFBAF1AAFFC9EC
      B9FFD8D599FFD6BF78FFB6BB65FF92D66FFF8ADF90FF6AAB7EFF9EC4A7FFF2F2
      F2FFEFEFEFFFEDEDEDFFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFCFC
      FCFFC9C9C9FFC2C2C2FFBBBBBBFFBBBBBBFFB3B3B3FFB3B3B3FFABABABFFABAB
      ABFFEBEBEBFFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003236333C61906ED2ADE2C1FFCDF2
      CFFFD4E4BDFFDBDBA2FFDAC283FFD1C28BFF99CAA1FF75AD84FFCDDDD2FFECEC
      ECFFEAEAEAFFE6E6E6FFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF2F2F2FFF2F2F2FFEBEBEBFFEBEB
      EBFFE4E4E4FFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000464F495D5E8F6BDC90C3
      A2FFB8DABBFFD4DCB4FFB9CB9EFF83B281FF76AE85FFBAD4C1FFEBEBEBFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFF4D4D4D8600000000C9C9C9FFFCFCFCFFF9F9
      F9FFC2C2C2FFBBBBBBFFB3B3B3FFABABABFFABABABFFABABABFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFC9C9C9FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000014F6054A2A0C8
      AAFF70AB80FF599E6BFF71AB80FF9EC4A8FFCFDFD3FFF0F0F0FFEAEAEAFFFCFC
      FCFFF6F6F6FFF4F4F4FF6B6B6BBD2B2B2B3800000000C9C9C9FFFCFCFCFFF8F8
      F8FFF9F9F9FFF8F8F8FFF8F8F8FFF2F2F2FFF2F2F2FFEBEBEBFFFCFCFCFFF8F8
      F8FFF2F2F2FFC2C2C2FF5151515C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004D4D4D86F9F9
      F9FDF4F4F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFC
      FCFFE7E7E7FF6A6A6ABD2B2B2B380101010200000000C9C9C9FFFBFBFBFFF2F2
      F2FFF2F2F2FFF8F8F8FFF2F2F2FFF2F2F2FFEBEBEBFFEBEBEBFFFCFCFCFFE4E4
      E4FFC2C2C2FF5151515C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004D4D4D84E9E9
      E9F0F9F9F9FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8
      F8FF6A6A6ABD2B2B2B38010101020000000000000000CCCCCCFFF8F8F8FFFBFB
      FBFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FFC2C2
      C2FF5151515C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003535354B4D4D
      4D844D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D4D864D4D
      4D862B2B2B38010101020000000000000000000000005151515CCCCCCCFFC9C9
      C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FF5151
      515C000000000000000000000000000000000000000000000000000000000000
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
      C000800100000000C00080010000000080008001000000008000800100000000
      0000800100000000000080010000000000008001000000000000800100000000
      0000800100000000000080010000000000008001000000008000800100000000
      8000800100000000C000800300000000C001800700000000C003800F00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
