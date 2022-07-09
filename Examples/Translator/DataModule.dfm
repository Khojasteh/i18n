object DM: TDM
  OldCreateOrder = False
  Height = 150
  Width = 299
  object Flags: TFlagImageList
    Left = 104
    Top = 16
  end
  object MessageDialog: TMessageDialog
    CheckBox.Caption = 'Do not show this message again'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Left = 192
    Top = 16
  end
  object Localizer: TLocalizer
    URI = 'dir:*.i18n'
    OnNotification = LocalizerNotification
    Left = 16
    Top = 16
    TragetCulture = 'en-US'
  end
  object Translator: TTranslator
    Localizer = Localizer
    Translatables.Properties = (
      'MessageDialog.ButtonCaptions.Abort'
      'MessageDialog.ButtonCaptions.All'
      'MessageDialog.ButtonCaptions.Cancel'
      'MessageDialog.ButtonCaptions.Close'
      'MessageDialog.ButtonCaptions.Help'
      'MessageDialog.ButtonCaptions.Ignore'
      'MessageDialog.ButtonCaptions.No'
      'MessageDialog.ButtonCaptions.NoToAll'
      'MessageDialog.ButtonCaptions.OK'
      'MessageDialog.ButtonCaptions.Retry'
      'MessageDialog.ButtonCaptions.Yes'
      'MessageDialog.ButtonCaptions.YesToAll'
      'MessageDialog.Captions.Confirmation'
      'MessageDialog.Captions.Error'
      'MessageDialog.Captions.Information'
      'MessageDialog.Captions.Warning'
      'MessageDialog.CheckBox.Caption')
    Translatables.Literals = (
      'DED20CCCE5F589FE69CF2810EA33FD23')
    Left = 16
    Top = 80
  end
end
