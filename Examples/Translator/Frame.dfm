object SampleFrame: TSampleFrame
  Left = 0
  Top = 0
  Width = 610
  Height = 291
  BiDiMode = bdLeftToRight
  ParentBiDiMode = False
  TabOrder = 0
  DesignSize = (
    610
    291)
  object RichEdit1: TRichEdit
    Left = 16
    Top = 16
    Width = 577
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ARABIC_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      #8220'All parts should go together without forcing.  You must remembe' +
      'r that the parts you are reassembling were disassembled by you.  There' +
      'fore, if you can'#8217't get them together again, there must be a re' +
      'ason.  By all means, do not use a hammer.'#8221
      #13#10
      '- IBM Manual, 1925')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object RichEdit2: TRichEdit
    Left = 16
    Top = 152
    Width = 577
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ARABIC_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      #8220'Mostly, when you see programmers, they aren'#8217't doing anything.  ' +
      'One of the attractive things about programmers is that you cannot tell whether' +
      'or not they are working simply by looking at them.  Very often they'#8217're' +
      'sitting there seemingly drinking coffee and gossiping, or just staring into sp' +
      'ace.  What the programmer is trying to do is get a handle on all the individua' +
      'l and unrelated ideas that are scampering around in his head.'#8221
      #13#10
      '- Charles M. Strauss')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Translator_of_SampleFrame: TTranslator
    Localizer = DM.Localizer
    Translatables.Properties = (
      'RichEdit1.Lines.Text'
      'RichEdit2.Lines.Text')
    Left = 144
    Top = 64
  end
end
