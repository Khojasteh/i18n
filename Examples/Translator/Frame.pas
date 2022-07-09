unit Frame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, i18nCore, i18nLocalizer;

type
  TSampleFrame = class(TFrame)
    RichEdit1: TRichEdit;
    RichEdit2: TRichEdit;
    Translator_of_SampleFrame: TTranslator;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
