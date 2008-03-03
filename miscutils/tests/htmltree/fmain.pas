unit fMain;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Windows}
  Windows, ShellApi,
  {$endif}
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, SynEdit, SynHighlighterPas, SynHighlighterHTML;

type

  THtmlTreeSampleProc = procedure(out SampleFile: String; Stream: TStream);

  { TFormMain }

  TFormMain = class(TForm)
    ButtonViewInBrowser: TButton;
    ListSamples: TListBox;
    PanelRight: TPanel;
    SplitterVertical: TSplitter;
    SplitterHorizontal: TSplitter;
    SynEditHtml: TSynEdit;
    SynEditCode: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    SynPasSyn1: TSynPasSyn;
    procedure ButtonViewInBrowserClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListSamplesSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 
  


var
  FormMain: TFormMain;

implementation

uses
  Samples;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ListSamples.Items.AddObject('Hello World', TObject(@SimpleHtmlTree));
  ListSamples.Items.AddObject('Raw List', TObject(@RawListHtmlTree));
  ListSamples.Items.AddObject('List', TObject(@ListHtmlTree));
  ListSamples.Items.AddObject('Table', TObject(@TableHtmlTree));
end;

procedure TFormMain.ButtonViewInBrowserClick(Sender: TObject);
begin
  SynEditHtml.Lines.SaveToFile(GetTempDir + 'html_tree_temp.html');
  {$ifdef Windows}
  ShellExecute(0, 'open', PChar(GetTempDir + 'html_tree_temp.html'), nil, nil, SW_SHOW);
  {$endif}
end;

procedure TFormMain.ListSamplesSelectionChange(Sender: TObject; User: boolean);
var
  SampleFile: String;
  SampleProc: THtmlTreeSampleProc;
  Stream: TMemoryStream;
begin
  if ListSamples.ItemIndex = -1 then
    Exit;

  Stream := TMemoryStream.Create;
  SampleProc := THtmlTreeSampleProc(ListSamples.Items.Objects[ListSamples.ItemIndex]);
  SampleProc(SampleFile, Stream);

  if FileExists(SampleFile) then
    SynEditCode.Lines.LoadFromFile(SampleFile);

  Stream.Position := 0;
  SynEditHtml.Lines.LoadFromStream(Stream);
  Stream.Destroy;
end;

initialization
  {$I fmain.lrs}

end.

