unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterPas, SynHighlighterHTML, SynEdit,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  THtmlTreeSampleProc = procedure(out SampleFile: String; Stream: TStream);

  { TMainViewForm }

  TMainViewForm = class(TForm)
    ButtonViewInBrowser: TButton;
    ListSamples: TListBox;
    PanelRight: TPanel;
    SplitterHorizontal: TSplitter;
    SplitterVertical: TSplitter;
    SynEditCode: TSynEdit;
    SynEditHtml: TSynEdit;
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
  MainViewForm: TMainViewForm;

implementation

uses
  LCLIntf, Samples;

{$R *.lfm}

procedure TMainViewForm.FormCreate(Sender: TObject);
begin
  ListSamples.Items.AddObject('Hello World', TObject(@SimpleHtmlTree));
  ListSamples.Items.AddObject('Raw List', TObject(@RawListHtmlTree));
  //todo
  //ListSamples.Items.AddObject('List', TObject(@ListHtmlTree));
  //ListSamples.Items.AddObject('Table', TObject(@TableHtmlTree));
end;

procedure TMainViewForm.ListSamplesSelectionChange(Sender: TObject; User: boolean);
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

procedure TMainViewForm.ButtonViewInBrowserClick(Sender: TObject);
begin
  SynEditHtml.Lines.SaveToFile(GetTempDir + 'html_tree_temp.html');
  OpenDocument(GetTempDir + 'html_tree_temp.html');
end;

end.

